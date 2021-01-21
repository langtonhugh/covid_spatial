# Load packages.
library(cowplot)
library(lorenzgini)
library(spdep)
library(readr)
library(janitor)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)
library(sf)

# Useful function.
`%nin%` <- Negate(`%in%`)

# Archive data downloaded January 2012, covering the 3-year period up to and including November 2020.
# download.file(url = "https://data.police.uk/data/archive/2020-11.zip", destfile = "data/archive2020-11.zip")

# Unzip.
# unzip(zipfile = "data/archive2020-11.zip", exdir = "data")

# List all those 'street' files (rather than outcomes or stop and search).
list_2020 <- paste("data/", list.files("data", pattern = glob2rx("2020*street.csv"),  recursive=TRUE), sep = "")
list_2019 <- paste("data/", list.files("data", pattern = glob2rx("2019*street.csv"),  recursive=TRUE), sep = "")

# Read in .csv files for each year.
data_2020 <- lapply(list_2020, read_csv)
data_2019 <- lapply(list_2019, read_csv)

# Bind each in to data frames. For now, we keep the years separate.
full_data_2020 <- data_2020 %>% 
  bind_rows() %>% 
  clean_names() %>% 
  mutate(year = "2020")

full_data_2019 <- data_2019 %>% 
  bind_rows() %>% 
  clean_names() %>% 
  mutate(year = "2019")

# Create year ID, then remove Greater Manchester Police and Police Service of NI.
sub_data_2020 <- full_data_2020 %>% 
  filter(reported_by != "Greater Manchester Police", 
         reported_by != "Police Service of Northern Ireland")

sub_data_2019 <- full_data_2019 %>% 
  filter(reported_by != "Greater Manchester Police", 
         reported_by != "Police Service of Northern Ireland")

# Remove existing data objects to free up memory if needed.
rm(data_2019, data_2020, full_data_2019, full_data_2020)

# Check months.
unique(sub_data_2020$month)
unique(sub_data_2019$month)

# Check crime types.
unique(sub_data_2020$crime_type)
unique(sub_data_2019$crime_type)

# Check missings.
sum(is.na(sub_data_2020$month))
sum(is.na(sub_data_2019$month))

sum(is.na(sub_data_2020$crime_type))
sum(is.na(sub_data_2019$crime_type))

sum(is.na(sub_data_2020$lsoa_code)) # ~98k crimes have no LSOA.
sum(is.na(sub_data_2019$lsoa_code)) # ~161k crimes have no LSOA.

# Drop crimes with missing LSOA (!!!).
sub_data_2020 <- drop_na(sub_data_2020, lsoa_code)
sub_data_2019 <- drop_na(sub_data_2019, lsoa_code)

# Aggregate by month (N = 11), crime type (N = 14), and LSOA (N = ~33,000). Keep 'Year' in for later use.
sub_data_agg_2020 <- sub_data_2020 %>% 
  group_by(crime_type, month, lsoa_code, year) %>% 
  summarise(crime_count = n()) %>% 
  ungroup() %>% 
  complete(crime_type, month, lsoa_code, year, fill = list(crime_count = 0))

sub_data_agg_2019 <- sub_data_2019 %>% 
  group_by(crime_type, month, lsoa_code, year) %>% 
  summarise(crime_count = n()) %>% 
  ungroup() %>% 
  complete(crime_type, month, lsoa_code, year, fill = list(crime_count = 0))

# Check number of each LSOAs appearing in each year. There is a difference.
# This will be because some LSOAs had no recorded crime, and thus never even appeared in individual records.
# Note that we DO have zeros in this data for some LSOAs in some months, but that will be because there was
# a crime recorded in those LSOA in another month. It's possible there is a lack of snap poits in some LSOA, too.
length(unique(sub_data_agg_2020$lsoa_code))
length(unique(sub_data_agg_2019$lsoa_code))

# To create zeros for the no-show LSOAs, first load in a complete set of LSOA codes. 

# Download LSOA from UK Data Service.
# download.file(url = "https://borders.ukdataservice.ac.uk/ukborders/easy_download/prebuilt/shape/infuse_lsoa_lyr_2011_clipped.zip",
#               destfile = "data/ukds_infuse_lsoa.zip")

# Unzip.
# unzip(zipfile = "data/ukds_infuse_lsoa.zip", exdir = "data/ukds_infuse_lsoa")

# Load.
lsoa_sf <- st_read("data/ukds_infuse_lsoa/infuse_lsoa_lyr_2011_clipped.shp")

# Subset LSOAs for E&W and then remove Greater Manchester.
lsoa_ew_sf <- lsoa_sf %>% 
  mutate(country_cd = str_extract(geo_code, "^.{1}")) %>% 
  filter(country_cd == "E" | country_cd == "W",
         !str_detect(geo_label, "Manchester|Bolton|Oldham|Rochdale|Stockport|Tameside|Salford|Bury|Trafford|Wigan"))

# Check validity of the remaining LSOAs. QGIS exploration indicated that there were four.
validity_check <- st_is_valid(lsoa_ew_sf)

# This confirms the four.
table(validity_check)

# Identify them
lsoa_ew_sf <- lsoa_ew_sf %>% 
  mutate(valid = validity_check)

filter(lsoa_ew_sf, valid == "FALSE")

# Try to resolve invalid geometries.
lsoa_ew_valid_sf <- st_make_valid(lsoa_ew_sf)

# Check. It worked.
table(st_is_valid(lsoa_ew_valid_sf))

# Remove objects to save space.
rm(sub_data_2019, sub_data_2020)

# There are multiple islands in the dataset, but most are not considered a problem, since they are LSOAs
# which share areas of the mainland or part of major islands (e.g. Isle of Wight). Of course,
# crimes on the islands cannot be considered 'neighbours' with those on the shore, but so few crimes
# actually occurr on these islands that it was deemed less of a problem than arbitarily removing them.
# That said, one island (Scilly) has no corresponding LSOA on the mainland, and thus would have no 
# neighbours in a continuity matrix. This becomes apparent later - just making a note for now.

# First, we identify LSOA within Northern Ireland and Greater Manchester. We have removed those crimes reported by
# these forces, but for consistency we also ensure that any crimes reported by other forces falling within
# their boundaries are removed.

gm_lsoa <- lsoa_sf %>% 
  filter(str_detect(geo_label, "Manchester|Bolton|Oldham|Rochdale|Stockport|Tameside|Salford|Bury|Trafford|Wigan")) %>% 
  select(geo_code) %>% 
  pluck(1)

ni_lsoa <- lsoa_sf %>%
  mutate(country_cd = str_extract(geo_code, "^.{1}")) %>% 
  filter(country_cd == "9") %>% 
  select(geo_code) %>% 
  pluck(1)

# We can now remove lsoa_sf to save space.
rm(lsoa_sf)

# Remove these LSOA from the crime data.
sub_data_agg_2020 <- sub_data_agg_2020 %>% 
  filter(lsoa_code %nin% gm_lsoa, lsoa_code %nin% ni_lsoa)

sub_data_agg_2019 <- sub_data_agg_2019 %>% 
  filter(lsoa_code %nin% gm_lsoa, lsoa_code %nin% ni_lsoa)

# Check that the crime data also doesn't include LSOAs from outside of England and Wales.
sub_data_agg_2020 <- sub_data_agg_2020 %>% 
  mutate(country_cd = str_extract(lsoa_code, "^.{1}"))

table(sub_data_agg_2020$country_cd) # Confirmed.

sub_data_agg_2019 <- sub_data_agg_2019 %>% 
  mutate(country_cd = str_extract(lsoa_code, "^.{1}"))

table(sub_data_agg_2019$country_cd) # Confirmed.

# Check LSOA in each. Confirms that police data has slightly less (N = 4) LSOA than the total data.
length(unique(lsoa_ew_valid_sf$geo_code))
length(unique(sub_data_agg_2019$lsoa_code))
length(unique(sub_data_agg_2020$lsoa_code))

# Pull out these 4 LSOA (for 2019 and 2020).
# All four are in London, so it's highly likely this is a snap point quirk, rather than due to zero crimes.
missing_lsoa <- lsoa_ew_sf %>% 
  filter(lsoa_ew_sf$geo_code %nin% sub_data_agg_2020$lsoa_code) # Could also be sub_data_agg_2019

# E01033493 is a football stadium. No roads therefore no snap points.
# E01032775 only covers ends of streets near river - may have missed the crime snap points.
# E01004711 has some construction work, and a break in Google Street View availability (on 11 January 2012).
#           This indicates another snap point street quirk.
# E01003179  appears to not contain any car-worthy roads are could therefore miss snap points.

# Conclusion: it would be misleading to say these had 'zero' crimes, so we will simply not include them
# in any analysis. The polygons are removed from the LSOA data too.
lsoa_ew_valid_sf <- lsoa_ew_valid_sf %>% 
  filter(geo_code %nin% missing_lsoa$geo_code)

# Check number of LSOA in each dataset again. Now identical.
length(unique(lsoa_ew_valid_sf$geo_code))    # 33076
length(unique(sub_data_agg_2019$lsoa_code))  # 33076
length(unique(sub_data_agg_2020$lsoa_code))  # 33076

# Load in urban-rural classification.
urban_df <- read_csv("data/Rural_Urban_Classification__2011__of_Lower_Layer_Super_Output_Areas_in_England_and_Wales.csv")

# Clean names
urban_df <- urban_df %>% 
  clean_names()

# Check classications.
table(urban_df$ruc11)
table(urban_df$ruc11cd)

# Create a broader urban and rural distinction.
urban_df <- urban_df %>% 
  mutate(urban_rural = if_else(condition = str_detect(string = ruc11, pattern = "Rural"), true = "Rural", false = "Urban"))

# Check classications.
table(urban_df$urban_rural)

# Append to the crime data.
sub_data_agg_2020 <- left_join(sub_data_agg_2020, urban_df, by = c("lsoa_code" = "lsoa11cd"))
sub_data_agg_2019 <- left_join(sub_data_agg_2019, urban_df, by = c("lsoa_code" = "lsoa11cd"))

# Check missings.
sum(is.na(sub_data_agg_2020$urban_rural))
sum(is.na(sub_data_agg_2019$urban_rural))

# Append to spatial data.
lsoa_ew_valid_sf <- left_join(lsoa_ew_valid_sf, urban_df, by = c("geo_code" = "lsoa11cd"))

# Create notifiable offences (minus Drugs) df for each April.
notif_off_lsoa_df <- sub_data_agg_1920_df %>% 
  filter(crime_type != "Anti-social behaviour" & crime_type != "Drugs",
         month == "2020-04" | month == "2019-04") %>% 
  group_by(lsoa_code, month) %>% 
  summarise(ew_crime_count = sum(crime_count)) %>% 
  ungroup() %>% 
  mutate(crime_type = "Total crime") %>% 
  pivot_wider(id_cols = lsoa_code, names_from = month, values_from = ew_crime_count) %>% 
  rename(april19 = `2019-04`,
         april20 = `2020-04`)
  
# Append to sf object.
lsoa_ew_valid_not_off_sf <- left_join(lsoa_ew_valid_sf, notif_off_lsoa_df, by = c("geo_code" = "lsoa_code"))

# Save for use in GeoDa.
st_write(obj = lsoa_ew_valid_not_off_sf, dsn = "data/")

# Combine 2019 and 2029 datasets for next steps.
sub_data_agg_1920_df <- bind_rows(sub_data_agg_2019, sub_data_agg_2020)

# Remove objects to retain memory if needed.
rm(lsoa_ew_sf, sub_data_agg_2019, sub_data_agg_2020)

# Raw counts plot, comparing trends across years, similar to first Crime Science paper.

# First, create a small, identical data frame with total notifiable offences as a 'crime type' (excluding drugs).
total_crime_agg_df <- sub_data_agg_1920_df %>%
  filter(crime_type != "Anti-social behaviour" & crime_type != "Drugs") %>% 
  group_by(month, year) %>% 
  summarise(ew_crime_count = sum(crime_count)) %>% 
  ungroup() %>% 
  mutate(crime_type = "Total crime (excl. drugs)") %>% 
  select(crime_type, year, month, ew_crime_count) %>% 
  separate(month, into = c("year", "month"), sep = "-") %>% 
  filter(month != "01", month != "09", month != "10", month != "11", month != "12") 
  
# Then calculate these counts by crime type.
raw_counts_gg <- sub_data_agg_1920_df %>% 
  group_by(crime_type, month, year) %>% 
  summarise(ew_crime_count = sum(crime_count)) %>% 
  ungroup() %>% 
  separate(month, into = c("year", "month"), sep = "-") %>% 
  filter(month != "01", month != "09", month != "10", month != "11", month != "12") %>% 
  bind_rows(total_crime_agg_df) %>% 
  mutate(year = recode_factor(year, "2019" = "2019", "2020" = "2020")) %>% 
  ggplot() +
  geom_line(mapping = aes(x = month, y = ew_crime_count, group = year, colour = year), size = 0.8) +
  geom_vline(xintercept = 1.7, linetype = "dotted") +
  facet_wrap(~ crime_type, ncol = 3, scales = "free_y") +
  scale_x_discrete(labels = c(str_extract(month.name[3:8], "^.{3}"), character(1))) +
  scale_color_manual(values = rev(c("black", "darkgrey"))) +
  labs(x = NULL, y = NULL, colour = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, hjust = -0.5),
        axis.ticks = element_line(size = 0.3, lineend = "round"),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 8),
        strip.background = element_rect(fill = "transparent"),
        legend.position = "bottom")

# Save.
ggsave(plot = raw_counts_gg, filename = "visuals/raw_counts_gg.png", width = 16, height = 20, units = "cm", dpi = 600)

# Calculate E&W-wide Generalized Gini coefficient for notifiable offences, for 2019 and 2020.
gini_tot_1920_df <- sub_data_agg_1920_df %>%
  filter(crime_type != "Anti-social behaviour" & crime_type != "Drugs") %>% 
  group_by(year, month) %>% 
  summarise(gini_coef = gini(crime_count, generalized = TRUE, unbiased = TRUE)) %>% 
  ungroup() %>% 
  separate(month, into = c("year","month"), sep = "-") %>% 
  mutate(crime_type = "Total crime (excl. drugs)") %>% 
  select(crime_type, gini_coef, year,  month)

# Do the same but by crime type.
gini_ct_1920_df <- sub_data_agg_1920_df %>%
  group_by(year, month, crime_type) %>% 
  summarise(gini_coef = gini(crime_count, generalized = TRUE, unbiased = TRUE)) %>% 
  ungroup() %>% 
  separate(month, into = c("year","month"), sep = "-") %>%
  bind_rows(gini_tot_1920_df) %>% 
  filter(month != "01", month != "09", month != "10", month != "11", month != "12") %>% 
  mutate(year = recode_factor(year, "2019" = "2019", "2020" = "2020")) 

# Facet plot.
gini_gg <- ggplot(data = gini_ct_1920_df) +
  geom_line(mapping = aes(x = month, y = gini_coef, group = year, colour = year), size = 0.8) +
  geom_vline(xintercept = 1.7, linetype = "dotted") +
  facet_wrap(~ crime_type, ncol = 3) +
  ylim(0, 1) +
  labs(x = NULL, y = "Generalized Gini Coefficient", colour = NULL) +
  scale_x_discrete(labels = c(str_extract(month.name[3:8], "^.{3}"), character(1))) +
  scale_color_manual(values = rev(c("black", "darkgrey"))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, hjust = -0.5),
        axis.ticks = element_line(size = 0.3, lineend = "round"),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 8),
        strip.background = element_rect(fill = "transparent"),
        legend.position = "bottom")

# Save.
ggsave(plot = gini_gg, filename = "visuals/gini_gg.png", width = 14, height = 20, units = "cm", dpi = 600)

# Save and load workspace as appropriate.
# save.image(file = "data_handling.RData")
load(file = "data_handling.RData")

# Use longitudinal k-means to unpick the total noficiable crimes (excl. drugs trend) at LSOA level.
total_crime_kmeans_df <- sub_data_agg_1920_df %>%
  filter(crime_type != "Anti-social behaviour" & crime_type != "Drugs",
         month != "2019-12") %>% 
  group_by(lsoa, month, year) %>% 
  summarise(ew_crime_count = sum(crime_count)) %>% 
  ungroup() %>% 


# We know that the most change occurred in April, across all crimes types.
# But what areas were driving this change? Was it previously criminal areas?
# Here, we calculate which LSOAs are driving the change observed in April.

# Note that we create a count+1 variable for the Poisson test.
aprils_lsoa_df <- sub_data_agg_1920_df %>%
  filter(crime_type != "Anti-social behaviour" & crime_type != "Drugs",
         month == "2019-04" | month == "2020-04") %>%
  group_by(lsoa_code, month) %>% 
  summarise(cc_total   = sum(crime_count)) %>% 
  mutate(cc_total_plus = cc_total+1) %>% 
  group_by(month) %>%
  mutate(decile = ntile(cc_total_plus, 10)) %>%
  ungroup() %>%
  arrange(decile)

# How much have the deciles changed?
aprils_lsoa_wide_df <- aprils_lsoa_df %>%
  pivot_wider(id_cols = lsoa_code, names_from = "month", values_from = "decile") %>% 
  rename(april19 = `2019-04`,
         april20 = `2020-04`) %>% 
  mutate(change_or_not = if_else(april19 == april20, "no_change", "changed"),
         up_or_not     = if_else(april19 < april20 , "up_decile", "down_or_no_change"))

table(aprils_lsoa_wide_df$change_or_not)
round(prop.table(table(aprils_lsoa_wide_df$change_or_not)), 2)

table(aprils_lsoa_wide_df$up_or_not)
round(prop.table(table(aprils_lsoa_wide_df$up_or_not)), 2)

# Check number of deciles. Each LSOA has been allocated to a decile for April 2019 and 2020 respectively.
table(aprils_lsoa_df$decile) 

# Calculate mean counts occuring in each decile, in each month.
decile_means_df <- aprils_lsoa_df %>% 
  group_by(decile, month) %>% 
  summarise(mean_dec_cc_plus = mean(cc_total_plus)) %>% 
  mutate(mean_dec_cc_plus = round(mean_dec_cc_plus)) %>% 
  arrange(month, decile)

# Check visually.
ggplot(data = decile_means_df) +
  geom_bar(mapping = aes(x = decile, y = mean_dec_cc_plus), stat = "identity") +
  facet_wrap(~ month)

# Now we need to link the LSOAs in each decile from 2019, to the counts for 2020.
april19_df <- aprils_lsoa_df %>% 
  filter(month == "2019-04")

april20_df <- aprils_lsoa_df %>% 
  filter(month == "2020-04") %>% 
  select(lsoa_code, month, cc_total, cc_total_plus) %>% 
  rename(cc_total_20      = cc_total,
         cc_total_plus_20 = cc_total_plus,
         month_20         = month)

# Join back with the 2019 deciles.
april19_joined_df <- left_join(april19_df, april20_df)

# Check distirbutions.
ggplot(data = filter(april19_joined_df, cc_total_plus < 125)) +
  geom_histogram(mapping = aes(x = cc_total_plus), bins = 100) 

ggplot(data = filter(april19_joined_df, cc_total_plus_20 < 125)) +
  geom_histogram(mapping = aes(x = cc_total_plus_20), bins = 100) 

# Create mean counts for each April (2019 and 2020) by the 2019 deciles.
# Removing outliers >125 does not change results.
decile_means_19 <- april19_joined_df %>%
  group_by(decile) %>% 
  summarise(mean_dec_count19 = round(mean(cc_total_plus)))

decile_means_20 <- april19_joined_df %>% 
  group_by(decile) %>% 
  summarise(mean_dec_count20 = round(mean(cc_total_plus_20))) %>% 
  select(-decile)

# Bind together, then into list, for poisson test.
decile_means_df <- bind_cols(decile_means_19, decile_means_20)
decile_means_list <- group_split(decile_means_df, decile)

perc_changes <- paste(round(100*(decile_means_df$mean_dec_count20-decile_means_df$mean_dec_count19)/decile_means_df$mean_dec_count19),
                      "%", sep = "")

# Run poisson text through list.
lapply(decile_means_list, function(x){poisson.test(c(x$mean_dec_count19, x$mean_dec_count20))})

# Visualise the change descriptively. 
deciles_tot_gg <- april19_joined_df %>%
  select(-month, -cc_total, -cc_total_20, -month_20) %>%
  pivot_longer(cols = c(-lsoa_code, -decile), values_to = "counts", names_to = "year") %>% 
  group_by(year, decile) %>% 
  summarise(mean_counts = round(mean(counts))) %>% 
  ggplot() +
  geom_bar(mapping = aes(x = as.factor(decile), y = mean_counts, group = year, fill = year),
           stat = "identity", position = "dodge") +
  scale_fill_discrete(labels = c("April 2019", "April 2020"), direction = -1) +
  labs(fill = NULL,  y = "Mean recorded crimes", x = "LSOA crime decile in April 2019",
       title = "Notifiable offences in England and Wales", caption = "Drug offences excluded") +
  theme_bw()

deciles_tot_an_gg <- deciles_tot_gg +
  annotate(geom = "curve", x = 0.5, xend = 9.5, y = 25, yend = 25, curvature = 0) +
  annotate(geom = "curve", x = 0.5, xend = 0.5, y = 25, yend = 23, curvature = 0) +
  annotate(geom = "curve", x = 9.5, xend = 9.5, y = 25, yend = 23, curvature = 0) +
  annotate(geom = "text" , x = 4, y = 27, label = "No stat. sig. change", size = 3) +
  annotate(geom = "curve", x = 8.5, xend = 10, y = 35, yend = 23, curvature = -0.2, arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text" , x = 7.4, y = 36, label = "Stat. sig. change", size = 3) +
  annotate(geom = "text" , x = 1 , y = 6, label = paste("+", perc_changes[1], sep = ""), size = 3) +
  annotate(geom = "text" , x = 2 , y = 7, label = paste("+", perc_changes[2], sep = ""), size = 3) +
  annotate(geom = "text" , x = 3 , y = 8, label = paste("+", perc_changes[3], sep = ""), size = 3) +
  annotate(geom = "text" , x = 4 , y = 9, label = perc_changes[4] , size = 3) +
  annotate(geom = "text" , x = 5 , y = 10, label = perc_changes[5] , size = 3) +
  annotate(geom = "text" , x = 6 , y = 12, label = perc_changes[6] , size = 3) +
  annotate(geom = "text" , x = 7 , y = 14, label = perc_changes[7] , size = 3) +
  annotate(geom = "text" , x = 8 , y = 17, label = perc_changes[8] , size = 3) +
  annotate(geom = "text" , x = 9 , y = 22, label = perc_changes[9] , size = 3) +
  annotate(geom = "text" , x = 10, y = 46, label = perc_changes[10], size = 3) 
  
ggsave(plot = deciles_tot_an_gg, filename = "visuals/deciles_tot_an_gg.png", height = 12, width = 18, unit = "cm")

# Create this crime distinciton in the main df.
crime_cats_1920_df <- sub_data_agg_1920_df %>% 
  filter(crime_type != "Drugs") %>% 
  mutate(crime_cat = if_else(condition = crime_type != "Anti-social behaviour", "Notifiable", crime_type))

# Spearman's rank between deciles.
cor_prep_list <- sub_data_agg_1920_df %>% 
  filter(month == "2019-04" | month == "2020-04") %>% 
  select(crime_type, month, lsoa_code, crime_count) %>% 
  group_split(crime_type) 

# Calculate Spearman's Rank correlation *between months of different years*.
table(sub_data_agg_1920_df$month)

# Remove December 2019 (until December 2020 is released), and split into list of df by crime type.
cor_prep_list <- sub_data_agg_1920_df %>% 
  filter(month != "2019-12") %>% 
  select(crime_type, month, lsoa_code, crime_count) %>% 
  group_split(crime_type) 

# Add names.
names(cor_prep_list) <- unique(sub_data_agg_1920_df$crime_type)

# Function to pivot each df in list long to wide.
cor_prep_fun <- function(x) {
  x %>% 
    select(-crime_type) %>% 
    pivot_wider(id_cols = lsoa_code, names_from = month, values_from = crime_count) %>% 
    select(-lsoa_code)
}

# Run function through list of df.
cor_output_list <- lapply(cor_prep_list, cor_prep_fun)

# Spearman's rank correlation on each.
cor_results_list <- lapply(cor_output_list, function(x){as_tibble(round(cor(x = x, method = "spearman"), 2))})

# Create month variable for each correlation table.
cor_results_pull_list <- lapply(cor_results_list, function(x){x %>%
    mutate(month_lag = names(.)) %>% 
    relocate(month_lag, .before = `2019-01`) %>% 
    slice(12:22) %>% 
    select(1:12) })

# Save each.
names(cor_results_pull_list) 
for(i in seq_along(cor_results_pull_list))
  write_csv(x = cor_results_pull_list[[i]], path = paste0("results/", names(cor_results_pull_list)[i], "_month_cor", ".csv"))

# # Retrieve the diagnal, which compared the same months of different years.
# cor_results_sub_list <- lapply(cor_results_list, function(x){diag(as.matrix(x[, -1]))})
# 
# # Stick together and create id column. Rows are in the order of months.
# cor_results_sub_df <- cor_results_sub_list %>% 
#   bind_rows() %>%
#   mutate(cor_compare = names(cor_results_list[[1]]))
# 
# 
# # Heat map does not work due to varying degrees of correlation by crime type. It masks
# # changes which -by crime type- are quite important.
# df <- cor_results_sub_df %>% 
#   mutate(months_id = month.name[1:11]) %>% 
#   pivot_longer(cols = "crime_type")

# Now move to MSOA. We are not pretending to do 'micro crime analysis', it's a national overview, so it
# will help in almost every aspect (e.g. zero counts, computation, maps).

# Download MSOA look-up table from ONS.
# download.file(url = "https://opendata.arcgis.com/datasets/fe6c55f0924b4734adf1cf7104a0173e_0.csv",
#               destfile = "data/census_unit_lookup.csv")

# Load.
lookup_df <- read_csv("data/census_unit_lookup.csv")

# Only keep unique LSOA, subset for those in the crime data, and select columns needed.
lookup_lsoa_df <- lookup_df %>% 
  clean_names() %>% 
  distinct(lsoa11cd, .keep_all = TRUE) %>% 
  filter(lsoa11cd %in% sub_data_agg_1920_df$lsoa_code) %>% 
  select(lsoa11cd, msoa11cd, msoa11nm, lad17cd, lad17nm) %>% 
  rename(msoa_code = msoa11cd)

# Join with crime data, and aggregate counts by MSOA.
msoa_agg_1920_df <- sub_data_agg_1920_df %>% 
  left_join(lookup_lsoa_df, by = c("lsoa_code" = "lsoa11cd")) %>%
  group_by(crime_type, year, month, msoa_code) %>% 
  summarise(cc_msoa = sum(crime_count)) %>% 
  ungroup()

# Recalculate Gini for MSOA units.
gini_msoa_tot_1920_df <- msoa_agg_1920_df %>%
  filter(crime_type != "Anti-social behaviour") %>% 
  group_by(year, month) %>% 
  summarise(gini_coef = gini(cc_msoa, generalized = TRUE, unbiased = TRUE)) %>% 
  ungroup() %>% 
  separate(month, into = c("year","month"), sep = "-") %>% 
  mutate(crime_type = "Total crime") %>% 
  select(crime_type, gini_coef, year,  month)

# Do the same but by crime type.
gini_ct_msoa_1920_df <- msoa_agg_1920_df %>%
  group_by(year, month, crime_type) %>% 
  summarise(gini_coef = gini(cc_msoa, generalized = TRUE, unbiased = TRUE)) %>% 
  ungroup() %>% 
  separate(month, into = c("year","month"), sep = "-") %>%
  bind_rows(gini_msoa_tot_1920_df) %>% 
  filter(month != "01", month != "09", month != "10", month != "11", month != "12") %>% 
  mutate(year = recode_factor(year, "2019" = "2019", "2020" = "2020")) 

# Facet plot.
gini_msoa_gg <- ggplot(data = gini_ct_msoa_1920_df) +
  geom_line(mapping = aes(x = month, y = gini_coef, group = year, colour = year), size = 0.8) +
  geom_vline(xintercept = 1.7, linetype = "dotted") +
  facet_wrap(~ crime_type, ncol = 3) +
  ylim(0, 1) +
  labs(x = NULL, y = NULL, colour = NULL) +
  scale_x_discrete(labels = c(str_extract(month.name[3:8], "^.{3}"), character(1))) +
  scale_color_manual(values = rev(c("black", "darkgrey"))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6, hjust = -0.5),
        axis.ticks = element_line(size = 0.3, lineend = "round"),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 8),
        strip.background = element_rect(fill = "transparent"),
        legend.position = "bottom")

# Save.
ggsave(plot = gini_msoa_gg, filename = "visuals/gini_msoa_gg.png", width = 14, height = 20, units = "cm", dpi = 600)

# Deciles.
aprils_msoa_df <- msoa_agg_1920_df %>%
  filter(crime_type == "Theft from the person", 
         month == "2019-04" | month == "2020-04") %>%
  group_by(month) %>%
  arrange(cc_msoa) %>%
  mutate(decile = ntile(cc_msoa, 10)) %>%
  ungroup() %>%
  arrange(decile)

aprils_msoa_df %>% 
  group_by(decile) %>% 
  summarise(mean_cc = mean(cc_msoa))

# Long to wide for comparison.
aprils_msoa_wide_df <- aprils_msoa_df %>% 
  pivot_wider(id_cols = msoa_code, names_from = month, values_from = c(cc_msoa, decile)) %>% 
  rename(cc_april19 = `cc_msoa_2019-04`,
         cc_april20 = `cc_msoa_2020-04`,
         dc_april19 = `decile_2019-04`,
         dc_april20 = `decile_2020-04`) %>% 
  mutate(cc_diff    = cc_april20-cc_april19,
         perc_diff  = 100*(cc_diff/cc_april19)) 


ggplot(data = aprils_msoa_wide_df) +
  geom_boxplot(mapping = aes(x = as.factor(dc_april19),
                             y = cc_diff,
                             group = dc_april19), outlier.shape = NA) +
  geom_jitter(mapping = aes(x = as.factor(dc_april19),
                            y = cc_diff,
                            group = dc_april19,
                            colour = as.factor(dc_april19)),
              alpha = 0.3, pch = 20, size = 1) +
  theme(legend.position = "none") +
  scale_y_continuous(limits = c(-20, 5))

# p1 <- ggplot(data = filter(aprils_msoa_wide_df, perc_diff < 1000)) +
#   geom_boxplot(mapping = aes(x = as.factor(dc_april19), y = perc_diff, group = dc_april19))
# 
# p2 <- ggplot(data = filter(aprils_msoa_wide_df, perc_diff < 1000)) +
#   geom_boxplot(mapping = aes(x = as.factor(dc_april19), y = cc_diff, group = dc_april19))

ggplot(data = filter(aprils_msoa_wide_df)) +
  geom_boxplot(mapping = aes(x = as.factor(dc_april19), y = cc_diff, group = dc_april19))

april_change_gg <- plot_grid(p1, p2, nrow = 2)

ggsave(plot = april_change_gg, filename = "visuals/april_change_gg.png", width = 10, height = 10, unit = "cm")  

# Calculate percentage changes in crime for rural/urban classifications.
aprils_lsoa_df <- sub_data_agg_1920_df %>%
  filter(crime_type == "Anti-social behaviour", 
         month == "2019-04" | month == "2020-04") %>%
  group_by(month) %>%
  arrange(crime_count) %>%
  mutate(decile = ntile(crime_count, 10)) %>%
  ungroup() %>%
  arrange(decile)

aprils_lsoa_df %>% 
  group_by(decile) %>% 
  summarise(mean_cc = mean(crime_count))

ggplot(data = aprils_lsoa_df) +
  geom_bar(mapping = aes(x = as.factor(decile), y = crime_count), stat = "identity") +
  facet_wrap(~month)

# Calculate difference between April 2019 and April 2020 for each LSOA.
aprils_lsoa_wide_df <- aprils_lsoa_df %>% 
  select(crime_type, month, lsoa_code, urban_rural, crime_count, decile) %>% 
  pivot_wider(id_cols = lsoa_code, names_from = month, values_from = c(crime_count, decile)) %>%
  rename(cc_april19 = `crime_count_2019-04`,
         cc_april20 = `crime_count_2020-04`,
         dc_april19 = `decile_2019-04`,
         dc_april20 = `decile_2020-04`) %>%
  mutate(diff_april = cc_april20-cc_april19) %>%
  left_join(urban_df, by = c("lsoa_code" = "lsoa11cd")) %>% 
  group_by(dc_april19) %>% 
  summarise(mean_diff_april = mean(diff_april)) %>% 
  ungroup()

ggplot(data = aprils_lsoa_wide_df) +
  geom_bar(mapping = aes(x = as.factor(dc_april19), y = mean_diff_april), stat = "identity")
  
table(aprils_lsoa_wide_df$cc_april19) # 7000 LSOAs have zero ASB in April 2019 (~21%).
table(aprils_lsoa_wide_df$cc_april20) # 3300 LSOAs have zero ASB in April 2020.

# Descriptives
# aprils_lsoa_wide_df %>% 
#   group_by(ruc11) %>%
#   summarise(mean19 = mean(`2019-04`),
#             mean20 = mean(`2020-04`))
# 
# aprils_lsoa_wide_df %>% 
#   group_by(ruc11) %>%
#   summarise(diff19 = mean(`2020-04`-`2019-04`))
# 
# poisson.test(x = c(1, 32), T = c(1,1), alternative = "greater")


# Join with the sf object.
total_crime_lsoa_sf <- left_join(lsoa_ew_valid_sf, total_crime_lsoa_df, by = c("geo_code" = "lsoa_code"))

# Save for checks in GeoDa.
st_write(obj = total_crime_lsoa_sf, dsn = "data/total_crime_lsoa_sf.shp")

# Check distribution for 2019.
ggplot(data = total_crime_lsoa_sf) +
  geom_histogram(mapping = aes(x = april_2019), bins = 60)

mean(total_crime_lsoa_sf$april_2019)   # 12
median(total_crime_lsoa_sf$april_2019) # 8
table(total_crime_lsoa_sf$april_2019)  # 625 zeros

# Check distribution for 2020/
ggplot(data = total_crime_lsoa_sf) +
  geom_histogram(mapping = aes(x = april_2020), bins = 60)

mean(total_crime_lsoa_sf$april_2020)   # 9
median(total_crime_lsoa_sf$april_2020) # 6
table(total_crime_lsoa_sf$april_2020)  # 1142 zeros

# Create new variable with high outliers as missing. 
total_crime_lsoa_sf <- total_crime_lsoa_sf %>% 
  mutate(april_no_2019 = replace(x = april_2019, april_2019 > 125, NA),
         april_no_2020 = replace(x = april_2020, april_2020 > 125, NA)) %>%
  filter(april_no_2019 != 0)

ggplot(data = total_crime_lsoa_sf) +
  geom_histogram(mapping = aes(x = log(april_no_2019)))


# Compute Global Moran's I for each April 2019/20.
moran.test(x = total_crime_lsoa_sf$april_2019, listw = lsoa_listW, zero.policy = TRUE, na.action = na.omit)
moran.test(x = total_crime_lsoa_sf$april_2020, listw = lsoa_listW, zero.policy = TRUE, na.action = na.omit)

# Create scatterplot for the Global Moran's I. Doesn't like NAs.
moran.plot(x = total_crime_lsoa_sf$april_no_2019, listw = lsoa_listW, zero.policy = TRUE)
moran.plot(x = total_crime_lsoa_sf$april_no_2020, listw = lsoa_listW, zero.policy = TRUE)

# Compute Local Moran's I for each April 2019/12.
april19_lm <- as.data.frame(localmoran(x = total_crime_lsoa_sf$april_2019, listw = lsoa_listW, zero.policy = TRUE, na.action = na.omit))
april20_lm <- as.data.frame(localmoran(x = total_crime_lsoa_sf$april_no_2020, listw = lsoa_listW, zero.policy = TRUE, na.action = na.omit))

# Rename variable in each to identify them within the sf object.
names(april19_lm) <- c("ii_2019", "e_ii_2019", "var_ii_2019", "z_ii_2019", "p_2019")
names(april20_lm) <- c("ii_2020", "e_ii_2020", "var_ii_2020", "z_ii_2020", "p_2020")

notsig <- filter(april19_lm, p_2019 > 0.05)
nrow(notsig)

total_crime_lsoa_sf <- bind_cols(total_crime_lsoa_sf, april19_lm, april20_lm)

# Create classification.





# Note we are using https://maczokni.github.io/crimemapping_textbook_bookdown/global-and-local-spatial-autocorrelation.html#generating-and-visualising-the-lisa-measures
# as the main resource for this analysis.

# Compute classification variable for 2019. Note missings for no neighbours.
lsoa_ew_valid_totcrim_no_sf <- lsoa_ew_valid_totcrim_no_sf %>% 
  mutate(`s_2019-04` = as.numeric(scale(lsoa_ew_valid_totcrim_no_sf$`2019-04`)),
         `lag_s_2019-04` = lag.listw(lsoa_no_listW, lsoa_ew_valid_totcrim_no_sf$`s_2019-04`))

# Check scatter. It's the same as the above.
ggplot(data = lsoa_ew_valid_totcrim_no_sf) +
  geom_point(mapping = aes(x = `s_2019-04`, y = `lag_s_2019-04`))

# Save for replication in GeoDa.
st_write(obj = lsoa_ew_valid_totcrim_no_sf, dsn = "data/lsoa_ew_valid_totcrim_no_sf.shp")
# Create classification.
test <- lsoa_ew_valid_totcrim_no_sf %>% 
  mutate(quad_sig = ifelse(lsoa_ew_valid_totcrim_no_sf$`s_2019-04` > 0 & 
                             lsoa_ew_valid_totcrim_no_sf$`lag_s_2019-04` > 0 & 
                             lsoa_ew_valid_totcrim_no_sf$`Pr(z > 0)_19` <= 0.05, 
                           "high-high",
                           ifelse(lsoa_ew_valid_totcrim_no_sf$`s_2019-04` <= 0 & 
                                    lsoa_ew_valid_totcrim_no_sf$`lag_s_2019-04` <= 0 & 
                                    lsoa_ew_valid_totcrim_no_sf$`Pr(z > 0)_19` <= 0.05, 
                                  "low-low", 
                                  ifelse(lsoa_ew_valid_totcrim_no_sf$`s_2019-04` > 0 & 
                                           lsoa_ew_valid_totcrim_no_sf$`lag_s_2019-04` <= 0 & 
                                           lsoa_ew_valid_totcrim_no_sf$`Pr(z > 0)_19` <= 0.05, 
                                         "high-low",
                                         ifelse(lsoa_ew_valid_totcrim_no_sf$`s_2019-04` <= 0 & 
                                                  lsoa_ew_valid_totcrim_no_sf$`lag_s_2019-04` > 0 & 
                                                  lsoa_ew_valid_totcrim_no_sf$`Pr(z > 0)_19` <= 0.05,
                                                "low-high", 
                                                "non-significant")))))


# Next step: remove zeros.








