# Load packages.
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

# Combine 2019 and 2029 datasets for next steps.
sub_data_agg_1920_df <- bind_rows(sub_data_agg_2019, sub_data_agg_2020)

# Remove objects to retain memory if needed.
rm(lsoa_ew_sf, sub_data_agg_2019, sub_data_agg_2020)

# Raw counts plot, comparing trends across years, similar to first Crime Science paper.

# First, create a small, identical data frame with total notifiable offences as a 'crime type'.
total_crime_agg_df <- sub_data_agg_1920_df %>%
  filter(crime_type != "Anti-social behaviour") %>% 
  group_by(month, year) %>% 
  summarise(ew_crime_count = sum(crime_count)) %>% 
  ungroup() %>% 
  mutate(crime_type = "Total crime") %>% 
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
  filter(crime_type != "Anti-social behaviour") %>% 
  group_by(year, month) %>% 
  summarise(gini_coef = gini(crime_count, generalized = TRUE, unbiased = TRUE)) %>% 
  ungroup() %>% 
  separate(month, into = c("year","month"), sep = "-") %>% 
  mutate(crime_type = "Total crime") %>% 
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
ggsave(plot = gini_gg, filename = "visuals/gini_gg.png", width = 14, height = 20, units = "cm", dpi = 600)

# # Initial plot.
# by_crimtyp_gini_gg <- ggplot(data = gini_1920_df) +
#   geom_line(mapping = aes(x = month, y = gini_coef, group = crime_type, colour = crime_type)) +
#   facet_wrap(~ year) +
#   scale_x_discrete(labels = str_extract(month.name, "^.{3}")) +
#   labs(x = NULL, y = "Generalized Gini") +
#   theme_bw() +
#   theme(legend.text = element_text(size = 5), legend.title = element_blank())
# 
# # Save.
# ggsave(filename = "visuals/by_crimtyp_gini.png", height = 9, width = 25, unit = "cm")

# # Do the same but group by the urban rural distinction.
# df_1920_rural_gini <- df_1920_df %>%
#   group_by(Year, Month, `Crime type`, urban_rural) %>% 
#   summarise(gini_coef = gini(crime_count, generalized = TRUE, unbiased = TRUE)) %>% 
#   ungroup() %>% 
#   separate(Month, into = c("Year_temp","Month"), sep = "-") %>% 
#   select(-Year_temp) 
# 
# # Initial plot.
# rural_gini_gg <- ggplot(data = df_1920_rural_gini) +
#   geom_line(mapping = aes(x = Month, y = gini_coef, group = `Crime type`, colour = `Crime type`)) +
#   geom_vline(xintercept = 3.7, linetype = "dotted") +
#   scale_x_discrete(labels = str_extract(month.name, "^.{3}")) +
#   facet_wrap(~ urban_rural + Year) +
#   theme_bw()
  
# # Save.
# ggsave(filename = "visuals/rural_gini.png", height = 16, width = 25, unit = "cm")

# Global Moran's I for E&W.

# # First attempt used GeoDa because it's computationally quick.
# # Save lsoa_ew_complete_sf object as shapefile for use in GeoDa.
# st_write(obj = lsoa_ew_valid_sf, dsn = "data/lsoa_ew_valid_sf.shp")
# # Rook contuinity matrix created in GeoDa Version 1.14.0. Descriptive statistics screenshot
# # has been saved in the 'img' folder of this project.
# lsoa_gal <- read.gal(file = "data/lsoa_ew_valid_sf.gal", override.id = TRUE)
# lsoa_listW <- nb2listw(lsoa_gal, style = "W", zero.policy = TRUE)
# # Check summary. Zero poly is TRUE because GeoDa warned about an island LSOA (which has no neighbour,
# # and also no part of the LSOA on mainland - the Isle of Scilly). This is confirmed using spdep.
# summary(lsoa_listW, zero.policy = TRUE)
# # We now remove the Isle of Scilly LSOA.
# lsoa_ew_valid_noisl_sf <- lsoa_ew_valid_sf %>%
#   filter(geo_code != "E01019077")

# Creating queens continuity matrix is problematic due to memory. Due to the shape of LSOAs, I don't think
# it is even necessary, so we use rook.
lsoa_nb <- poly2nb(pl = lsoa_ew_valid_sf, queen = FALSE)
lsoa_listW <- nb2listw(lsoa_nb, style = "W", zero.policy = TRUE)
summary(lsoa_listW, zero.policy = TRUE) # average number of links ~5.7
lsoa_ew_valid_sf[1285, ] # Isle of Scilly is the neighborless LSOA.

# First, let's run Global/Local Moran's I comparing April 2019 and April 2020. 

# Calculate total notifiable offences per LSOA.
total_crime_lsoa_df <- sub_data_agg_1920_df %>%
  filter(crime_type != "Anti-social behaviour", 
         month == "2019-04" | month == "2020-04") %>% 
  group_by(month, lsoa_code) %>% 
  summarise(total_crime = sum(crime_count)) %>% 
  ungroup() %>% 
  complete(month, lsoa_code, fill = list(total_crime = 0)) %>% 
  pivot_wider(id_cols = lsoa_code, names_from = month, values_from = total_crime) %>% 
  rename(april_2019 = `2019-04`,
         april_2020 = `2020-04`)

# Join with the sf object.
total_crime_lsoa_sf <- left_join(lsoa_ew_valid_sf, total_crime_lsoa_df, by = c("geo_code" = "lsoa_code"))

# Save for checks in GeoDa.
st_write(obj = total_crime_lsoa_sf, dsn = "data/total_crime_lsoa_sf.shp")

# Save and load workspace as appropriate.
# save.image(file = "data_handling.RData")
load(file = "data_handling.RData")

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
         april_no_2020 = replace(x = april_2020, april_2020 > 125, NA))

# Compute Global Moran's I for each April 2019/20.
moran.test(x = total_crime_lsoa_sf$april_no_2019, listw = lsoa_listW, zero.policy = TRUE, na.action = na.omit)
moran.test(x = total_crime_lsoa_sf$april_no_2020, listw = lsoa_listW, zero.policy = TRUE, na.action = na.omit)

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




# # Create deciles for ASB as a test:
# asb_2020_df <- df_2020_sub_agg %>% 
#   filter(`Crime type` == "Anti-social behaviour") %>% 
#   group_by(Month) %>% 
#   arrange(crime_count) %>% 
#   mutate(decile = ntile(crime_count, 10)) %>% 
#   ungroup() %>% 
#   arrange(decile)
# 
# # How many ASB counts in each LSOA, on each month?
# nrow(filter(asb_2020_df, crime_count == 0)) # 69k zeros (19%)
# nrow(filter(asb_2020_df, crime_count == 1)) # 62k one crime count (17%)
#   
# # Many zeros and low counts. Percentage change analysis at this level would not be insightful or useful.
# # Talking about decile change would also be odd, because so many are 'draws'.






