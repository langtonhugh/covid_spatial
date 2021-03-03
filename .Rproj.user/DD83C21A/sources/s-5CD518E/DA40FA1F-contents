# Load packages.
library(kml)
library(cowplot)
library(lorenzgini)
library(spdep)
library(readr)
library(janitor)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(forcats)
library(ggplot2)
library(sf)

# Useful function.
`%nin%` <- Negate(`%in%`)

# Archive data downloaded January 2012, covering the 3-year period up to and including November 2020.
# download.file(url = "https://data.police.uk/data/archive/2020-11.zip", destfile = "data/archive2020-11.zip")

# Unzip.
# unzip(zipfile = "data/archive2020-11.zip", exdir = "data")

# Note the loading and handling data is largely determined by available memory. Many people may be able to
# run things more efficiently (and in a more straightforward order) with a more powerful computer.

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

# Remove existing data objects to free up memory for 2018 if needed.
rm(data_2019, data_2020, full_data_2019, full_data_2020)

# Load and run the equivalent for 2018.
list_2018 <- paste("data/", list.files("data", pattern = glob2rx("2018*street.csv"),  recursive=TRUE), sep = "")

# Read in .csv files for each year.
data_2018 <- lapply(list_2018, read_csv)

# Bind each in to data frames. For now, we keep the years separate.
full_data_2018 <- data_2018 %>% 
  bind_rows() %>% 
  clean_names() %>% 
  mutate(year = "2018")

# Create year ID, then remove Greater Manchester Police and Police Service of NI.
sub_data_2018 <- full_data_2018 %>% 
  filter(reported_by != "Greater Manchester Police", 
         reported_by != "Police Service of Northern Ireland")

# Remove object for memory as needed.
rm(data_2018, full_data_2018)

# Check months.
# unique(sub_data_2020$month)
# unique(sub_data_2019$month)
# unique(sub_data_2018$month)

# Check crime types.
# unique(sub_data_2020$crime_type)
# unique(sub_data_2019$crime_type)
# unique(sub_data_2018$crime_type)

# Check missings in dates.
sum(is.na(sub_data_2020$month))
sum(is.na(sub_data_2019$month))
sum(is.na(sub_data_2018$month))

# Check missings in crime types.
sum(is.na(sub_data_2020$crime_type))
sum(is.na(sub_data_2019$crime_type))
sum(is.na(sub_data_2018$crime_type))

# Check missings in LSOA codes.
sum(is.na(sub_data_2020$lsoa_code)) # ~98k 
sum(is.na(sub_data_2019$lsoa_code)) # ~161k 
sum(is.na(sub_data_2018$lsoa_code)) # ~123k 

# Explore these missings - run for each year. It is clear that crimes with missing LSOA almost always
# also have missing latitude and longitude coordinates, so this is unsalvagable.
miss_lsoa_2018_df <- sub_data_2018 %>% 
  filter(is.na(lsoa_code))
sum(is.na(miss_lsoa_2018_df$longitude))
sum(is.na(miss_lsoa_2018_df$latitude))
rm(miss_lsoa_2018_df)

# Drop crimes with missing LSOA.
sub_data_2020 <- drop_na(sub_data_2020, lsoa_code)
sub_data_2019 <- drop_na(sub_data_2019, lsoa_code)
sub_data_2018 <- drop_na(sub_data_2018, lsoa_code)

# Aggregate by month (N = 11), crime type (N = 14), and LSOA (N = ~33,000). Keep 'Year' in for later use.
sub_data_agg_2020 <- sub_data_2020 %>% 
  group_by(crime_type, month, lsoa_code, year) %>% 
  summarise(crime_count = n()) %>% 
  ungroup() %>% 
  complete(crime_type, month, lsoa_code, year, fill = list(crime_count = 0))

rm(sub_data_2020) # free up space if needed.

sub_data_agg_2019 <- sub_data_2019 %>% 
  group_by(crime_type, month, lsoa_code, year) %>% 
  summarise(crime_count = n()) %>% 
  ungroup() %>% 
  complete(crime_type, month, lsoa_code, year, fill = list(crime_count = 0))

rm(sub_data_2019) # free up space if needed.

sub_data_agg_2018 <- sub_data_2018 %>% 
  group_by(crime_type, month, lsoa_code, year) %>% 
  summarise(crime_count = n()) %>% 
  ungroup() %>% 
  complete(crime_type, month, lsoa_code, year, fill = list(crime_count = 0))

rm(sub_data_2018) # free up space if needed.

# Check number of each LSOAs appearing in each year. There is a difference. This will likely be
# because some LSOAs had no recorded crime assigned to them, and thus never even appeared
# in individual records. Note that we DO have zeros in this data for some LSOAs in some
# months, but that will be because there was a crime recorded in those LSOA in another month.
# It's possible there is a lack of snap poits in some LSOA.
length(unique(sub_data_agg_2020$lsoa_code))
length(unique(sub_data_agg_2019$lsoa_code))
length(unique(sub_data_agg_2018$lsoa_code))

# To explore these LSOA, and subset for E&W, we will download the full set, including spatial information.

# Download LSOA from UK Data Service.
# download.file(url = "https://borders.ukdataservice.ac.uk/ukborders/easy_download/prebuilt/shape/infuse_lsoa_lyr_2011_clipped.zip",
#               destfile = "data/ukds_infuse_lsoa.zip")

# Unzip.
# unzip(zipfile = "data/ukds_infuse_lsoa.zip", exdir = "data/ukds_infuse_lsoa")

# Load LSOA boundaries.
lsoa_sf <- st_read("data/ukds_infuse_lsoa/infuse_lsoa_lyr_2011_clipped.shp")

# Subset LSOAs for E&W and then remove Greater Manchester due to the lack of police.uk data in this region.
lsoa_ew_sf <- lsoa_sf %>% 
  mutate(country_cd = str_extract(geo_code, "^.{1}")) %>% 
  filter(country_cd == "E" | country_cd == "W",
         !str_detect(geo_label, "Manchester|Bolton|Oldham|Rochdale|Stockport|Tameside|Salford|Bury|Trafford|Wigan"))

# This leaves us with 33080 LSOA in E&W.

# Check validity of the remaining LSOAs. QGIS exploration indicated that there were four.
validity_check <- st_is_valid(lsoa_ew_sf)

# N = 4 invalid geometries.
table(validity_check) 

# Identify them
lsoa_ew_sf <- lsoa_ew_sf %>% 
  mutate(valid = validity_check)

filter(lsoa_ew_sf, valid == "FALSE")

# Resolve invalid geometries.
lsoa_ew_valid_sf <- st_make_valid(lsoa_ew_sf)

# Check. It worked.
table(st_is_valid(lsoa_ew_valid_sf))

# We identify LSOA within Northern Ireland and Greater Manchester. We have removed those crimes reported by
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

# Remove objects to save space if needed.
rm(lsoa_sf, lsoa_ew_sf)

# Remove the GM and NI LSOA from the crime data.
sub_data_agg_2020 <- sub_data_agg_2020 %>% 
  filter(lsoa_code %nin% gm_lsoa, lsoa_code %nin% ni_lsoa)

sub_data_agg_2019 <- sub_data_agg_2019 %>% 
  filter(lsoa_code %nin% gm_lsoa, lsoa_code %nin% ni_lsoa)

sub_data_agg_2018 <- sub_data_agg_2018 %>% 
  filter(lsoa_code %nin% gm_lsoa, lsoa_code %nin% ni_lsoa)

# Create country ID.
sub_data_agg_2020 <- sub_data_agg_2020 %>% 
  mutate(country_cd = str_extract(lsoa_code, "^.{1}"))

sub_data_agg_2019 <- sub_data_agg_2019 %>% 
  mutate(country_cd = str_extract(lsoa_code, "^.{1}"))

sub_data_agg_2018 <- sub_data_agg_2018 %>% 
  mutate(country_cd = str_extract(lsoa_code, "^.{1}"))

# Check that the crime data now doesn't include LSOAs from outside of England and Wales.
table(sub_data_agg_2020$country_cd) # Confirmed.
table(sub_data_agg_2019$country_cd) # Confirmed.
table(sub_data_agg_2018$country_cd) # Confirmed.

# Check LSOA in each. Confirms that police data has slightly less (N = 4) LSOA than the complete data.
length(unique(lsoa_ew_valid_sf$geo_code))
length(unique(sub_data_agg_2020$lsoa_code))
length(unique(sub_data_agg_2019$lsoa_code))
length(unique(sub_data_agg_2018$lsoa_code))

# Are these the same LSOA for each year? Run as appropriate for each year.
lsoa_ew_valid_sf %>% 
  filter(lsoa_ew_valid_sf$geo_code %nin% sub_data_agg_2020$lsoa_code)

# The answer is yes. It must be a snap point or boundary change thing.

# Conclusion: it would be misleading to say these had 'zero' crimes, so we will simply not include them
# in any analysis.
missing_lsoa <- lsoa_ew_valid_sf %>% 
  filter(lsoa_ew_valid_sf$geo_code %nin% sub_data_agg_2020$lsoa_code) # 2018 or 2019 would also work.

lsoa_ew_valid_sf <- lsoa_ew_valid_sf %>% 
  filter(geo_code %nin% missing_lsoa$geo_code)

# Check number of LSOA in each dataset again. Now identical.
length(unique(lsoa_ew_valid_sf$geo_code))    # 33076
length(unique(sub_data_agg_2020$lsoa_code))  # 33076
length(unique(sub_data_agg_2019$lsoa_code))  # 33076
length(unique(sub_data_agg_2018$lsoa_code))  # 33076

# Combine 2018, 2019 and 2020 datasets for next steps.
sub_data_agg_full_df <- bind_rows(sub_data_agg_2018, sub_data_agg_2019, sub_data_agg_2020)

# Remove objects to free up memory if needed.
rm(sub_data_agg_2018, sub_data_agg_2019, sub_data_agg_2020)

# Create raw counts plot, comparing trends across years.

# First, create a small, identical data frame with total notifiable offences as a 'crime type' (excluding drugs).
total_crime_agg_df <- sub_data_agg_full_df %>%
  filter(crime_type != "Anti-social behaviour" & crime_type != "Drugs") %>% 
  group_by(month, year) %>% 
  summarise(ew_crime_count = sum(crime_count)) %>% 
  ungroup() %>% 
  mutate(crime_type = "Notifiable offences (excl. drugs)") %>% 
  select(crime_type, year, month, ew_crime_count) %>% 
  separate(month, into = c("year", "month"), sep = "-") %>% 
  filter(month != "01", month != "09", month != "10", month != "11", month != "12") 

# Single plot for notifiable offences.
no_raw_counts_gg <- ggplot(data = total_crime_agg_df) +
  geom_line(mapping = aes(x = month, y = ew_crime_count, group = year, colour = year), size = 0.8) +
  geom_vline(xintercept = 1.7, linetype = "dotted") +
  scale_x_discrete(labels = str_extract(month.name[2:9], "^.{3}")) +
  scale_color_manual(values = rev(c("black", "darkgrey", "lightgrey"))) +
  labs(x = NULL, y = "Count", colour = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8), #, hjust = -0.4
        axis.ticks = element_line(size = 0.3, lineend = "round"),
        axis.text.y = element_text(size = 6),
        legend.position = "bottom")

# Save.
ggsave(plot = no_raw_counts_gg, filename = "visuals/no_raw_counts_gg.png", width = 12, height = 8, unit = "cm")

# Then calculate these counts by crime type, bind the total crime data frame to it, and visualise.
raw_counts_gg <- sub_data_agg_full_df %>% 
  group_by(crime_type, month, year) %>% 
  summarise(ew_crime_count = sum(crime_count)) %>% 
  ungroup() %>% 
  separate(month, into = c("year", "month"), sep = "-") %>% 
  filter(month != "01", month != "09", month != "10", month != "11", month != "12") %>% 
  bind_rows(total_crime_agg_df) %>% 
  mutate(year = recode_factor(year, "2018" = "2018", "2019" = "2019", "2020" = "2020")) %>% 
  ggplot() +
  geom_line(mapping = aes(x = month, y = ew_crime_count, group = year, colour = year), size = 0.8) +
  geom_vline(xintercept = 1.7, linetype = "dotted") +
  facet_wrap(~ crime_type, ncol = 3, scales = "free_y") +
  scale_x_discrete(labels = str_extract(month.name[2:9], "^.{3}")) +
  scale_color_manual(values = rev(c("black", "darkgrey", "lightgrey"))) +
  labs(x = NULL, y = NULL, colour = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6), #, hjust = -0.4
        axis.ticks = element_line(size = 0.3, lineend = "round"),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 8),
        strip.background = element_rect(fill = "transparent"),
        legend.position = "bottom")

# Save.
ggsave(plot = raw_counts_gg, filename = "visuals/raw_counts_gg.png", width = 16, height = 20, units = "cm", dpi = 600)

# Calculate E&W-wide Generalized Gini coefficient for notifiable offences, for 2019 and 2020.
gini_total_crime_df <- sub_data_agg_full_df %>%
  filter(crime_type != "Anti-social behaviour" & crime_type != "Drugs") %>% 
  group_by(year, month) %>% 
  summarise(gini_coef = gini(crime_count, generalized = TRUE, unbiased = TRUE)) %>% 
  ungroup() %>% 
  separate(month, into = c("year","month"), sep = "-") %>% 
  mutate(crime_type = "Notifiable offences (excl. drugs)") %>% 
  select(crime_type, gini_coef, year,  month) %>% 
  filter(month != "01", month != "09", month != "10", month != "11", month != "12") %>% 
  mutate(year = recode_factor(year, "2018" = "2018", "2019" = "2019", "2020" = "2020")) 

# Single plot for notifiable offences.
no_gini_gg <- ggplot(data = gini_total_crime_df) +
  geom_line(mapping = aes(x = month, y = gini_coef, group = year, colour = year), size = 0.8) +
  geom_vline(xintercept = 1.7, linetype = "dotted") +
  ylim(0, 1) +
  labs(x = NULL, y = "Generalized Gini Coefficient", colour = NULL) +
  scale_x_discrete(labels = str_extract(month.name[2:9], "^.{3}")) +
  scale_color_manual(values = rev(c("black", "darkgrey", "lightgrey"))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8), #, hjust = -0.4
        axis.ticks = element_line(size = 0.3, lineend = "round"),
        axis.text.y = element_text(size = 6),
        legend.position = "bottom")

# Save.
ggsave(plot = no_gini_gg, filename = "visuals/no_gini_gg.png", width = 12, height = 8, unit = "cm")

# Do the same but by crime type and add the total crime category on.
gini_crime_type_df <- sub_data_agg_full_df %>%
  group_by(year, month, crime_type) %>% 
  summarise(gini_coef = gini(crime_count, generalized = TRUE, unbiased = TRUE)) %>% 
  ungroup() %>% 
  separate(month, into = c("year","month"), sep = "-") %>%
  bind_rows(gini_total_crime_df) %>% 
  filter(month != "01", month != "09", month != "10", month != "11", month != "12") %>% 
  mutate(year = recode_factor(year, "2018" = "2018", "2019" = "2019", "2020" = "2020")) 

# Facet plot.
gini_gg <- ggplot(data = gini_crime_type_df) +
  geom_line(mapping = aes(x = month, y = gini_coef, group = year, colour = year), size = 0.8) +
  geom_vline(xintercept = 1.7, linetype = "dotted") +
  facet_wrap(~ crime_type, ncol = 3) +
  ylim(0, 1) +
  labs(x = NULL, y = "Generalized Gini Coefficient", colour = NULL) +
  scale_x_discrete(labels = str_extract(month.name[2:9], "^.{3}")) +
  scale_color_manual(values = rev(c("black", "darkgrey", "lightgrey"))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 6), #, hjust = -0.4
        axis.ticks = element_line(size = 0.3, lineend = "round"),
        axis.text.y = element_text(size = 6),
        strip.text = element_text(size = 8),
        strip.background = element_rect(fill = "transparent"),
        legend.position = "bottom")

# Save.
ggsave(plot = gini_gg, filename = "visuals/gini_gg.png", width = 14, height = 20, units = "cm", dpi = 600)

# Long-term trends for public order.
long_term_gg <- sub_data_agg_full_df %>% 
  filter(crime_type == "Public order" | crime_type == "Violence and sexual offences" |
         crime_type == "Other crime",
         month != "2020-09" & month != "2020-10" & month != "2020-11") %>% 
  group_by(crime_type, month, year) %>% 
  summarise(ew_crime_count = sum(crime_count)) %>% 
  ungroup() %>% 
  mutate(year = recode_factor(year, "2018" = "2018", "2019" = "2019", "2020" = "2020")) %>% 
  ggplot() +
  geom_line(mapping = aes(x = month, y = ew_crime_count, group = 1)) +
  geom_vline(xintercept = 27, linetype = "dotted") +
  facet_wrap(~crime_type, nrow = 3, scales = "free_y") +
  labs(y = "Counts", x = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
  
ggsave(plot = long_term_gg, filename = "visuals/long_term_gg.png", height = 20, width = 20, unit = "cm")

# Remove object to free up memory if needed.
rm(total_crime_agg_df, gini_total_crime_df, gini_crime_type_df, gini_gg, raw_counts_gg)

# Prepare the data for the longitudinal k-means.

# Subset for the study period (February to August) and aggregate crime counts across 
# categories, excluding ASB and drugs.
tc_kmeans_sub_df <- sub_data_agg_full_df %>% 
  filter(crime_type != "Anti-social behaviour" & crime_type != "Drugs",
         month == "2020-02" | month == "2020-03" | month == "2020-04" | month == "2020-05" |
         month == "2020-06" | month == "2020-07" | month == "2020-08") %>% 
  group_by(lsoa_code, month) %>% 
  summarise(ew_crime_count = sum(crime_count)) %>% 
  ungroup() 

# Check for those LSOAs which were essentially crime-free during the study period, and recode
# the month names to keep things simple. We keep in wide because that's what kml package likes.
tc_kmeans_sub_clean_df <- tc_kmeans_sub_df %>% 
  pivot_wider(id_cols = lsoa_code, names_from = "month", values_from = "ew_crime_count") %>%
  mutate(total_months = rowSums(.[2:8])) %>% 
  filter(total_months != 0) %>% # There is actually none for the February-August study period.
  select(-total_months) %>% 
  rename(february = `2020-02`,
         march = `2020-03`, april = `2020-04`, may = `2020-05`, june = `2020-06`,
         july = `2020-07` , august = `2020-08`)

# Check that we've retained all LSOA for final analysis (N = 33076).
length(unique(tc_kmeans_sub_df$lsoa_code))
length(unique(tc_kmeans_sub_clean_df$lsoa_code))

# Perform kmeans analysis on the February to August 2020 counts.

# Restrict potential maximum to 6 for simplicity.
n <- 3:6 

# Set seed.
set.seed(1612)

# Convert to matrix, cluster data, and perform kmeans on all potential clusters from 3 to 6.
tc_kml_mat <- as.matrix(tc_kmeans_sub_clean_df[2:8])
tc_traj    <- clusterLongData(traj = tc_kml_mat) 
kml(tc_traj, nbClusters = n, toPlot = "criterion", nbRedrawing = 20)

# Collate LSOA codes and cluster labels into a data frame.
tc_clusters_df <- cbind.data.frame(lsoa_code = tc_kmeans_sub_clean_df$lsoa_code,
                                   traj      = getClusters(tc_traj, 6)) # We select 6 due to CH value.

# Compute cluster sizes.
as.data.frame(table(tc_clusters_df$traj)) %>% 
  mutate(total = sum(Freq),
         prop  = 100*Freq/total)
  
# Create new label for each cluster. Convert to tibble along the way.
tc_clusters_df <- tc_clusters_df %>% 
  as_tibble() %>% 
  mutate(traj_titles  = ifelse(test = traj == "A", yes = "[A] N = 19,162 (58%)" , no = traj),
         traj_titles  = ifelse(test = traj == "B", yes = "[B] N = 10,087 (31%)" , no = traj_titles),
         traj_titles  = ifelse(test = traj == "C", yes = "[C] N = 3,184 (10%)"  , no = traj_titles),
         traj_titles  = ifelse(test = traj == "D", yes = "[D] N = 533 (1.6%)"   , no = traj_titles),
         traj_titles  = ifelse(test = traj == "E", yes = "[E] N = 101 (0.3%)"   , no = traj_titles),
         traj_titles  = ifelse(test = traj == "F", yes = "[F] N = 9 (0.03%)"    , no = traj_titles),
         traj_titles  = fct_relevel(traj_titles, "[A] N = 19,162 (58%)",
                                                 "[B] N = 10,087 (31%)",
                                                 "[C] N = 3,184 (10%)",
                                                 "[D] N = 533 (1.6%)",
                                                 "[E] N = 101 (0.3%)",
                                                 "[F] N = 9 (0.03%)"))

# Join cluster information back with main data frame.
sub_data_agg_full_df <- left_join(sub_data_agg_full_df, tc_clusters_df)

# Remove the kmeans data frame to save space if needed.
rm(tc_kmeans_sub_clean_df, tc_kmeans_sub_df)

# Subset data for the study period February to August across all years, and aggregate to all crime excluding drugs
# for each cluster identified by kmeans. This repeats some of what we've done already, but it stops too many
# big objects building up previously.
tc_clusters_agg_df <- sub_data_agg_full_df %>%
  filter(crime_type != "Anti-social behaviour" & crime_type != "Drugs",
         month != "2018-01" & month != "2018-09" & month != "2018-10" & month != "2018-11" & month != "2018-12" &
         month != "2019-01" & month != "2019-09" & month != "2019-10" & month != "2019-11" & month != "2019-12" &
         month != "2020-01" & month != "2020-09" & month != "2020-10" & month != "2020-11") %>% 
  group_by(lsoa_code, month, year, traj, traj_titles) %>% 
  summarise(ew_crime_count = sum(crime_count)) %>% 
  ungroup() %>% 
  mutate(month_name = if_else(condition = str_detect(month, "02"), true = "February", false = month),
         month_name = if_else(condition = str_detect(month, "03"), true = "March"   , false = month_name),
         month_name = if_else(condition = str_detect(month, "04"), true = "April"   , false = month_name),
         month_name = if_else(condition = str_detect(month, "05"), true = "May"     , false = month_name),
         month_name = if_else(condition = str_detect(month, "06"), true = "June"    , false = month_name),
         month_name = if_else(condition = str_detect(month, "07"), true = "July"    , false = month_name),
         month_name = if_else(condition = str_detect(month, "08"), true = "August"  , false = month_name),
         month_name = fct_relevel(month_name, month.name[2:8])) 

# Violin plot. Filter for years as we go.
kmeans_violin_full_gg <- ggplot() + theme_bw() +
  geom_violin(data = filter(tc_clusters_agg_df, year == "2020"),
              mapping = aes(x = month_name, y = ew_crime_count, fill = traj_titles),
              alpha = 0.3, colour = "transparent", adjust = 2) + 
  scale_fill_hue(guide = FALSE) +
  stat_summary(data = filter(tc_clusters_agg_df, year == "2018"),
               mapping = aes(x = month_name, y = ew_crime_count, group = traj_titles, colour = "blue"),
               fun = "median",  size = 0.8, geom = "line") +
  stat_summary(data = filter(tc_clusters_agg_df, year == "2018"),
               mapping = aes(x = month_name, y = ew_crime_count, group = traj_titles, colour = "blue"),
               fun = "mean", linetype = "dotted", size = 0.8, geom = "line") +
  stat_summary(data = filter(tc_clusters_agg_df, year == "2019"),
               mapping = aes(x = month_name, y = ew_crime_count, group = traj_titles, colour = "red"),
               fun = "median",  size = 0.8, geom = "line") +
  stat_summary(data = filter(tc_clusters_agg_df, year == "2019"),
               mapping = aes(x = month_name, y = ew_crime_count, group = traj_titles, colour = "red"),
               fun = "mean", linetype = "dotted", size = 0.8, geom = "line") +
  stat_summary(data = filter(tc_clusters_agg_df, year == "2020"),
               mapping = aes(x = month_name, y = ew_crime_count, group = traj_titles, colour = "black"),
               fun = "median", size = 0.8, geom = "line") +
  stat_summary(data = filter(tc_clusters_agg_df, year == "2020"),
               mapping = aes(x = month_name, y = ew_crime_count, group = traj_titles, colour = "black"),
               fun = "mean", linetype = "dotted", size = 0.8, geom = "line") +
  scale_colour_manual(values = c("black", "blue", "red"), labels = c("2020", "2018", "2019")) +
  facet_wrap(~traj_titles, ncol = 2, scales = "free_y") +
  scale_x_discrete(labels = c(str_extract(month.name[2:8], "^.{3}"), character(1))) +
  labs(x = NULL, colour = NULL, y = "crime count") +
  guides(colour = guide_legend(nrow = 1)) +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 8),
        strip.background = element_rect(fill = "transparent"))

# Save full plot.
ggsave(plot = kmeans_violin_full_gg, filename = "visuals/kmeans_violin_full_gg.png",
       height = 24, width = 20, unit = "cm", dpi = 200)

# Calculate the absolute % of month-on-month change each cluster contributed to. This is a bit fiddly!
change_df <- tc_clusters_agg_df %>% 
  filter(year == "2020") %>% 
  select(lsoa_code, month_name, traj_titles, ew_crime_count) %>% 
  group_by(month_name, traj_titles) %>%
  summarise(traj_crimes = sum(ew_crime_count)) %>%
  ungroup() %>%
  group_by(month_name) %>%
  mutate(total_crimes = sum(traj_crimes),         # sum crimes across all trajectories (i.e. count per month in e&w)
         month_name   = tolower(month_name)) %>%  # lower case for code rather than labels
  pivot_wider(id_cols = traj_titles, names_from = month_name, values_from = c(traj_crimes, total_crimes)) %>% 
  mutate(traj_feb_mar = traj_crimes_march-traj_crimes_february,
         traj_mar_apr = traj_crimes_april-traj_crimes_march,
         traj_apr_may = traj_crimes_may-traj_crimes_april,
         traj_may_jun = traj_crimes_june-traj_crimes_may,
         traj_jun_jul = traj_crimes_july-traj_crimes_june,
         traj_jul_aug = traj_crimes_august-traj_crimes_july,
         tot_feb_mar  = sum(abs(traj_feb_mar)), # This actually the same as just doing it with totals.
         tot_mar_apr  = sum(abs(traj_mar_apr)),
         tot_apr_may  = sum(abs(traj_apr_may)),
         tot_may_jun  = sum(abs(traj_may_jun)),
         tot_jun_jul  = sum(abs(traj_jun_jul)),
         tot_jul_aug  = sum(abs(traj_jul_aug)),
         prop_feb_mar = 100*round(traj_feb_mar/tot_feb_mar, 2),
         prop_mar_apr = 100*round(traj_mar_apr/tot_mar_apr, 2),
         prop_apr_may = 100*round(traj_apr_may/tot_apr_may, 2),
         prop_may_jun = 100*round(traj_may_jun/tot_may_jun, 2),
         prop_jun_jul = 100*round(traj_jun_jul/tot_jun_jul, 2),
         prop_jul_aug = 100*round(traj_jul_aug/tot_jul_aug, 2)) %>% 
  select(traj_titles, traj_feb_mar:traj_jul_aug, prop_feb_mar:prop_jul_aug) %>% 
  pivot_longer(cols = -traj_titles, names_to = "month_change", values_to = "change") %>% 
  mutate(stat  = if_else(condition = str_detect(month_change, "prop"), true = "prop_change", false = "count_change"),
         month = if_else(condition = str_detect(month_change, "feb_mar"), true = "feb_mar", false = month_change),
         month = if_else(condition = str_detect(month_change, "mar_apr"), true = "mar_apr", false = month),
         month = if_else(condition = str_detect(month_change, "apr_may"), true = "apr_may", false = month),
         month = if_else(condition = str_detect(month_change, "may_jun"), true = "may_jun", false = month),
         month = if_else(condition = str_detect(month_change, "jun_jul"), true = "jun_jul", false = month),
         month = if_else(condition = str_detect(month_change, "jul_aug"), true = "jul_aug", false = month)) %>%
  pivot_wider(id_cols = c(month, traj_titles), names_from = stat, values_from = change) %>% 
  mutate(month = fct_relevel(month,
                             "feb_mar",
                             "mar_apr",
                             "apr_may",
                             "may_jun",
                             "jun_jul",
                             "jul_aug"),
         prop_change = paste(abs(prop_change), "%", sep = ""))

# Create base plot of the count and absolute % changes.
change_gg <- ggplot(data = change_df) +
  geom_bar(mapping = aes(x = month, y = count_change,
                         group = traj_titles, fill = traj_titles), stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dotted", size = 0.3, alpha = 1) +
  geom_text(mapping = aes(x = month, y = count_change, group = traj_titles, label = prop_change),
            position = position_dodge(width = 0.9), size = 1.7,  vjust = -1.5) +
  labs(y = "Count change attributable to cluster", x = NULL, fill = NULL) +
  scale_x_discrete(labels = c("February to March", "March to April", "April to May", "May to June", "June to July", "July to August")) +
  theme_bw() +
  theme(legend.position = "bottom")

# Annotate the base plot.
change_ann_gg <- change_gg +
  annotate(geom = "text", x = 1.5, y = 14000, size = 3, label = "Percentage of nationwide \n absolute monthly change \n attributable to cluster") +
  annotate(geom = "curve", x = 2.15, xend = 2.65, y = 14500, yend = 15000, curvature = -0.1, arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text", x = 3.5, y = -11000, size = 3,
           label = "15% of the lockdown drop  \n between March and April \n attributable to just 110 LSOA") +
  annotate(geom = "curve", x = 3.5, xend = 2.52, y = -9000, yend = -3000, curvature = 0.2, arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "curve", x = 3.5, xend = 2.32, y = -9000, yend = -6000, curvature = 0.2, arrow = arrow(length = unit(1, "mm"))) +
  annotate(geom = "text", x = 5, y = -8000, size = 3, label = "Further nationwide increase \n partially offset") +
  annotate(geom = "curve", x = 5.2, xend = 5.6, y = -6500, yend = -1300, curvature = -0.3, arrow = arrow(length = unit(1, "mm")))

# Save count and % change plot.
ggsave(plot = change_ann_gg, filename = "visuals/change_k6_gg.png", width = 18, height = 16, unit = "cm")

# Calculate crime composition of each cluster.

# Subset only the study period February to August 2020, calculate crime type % composition of each cluster.
traj_crime_types_df <- sub_data_agg_full_df %>% 
  filter(crime_type != "Anti-social behaviour" & crime_type != "Drugs",
         month == "2020-02" | month == "2020-03" | month == "2020-04" | month == "2020-05" |
         month == "2020-06" | month == "2020-07" | month == "2020-08") %>% 
  group_by(traj, month) %>% 
  mutate(traj_crimes = sum(crime_count)) %>% 
  ungroup() %>% 
  group_by(crime_type, traj, month) %>% 
  mutate(traj_ct_crime = sum(crime_count)) %>% 
  ungroup() %>% 
  mutate(traj_ct_prop = traj_ct_crime/traj_crimes) %>% 
  distinct(crime_type, month, traj, traj_ct_prop) %>% 
  arrange(month, traj)

# Check.
check_df <- traj_crime_types_df %>% 
  group_by(traj, month) %>% 
  summarise(totals = sum(traj_ct_prop))

# Retrieve full cluster names for plot - too big earlier. Obtain individual names.
traj_names_df <- tc_clusters_df %>% 
  distinct(traj, traj_titles)

# Join with data.
traj_crime_types_df <- traj_crime_types_df %>% 
  left_join(traj_names_df)

# Plot.
traj_crimes_gg <- ggplot(data = traj_crime_types_df) +
  geom_bar(mapping = aes(x = month, y = traj_ct_prop, group = crime_type, fill = crime_type),
            stat = "identity", colour = "black", size = 0.2) +
  facet_wrap(~traj_titles, ncol = 2) +
  labs(y = "Proportion comprising total crime", x = NULL, fill = NULL) +
  scale_x_discrete(labels = c(str_extract(month.name[2:8], "^.{3}"), character(1))) +
  guides(fill = guide_legend(ncol = 3)) +
  theme_bw() +
  theme(legend.position = "bottom", strip.background = element_rect(fill = "transparent"))

# Save.
ggsave(plot = traj_crimes_gg, filename = "visuals/traj_crimes_monthly_k6_gg.png", width = 16, height = 24, unit = "cm")

# Save and load workspace as appropriate.
# save.image(file = "data_handling_cleaning.RData")
load(file = "data_handling_cleaning.RData")

# Maps of major cities.

# Join cluster information with lsoa sf object.
lsoa_ew_valid_sf <- sub_data_agg_full_df %>% 
  distinct(lsoa_code, traj, traj_titles) %>% 
  right_join(lsoa_ew_valid_sf, by = c("lsoa_code" = "geo_code")) %>% 
  st_as_sf()

# Save.
st_write(obj = lsoa_ew_valid_sf, dsn = "data/lsoa_ew_valid_k6_sf.shp")

# Selection of cities. Top 5 by population in England minus London plus Cardiff.
birm_sf <- lsoa_ew_valid_sf %>% 
  filter(str_detect(string = lsoa_ew_valid_sf$geo_label, pattern = "Birmingham"))

leeds_sf <- lsoa_ew_valid_sf %>% 
  filter(str_detect(string = lsoa_ew_valid_sf$geo_label, pattern = "Leeds"))

sheff_sf <- lsoa_ew_valid_sf %>% 
  filter(str_detect(string = lsoa_ew_valid_sf$geo_label, pattern = "Sheffield"))

brad_sf <- lsoa_ew_valid_sf %>% 
  filter(str_detect(string = lsoa_ew_valid_sf$geo_label, pattern = "Bradford"))

liver_sf <- lsoa_ew_valid_sf %>% 
  filter(str_detect(string = lsoa_ew_valid_sf$geo_label, pattern = "Liverpool"))

cardiff_sf <- lsoa_ew_valid_sf %>% 
  filter(str_detect(string = lsoa_ew_valid_sf$geo_label, pattern = "Cardiff"))

# Visual inspection tells us that Cardiff has an island LSOA to the south. This is Flat Holne Island - we
# remove this for the visual, and because its policing/opportunity structure will perform independently 
# to Cardiff. LSOA code identified interactively in QGIS.
cardiff_sf <- cardiff_sf %>% 
  filter(lsoa_code != "W01001940")

# Create list.
cities_list <- list(birm_sf, leeds_sf, sheff_sf, brad_sf, liver_sf, cardiff_sf)

# Create map plot function.
# Get colours manually, because not all cities have all clusters.
map_fun <- function(x) {
  ggplot(data = x) +
    geom_sf(mapping = aes(fill = traj_titles), size = 0.0001, colour = "black", alpha = 0.8) +
    theme_void() +
    theme(legend.position = "none") +
    scale_fill_manual(values = scales::hue_pal()(6), drop = FALSE)
}

# Create plots.
maps_list <- lapply(cities_list, map_fun)

# Plot appearance.
maps_gg <- plot_grid(plotlist = maps_list, ncol = 2, labels = c("Birmingham", "Leeds", "Sheffield",
                                                                "Bradford", "Liverpool", "Cardiff"),
                     label_size = 7, scale = c(1, 1, 1.1, 1.05, 1, 1.1))

# Create legend (Birmingham needed as it has all 6 clusters).
temp_gg <- birm_sf %>% 
  mutate(traj_titles = str_extract(traj_titles, "^.{3}")) %>% 
  ggplot() +
  geom_sf(mapping = aes(fill = traj_titles)) +
  theme_void() +
  labs(fill = NULL) +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_manual(values = scales::hue_pal()(6), drop = FALSE)
  
legend_gg <- get_legend(temp_gg)

# Arrange with existing plot.
maps_leg_gg <- plot_grid(maps_gg, legend_gg, nrow = 2, rel_heights = c(1,0.08))

# Save
ggsave(plot = maps_leg_gg, filename = "visuals/maps_gg.png", height = 22, width = 16, unit = "cm", dpi = 300)

# Load in LSOA-level facilities down OSM (see osm_handling.r).
osm_df <- read_csv("data/osm_full.csv")

# Create transport and shops total not completed in osm_handling.r script.
osm_df <- osm_df %>% 
  mutate(transport_total = trains + bus,
         shops_total     = shops_total + conveniences)

# Get LSOA and cluster names for join.
traj_names_df <- tc_clusters_df %>% 
  distinct(lsoa_code, traj, traj_titles) 

# Join OSM data.
traj_names_osm_df <- left_join(traj_names_df, osm_df, by = c("lsoa_code" = "geo_code"))

# Calculate descriptives by cluster.
# Cluster descriptives.
osm_stats_df <- traj_names_osm_df %>%
  group_by(traj_titles) %>% 
  summarise(`Nightlife (mean)`   = mean(nightlife_total),
            `Nightlife (median)` = median(nightlife_total),
            `Nightlife (SD)`     = sd(nightlife_total),
            `Shops (mean)`       = mean(shops_total),
            `Shops (median)`     = median(shops_total),
            `Shops (SD)`         = sd(shops_total),
            `Public transport (mean)`   = mean(transport_total),
            `Public transport (median)` = median(transport_total),
            `Public transport (SD)`     = sd(transport_total),
            `Bike parking (mean)`       = mean(bikes),
            `Bike parking (median)`     = median(bikes),
            `Bike parking (SD)`         = sd(bikes)) %>% 
  mutate_if(is.numeric, round, 2)

# Save stats.
write_csv(x = osm_stats_df, path = "data/osm_stats_rounded_k6.csv")

# Long format stats table.
osm_stats_long <- osm_stats_df %>% 
  mutate(traj_titles = str_extract(traj_titles, "^.{3}")) %>% 
  pivot_longer(cols = -traj_titles, names_to = "Statistic", values_to = "hello") %>% 
  pivot_wider(id_cols = Statistic, names_from = traj_titles, values_from = hello)

# Save stats.
write_csv(x = osm_stats_long, path = "data/osm_stats_long.csv")

# OSM stats visual.
osm_stats_gg <- traj_names_osm_df %>%
  select(traj_titles, nightlife_total, shops_total, transport_total, bikes) %>%
  pivot_longer(cols = -traj_titles, names_to = "type", values_to = "count") %>%
  ggplot(.) +
  geom_histogram(mapping = aes(x = count), bins = 30) +
  facet_wrap(~type + traj_titles, scales = "free_y", ncol = 6) +
  theme_bw() +
  theme(axis.text = element_text(size = 6))

# Save.
ggsave(plot = osm_stats_gg, filename = "visuals/osm_plots_k6.png", height = 7, width = 12)
