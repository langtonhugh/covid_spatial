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
library(factoextra)

# Useful function.
`%nin%` <- Negate(`%in%`)


# Archive data downloaded January 2012, covering the 3-year period up to and including November 2020.
# if(!file.exists('data')){dir.create("data")}
# download.file(url = "https://data.police.uk/data/archive/2020-11.zip", destfile = "data/archive2020-11.zip")

# Unzip.
# unzip(zipfile = "data/archive2020-11.zip", exdir = "data")

# Note the loading and handling data is largely determined by available memory. Many people may be able to
# run things more efficiently (and in a more straightforward order) with a more powerful computer.

# List all those 'street' files (rather than outcomes or stop and search).
list_2020 <- paste("data/", list.files("data", pattern = glob2rx("2020*street.csv"),  recursive=TRUE), sep = "")
list_2019 <- paste("data/", list.files("data", pattern = glob2rx("2019*street.csv"),  recursive=TRUE), sep = "")

# Add December 2020 manually to avoid conflicts with the main paper.
wd <- "C:/Users/langt/OneDrive - University of Leeds/Lenovo/Leeds_covid/spatial/dec_2020/"
list_dec_2020 <- paste(wd, list.files(wd, pattern = glob2rx("2020*street.csv"),  recursive=TRUE), sep = "")

# Read in .csv files for each year.
data_2020     <- lapply(list_2020    , read_csv)
data_dec_2020 <- lapply(list_dec_2020, read_csv)
data_2019     <- lapply(list_2019    , read_csv)

# Bind each in to data frames. For now, we keep the years separate.
full_data_2020 <- data_2020 %>% 
  bind_rows() %>% 
  clean_names() %>% 
  mutate(year = "2020") %>% 
  filter(crime_type == "Burglary")

full_dec_data_2020 <- data_dec_2020 %>% 
  bind_rows() %>% 
  clean_names() %>% 
  mutate(year = "2020") %>% 
  filter(crime_type == "Burglary")

full_data_2020 <- bind_rows(full_data_2020, full_dec_data_2020)

full_data_2019 <- data_2019 %>% 
  bind_rows() %>% 
  clean_names() %>% 
  mutate(year = "2019") %>% 
  filter(crime_type == "Burglary")

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

# Bind each in to data frames, clean and subset for burg only. For now, we keep the years separate.
full_data_2018 <- data_2018 %>% 
  bind_rows() %>% 
  clean_names() %>% 
  mutate(year = "2018") %>% 
  filter(crime_type == "Burglary")

# Create year ID, then remove Greater Manchester Police and Police Service of NI.
sub_data_2018 <- full_data_2018 %>% 
  filter(reported_by != "Greater Manchester Police", 
         reported_by != "Police Service of Northern Ireland")

# Remove object for memory as needed.
rm(data_2018, full_data_2018)

# Drop crimes with missing LSOA.
sub_data_2020 <- drop_na(sub_data_2020, lsoa_code)
sub_data_2019 <- drop_na(sub_data_2019, lsoa_code)
sub_data_2018 <- drop_na(sub_data_2018, lsoa_code)

# Bind together now for aggregation. For burg only, many LSOA are missing across years due to sparse
# counts, so this tackles this issue earlier on.
sub_data_181920_df <- bind_rows(sub_data_2018, sub_data_2019, sub_data_2020)

# Remove objects.
rm(sub_data_2018, sub_data_2019, sub_data_2020)

# Save worksapce.
save.image("burg_replication.RData")
# load("burg_replication.RData")

# Aggregate by month (N = 11), and LSOA (N = ~33,000). Keep 'Year' in for later use.
sub_data_181920_agg_df <- sub_data_181920_df %>% 
  group_by(month, lsoa_code) %>% 
  summarise(crime_count = n()) %>% 
  ungroup() %>% 
  complete(month, lsoa_code, fill = list(crime_count = 0)) 

# Remove.
rm(sub_data_181920_df)

# Missings? No.
sum(is.na(sub_data_181920_agg_df))

# But note that the number of LSOA is still lower than E&W, but these are LSOA that had no drug crimes
# across the 3 years, and the 4 LSOA with no snap points.
length(unique(sub_data_181920_agg_df$lsoa_code))

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
# validity_check <- st_is_valid(lsoa_ew_sf)

# N = 4 invalid geometries.
# table(validity_check) 

# Identify them
# lsoa_ew_sf <- lsoa_ew_sf %>% 
# mutate(valid = validity_check)

# filter(lsoa_ew_sf, valid == "FALSE")

# Resolve invalid geometries.
# lsoa_ew_valid_sf <- st_make_valid(lsoa_ew_sf)
lsoa_ew_valid_sf <- lsoa_ew_sf

# Check. It worked.
# table(st_is_valid(lsoa_ew_valid_sf))

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
sub_data_181920_agg_df <- sub_data_181920_agg_df %>% 
  filter(lsoa_code %nin% gm_lsoa, lsoa_code %nin% ni_lsoa)

# Create country ID.
sub_data_181920_agg_df <- sub_data_181920_agg_df %>% 
  mutate(country_cd = str_extract(lsoa_code, "^.{1}"))

# Check that the crime data now doesn't include LSOAs from outside of England and Wales.
table(sub_data_181920_agg_df$country_cd) # Confirmed.

# Check LSOA in each.
# Because we are now dealing with burg, we don't have matching LSOA N anymore.
length(unique(lsoa_ew_valid_sf$geo_code))
length(unique(sub_data_181920_agg_df$lsoa_code))

# Create counts by month.
monthly_agg_df <- sub_data_181920_agg_df %>% 
  group_by(month) %>% 
  summarise(ew_crime_count = sum(crime_count)) %>% 
  ungroup() %>% 
  separate(month, into = c("year", "month"), sep = "-")

# Single plot.
burg_raw_counts_gg <- ggplot(data = monthly_agg_df) +
  geom_line(mapping = aes(x = month, y = ew_crime_count, group = year, colour = year), size = 0.8) +
  geom_vline(xintercept = 2.7, linetype = "dotted") +
  scale_x_discrete(labels = c(" ", str_extract(month.name[2:12], "^.{3}"))) +
  scale_color_manual(values = rev(c("black", "darkgrey", "lightgrey"))) +
  labs(x = NULL, y = NULL, colour = NULL) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, vjust = -1.4, angle = 90), #, hjust = -0.4
        axis.ticks = element_line(size = 0.3, lineend = "round"),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        legend.position = "bottom")

# Save.
ggsave(plot = burg_raw_counts_gg, filename = "visuals/burg/burg_raw_counts_gg.png", width = 12, height = 9, unit = "cm")

# Calculate E&W-wide Generalized Gini coefficient.
gini_df <- sub_data_181920_agg_df %>%
  group_by(month) %>% 
  summarise(gini_coef = gini(crime_count, generalized = TRUE, unbiased = TRUE)) %>% 
  ungroup() %>% 
  separate(month, into = c("year","month"), sep = "-") %>% 
  select(gini_coef, year,  month) %>% 
  mutate(year = recode_factor(year, "2018" = "2018", "2019" = "2019", "2020" = "2020")) 

# Single plot.
gini_gg <- ggplot(data = gini_df) +
  geom_line(mapping = aes(x = month, y = gini_coef, group = year, colour = year), size = 0.8) +
  geom_vline(xintercept = 2.7, linetype = "dotted") +
  ylim(0, 1) +
  labs(x = NULL, y = NULL, colour = NULL) +
  scale_x_discrete(labels = c(" ", str_extract(month.name[2:12], "^.{3}"))) +
  scale_color_manual(values = rev(c("black", "darkgrey", "lightgrey"))) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 8, vjust = -1.4, angle = 90), #, hjust = -0.4
        axis.ticks = element_line(size = 0.3, lineend = "round"),
        axis.text.y = element_text(size = 6),
        axis.title.y = element_text(size = 6),
        legend.position = "bottom")

# Save.
ggsave(plot = gini_gg, filename = "visuals/burg/gini_gg.png", width = 12, height = 8, unit = "cm")

# Remove object to free up memory if needed.
rm(data_dec_2020)

######## Prepare the data for the longitudinal k-means.########

tc_kmeans_sub_df <- sub_data_181920_agg_df %>% 
  filter(str_detect(month, "2020")) 

# Check for those LSOAs which were essentially crime-free during the study period, and recode
# the month names to keep things simple. We keep in wide because that's what kml package likes.
tc_kmeans_sub_clean_df <- tc_kmeans_sub_df %>% 
  pivot_wider(id_cols = lsoa_code, names_from = "month", values_from = "crime_count") %>%
  mutate(total_months = rowSums(.[2:13])) %>% 
  filter(total_months != 0) %>% # pointless including zeros for the whole period.
  select(-total_months) %>% 
  rename(january = `2020-01`, february = `2020-02`,
         march = `2020-03`, april = `2020-04`, may = `2020-05`, june = `2020-06`,
         july = `2020-07` , august = `2020-08`, september = `2020-09`, october = `2020-10`,
         november = `2020-11`, december = `2020-12`)

# Perform kmeans analysis.

# Choose array of possible clusters.
n <- 3:6
# n <- 3:10 

# Set seed.
set.seed(1612)

# Convert to matrix, cluster data, and perform kmeans on all potential clusters from 3 to 10.
tc_kml_mat <- as.matrix(tc_kmeans_sub_clean_df[2:13])
tc_traj_burg    <- clusterLongData(traj = tc_kml_mat) 
kml(tc_traj_burg, nbClusters = n, toPlot = "criterion", nbRedrawing = 20)
#save(tc_traj, , file = 'results/tc_traj.Rdata') #save the file as it takes along time to compute.

######### Analysis of selected clusters (3) #############

# We select 4 clusters due to the closeness of 3-4 with Calinki, and usefulness of a fourth one.

# Collate LSOA codes and cluster labels into a data frame.
tc_clusters_df <- cbind.data.frame(lsoa_code = tc_kmeans_sub_clean_df$lsoa_code,
                                   traj      = getClusters(tc_traj_burg, 3)) 

# Compute cluster sizes.
as.data.frame(table(tc_clusters_df$traj)) %>% 
  mutate(total = sum(Freq),
         prop  = 100*Freq/total)

# Join cluster information back with main data frame.
sub_data_agg_full_df <- left_join(sub_data_181920_agg_df, tc_clusters_df)

# Basic visual of clusters.
sub_data_agg_full_df %>% 
  filter(str_detect(month, "2020")) %>%
  ggplot() +
  geom_violin(mapping  = aes(x = month, y = crime_count, fill = traj), adjust = 3) +
  stat_summary(mapping = aes(x = month, y = crime_count, group = traj),
               fun = "mean",  size = 0.8, geom = "line") +
  facet_wrap(~traj, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none")
