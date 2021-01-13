# Load packages.
library(lorenzgini)
library(spdep)
library(readr)
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
df_2020 <- bind_rows(data_2020)
df_2019 <- bind_rows(data_2019)

# Create year ID, then remove Greater Manchester Police and Police Service of NI.
df_2020_sub <- df_2020 %>% 
  mutate(Year = "2020") %>% 
  filter(`Reported by` != "Greater Manchester Police", 
         `Reported by` != "Police Service of Northern Ireland")

df_2019_sub <- df_2019 %>% 
  mutate(Year = "2019") %>% 
  filter(`Reported by` != "Greater Manchester Police", 
         `Reported by` != "Police Service of Northern Ireland")

# Remove existing data objects to free up memory if needed.
rm(data_2019, data_2020, df_2019, df_2020)

# Check months.
unique(df_2020_sub$Month)
unique(df_2019_sub$Month)

# Check crime types.
unique(df_2020_sub$`Crime type`)
unique(df_2019_sub$`Crime type`)

# Check missings.
sum(is.na(df_2020_sub$Month))
sum(is.na(df_2019_sub$Month))

sum(is.na(df_2020_sub$`Crime type`))
sum(is.na(df_2019_sub$`Crime type`))

sum(is.na(df_2020_sub$`LSOA code`)) # ~96k crimes have no LSOA.
sum(is.na(df_2019_sub$`LSOA code`)) # ~160k crimes have no LSOA.

# Drop crimes with missing LSOA (!!!).
df_2020_nm_sub <- drop_na(df_2020_sub, `LSOA code`)
df_2019_nm_sub <- drop_na(df_2019_sub, `LSOA code`)

# Remove previous objects to free up memory if needed.
rm(df_2020_sub, df_2019_sub)

# Aggregate by month (N = 11), crime type (N = 14), and LSOA (N = ~33,000). Keep 'Year' in for later use.
df_2020_agg <- df_2020_nm_sub %>% 
  group_by(`Crime type`, Month, `LSOA code`, Year) %>% 
  summarise(crime_count = n()) %>% 
  ungroup() %>% 
  complete(`Crime type`, Month, `LSOA code`, Year, fill = list(crime_count = 0))

df_2019_agg <- df_2019_nm_sub %>% 
  group_by(`Crime type`, Month, `LSOA code`, Year) %>% 
  summarise(crime_count = n()) %>% 
  ungroup() %>% 
  complete(`Crime type`, Month, `LSOA code`, Year, fill = list(crime_count = 0))

# Check number of each LSOAs appearing in each year. There is a difference.
# This will be because some LSOAs had no crimes, and thus never even appeared in individual records.
# Note that we DO have zeros in this data for some LSOAs in some months, but that will be because there was
# a crime recorded in those LSOA in another month.
length(unique(df_2020_agg$`LSOA code`))
length(unique(df_2019_agg$`LSOA code`))

# Remove previous objects to free up memory if needed.
rm(df_2020_nm_sub, df_2019_nm_sub)

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

# There are multiple islands in the dataset, but most are not considered a problem, since they are LSOAs
# which share areas of the mainland or part of major islands (e.g. Isle of Wight). Of course,
# crimes on the islands cannot be considered 'neighbours' with those on the shore, but so few crimes
# actually occurr on these islands that it was deemed less of a problem than arbitarily removing them.
# That said, one island has no corresponding LSOA on the mainland, and thus would have no neighbours in
# a continuity matrix. This becomes apparent later - just making a note for now.

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

# Remove these LSOA from the crime data.
df_2020_sub_agg <- df_2020_agg %>% 
  filter(`LSOA code` %nin% gm_lsoa, `LSOA code` %nin% ni_lsoa)

df_2019_sub_agg <- df_2019_agg %>% 
  filter(`LSOA code` %nin% gm_lsoa, `LSOA code` %nin% ni_lsoa)


# Check that the crime data also doesn't include LSOAs from outside of England and Wales.
df_2020_sub_agg <- df_2020_sub_agg %>% 
  mutate(country_cd = str_extract(`LSOA code`, "^.{1}"))

table(df_2020_sub_agg$country_cd) # Confirmed.

df_2019_sub_agg <- df_2019_sub_agg %>% 
  mutate(country_cd = str_extract(`LSOA code`, "^.{1}"))

table(df_2019_sub_agg$country_cd) # Confirmed.

# Check LSOA in each. Confirms that police data has slightly less (N = 4) LSOA than the total data.
length(unique(lsoa_ew_valid_sf$geo_code))
length(unique(df_2019_sub_agg$`LSOA code`))
length(unique(df_2020_sub_agg$`LSOA code`))

# Pull out these 4 LSOA (for 2019 and 2020).
# All four are in London, so it's highly likely this is an error, rather than due to zero crimes.
missing_lsoa <- lsoa_ew_sf %>% 
  filter(lsoa_ew_sf$geo_code %nin% df_2020_sub_agg$`LSOA code`)

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
length(unique(lsoa_ew_valid_sf$geo_code)) # 33076
length(unique(df_2019_sub_agg$`LSOA code`))  # 33076
length(unique(df_2020_sub_agg$`LSOA code`))  # 33076

# Save and load workspace as appropriate.
# save.image(file = "data_handling.RData")
# load(file = "data_handling.RData")

# Load in urban-rural classification.
urban_df <- read_csv("data/Rural_Urban_Classification__2011__of_Lower_Layer_Super_Output_Areas_in_England_and_Wales.csv")

# Check classications.
table(urban_df$RUC11)
table(urban_df$RUC11CD)

# Create a broader urban and rural distinction.
urban_df <- urban_df %>% 
  mutate(urban_rural = if_else(condition = str_detect(string = RUC11, pattern = "Rural"), true = "Rural", false = "Urban"))

# Check classications.
table(urban_df$urban_rural)

# Append to the crime data.
df_2020_sub_agg <- left_join(df_2020_sub_agg, urban_df, by = c("LSOA code" = "LSOA11CD"))
df_2019_sub_agg <- left_join(df_2019_sub_agg, urban_df, by = c("LSOA code" = "LSOA11CD"))

# Check missings.
sum(is.na(df_2020_sub_agg$urban_rural))
sum(is.na(df_2019_sub_agg$urban_rural))

# Append to spatial data.
lsoa_ew_valid_sf <- left_join(lsoa_ew_valid_sf, urban_df, by = c("geo_code" = "LSOA11CD"))

# Check (takes a while).
# ggplot(data = lsoa_ew_complete_sf) +
#   geom_sf(mapping = aes(fill = urban_rural), colour = "transparent")

# Remove objects to retain memory if needed.
rm(lsoa_ew_sf, lsoa_sf, df_2020_agg, df_2019_agg)

# Combine 2019 and 2029 datasets for next steps.
df_1920_df <- bind_rows(df_2019_sub_agg, df_2020_sub_agg)

# Calculate E&W-wide Generalized Gini coefficient for each year.
df_1920_total_gini <- df_1920_df %>%
  group_by(Year, Month, `Crime type`) %>% 
  summarise(gini_coef = gini(crime_count, generalized = TRUE, unbiased = TRUE)) %>% 
  ungroup() %>% 
  separate(Month, into = c("Year_temp","Month"), sep = "-") %>% 
  select(-Year_temp) 

# Initial plot.
total_gini_gg <- ggplot(data = df_1920_total_gini) +
  geom_line(mapping = aes(x = Month, y = gini_coef, group = `Crime type`, colour = `Crime type`)) +
  facet_wrap(~ Year) +
  scale_x_discrete(labels = str_extract(month.name, "^.{3}")) +
  theme_bw()

# Save.
ggsave(filename = "visuals/total_gini.png", height = 9, width = 25, unit = "cm")


# Do the same but group by the urban rural distinction.
df_1920_rural_gini <- df_1920_df %>%
  group_by(Year, Month, `Crime type`, urban_rural) %>% 
  summarise(gini_coef = gini(crime_count, generalized = TRUE, unbiased = TRUE)) %>% 
  ungroup() %>% 
  separate(Month, into = c("Year_temp","Month"), sep = "-") %>% 
  select(-Year_temp) 

# Initial plot.
rural_gini_gg <- ggplot(data = df_1920_rural_gini) +
  geom_line(mapping = aes(x = Month, y = gini_coef, group = `Crime type`, colour = `Crime type`)) +
  geom_vline(xintercept = 3.7, linetype = "dotted") +
  scale_x_discrete(labels = str_extract(month.name, "^.{3}")) +
  facet_wrap(~ urban_rural + Year) +
  theme_bw()
  
# Save.
ggsave(filename = "visuals/rural_gini.png", height = 16, width = 25, unit = "cm")

# Remove objects to free memory as needed.
rm(df_2019_sub_agg, df_2020_sub_agg)

# Save and load workspace as appropriate.
# save.image(file = "data_handling.RData") # ***Last save*** dated 13 January 2021.
load(file = "data_handling.RData")

# Global Moran's I for E&W.

# Creating queens continuity matrix is problematic due to memory.
# lsoa_nb <- poly2nb(pl = lsoa_ew_complete_sf, queen = TRUE)

# Save lsoa_ew_complete_sf object as shapefile for use in GeoDa.
# st_write(obj = lsoa_ew_valid_sf, dsn = "data/lsoa_ew_valid_sf.shp")

# Rook contuinity matrix created in GeoDa Version 1.14.0. Descriptive statistics screenshot
# has been saved in the 'img' folder of this project.
lsoa_gal <- read.gal(file = "data/lsoa_ew_valid_sf.gal", override.id = TRUE)
lsoa_listW <- nb2listw(lsoa_gal, style = "W", zero.policy = TRUE)

# Check summary. Zero poly is TRUE because GeoDa warned about an island LSOA (which has no neighbour,
# and also no part of the LSOA on mainland - the Isle of Scilly). This is confirmed using spdep.
summary(lsoa_listW, zero.policy = TRUE)

# We now remove the Isle of Scilly LSOA.
lsoa_ew_valid_noisl_sf <- lsoa_ew_valid_sf %>% 
  filter(geo_code != "E01019077")

# Append the crime data to the spatial object in order to compute the Global Moran's I.

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






