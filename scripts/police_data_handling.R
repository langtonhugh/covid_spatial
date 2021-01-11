# Load packages.
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(sf)

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

# Bind each in to data frames. For now, we keep them separate.
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

sum(is.na(df_2020_sub$`LSOA code`)) # 97935 crimes have no LSOA.
sum(is.na(df_2019_sub$`LSOA code`)) # 160541 crimes have no LSOA.

# Drop crimes with missing LSOA (!!!).
df_2020_nm_sub <- drop_na(df_2020_sub, `LSOA code`)
df_2019_nm_sub <- drop_na(df_2019_sub, `LSOA code`)

# Remove previous objects to free up memory if needed.
rm(df_2020_sub, df_2019_sub)

# Aggregate by month (N = 11), crime type (N = 14), and LSOA (N = ~33,000). 
df_2020_agg <- df_2020_nm_sub %>% 
  group_by(`Crime type`, Month, `LSOA code`) %>% 
  summarise(crime_count = n()) %>% 
  ungroup() %>% 
  complete(`Crime type`, Month, `LSOA code`, fill = list(crime_count = 0))

df_2019_agg <- df_2019_nm_sub %>% 
  group_by(`Crime type`, Month, `LSOA code`) %>% 
  summarise(crime_count = n()) %>% 
  ungroup() %>% 
  complete(`Crime type`, Month, `LSOA code`, fill = list(crime_count = 0))

# Check number of each LSOAs appearing in each year. There is a difference.
# This will be because some LSOAs had no crimes, and thus never even appeared in individual records.
# Note that we DO have zeros in this data for some LSOAs in some months, but that will be because there was
# a crime recorded in those LSOA in another month.
length(unique(df_2020_agg$`LSOA code`))
length(unique(df_2019_agg$`LSOA code`))

# Remove previous objects to free up memory if needed.
rm(df_2020_nm_sub, df_2019_nm_sub)

# Save and load workspace as appropriate.
# save.image(file = "data_handling.RData")
load(file = "data_handling.RData")

# To create zeros for the no-show LSOAs, first load in a complete set of LSOA codes. 

# Download LSOA from UK Data Service.
# download.file(url = "https://borders.ukdataservice.ac.uk/ukborders/easy_download/prebuilt/shape/infuse_lsoa_lyr_2011_clipped.zip",
#               destfile = "data/ukds_infuse_lsoa.zip")

# Unzip.
# unzip(zipfile = "data/ukds_infuse_lsoa.zip", exdir = "data/ukds_infuse_lsoa")

# Useful function.
`%nin%` <- Negate(`%in%`)

# Load.
lsoa_sf <- st_read("data/ukds_infuse_lsoa/infuse_lsoa_lyr_2011_clipped.shp")

# Identify LSOA within Northern Ireland and Greater Manchester. We have removed those crimes reported by
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

# Now do the same, but for the LSOA boundaries. 
lsoa_ew_sf <- lsoa_sf %>% 
  mutate(country_cd = str_extract(geo_code, "^.{1}")) %>% 
  filter(country_cd == "E" | country_cd == "W",
         !str_detect(geo_label, "Manchester|Bolton|Oldham|Rochdale|Stockport|Tameside|Salford|Bury|Trafford|Wigan"))

# Check that the crime data also doesn't include LSOAs from outside of England and Wales.
df_2020_sub_agg <- df_2020_sub_agg %>% 
  mutate(country_cd = str_extract(`LSOA code`, "^.{1}"))

table(df_2020_sub_agg$country_cd) # Confirmed.

df_2019_sub_agg <- df_2019_sub_agg %>% 
  mutate(country_cd = str_extract(`LSOA code`, "^.{1}"))

table(df_2019_sub_agg$country_cd) # Confirmed.

# Check LSOA in each. Confirms that police data has slightly less (N = 4) LSOA than the total data.
length(unique(lsoa_ew_sf$geo_code))
length(unique(df_2019_sub_agg$`LSOA code`))
length(unique(df_2020_sub_agg$`LSOA code`))

# Pull out these 4 LSOA (for 2019 and 2020).
# All four are in London, so it's highly likely this is an error, rather than due to zero crimes.
missing_lsoa <- lsoa_ew_sf %>% 
  filter(lsoa_ew_sf$geo_code %nin% df_2020_sub_agg$`LSOA code`)

# E01033493 is football stadium. No snap points?
# E01032775 only covers ends of streets near river - may have missed the crime snap points.
# E01004711 has some construction work, and a break in Google Street View availability. This
#           indicates another snap point street quirk.
# E01003179  appears to not contain actually roads which could therefore miss snap points.

