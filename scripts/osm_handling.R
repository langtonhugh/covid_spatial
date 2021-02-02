library(dplyr)
library(osmdata)
library(sf)
library(stringr)
library(ggplot2)
library(tidyr)
library(readr)

# Load E&W.
lsoa_sf <- st_read("data/ukds_infuse_lsoa/infuse_lsoa_lyr_2011_clipped.shp")

# Subset LSOAs for E&W and then remove Greater Manchester.
lsoa_ew_sf <- lsoa_sf %>% 
  mutate(country_cd = str_extract(geo_code, "^.{1}")) %>% 
  filter(country_cd == "E" | country_cd == "W",
         !str_detect(geo_label, "Manchester|Bolton|Oldham|Rochdale|Stockport|Tameside|Salford|Bury|Trafford|Wigan"))

# Validity checks to be done once we get to the aggregation stage.

# Get bounding box of E&W.
bb_sf <- lsoa_ew_sf %>% 
  st_transform(crs = 4326) %>% 
  st_bbox() 

ew_bb <- c(bb_sf[[1]], bb_sf[[2]], bb_sf[[3]], bb_sf[[4]])

# Pull pubs in bb
pubs_sf <- opq(bbox = ew_bb, timeout = 100) %>%
  add_osm_feature(key = "amenity", value = "pub") %>% 
  osmdata_sf()

# Pull points only.
pubs_pts_sf <- pubs_sf$osm_points

# We drop any pub that does not have a name allocated to it. This based on an overview of OSM
# data, some LSOAs had upto ~400 pubs, but most were not named. If they have no name, I doubt
# it's a legitimate to widely accepted definition of a pub. This actually what OverPass Turbo does
# automatically (see https://overpass-turbo.eu/).
# Doing this drops data from ~174k to ~21k which is closer to other estimates of the N pubs in E&W.
pubs_pts_complete_sf <- pubs_pts_sf %>%
  drop_na(name)

# Remove the original query to save memory.
rm(pubs_sf)

# Check duplicates (none).
length(unique(pubs_pts_complete_sf$geometry))

# Project to BNG
pubs_pts_complete_sf <- pubs_pts_complete_sf %>% 
  st_transform(crs = 27700)

# As per police_data_handling.r there are invalid geometries (N = 4), so let's resolve them.
lsoa_ew_valid_sf <- st_make_valid(lsoa_ew_sf)

# Aggregate pub points to LSOA.
lsoa_ew_valid_sf <- lsoa_ew_valid_sf %>% 
  mutate(pubs = lengths(st_intersects(lsoa_ew_valid_sf, pubs_pts_complete_sf)))

# Which LSOA has the most pubs?
most_pubs_sf <- lsoa_ew_valid_sf %>% 
  arrange(desc(pubs)) %>% 
  slice(1)

test_pub_sf <- st_intersection(most_pubs_sf, pubs_pts_complete_sf)

# An investigation into bars as above suggested that many pubs are also labelled as bars, so
# to avoid duplicates, we have stuck with pubs. Also likely to be similar areas anyway (e.g. the 
# same LSOA in City of London held both the pub and bar top, but had many duplicates).

# Perform the same thing but for nightclubs too.
nightclubs_sf <- opq(bbox = ew_bb, timeout = 100) %>%
  add_osm_feature(key = "amenity", value = "nightclub") %>% 
  osmdata_sf()

# Pull points only.
nightclubs_pts_sf <- nightclubs_sf$osm_points

# Remove original.
rm(nightclubs_sf)

# Names present.
nightclubs_pts_complete_sf <- nightclubs_pts_sf %>%
  drop_na(name)

# Check duplicates (none).
length(unique(nightclubs_pts_complete_sf$geometry))

# Project
nightclubs_pts_complete_sf <- nightclubs_pts_complete_sf %>% 
  st_transform(crs = 27700)

# Aggregate bar points to LSOA.
lsoa_ew_valid_sf <- lsoa_ew_valid_sf %>% 
  mutate(nightclubs = lengths(st_intersects(lsoa_ew_valid_sf, nightclubs_pts_complete_sf)))

# Which LSOA has the most nightclubs?
most_nightclubs_sf <- lsoa_ew_valid_sf %>% 
  arrange(desc(nightclubs)) %>% 
  slice(1)

test_nightclubs_sf <- st_intersection(most_nightclubs_sf, nightclubs_pts_complete_sf)

# Same for restaurants.
restaurants_sf <- opq(bbox = ew_bb, timeout = 100) %>%
  add_osm_feature(key = "amenity", value = "restaurant") %>% 
  osmdata_sf()

# Pull points only.
restaurants_pts_sf <- restaurants_sf$osm_points

# Remove original.
rm(restaurants_sf)

# Names present.
restaurants_pts_complete_sf <- restaurants_pts_sf %>%
  drop_na(name)

# Check duplicates (none).
length(unique(restaurants_pts_complete_sf$geometry))

# Project
restaurants_pts_complete_sf <- restaurants_pts_complete_sf %>% 
  st_transform(crs = 27700)

# Aggregate bar points to LSOA.
lsoa_ew_valid_sf <- lsoa_ew_valid_sf %>% 
  mutate(restaurants = lengths(st_intersects(lsoa_ew_valid_sf, restaurants_pts_complete_sf)))

# Same for commercial clothes outlets.
shops_sf <- opq(bbox = ew_bb, timeout = 100) %>%
  add_osm_feature(key = "shop", value = "clothes") %>% 
  osmdata_sf()

# Pull points only.
shops_pts_sf <- shops_sf$osm_points

# Remove original.
rm(shops_sf)

# Names present.
shops_pts_complete_sf <- shops_pts_sf %>%
  drop_na(name)

# Check duplicates (none).
length(unique(shops_pts_complete_sf$geometry))

# Project
shops_pts_complete_sf <- shops_pts_complete_sf %>% 
  st_transform(crs = 27700)

# Aggregate bar points to LSOA.
lsoa_ew_valid_sf <- lsoa_ew_valid_sf %>% 
  mutate(clothes_shops = lengths(st_intersects(lsoa_ew_valid_sf, shops_pts_complete_sf)))

# Same for commercial shoe outlets.
shoes_sf <- opq(bbox = ew_bb, timeout = 100) %>%
  add_osm_feature(key = "shop", value = "shoes") %>% 
  osmdata_sf()

# Pull points only.
shoes_pts_sf <- shoes_sf$osm_points


# Remove original.
rm(shoes_sf)

# Names present.
shoes_pts_complete_sf <- shoes_pts_sf %>%
  drop_na(name)

# Check duplicates (none).
length(unique(shoes_pts_complete_sf$geometry))

# Project
shoes_pts_complete_sf <- shoes_pts_complete_sf %>% 
  st_transform(crs = 27700)

# Aggregate bar points to LSOA.
lsoa_ew_valid_sf <- lsoa_ew_valid_sf %>% 
  mutate(shoe_shops = lengths(st_intersects(lsoa_ew_valid_sf, shoes_pts_complete_sf)))

# Same for mall outlets.
malls_sf <- opq(bbox = ew_bb, timeout = 100) %>%
  add_osm_feature(key = "shop", value = "mall") %>% 
  osmdata_sf()

# Pull points only.
malls_pts_sf <- malls_sf$osm_points


# Remove original.
rm(malls_sf)


# Names present.
malls_pts_complete_sf <- malls_pts_sf %>%
  drop_na(name)

# Check duplicates (none).
length(unique(malls_pts_complete_sf$geometry))

# Project
malls_pts_complete_sf <- malls_pts_complete_sf %>% 
  st_transform(crs = 27700)

# Aggregate bar points to LSOA.
lsoa_ew_valid_sf <- lsoa_ew_valid_sf %>% 
  mutate(malls_shops = lengths(st_intersects(lsoa_ew_valid_sf, malls_pts_complete_sf)))

# Same for department outlets.
deps_sf <- opq(bbox = ew_bb, timeout = 100) %>%
  add_osm_feature(key = "shop", value = "department_store") %>% 
  osmdata_sf()

# Pull points only.
deps_pts_sf <- deps_sf$osm_points

# Remove original.
rm(deps_sf)

# Names present.
deps_pts_complete_sf <- deps_pts_sf %>%
  drop_na(name)


# Check duplicates (none).
length(unique(deps_pts_complete_sf$geometry))

# Project
deps_pts_complete_sf <- deps_pts_complete_sf %>% 
  st_transform(crs = 27700)

# Aggregate bar points to LSOA.
lsoa_ew_valid_sf <- lsoa_ew_valid_sf %>% 
  mutate(deps_shops = lengths(st_intersects(lsoa_ew_valid_sf, deps_pts_complete_sf)))

# Same for supermarket outlets.
supermarks_sf <- opq(bbox = ew_bb, timeout = 100) %>%
  add_osm_feature(key = "shop", value = "supermarket") %>% 
  osmdata_sf()

# Pull points only.
supermarks_pts_sf <- supermarks_sf$osm_points

# Names present.
supermarks_pts_complete_sf <- supermarks_pts_sf %>%
  drop_na(name)

# Remove original.
rm(supermarks_pts_sf)

# Check duplicates (none).
length(unique(supermarks_pts_complete_sf$geometry))

# Project
supermarks_pts_complete_sf <- supermarks_pts_complete_sf %>% 
  st_transform(crs = 27700)

# Aggregate bar points to LSOA.
lsoa_ew_valid_sf <- lsoa_ew_valid_sf %>% 
  mutate(supermarks_shops = lengths(st_intersects(lsoa_ew_valid_sf, supermarks_pts_complete_sf)))

# Same for chemists outlets.
general_sf <- opq(bbox = ew_bb, timeout = 100) %>%
  add_osm_feature(key = "shop", value = "chemist") %>% 
  osmdata_sf()

# Pull points only.
general_pts_sf <- general_sf$osm_points

# Remove original.
rm(general_sf)

# Names present.
general_pts_complete_sf <- general_pts_sf %>%
  drop_na(name)

# Check duplicates (none).
length(unique(general_pts_complete_sf$geometry))

# Project
general_pts_complete_sf <- general_pts_complete_sf %>% 
  st_transform(crs = 27700)

# Aggregate bar points to LSOA.
lsoa_ew_valid_sf <- lsoa_ew_valid_sf %>% 
  mutate(general_shops = lengths(st_intersects(lsoa_ew_valid_sf, general_pts_complete_sf)))

# Same for cosmetic outlets.
cosmetic_sf <- opq(bbox = ew_bb, timeout = 100) %>%
  add_osm_feature(key = "shop", value = "cosmetics") %>% 
  osmdata_sf()

# Pull points only.
cosmetic_pts_sf <- cosmetic_sf$osm_points

# Names present.
cosmetic_pts_complete_sf <- cosmetic_pts_sf %>%
  drop_na(name)

# Remove original.
rm(cosmetic_pts_sf)

# Check duplicates (none).
length(unique(cosmetic_pts_complete_sf$geometry))

# Project
cosmetic_pts_complete_sf <- cosmetic_pts_complete_sf %>% 
  st_transform(crs = 27700)

# Aggregate bar points to LSOA.
lsoa_ew_valid_sf <- lsoa_ew_valid_sf %>% 
  mutate(cosmetic_shops = lengths(st_intersects(lsoa_ew_valid_sf, cosmetic_pts_complete_sf)))

# Same for electircal outlets.
electric_sf <- opq(bbox = ew_bb, timeout = 100) %>%
  add_osm_feature(key = "shop", value = "electrical") %>% 
  osmdata_sf()

# Pull points only.
electric_pts_sf <- electric_sf$osm_points

# Remove original.
rm(electric_sf)


# Names present.
electric_pts_complete_sf <- electric_pts_sf %>%
  drop_na(name)

# Check duplicates (none).
length(unique(electric_pts_complete_sf$geometry))

# Project
electric_pts_complete_sf <- electric_pts_complete_sf %>% 
  st_transform(crs = 27700)

# Aggregate bar points to LSOA.
lsoa_ew_valid_sf <- lsoa_ew_valid_sf %>% 
  mutate(electric_shops = lengths(st_intersects(lsoa_ew_valid_sf, electric_pts_complete_sf)))

# Same for chemist outlets.
chemist_sf <- opq(bbox = ew_bb, timeout = 100) %>%
  add_osm_feature(key = "shop", value = "chemist") %>% 
  osmdata_sf()

# Pull points only.
chemist_pts_sf <- chemist_sf$osm_points

# Remove original.
rm(chemist_sf)

# Names present.
chemist_pts_complete_sf <- chemist_pts_sf %>%
  drop_na(name)

# Check duplicates (none).
length(unique(chemist_pts_complete_sf$geometry))

# Project
chemist_pts_complete_sf <- chemist_pts_complete_sf %>% 
  st_transform(crs = 27700)

# Aggregate bar points to LSOA.
lsoa_ew_valid_sf <- lsoa_ew_valid_sf %>% 
  mutate(chemist_shops = lengths(st_intersects(lsoa_ew_valid_sf, chemist_pts_complete_sf)))

# Same for greengrocer outlets.
greengrocer_sf <- opq(bbox = ew_bb, timeout = 100) %>%
  add_osm_feature(key = "shop", value = "greengrocer") %>% 
  osmdata_sf()

# Pull points only.
greengrocer_pts_sf <- greengrocer_sf$osm_points

# Names present.
greengrocer_pts_complete_sf <- greengrocer_pts_sf %>%
  drop_na(name)

# Check duplicates (none).
length(unique(greengrocer_pts_complete_sf$geometry))

# Project
greengrocer_pts_complete_sf <- greengrocer_pts_complete_sf %>% 
  st_transform(crs = 27700)

# Aggregate bar points to LSOA.
lsoa_ew_valid_sf <- lsoa_ew_valid_sf %>% 
  mutate(greengrocer_shops = lengths(st_intersects(lsoa_ew_valid_sf, greengrocer_pts_complete_sf)))


# Check
names(lsoa_ew_valid_sf)

lsoa_ew_valid_sf <- lsoa_ew_valid_sf %>% 
  mutate(shops_total = shoe_shops + malls_shops + deps_shops + clothes_shops + supermarks_shops +
           electric_shops + chemist_shops + greengrocer_shops,
         nightlife_total   = pubs + nightclubs + restaurants)

osm_df <- lsoa_ew_valid_sf %>% 
  as_tibble() %>% 
  select(geo_code, geo_label, pubs, nightclubs, nightlife_total, restaurants,
         shoe_shops, deps_shops, supermarks_shops, clothes_shops,
         electric_shops, chemist_shops, greengrocer_shops, shops_total)

sum(osm_df$nightlife_total)
sum(osm_df$shops_total)

# Save for use in main workspace.
write_csv(x = osm_df, path = "data/osm.csv")

# Save workspace.
# save.image("osm.RData")

# Train stops
train_sf <- opq(bbox = ew_bb, timeout = 100) %>%
  add_osm_feature(key = "railway", value = "station") %>% 
  osmdata_sf()

# Points.
train_pts_sf <- train_sf$osm_points

# Complete.
train_pts_complete_sf <- train_pts_sf %>% 
  drop_na(name)

# Deplicates (none).
length(unique(train_pts_complete_sf$geometry)) 

# Project
train_pts_complete_sf <- train_pts_complete_sf %>% 
  st_transform(crs = 27700)

# Aggregate bar points to LSOA.
lsoa_ew_valid_sf <- lsoa_ew_valid_sf %>% 
  mutate(trains = lengths(st_intersects(lsoa_ew_valid_sf, train_pts_complete_sf)),
         train_bin = if_else(trains > 0, 1, 0))

# Bus stops
bus_sf <- opq(bbox = ew_bb, timeout = 200) %>%
  add_osm_feature(key = "highway", value = "bus_stop") %>% 
  osmdata_sf()

# Points.
bus_pts_sf <- bus_sf$osm_points

# Complete.
bus_pts_complete_sf <- bus_pts_sf %>% 
  drop_na(name)

# Deplicates (very few, understandible for bus stops nationwide).
length(unique(bus_pts_complete_sf$geometry)) 

# Project
bus_pts_complete_sf <- bus_pts_complete_sf %>% 
  st_transform(crs = 27700)

# Aggregate bar points to LSOA.
lsoa_ew_valid_sf <- lsoa_ew_valid_sf %>% 
  mutate(bus = lengths(st_intersects(lsoa_ew_valid_sf, bus_pts_complete_sf)),
         bus_bin = if_else(bus > 0, 1, 0))

# subset for join
osm_transport_df <- lsoa_ew_valid_sf %>% 
  as_tibble() %>% 
  select(-geometry, -geo_labelw, -country_cd)

# Open OSM data.
osm_df <- read_csv("data/osm.csv")

# Join.
osm_full_df <- left_join(osm_df, osm_transport_df)

# Save.
write_csv(x = osm_full_df, path = "data/osm_full.csv")

# Save workspace
save.image(file = "osm_transport.RData")

# Load, round and save again.
osm_full_df <- read_csv(file = "data/osm_full.csv")

osm_full_df <- osm_full_df %>% 
  mutate_all()

