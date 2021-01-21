
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

# First attempt used GeoDa because it's computationally quick.
# Save lsoa_ew_complete_sf object as shapefile for use in GeoDa.
st_write(obj = lsoa_ew_valid_sf, dsn = "data/lsoa_ew_valid_sf.shp")
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

# Creating queens continuity matrix is problematic due to memory. Due to the shape of LSOAs, I don't think
# it is even necessary, so we use rook.
# lsoa_nb <- poly2nb(pl = lsoa_ew_valid_sf, queen = FALSE)
# lsoa_listW <- nb2listw(lsoa_nb, style = "W", zero.policy = TRUE)
# summary(lsoa_listW, zero.policy = TRUE) # average number of links ~5.7
# lsoa_ew_valid_sf[1285, ] # Isle of Scilly is the neighborless LSOA.

# First, let's run Global/Local Moran's I comparing April 2019 and April 2020. 

# Calculate total notifiable offences per LSOA.
# total_crime_lsoa_df <- sub_data_agg_1920_df %>%
#   filter(crime_type != "Anti-social behaviour", 
#          month == "2019-04" | month == "2020-04") %>% 
#   group_by(month, lsoa_code) %>% 
#   summarise(total_crime = sum(crime_count)) %>% 
#   ungroup() %>% 
#   complete(month, lsoa_code, fill = list(total_crime = 0)) %>% 
#   pivot_wider(id_cols = lsoa_code, names_from = month, values_from = total_crime) %>% 
#   rename(april_2019 = `2019-04`,
#          april_2020 = `2020-04`)

# As of 18 January, temporarily halting the Moran's I stuff. Intead, we generate a descriptive
# visual for ASB change between deciles, and and then total crime. We can then run a test
# of statistical signifiance using poisson-specific test.