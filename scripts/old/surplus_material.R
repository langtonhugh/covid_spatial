
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

