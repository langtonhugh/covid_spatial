
aprils_lsoa_wide_df <- aprils_lsoa_df %>% 
  select(lsoa_code, month, crime_count, decile) %>% 
  pivot_wider(id_cols = lsoa_code, names_from = month, values_from = c(crime_count, decile)) %>% 
  rename(cc_april19 = `crime_count_2019-04`,
         cc_april20 = `crime_count_2020-04`,
         dc_april19 = `decile_2019-04`,
         dc_april20 = `decile_2020-04`) %>% 
  mutate(cc_diff    = cc_april20-cc_april19,
         perc_diff  = ) %>% 
  select(lsoa_code, cc_diff, dc_april19) 

ggplot(data = aprils_lsoa_wide_df) +
  geom_jitter(mapping = aes(x = as.factor(dc_april19),
                            y = cc_diff,
                            group = dc_april19,
                            colour = as.factor(dc_april19)),
              alpha = 0.3, pch = 20) +
  geom_boxplot(mapping = aes(x = as.factor(dc_april19),
                             y = cc_diff,
                             group = dc_april19), outlier.shape = NA) +
  theme(legend.position = "none") 