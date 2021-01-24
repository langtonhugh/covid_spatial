# Use longitudinal k-means to unpick the total noficiable crimes (excl. drugs trend) at LSOA level.
tc_asb_kmeans_df <- sub_data_agg_1920_df %>%
  filter(crime_type == "Anti-social behaviour",
         month != "2019-12") %>% 
  group_by(lsoa_code, month) %>% 
  summarise(ew_crime_count = sum(crime_count)) %>% 
  ungroup() 

# For now, only keep the lockdown period in the first visual: March to August.
tc_asb_kmeans_sub_df <- tc_asb_kmeans_df %>% 
  filter(month == "2020-03" | month == "2020-04" | month == "2020-05" | month == "2020-06" |
         month == "2020-07" | month == "2020-08")

# Check for those LSOAs which were essentially crime-free during the study period.
# We keep in wide because that's what kml likes.
tc_asb_kmeans_sub_clean_df <- tc_asb_kmeans_sub_df %>% 
  pivot_wider(id_cols = lsoa_code, names_from = "month", values_from = "ew_crime_count") %>%
  mutate(total_months = rowSums(.[2:7])) %>% 
  filter(total_months != 0) %>% # N = 111
  select(-total_months) %>% 
  rename(march = `2020-03`, april = `2020-04`, may = `2020-05`, june = `2020-06`,
         july = `2020-07` , august = `2020-08`)

# Perform kmeans
n <- 3:6

tc_asb_kml_mat <- as.matrix(tc_asb_kmeans_sub_clean_df[2:7])
tc_asb_traj    <- clusterLongData(traj = tc_asb_kml_mat) 
kml(tc_asb_traj, nbClusters = n, toPlot = "criterion", nbRedrawing = 20)

# Apppend clusters back with data frame.
tc_asb_clusters <- cbind.data.frame(lsoa_code   = tc_asb_kmeans_sub_clean_df$lsoa_code,
                                traj       = getClusters(tc_asb_traj, 3)) # We select 4 due to CH value.

tc_asb_clusters_df <- tc_asb_kmeans_sub_clean_df %>% 
  left_join(tc_asb_clusters) %>% 
  pivot_longer(cols = c(-lsoa_code, -traj), names_to = "month", values_to = "ew_crime_counts") %>% 
  mutate(month_fac = as.factor(month),
         month_fac = forcats::fct_relevel(month_fac, "march", "april", "may", "june", "july", "august"))

# Check cluster sizes.
as.data.frame(table(tc_asb_clusters_df$traj)) %>% 
  mutate(traj_count = Freq/6,
         traj_prop  = 100*(traj_count/sum(traj_count)))

# Create new label for each cluster.
tc_asb_clusters_df <- tc_asb_clusters_df %>% 
  mutate(traj_titles = ifelse(test = traj == "A", yes = "N = 24,269 (74%)", no = traj),
         traj_titles = ifelse(test = traj == "B", yes = "N = 7,704 (23%)" , no = traj_titles),
         traj_titles = ifelse(test = traj == "C", yes = "N = 992 (3%)"    , no = traj_titles))

# Check.
table(tc_asb_clusters_df$traj_titles)
class(tc_asb_clusters_df$traj_titles)

# Plot (violin plot).
kmeans_violin_asb_gg <- ggplot(data = tc_asb_clusters_df,
                           mapping = aes(x = month_fac, y = ew_crime_counts,#
                                         fill = traj_titles)) +
  geom_violin(alpha = 0.3, colour = "transparent", adjust = 2) + # non-informative bulges for low counts.
  stat_summary(aes(group = traj_titles), fun = "median", colour = "black", size = 0.5, geom = "line") +
  stat_summary(aes(group = traj_titles), fun = "mean", colour = "black", linetype = "dotted", size = 0.5, geom = "line") +
  facet_wrap(~traj_titles, ncol = 2, scales = "free") +
  scale_x_discrete(labels = c(str_extract(month.name[3:8], "^.{3}"), character(1))) +
  labs(x = NULL, y = "ASB count") +
  theme_bw() +
  theme(legend.position = "none")

# Save.
ggsave(plot = kmeans_violin_asb_gg, filename = "visuals/kmeans_violin_asb_gg.png",
       height = 20, width = 20, unit = "cm", dpi = 200)

# Create proportion contribution to total crime in each month.
tc_asb_clusters_props_df <- tc_asb_clusters_df %>%
  group_by(month_fac) %>% 
  mutate(monthly_counts = sum(ew_crime_counts)) %>% 
  ungroup() %>% 
  mutate(monthly_props = 100*(ew_crime_counts/monthly_counts)) %>% 
  group_by(month_fac, traj_titles) %>% 
  summarise(sum_monthly_props = sum(monthly_props)) %>% 
  ungroup()

# Plot stacks
tc_asb_props_gg <- ggplot(data = tc_asb_clusters_props_df) +
  geom_bar(mapping = aes(x = month_fac, y = sum_monthly_props, group = traj_titles, fill = traj_titles),
            alpha = 0.8, stat = "identity") +
  scale_x_discrete(labels = c(str_extract(month.name[3:8], "^.{3}"), character(1))) +
  labs(x = NULL, y = "% total ASB", fill = NULL) +
  theme_bw() +
  theme(legend.position = "bottom")

# Save.
ggsave(plot = tc_asb_props_gg, filename = "visuals/tc_asb_props_gg.png",
       height = 14, width = 16, unit = "cm", dpi = 200)
