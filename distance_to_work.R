# Distance to work
# ================

dir.create(path = '.\\outputs\\12_distance_to_work', showWarnings = FALSE, recursive = TRUE)

# Load the TS058 - Distance to work data
df_ts058_lsoa <- read.csv(unzip('.\\data\\06_work_and_travel\\census2021-ts058.zip', 'census2021-ts058-lsoa.csv')) %>%
  rename_with(.fn = function(x){c('YEAR','LSOA21NM','LSOA21CD',
                                  'EMPLOYED_POPN', 
                                  'DIST_LT_2KM', 'DIST_BETWEEN_2KM_AND_5KM',
                                  'DIST_BETWEEN_5KM_AND_10KM', 'DIST_BETWEEN_10KM_AND_20KM',
                                  'DIST_BETWEEN_20KM_AND_30KM', 'DIST_BETWEEN_30KM_AND_40KM',
                                  'DIST_BETWEEN_40KM_AND_60KM', 'DIST_GE_60KM',
                                  'HOME_WORKER', 'OFFSHORE_WORKER')}) %>%
  mutate(
    PCT_DIST_LT_2KM = DIST_LT_2KM / EMPLOYED_POPN,
    PCT_DIST_BETWEEN_2KM_AND_5KM = DIST_BETWEEN_2KM_AND_5KM / EMPLOYED_POPN,
    PCT_DIST_BETWEEN_5KM_AND_10KM = DIST_BETWEEN_5KM_AND_10KM / EMPLOYED_POPN,
    PCT_DIST_BETWEEN_10KM_AND_20KM = DIST_BETWEEN_10KM_AND_20KM / EMPLOYED_POPN,
    PCT_DIST_BETWEEN_20KM_AND_30KM = DIST_BETWEEN_20KM_AND_30KM / EMPLOYED_POPN,
    PCT_DIST_BETWEEN_30KM_AND_40KM = DIST_BETWEEN_30KM_AND_40KM / EMPLOYED_POPN,
    PCT_DIST_BETWEEN_40KM_AND_60KM = DIST_BETWEEN_40KM_AND_60KM / EMPLOYED_POPN,
    PCT_DIST_GE_60KM = DIST_GE_60KM / EMPLOYED_POPN,
    PCT_HOME_WORKER = HOME_WORKER / EMPLOYED_POPN,
    PCT_OFFSHORE_WORKER = OFFSHORE_WORKER / EMPLOYED_POPN
    )

df_ts058_ltla <- read.csv(unzip('.\\data\\06_work_and_travel\\census2021-ts058.zip', 'census2021-ts058-ltla.csv')) %>%
  rename_with(.fn = function(x){c('YEAR','LAD22NM','LAD22CD',
                                  'EMPLOYED_POPN', 
                                  'DIST_LT_2KM', 'DIST_BETWEEN_2KM_AND_5KM',
                                  'DIST_BETWEEN_5KM_AND_10KM', 'DIST_BETWEEN_10KM_AND_20KM',
                                  'DIST_BETWEEN_20KM_AND_30KM', 'DIST_BETWEEN_30KM_AND_40KM',
                                  'DIST_BETWEEN_40KM_AND_60KM', 'DIST_GE_60KM',
                                  'HOME_WORKER', 'OFFSHORE_WORKER')}) %>%
  inner_join(df_lad22_rgn22_lu %>% select(LAD22CD, RGN22CD, RGN22NM), by = c('LAD22CD' = 'LAD22CD')) %>%
  transmute(YEAR, 
            RGN22CD, RGN22NM, 
            LAD22CD, LAD22NM, 
            EMPLOYED_POPN, 
            DIST_LT_2KM, DIST_BETWEEN_2KM_AND_5KM,
            DIST_BETWEEN_5KM_AND_10KM, DIST_BETWEEN_10KM_AND_20KM,
            DIST_BETWEEN_20KM_AND_30KM, DIST_BETWEEN_30KM_AND_40KM,
            DIST_BETWEEN_40KM_AND_60KM, DIST_GE_60KM,
            HOME_WORKER, OFFSHORE_WORKER)
  
df_ts058_ltla_summary <- df_ts058_ltla %>% 
  summarise(
    EMPLOYED_POPN = sum(EMPLOYED_POPN), 
    DIST_LT_2KM = sum(DIST_LT_2KM), DIST_BETWEEN_2KM_AND_5KM = sum(DIST_BETWEEN_2KM_AND_5KM),
    DIST_BETWEEN_5KM_AND_10KM = sum(DIST_BETWEEN_5KM_AND_10KM), DIST_BETWEEN_10KM_AND_20KM = sum(DIST_BETWEEN_10KM_AND_20KM),
    DIST_BETWEEN_20KM_AND_30KM = sum(DIST_BETWEEN_20KM_AND_30KM), DIST_BETWEEN_30KM_AND_40KM = sum(DIST_BETWEEN_30KM_AND_40KM),
    DIST_BETWEEN_40KM_AND_60KM = sum(DIST_BETWEEN_40KM_AND_60KM), DIST_GE_60KM = sum(DIST_GE_60KM),
    HOME_WORKER = sum(HOME_WORKER), OFFSHORE_WORKER = sum(OFFSHORE_WORKER),
    .groups = 'keep') %>%
  transmute(LEVEL = 0, AREA_CODE = 'E92000001', AREA_NAME = 'ENGLAND', 
            EMPLOYED_POPN, 
            DIST_LT_2KM, DIST_BETWEEN_2KM_AND_5KM,
            DIST_BETWEEN_5KM_AND_10KM, DIST_BETWEEN_10KM_AND_20KM,
            DIST_BETWEEN_20KM_AND_30KM, DIST_BETWEEN_30KM_AND_40KM,
            DIST_BETWEEN_40KM_AND_60KM, DIST_GE_60KM,
            HOME_WORKER, OFFSHORE_WORKER
  ) %>% 
  bind_rows(
    df_ts058_ltla %>% 
      group_by(RGN22CD, RGN22NM) %>% 
      summarise(
        EMPLOYED_POPN = sum(EMPLOYED_POPN), 
        DIST_LT_2KM = sum(DIST_LT_2KM), DIST_BETWEEN_2KM_AND_5KM = sum(DIST_BETWEEN_2KM_AND_5KM),
        DIST_BETWEEN_5KM_AND_10KM = sum(DIST_BETWEEN_5KM_AND_10KM), DIST_BETWEEN_10KM_AND_20KM = sum(DIST_BETWEEN_10KM_AND_20KM),
        DIST_BETWEEN_20KM_AND_30KM = sum(DIST_BETWEEN_20KM_AND_30KM), DIST_BETWEEN_30KM_AND_40KM = sum(DIST_BETWEEN_30KM_AND_40KM),
        DIST_BETWEEN_40KM_AND_60KM = sum(DIST_BETWEEN_40KM_AND_60KM), DIST_GE_60KM = sum(DIST_GE_60KM),
        HOME_WORKER = sum(HOME_WORKER), OFFSHORE_WORKER = sum(OFFSHORE_WORKER),
        .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 1, AREA_CODE = RGN22CD, AREA_NAME = RGN22NM, 
                EMPLOYED_POPN, 
                DIST_LT_2KM, DIST_BETWEEN_2KM_AND_5KM,
                DIST_BETWEEN_5KM_AND_10KM, DIST_BETWEEN_10KM_AND_20KM,
                DIST_BETWEEN_20KM_AND_30KM, DIST_BETWEEN_30KM_AND_40KM,
                DIST_BETWEEN_40KM_AND_60KM, DIST_GE_60KM,
                HOME_WORKER, OFFSHORE_WORKER)
  ) %>% 
  bind_rows(
    df_ts058_ltla %>% 
      group_by(LAD22CD, LAD22NM) %>% 
      summarise(
        EMPLOYED_POPN = sum(EMPLOYED_POPN), 
        DIST_LT_2KM = sum(DIST_LT_2KM), DIST_BETWEEN_2KM_AND_5KM = sum(DIST_BETWEEN_2KM_AND_5KM),
        DIST_BETWEEN_5KM_AND_10KM = sum(DIST_BETWEEN_5KM_AND_10KM), DIST_BETWEEN_10KM_AND_20KM = sum(DIST_BETWEEN_10KM_AND_20KM),
        DIST_BETWEEN_20KM_AND_30KM = sum(DIST_BETWEEN_20KM_AND_30KM), DIST_BETWEEN_30KM_AND_40KM = sum(DIST_BETWEEN_30KM_AND_40KM),
        DIST_BETWEEN_40KM_AND_60KM = sum(DIST_BETWEEN_40KM_AND_60KM), DIST_GE_60KM = sum(DIST_GE_60KM),
        HOME_WORKER = sum(HOME_WORKER), OFFSHORE_WORKER = sum(OFFSHORE_WORKER),
        .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 2, AREA_CODE = LAD22CD, AREA_NAME = LAD22NM, 
                EMPLOYED_POPN, 
                DIST_LT_2KM, DIST_BETWEEN_2KM_AND_5KM,
                DIST_BETWEEN_5KM_AND_10KM, DIST_BETWEEN_10KM_AND_20KM,
                DIST_BETWEEN_20KM_AND_30KM, DIST_BETWEEN_30KM_AND_40KM,
                DIST_BETWEEN_40KM_AND_60KM, DIST_GE_60KM,
                HOME_WORKER, OFFSHORE_WORKER)
  ) %>% 
  mutate(
    PCT_DIST_LT_2KM = DIST_LT_2KM / EMPLOYED_POPN,
    PCT_DIST_BETWEEN_2KM_AND_5KM = DIST_BETWEEN_2KM_AND_5KM / EMPLOYED_POPN,
    PCT_DIST_BETWEEN_5KM_AND_10KM = DIST_BETWEEN_5KM_AND_10KM / EMPLOYED_POPN,
    PCT_DIST_BETWEEN_10KM_AND_20KM = DIST_BETWEEN_10KM_AND_20KM / EMPLOYED_POPN,
    PCT_DIST_BETWEEN_20KM_AND_30KM = DIST_BETWEEN_20KM_AND_30KM / EMPLOYED_POPN,
    PCT_DIST_BETWEEN_30KM_AND_40KM = DIST_BETWEEN_30KM_AND_40KM / EMPLOYED_POPN,
    PCT_DIST_BETWEEN_40KM_AND_60KM = DIST_BETWEEN_40KM_AND_60KM / EMPLOYED_POPN,
    PCT_DIST_GE_60KM = DIST_GE_60KM / EMPLOYED_POPN,
    PCT_HOME_WORKER = HOME_WORKER / EMPLOYED_POPN,
    PCT_OFFSHORE_WORKER = OFFSHORE_WORKER / EMPLOYED_POPN
  ) %>% 
  group_by(LEVEL) %>%
  mutate(
    RANK_DIST_LT_2KM = rank(desc(PCT_DIST_LT_2KM ), ties.method = 'first'),
    RANK_DIST_LT_2KM = sprintf('(%d of %d)', RANK_DIST_LT_2KM , n()),
    RANK_DIST_BETWEEN_2KM_AND_5KM = rank(desc(PCT_DIST_BETWEEN_2KM_AND_5KM ), ties.method = 'first'),
    RANK_DIST_BETWEEN_2KM_AND_5KM = sprintf('(%d of %d)', RANK_DIST_BETWEEN_2KM_AND_5KM , n()),
    RANK_DIST_BETWEEN_5KM_AND_10KM = rank(desc(PCT_DIST_BETWEEN_5KM_AND_10KM ), ties.method = 'first'),
    RANK_DIST_BETWEEN_5KM_AND_10KM = sprintf('(%d of %d)', RANK_DIST_BETWEEN_5KM_AND_10KM , n()),
    RANK_DIST_BETWEEN_10KM_AND_20KM = rank(desc(PCT_DIST_BETWEEN_10KM_AND_20KM ), ties.method = 'first'),
    RANK_DIST_BETWEEN_10KM_AND_20KM = sprintf('(%d of %d)', RANK_DIST_BETWEEN_10KM_AND_20KM , n()),
    RANK_DIST_BETWEEN_20KM_AND_30KM = rank(desc(PCT_DIST_BETWEEN_20KM_AND_30KM ), ties.method = 'first'),
    RANK_DIST_BETWEEN_20KM_AND_30KM = sprintf('(%d of %d)', RANK_DIST_BETWEEN_20KM_AND_30KM , n()),
    RANK_DIST_BETWEEN_30KM_AND_40KM = rank(desc(PCT_DIST_BETWEEN_30KM_AND_40KM ), ties.method = 'first'),
    RANK_DIST_BETWEEN_30KM_AND_40KM = sprintf('(%d of %d)', RANK_DIST_BETWEEN_30KM_AND_40KM , n()),
    RANK_DIST_BETWEEN_40KM_AND_60KM = rank(desc(PCT_DIST_BETWEEN_40KM_AND_60KM ), ties.method = 'first'),
    RANK_DIST_BETWEEN_40KM_AND_60KM = sprintf('(%d of %d)', RANK_DIST_BETWEEN_40KM_AND_60KM , n()),
    RANK_DIST_GE_60KM = rank(desc(PCT_DIST_GE_60KM ), ties.method = 'first'),
    RANK_DIST_GE_60KM = sprintf('(%d of %d)', RANK_DIST_GE_60KM , n()),
    RANK_HOME_WORKER = rank(desc(PCT_HOME_WORKER ), ties.method = 'first'),
    RANK_HOME_WORKER = sprintf('(%d of %d)', RANK_HOME_WORKER , n()),
    RANK_OFFSHORE_WORKER = rank(desc(PCT_OFFSHORE_WORKER ), ties.method = 'first'),
    RANK_OFFSHORE_WORKER = sprintf('(%d of %d)', RANK_OFFSHORE_WORKER , n())
  ) %>%
  ungroup() %>%
  filter(LEVEL < 2 | AREA_CODE %in% df_swahsn_lu$LAD22CD)

plt_ts058_home_worker <- ggplot(
  df_ts058_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_HOME_WORKER)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
  ) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Employed Population Working from Home for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Employed Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.4,0.1)) %+%
  #scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_HOME_WORKER, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts058_home_worker
ggsave('.\\outputs\\12_distance_to_work\\plt_ts058_home_worker.png', plt_ts058_home_worker, height = 210, width = 297, units = 'mm')

plt_ts058_lt_5km <- ggplot(
  df_ts058_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_DIST_LT_2KM + PCT_DIST_BETWEEN_2KM_AND_5KM)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
  ) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Employed Population Working Within 5km of Home for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Employed Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.4,0.1)) %+%
  #scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_DIST_LT_2KM + PCT_DIST_BETWEEN_2KM_AND_5KM, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts058_lt_5km
ggsave('.\\outputs\\12_distance_to_work\\plt_ts058_lt_5km.png', plt_ts058_lt_5km, height = 210, width = 297, units = 'mm')

plt_ts058_between_5km_and_20km <- ggplot(
  df_ts058_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_DIST_BETWEEN_5KM_AND_10KM + PCT_DIST_BETWEEN_10KM_AND_20KM)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Employed Population Working Between 5km and 20km of Home for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Employed Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.3,0.05)) %+%
  #scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_DIST_BETWEEN_5KM_AND_10KM + PCT_DIST_BETWEEN_10KM_AND_20KM, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts058_between_5km_and_20km
ggsave('.\\outputs\\12_distance_to_work\\plt_ts058_between_5km_and_20km.png', plt_ts058_between_5km_and_20km, height = 210, width = 297, units = 'mm')

plt_ts058_gt_20km <- ggplot(
  df_ts058_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_DIST_BETWEEN_20KM_AND_30KM + PCT_DIST_BETWEEN_30KM_AND_40KM + PCT_DIST_BETWEEN_40KM_AND_60KM + PCT_DIST_GE_60KM)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
  ) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Employed Population Working more than 20km from Home for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Employed Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.25,0.05)) %+%
  #scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_DIST_BETWEEN_20KM_AND_30KM + PCT_DIST_BETWEEN_30KM_AND_40KM + PCT_DIST_BETWEEN_40KM_AND_60KM + PCT_DIST_GE_60KM, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts058_gt_20km
ggsave('.\\outputs\\12_distance_to_work\\plt_ts058_gt_20km.png', plt_ts058_gt_20km, height = 210, width = 297, units = 'mm')

write.csv(df_ts058_ltla_summary, '.\\outputs\\12_distance_to_work\\ts058_summary.csv', row.names = FALSE)

map_ts058_home_worker <- ggplot(sf_lsoa21 %>% inner_join(df_ts058_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Employed Population Working from Home for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Employed Population',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_HOME_WORKER),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\12_distance_to_work\\map_ts058_home_worker_dark.png', map_ts058_home_worker, height = 297, width = 210, units = 'mm')
map_ts058_home_worker <- map_ts058_home_worker %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\12_distance_to_work\\map_ts058_home_worker_light.png', map_ts058_home_worker, height = 297, width = 210, units = 'mm')

map_ts058_lt_5km <- ggplot(sf_lsoa21 %>% inner_join(df_ts058_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Employed Population Working Within 5km of Home for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Employed Population',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_DIST_LT_2KM + PCT_DIST_BETWEEN_2KM_AND_5KM),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\12_distance_to_work\\map_ts058_lt_5km_dark.png', map_ts058_lt_5km, height = 297, width = 210, units = 'mm')
map_ts058_lt_5km <- map_ts058_lt_5km %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\12_distance_to_work\\map_ts058_lt_5km_light.png', map_ts058_lt_5km, height = 297, width = 210, units = 'mm')

map_ts058_between_5km_and_20km <- ggplot(sf_lsoa21 %>% inner_join(df_ts058_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Employed Population Working Between 5km and 20km of Home for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Employed Population',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_DIST_BETWEEN_5KM_AND_10KM + PCT_DIST_BETWEEN_10KM_AND_20KM),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\12_distance_to_work\\map_ts058_between_5km_and_20km_dark.png', map_ts058_between_5km_and_20km, height = 297, width = 210, units = 'mm')
map_ts058_between_5km_and_20km <- map_ts058_between_5km_and_20km %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\12_distance_to_work\\map_ts058_between_5km_and_20km_light.png', map_ts058_between_5km_and_20km, height = 297, width = 210, units = 'mm')

map_ts058_gt_20km <- ggplot(sf_lsoa21 %>% inner_join(df_ts058_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Employed Population Working Between 5km and 20km of Home for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Employed Population',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_DIST_BETWEEN_20KM_AND_30KM + PCT_DIST_BETWEEN_30KM_AND_40KM + PCT_DIST_BETWEEN_40KM_AND_60KM + PCT_DIST_GE_60KM),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\12_distance_to_work\\map_ts058_gt_20km_dark.png', map_ts058_gt_20km, height = 297, width = 210, units = 'mm')
map_ts058_gt_20km <- map_ts058_gt_20km %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\12_distance_to_work\\map_ts058_gt_20km_light.png', map_ts058_gt_20km, height = 297, width = 210, units = 'mm')