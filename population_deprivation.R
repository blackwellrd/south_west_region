# Population deprivation
# ======================

dir.create(path = '.\\outputs\\03_population_deprivation', showWarnings = FALSE, recursive = TRUE)

# Load the TS011 - Population deprivation data (Employment, education, health and disability, and household overcrowding)
df_ts011_lsoa <- read.csv(unzip('.\\data\\01_demography_and_migration\\census2021-ts011.zip', 'census2021-ts011-lsoa.csv')) %>%
  rename_with(.fn = function(x){c('YEAR','LSOA21NM','LSOA21CD','HOUSEHOLDS', 'HOUSEHOLDS_0', 'HOUSEHOLDS_1', 'HOUSEHOLDS_2', 'HOUSEHOLDS_3', 'HOUSEHOLDS_4')}) %>%
  mutate(
    PCT_HOUSEHOLDS_0 = HOUSEHOLDS_0 / HOUSEHOLDS, 
    PCT_HOUSEHOLDS_4 = HOUSEHOLDS_4 / HOUSEHOLDS, 
    DEPRIVATION_SCORE = HOUSEHOLDS_1 + (HOUSEHOLDS_2 * 2) + (HOUSEHOLDS_3 * 3) + (HOUSEHOLDS_4 * 4),
    AVG_DEPRIVATION_SCORE = DEPRIVATION_SCORE / HOUSEHOLDS
  )

df_ts011_ltla <- read.csv(unzip('.\\data\\01_demography_and_migration\\census2021-ts011.zip', 'census2021-ts011-ltla.csv')) %>%
  rename_with(.fn = function(x){c('YEAR','LAD22NM','LAD22CD','HOUSEHOLDS','HOUSEHOLDS_0','HOUSEHOLDS_1','HOUSEHOLDS_2','HOUSEHOLDS_3','HOUSEHOLDS_4')}) %>%
  inner_join(df_lad22_rgn22_lu %>% select(LAD22CD, RGN22CD, RGN22NM), by = c('LAD22CD' = 'LAD22CD')) %>%
  transmute(YEAR, RGN22CD, RGN22NM, LAD22CD, LAD22NM, HOUSEHOLDS, HOUSEHOLDS_0, HOUSEHOLDS_1, HOUSEHOLDS_2, HOUSEHOLDS_3, HOUSEHOLDS_4)

df_ts011_ltla_summary <- df_ts011_ltla %>% 
  summarise(
    HOUSEHOLDS = sum(HOUSEHOLDS), 
    HOUSEHOLDS_0 = sum(HOUSEHOLDS_0), 
    HOUSEHOLDS_1 = sum(HOUSEHOLDS_1), 
    HOUSEHOLDS_2 = sum(HOUSEHOLDS_2), 
    HOUSEHOLDS_3 = sum(HOUSEHOLDS_3), 
    HOUSEHOLDS_4 = sum(HOUSEHOLDS_4), 
    .groups = 'keep') %>%
  transmute(
    LEVEL = 0, AREA_CODE = 'E92000001', AREA_NAME = 'ENGLAND', 
    HOUSEHOLDS, HOUSEHOLDS_0, HOUSEHOLDS_1, HOUSEHOLDS_2, HOUSEHOLDS_3, HOUSEHOLDS_4
  ) %>%
  bind_rows(
    df_ts011_ltla %>% 
      group_by(RGN22CD, RGN22NM) %>% 
      summarise(
        HOUSEHOLDS = sum(HOUSEHOLDS), 
        HOUSEHOLDS_0 = sum(HOUSEHOLDS_0), 
        HOUSEHOLDS_1 = sum(HOUSEHOLDS_1), 
        HOUSEHOLDS_2 = sum(HOUSEHOLDS_2), 
        HOUSEHOLDS_3 = sum(HOUSEHOLDS_3), 
        HOUSEHOLDS_4 = sum(HOUSEHOLDS_4), 
        .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 1, AREA_CODE = RGN22CD, AREA_NAME = RGN22NM,
                HOUSEHOLDS, HOUSEHOLDS_0, HOUSEHOLDS_1, HOUSEHOLDS_2, HOUSEHOLDS_3, HOUSEHOLDS_4)
  ) %>%
  bind_rows(
    df_ts011_ltla %>% 
      inner_join(df_swahsn_lu, by = c('LAD22CD' = 'LAD22CD')) %>%
      group_by(ICB22CD, ICB22NM) %>% 
      summarise(
        HOUSEHOLDS = sum(HOUSEHOLDS), 
        HOUSEHOLDS_0 = sum(HOUSEHOLDS_0), 
        HOUSEHOLDS_1 = sum(HOUSEHOLDS_1), 
        HOUSEHOLDS_2 = sum(HOUSEHOLDS_2), 
        HOUSEHOLDS_3 = sum(HOUSEHOLDS_3), 
        HOUSEHOLDS_4 = sum(HOUSEHOLDS_4), 
        .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 2, AREA_CODE = ICB22CD, AREA_NAME = ICB22NM,
                HOUSEHOLDS, HOUSEHOLDS_0, HOUSEHOLDS_1, HOUSEHOLDS_2, HOUSEHOLDS_3, HOUSEHOLDS_4)
  ) %>%
  bind_rows(
    df_ts011_ltla %>% 
      semi_join(df_swahsn_lu, by = c('LAD22CD' = 'LAD22CD')) %>%
      group_by(LAD22CD, LAD22NM) %>% 
      summarise(
        HOUSEHOLDS = sum(HOUSEHOLDS), 
        HOUSEHOLDS_0 = sum(HOUSEHOLDS_0), 
        HOUSEHOLDS_1 = sum(HOUSEHOLDS_1), 
        HOUSEHOLDS_2 = sum(HOUSEHOLDS_2), 
        HOUSEHOLDS_3 = sum(HOUSEHOLDS_3), 
        HOUSEHOLDS_4 = sum(HOUSEHOLDS_4), 
        .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 3, AREA_CODE = LAD22CD, AREA_NAME = LAD22NM,
                HOUSEHOLDS, HOUSEHOLDS_0, HOUSEHOLDS_1, HOUSEHOLDS_2, HOUSEHOLDS_3, HOUSEHOLDS_4)
  ) 

df_ts011_ltla_summary <- df_ts011_ltla_summary %>% 
  mutate(
    PCT_HOUSEHOLDS_0 = HOUSEHOLDS_0 / HOUSEHOLDS, 
    PCT_HOUSEHOLDS_4 = HOUSEHOLDS_4 / HOUSEHOLDS, 
    DEPRIVATION_SCORE = HOUSEHOLDS_1 + (HOUSEHOLDS_2 * 2) + (HOUSEHOLDS_3 * 3) + (HOUSEHOLDS_4 * 4),
    AVG_DEPRIVATION_SCORE = DEPRIVATION_SCORE / HOUSEHOLDS
  )

plt_ts011_not_deprived <- ggplot(
  df_ts011_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_HOUSEHOLDS_0)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(title = str_wrap('Proportion of Households in Area that are not in any of the Four Dimensions of Deprivation (Employment, Education, Health and Disability, and Household Overcrowding) for England, Regions, Local Integrated Care Boards (ICB) and Local Authority Districts (LAD)', 80),
       x = 'Area Name', y = '% of Households Not Deprived') %+%
  scale_y_continuous(label = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:3), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a', '3' = '#984ea3'), 
    labels = c('England','Region','Local ICB','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_HOUSEHOLDS_0, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts011_not_deprived
ggsave('.\\outputs\\03_population_deprivation\\plt_ts011_not_deprived.png', plt_ts011_not_deprived, height = 210, width = 297, units = 'mm')

plt_ts011_most_deprived <- ggplot(
  df_ts011_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_HOUSEHOLDS_4)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(title = str_wrap('Proportion of Households in Area that are in all Four Dimensions of Deprivation (Employment, Education, Health and Disability, and Household Overcrowding) for England, Regions, Local Integrated Care Boards (ICB) and Local Authority Districts (LAD)', 80),
       x = 'Area Name', y = '% of Households Most Deprived'
  ) %+%
  scale_y_continuous(label = percent_format(accuracy = 0.1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:3), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a', '3' = '#984ea3'), 
    labels = c('England','Region','Local ICB','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_HOUSEHOLDS_4, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts011_most_deprived
ggsave('.\\outputs\\03_population_deprivation\\plt_ts011_most_deprived.png', plt_ts011_most_deprived, height = 210, width = 297, units = 'mm')

plt_ts011_deprivation_score <- ggplot(
  df_ts011_ltla_summary %>% 
    arrange(LEVEL, desc(AVG_DEPRIVATION_SCORE)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Mean Number of the Four Dimensions Deprivation (Employment, Education, Health and Disability, and Household Overcrowding) for Households for England, Regions, Local Integrated Care Boards (ICB) and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = 'Mean Number of Dimensions per Household'
  ) %+%
  scale_y_continuous(labels = function(x){round(x, 1)}, breaks = seq(0,1,0.2)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:3), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a', '3' = '#984ea3'), 
    labels = c('England','Region','Local ICB','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = AVG_DEPRIVATION_SCORE, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts011_deprivation_score
ggsave('.\\outputs\\03_population_deprivation\\plt_ts011_deprivation_score.png', plt_ts011_deprivation_score, height = 210, width = 297, units = 'mm')
write.csv(df_ts011_ltla_summary, '.\\outputs\\03_population_deprivation\\ts011_summary.csv', row.names = FALSE)

map_ts011_not_deprived <- ggplot(sf_lsoa21 %>% inner_join(df_ts011_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Proportion of Households not in any of the Four Dimensions of Deprivation (Employment, Education, Health and Disability, and Household Overcrowding) for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Households not Deprived',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_HOUSEHOLDS_0),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\03_population_deprivation\\map_ts011_not_deprived_dark.png', map_ts011_not_deprived, height = 297, width = 210, units = 'mm')
map_ts011_not_deprived <- map_ts011_not_deprived %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 0.1))
ggsave('.\\outputs\\03_population_deprivation\\map_ts011_not_deprived_light.png', map_ts011_not_deprived, height = 297, width = 210, units = 'mm')

map_ts011_most_deprived <- ggplot(sf_lsoa21 %>% inner_join(df_ts011_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Proportion of Households in all of the Four Dimensions of Deprivation (Employment, Education, Health and Disability, and Household Overcrowding) for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Households Most Deprived',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_HOUSEHOLDS_4),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 0.1))
ggsave('.\\outputs\\03_population_deprivation\\map_ts011_most_deprived_dark.png', map_ts011_most_deprived, height = 297, width = 210, units = 'mm')
map_ts011_most_deprived <- map_ts011_most_deprived %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 0.1))
ggsave('.\\outputs\\03_population_deprivation\\map_ts011_most_deprived_light.png', map_ts011_most_deprived, height = 297, width = 210, units = 'mm')

map_ts011_deprivation_score <- ggplot(sf_lsoa21 %>% inner_join(df_ts011_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Mean Number of the Four Dimensions Deprivation (Employment, Education, Health and Disability, and Household Overcrowding) for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = 'Mean Number of Dimensions per Household',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = AVG_DEPRIVATION_SCORE),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = function(x){round(x, 2)})
ggsave('.\\outputs\\03_population_deprivation\\map_ts011_deprivation_score_dark.png', map_ts011_deprivation_score, height = 297, width = 210, units = 'mm')
map_ts011_deprivation_score <- map_ts011_deprivation_score %+% scale_fill_viridis_c(direction = -1, labels = function(x){round(x, 2)})
ggsave('.\\outputs\\03_population_deprivation\\map_ts011_deprivation_score_light.png', map_ts011_deprivation_score, height = 297, width = 210, units = 'mm')
