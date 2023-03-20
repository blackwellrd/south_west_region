# Ethnicity
# =========

dir.create(path = '.\\outputs\\04_ethnicity', showWarnings = FALSE, recursive = TRUE)

# Load the TS021 - Ethnic group
df_ts021_lsoa <- read.csv(unzip('.\\data\\02_ethnicity_identity_language_and_religion\\census2021-ts021.zip', 'census2021-ts021-lsoa.csv')) %>%
  rename_with(.fn = function(x){c('YEAR','LSOA21NM','LSOA21CD',
                                  'POPN', 
                                  'ASIAN','ASIAN_BANGLADESHI','ASIAN_CHINESE','ASIAN_INDIAN','ASIAN_PAKISTANI','ASIAN_OTHER', 
                                  'BLACK','BLACK_AFRICAN','BLACK_CARIBBEAN','BLACK_OTHER',
                                  'MIXED','MIXED_WHITE_ASIAN','MIXED_WHITE_BLACK_AFRICAN','MIXED_WHITE_BLACK_CARIBBEAN','MIXED_OTHER',
                                  'WHITE','WHITE_BRITISH','WHITE_IRISH','WHITE_GYPSY','WHITE_ROMA','WHITE_OTHER',
                                  'OTHER','OTHER_ARAB','OTHER_OTHER')}) %>%
  mutate(PCT_WHITE = WHITE / POPN, PCT_ASIAN = ASIAN / POPN, PCT_BLACK = BLACK / POPN, PCT_MIXED = MIXED / POPN, PCT_OTHER = OTHER / POPN)

df_ts021_ltla <- read.csv(unzip('.\\data\\02_ethnicity_identity_language_and_religion\\census2021-ts021.zip', 'census2021-ts021-ltla.csv')) %>%
  rename_with(.fn = function(x){c('YEAR','LAD22NM','LAD22CD',
                                  'POPN', 
                                  'ASIAN','ASIAN_BANGLADESHI','ASIAN_CHINESE','ASIAN_INDIAN','ASIAN_PAKISTANI','ASIAN_OTHER', 
                                  'BLACK','BLACK_AFRICAN','BLACK_CARIBBEAN','BLACK_OTHER',
                                  'MIXED','MIXED_WHITE_ASIAN','MIXED_WHITE_BLACK_AFRICAN','MIXED_WHITE_BLACK_CARIBBEAN','MIXED_OTHER',
                                  'WHITE','WHITE_BRITISH','WHITE_IRISH','WHITE_GYPSY','WHITE_ROMA','WHITE_OTHER',
                                  'OTHER','OTHER_ARAB','OTHER_OTHER')}) %>%
  inner_join(df_lad22_rgn22_lu %>% select(LAD22CD, RGN22CD, RGN22NM), by = c('LAD22CD' = 'LAD22CD')) %>%
  transmute(YEAR, 
            RGN22CD, RGN22NM, 
            LAD22CD, LAD22NM, 
            POPN, 
            ASIAN, ASIAN_BANGLADESHI, ASIAN_CHINESE, ASIAN_INDIAN, ASIAN_PAKISTANI, ASIAN_OTHER, 
            BLACK, BLACK_AFRICAN, BLACK_CARIBBEAN, BLACK_OTHER,
            MIXED, MIXED_WHITE_ASIAN, MIXED_WHITE_BLACK_AFRICAN, MIXED_WHITE_BLACK_CARIBBEAN, MIXED_OTHER,
            WHITE, WHITE_BRITISH, WHITE_IRISH, WHITE_GYPSY, WHITE_ROMA, WHITE_OTHER,
            OTHER, OTHER_ARAB, OTHER_OTHER)

df_ts021_ltla_summary <- df_ts021_ltla %>% 
  summarise(POPN = sum(POPN), 
            WHITE = sum(WHITE), ASIAN = sum(ASIAN), BLACK = sum(BLACK), MIXED = sum(MIXED), OTHER = sum(OTHER),
            .groups = 'keep') %>%
  transmute(LEVEL = 0, AREA_CODE = 'E92000001', AREA_NAME = 'ENGLAND', POPN, WHITE, ASIAN, BLACK, MIXED, OTHER) %>%
  bind_rows(
    df_ts021_ltla %>% 
      group_by(RGN22CD, RGN22NM) %>% 
      summarise(POPN = sum(POPN), 
                WHITE = sum(WHITE), ASIAN = sum(ASIAN), BLACK = sum(BLACK), MIXED = sum(MIXED), OTHER = sum(OTHER),
                .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 1, AREA_CODE = RGN22CD, AREA_NAME = RGN22NM, POPN, WHITE, ASIAN, BLACK, MIXED, OTHER)
  ) %>%
  bind_rows(
    df_ts021_ltla %>% 
      inner_join(df_swahsn_lu, by = c('LAD22CD' = 'LAD22CD')) %>%
      group_by(ICB22CD, ICB22NM) %>% 
      summarise(POPN = sum(POPN), 
                WHITE = sum(WHITE), ASIAN = sum(ASIAN), BLACK = sum(BLACK), MIXED = sum(MIXED), OTHER = sum(OTHER),
                .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 2, AREA_CODE = ICB22CD, AREA_NAME = ICB22NM, POPN, WHITE, ASIAN, BLACK, MIXED, OTHER)
  ) %>%
  bind_rows(
    df_ts021_ltla %>% 
      semi_join(df_swahsn_lu, by = c('LAD22CD' = 'LAD22CD')) %>%
      group_by(LAD22CD, LAD22NM) %>% 
      summarise(POPN = sum(POPN), 
                WHITE = sum(WHITE), ASIAN = sum(ASIAN), BLACK = sum(BLACK), MIXED = sum(MIXED), OTHER = sum(OTHER),
                .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 3, AREA_CODE = LAD22CD, AREA_NAME = LAD22NM, POPN, WHITE, ASIAN, BLACK, MIXED, OTHER)
  ) %>% 
  mutate(PCT_WHITE = WHITE/POPN, PCT_ASIAN = ASIAN/POPN, PCT_BLACK = BLACK/POPN, PCT_MIXED = MIXED/POPN, PCT_OTHER = OTHER/POPN)

plt_ts021_ethnicity_white <- ggplot(
  df_ts021_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_WHITE)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Population of White Ethnic Group for England, Regions, Local Integrated Care Boards (ICB) and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,1,0.2)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:3), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a', '3' = '#984ea3'), 
    labels = c('England','Region','Local ICB','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_WHITE, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts021_ethnicity_white
ggsave('.\\outputs\\04_ethnicity\\plt_ts021_ethnicity_white.png', plt_ts021_ethnicity_white, height = 210, width = 297, units = 'mm')

plt_ts021_ethnicity_asian <- ggplot(
  df_ts021_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_ASIAN)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Population of Asian Ethnic Group for England, Regions, Local Integrated Care Boards (ICB) and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.2,0.05)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:3), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a', '3' = '#984ea3'), 
    labels = c('England','Region','Local ICB','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_ASIAN, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts021_ethnicity_asian
ggsave('.\\outputs\\04_ethnicity\\plt_ts021_ethnicity_asian.png', plt_ts021_ethnicity_asian, height = 210, width = 297, units = 'mm')

plt_ts021_ethnicity_black <- ggplot(
  df_ts021_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_BLACK)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Population of Black Ethnic Group for England, Regions, Local Integrated Care Boards (ICB) and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.2,0.05)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:3), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a', '3' = '#984ea3'), 
    labels = c('England','Region','Local ICB','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_BLACK, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts021_ethnicity_black
ggsave('.\\outputs\\04_ethnicity\\plt_ts021_ethnicity_black.png', plt_ts021_ethnicity_asian, height = 210, width = 297, units = 'mm')

plt_ts021_ethnicity_mixed <- ggplot(
  df_ts021_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_MIXED)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Population of Mixed Ethnic Group for England, Regions, Local Integrated Care Boards (ICB) and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.05,0.01)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:3), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a', '3' = '#984ea3'), 
    labels = c('England','Region','Local ICB','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_MIXED, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts021_ethnicity_mixed
ggsave('.\\outputs\\04_ethnicity\\plt_ts021_ethnicity_mixed.png', plt_ts021_ethnicity_mixed, height = 210, width = 297, units = 'mm')

plt_ts021_ethnicity_other <- ggplot(
  df_ts021_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_OTHER)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Population of Other Ethnic Groups for England, Regions, Local Integrated Care Boards (ICB) and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.07,0.01)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:3), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a', '3' = '#984ea3'), 
    labels = c('England','Region','Local ICB','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_OTHER, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts021_ethnicity_other
ggsave('.\\outputs\\04_ethnicity\\plt_ts021_ethnicity_other.png', plt_ts021_ethnicity_other, height = 210, width = 297, units = 'mm')
write.csv(df_ts021_ltla_summary, '.\\outputs\\04_ethnicity\\ts021_summary.csv', row.names = FALSE)

map_ts021_white <- ggplot(sf_lsoa21 %>% inner_join(df_ts021_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Population of White Ethnic Group for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Population',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_WHITE),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\04_ethnicity\\map_ts021_white_dark.png', map_ts021_white, height = 297, width = 210, units = 'mm')
map_ts021_white <- map_ts021_white %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\04_ethnicity\\map_ts021_white_light.png', map_ts021_white, height = 297, width = 210, units = 'mm')

map_ts021_asian <- ggplot(sf_lsoa21 %>% inner_join(df_ts021_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Population of Asian Ethnic Group for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Population',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_ASIAN),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\04_ethnicity\\map_ts021_asian_dark.png', map_ts021_asian, height = 297, width = 210, units = 'mm')
map_ts021_asian <- map_ts021_asian %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\04_ethnicity\\map_ts021_asian_light.png', map_ts021_asian, height = 297, width = 210, units = 'mm')

map_ts021_black <- ggplot(sf_lsoa21 %>% inner_join(df_ts021_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Population of Black Ethnic Group for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Population',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_BLACK),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\04_ethnicity\\map_ts021_black_dark.png', map_ts021_black, height = 297, width = 210, units = 'mm')
map_ts021_black <- map_ts021_black %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\04_ethnicity\\map_ts021_black_light.png', map_ts021_black, height = 297, width = 210, units = 'mm')

map_ts021_mixed <- ggplot(sf_lsoa21 %>% inner_join(df_ts021_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Population of Mixed Ethnic Group for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Population',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_MIXED),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\04_ethnicity\\map_ts021_mixed_dark.png', map_ts021_mixed, height = 297, width = 210, units = 'mm')
map_ts021_mixed <- map_ts021_mixed %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\04_ethnicity\\map_ts021_mixed_light.png', map_ts021_mixed, height = 297, width = 210, units = 'mm')

map_ts021_other <- ggplot(sf_lsoa21 %>% inner_join(df_ts021_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Population of Other Ethnic Groups for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Population',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_OTHER),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\04_ethnicity\\map_ts021_other_dark.png', map_ts021_other, height = 297, width = 210, units = 'mm')
map_ts021_other <- map_ts021_other %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\04_ethnicity\\map_ts021_other_light.png', map_ts021_other, height = 297, width = 210, units = 'mm')
