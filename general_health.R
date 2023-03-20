# General Health
# ==============

dir.create(path = '.\\outputs\\05_general_health', showWarnings = FALSE, recursive = TRUE)

# Load the TS037 - General Health
df_ts037_lsoa <- read.csv(unzip('.\\data\\03_health\\census2021-ts037.zip', 'census2021-ts037-lsoa.csv')) %>%
  rename_with(.fn = function(x){c('YEAR','LSOA21NM','LSOA21CD',
                                  'POPN', 
                                  'VERY_GOOD_HEALTH', 'GOOD_HEALTH', 'FAIR_HEALTH', 'BAD_HEALTH', 'VERY_BAD_HEALTH')}) %>%
  mutate(PCT_VERY_GOOD_HEALTH = VERY_GOOD_HEALTH / POPN, PCT_GOOD_HEALTH = GOOD_HEALTH / POPN, 
         PCT_FAIR_HEALTH = FAIR_HEALTH / POPN, PCT_BAD_HEALTH = BAD_HEALTH / POPN, PCT_VERY_BAD_HEALTH = VERY_BAD_HEALTH / POPN,
         PCT_VERY_GOOD_AND_GOOD_HEALTH = (VERY_GOOD_HEALTH + GOOD_HEALTH) / POPN,
         PCT_VERY_BAD_AND_BAD_HEALTH = (VERY_BAD_HEALTH + BAD_HEALTH) / POPN)

df_ts037_ltla <- read.csv(unzip('.\\data\\03_health\\census2021-ts037.zip', 'census2021-ts037-ltla.csv')) %>%
  rename_with(.fn = function(x){c('YEAR','LAD22NM','LAD22CD',
                                  'POPN', 
                                  'VERY_GOOD_HEALTH', 'GOOD_HEALTH', 'FAIR_HEALTH', 'BAD_HEALTH', 'VERY_BAD_HEALTH')}) %>%
  inner_join(df_lad22_rgn22_lu %>% select(LAD22CD, RGN22CD, RGN22NM), by = c('LAD22CD' = 'LAD22CD')) %>%
  transmute(YEAR, 
            RGN22CD, RGN22NM, 
            LAD22CD, LAD22NM, 
            POPN, 
            VERY_GOOD_HEALTH, GOOD_HEALTH, 
            FAIR_HEALTH,
            BAD_HEALTH, VERY_BAD_HEALTH)

df_ts037_ltla_summary <- df_ts037_ltla %>% 
  summarise(POPN = sum(POPN), 
            VERY_GOOD_HEALTH = sum(VERY_GOOD_HEALTH), GOOD_HEALTH = sum(GOOD_HEALTH), 
            FAIR_HEALTH = sum(FAIR_HEALTH), 
            BAD_HEALTH = sum(BAD_HEALTH), VERY_BAD_HEALTH = sum(VERY_BAD_HEALTH),
            .groups = 'keep') %>%
  transmute(LEVEL = 0, AREA_CODE = 'E92000001', AREA_NAME = 'ENGLAND', 
            POPN, 
            VERY_GOOD_HEALTH, GOOD_HEALTH, 
            FAIR_HEALTH,
            BAD_HEALTH, VERY_BAD_HEALTH) %>%
  bind_rows(
    df_ts037_ltla %>% 
      group_by(RGN22CD, RGN22NM) %>% 
      summarise(POPN = sum(POPN), 
                VERY_GOOD_HEALTH = sum(VERY_GOOD_HEALTH), GOOD_HEALTH = sum(GOOD_HEALTH), 
                FAIR_HEALTH = sum(FAIR_HEALTH), 
                BAD_HEALTH = sum(BAD_HEALTH), VERY_BAD_HEALTH = sum(VERY_BAD_HEALTH),
                .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 1, AREA_CODE = RGN22CD, AREA_NAME = RGN22NM, 
                POPN, 
                VERY_GOOD_HEALTH, GOOD_HEALTH, 
                FAIR_HEALTH,
                BAD_HEALTH, VERY_BAD_HEALTH)
  ) %>%
  bind_rows(
    df_ts037_ltla %>% 
      inner_join(df_swahsn_lu, by = c('LAD22CD' = 'LAD22CD')) %>%
      group_by(ICB22CD, ICB22NM) %>% 
      summarise(POPN = sum(POPN), 
                VERY_GOOD_HEALTH = sum(VERY_GOOD_HEALTH), GOOD_HEALTH = sum(GOOD_HEALTH), 
                FAIR_HEALTH = sum(FAIR_HEALTH), 
                BAD_HEALTH = sum(BAD_HEALTH), VERY_BAD_HEALTH = sum(VERY_BAD_HEALTH),
                .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 2, AREA_CODE = ICB22CD, AREA_NAME = ICB22NM, 
                POPN, 
                VERY_GOOD_HEALTH, GOOD_HEALTH, 
                FAIR_HEALTH,
                BAD_HEALTH, VERY_BAD_HEALTH)
  ) %>%
  bind_rows(
    df_ts037_ltla %>% 
      semi_join(df_swahsn_lu, by = c('LAD22CD' = 'LAD22CD')) %>%
      group_by(LAD22CD, LAD22NM) %>% 
      summarise(POPN = sum(POPN), 
                VERY_GOOD_HEALTH = sum(VERY_GOOD_HEALTH), GOOD_HEALTH = sum(GOOD_HEALTH), 
                FAIR_HEALTH = sum(FAIR_HEALTH), 
                BAD_HEALTH = sum(BAD_HEALTH), VERY_BAD_HEALTH = sum(VERY_BAD_HEALTH),
                .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 3, AREA_CODE = LAD22CD, AREA_NAME = LAD22NM, 
                POPN, 
                VERY_GOOD_HEALTH, GOOD_HEALTH, 
                FAIR_HEALTH,
                BAD_HEALTH, VERY_BAD_HEALTH)
  ) %>% 
  mutate(PCT_VERY_GOOD_HEALTH = VERY_GOOD_HEALTH / POPN, PCT_GOOD_HEALTH = GOOD_HEALTH / POPN, 
         PCT_FAIR_HEALTH = FAIR_HEALTH / POPN, PCT_BAD_HEALTH = BAD_HEALTH / POPN, PCT_VERY_BAD_HEALTH = VERY_BAD_HEALTH / POPN,
         PCT_VERY_GOOD_AND_GOOD_HEALTH = (VERY_GOOD_HEALTH + GOOD_HEALTH) / POPN,
         PCT_VERY_BAD_AND_BAD_HEALTH = (VERY_BAD_HEALTH + BAD_HEALTH) / POPN)

plt_ts037_very_good_health <- ggplot(
  df_ts037_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_VERY_GOOD_HEALTH)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Population in Very Good Health for England, Regions, Local Integrated Care Boards (ICB) and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.5,0.1)) %+%
  #  scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:3), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a', '3' = '#984ea3'), 
    labels = c('England','Region','Local ICB','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_VERY_GOOD_HEALTH, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts037_very_good_health
ggsave('.\\outputs\\05_general_health\\plt_ts037_very_good_health.png', plt_ts037_very_good_health, height = 210, width = 297, units = 'mm')

plt_ts037_good_health <- ggplot(
  df_ts037_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_GOOD_HEALTH)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Population in Good Health for England, Regions, Local Integrated Care Boards (ICB) and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Population'
  ) %+%
  #  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.5,0.1)) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:3), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a', '3' = '#984ea3'), 
    labels = c('England','Region','Local ICB','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_GOOD_HEALTH, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts037_good_health
ggsave('.\\outputs\\05_general_health\\plt_ts037_good_health.png', plt_ts037_good_health, height = 210, width = 297, units = 'mm')

plt_ts037_fair_health <- ggplot(
  df_ts037_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_FAIR_HEALTH)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Population in Fair Health for England, Regions, Local Integrated Care Boards (ICB) and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Population'
  ) %+%
  #  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.5,0.1)) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:3), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a', '3' = '#984ea3'), 
    labels = c('England','Region','Local ICB','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_FAIR_HEALTH, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts037_fair_health
ggsave('.\\outputs\\05_general_health\\plt_ts037_fair_health.png', plt_ts037_fair_health, height = 210, width = 297, units = 'mm')

plt_ts037_bad_health <- ggplot(
  df_ts037_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_BAD_HEALTH)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Population in Bad Health for England, Regions, Local Integrated Care Boards (ICB) and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.05,0.01)) %+%
  #scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:3), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a', '3' = '#984ea3'), 
    labels = c('England','Region','Local ICB','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_BAD_HEALTH, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts037_bad_health
ggsave('.\\outputs\\05_general_health\\plt_ts037_bad_health.png', plt_ts037_bad_health, height = 210, width = 297, units = 'mm')

plt_ts037_very_bad_health <- ggplot(
  df_ts037_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_VERY_BAD_HEALTH)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Population in Very Bad Health for England, Regions, Local Integrated Care Boards (ICB) and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 0.1), breaks = seq(0,0.02,0.005)) %+%
  #scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:3), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a', '3' = '#984ea3'), 
    labels = c('England','Region','Local ICB','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_VERY_BAD_HEALTH, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts037_very_bad_health
ggsave('.\\outputs\\05_general_health\\plt_ts037_very_bad_health.png', plt_ts037_very_bad_health, height = 210, width = 297, units = 'mm')

plt_ts037_very_good_and_good_health <- ggplot(
  df_ts037_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_VERY_GOOD_AND_GOOD_HEALTH)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Population in Very Good or Good Health for England, Regions, Local Integrated Care Boards (ICB) and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.8,0.2)) %+%
  #scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:3), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a', '3' = '#984ea3'), 
    labels = c('England','Region','Local ICB','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_VERY_GOOD_AND_GOOD_HEALTH, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts037_very_good_and_good_health
ggsave('.\\outputs\\05_general_health\\plt_ts037_very_good_and_good_health.png', plt_ts037_very_good_and_good_health, height = 210, width = 297, units = 'mm')

plt_ts037_very_bad_and_bad_health <- ggplot(
  df_ts037_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_VERY_BAD_AND_BAD_HEALTH)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Population in Very Bad or Bad Health for England, Regions, Local Integrated Care Boards (ICB) and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.07,0.01)) %+%
  #scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:3), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a', '3' = '#984ea3'), 
    labels = c('England','Region','Local ICB','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_VERY_BAD_AND_BAD_HEALTH, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts037_very_bad_and_bad_health
ggsave('.\\outputs\\05_general_health\\plt_ts037_very_bad_and_bad_health.png', plt_ts037_very_bad_and_bad_health, height = 210, width = 297, units = 'mm')

write.csv(df_ts037_ltla_summary, '.\\outputs\\05_general_health\\ts037_summary.csv', row.names = FALSE)

map_ts037_very_good_and_good_health <- ggplot(sf_lsoa21 %>% inner_join(df_ts037_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Population in Very Good or Good Health for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Population',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_VERY_GOOD_AND_GOOD_HEALTH),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\05_general_health\\map_ts037_very_good_and_good_health_dark.png', map_ts037_very_good_and_good_health, height = 297, width = 210, units = 'mm')
map_ts037_very_good_and_good_health <- map_ts037_very_good_and_good_health %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\05_general_health\\map_ts037_very_good_and_good_health_light.png', map_ts037_very_good_and_good_health, height = 297, width = 210, units = 'mm')

map_ts037_very_bad_and_bad_health <- ggplot(sf_lsoa21 %>% inner_join(df_ts037_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Population in Very Bad or Bad Health for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Population',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_VERY_BAD_AND_BAD_HEALTH),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\05_general_health\\map_ts037_very_bad_and_bad_health_dark.png', map_ts037_very_bad_and_bad_health, height = 297, width = 210, units = 'mm')
map_ts037_very_bad_and_bad_health <- map_ts037_very_bad_and_bad_health %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\05_general_health\\map_ts037_very_bad_and_bad_health_light.png', map_ts037_very_bad_and_bad_health, height = 297, width = 210, units = 'mm')
