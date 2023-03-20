# Disability
# ==========

dir.create(path = '.\\outputs\\06_disability', showWarnings = FALSE, recursive = TRUE)

# Load the TS038 - Disability
df_ts038_lsoa <- read.csv(unzip('.\\data\\03_health\\census2021-ts038.zip', 'census2021-ts038-lsoa.csv')) %>%
  rename_with(.fn = function(x){c('YEAR','LSOA21NM','LSOA21CD',
                                  'POPN', 
                                  'DISABLED', 'DISABLED_LIMITED_A_LOT', 'DISABLED_LIMITED_A_LITTLE', 'NOT_DISABLED', 'NOT_DISABLED_WITH_LTC', 'NOT_DISABLED_NO_LTC')}) %>%
  mutate(PCT_DISABLED = DISABLED / POPN,PCT_DISABLED_LIMITED_A_LOT = DISABLED_LIMITED_A_LOT / POPN, PCT_DISABLED_LIMITED_A_LITTLE = DISABLED_LIMITED_A_LITTLE / POPN, 
         PCT_NOT_DISABLED = NOT_DISABLED / POPN, PCT_NOT_DISABLED_WITH_LTC = NOT_DISABLED_WITH_LTC / POPN, PCT_NOT_DISABLED_NO_LTC = NOT_DISABLED_NO_LTC / POPN)


df_ts038_ltla <- read.csv(unzip('.\\data\\03_health\\census2021-ts038.zip', 'census2021-ts038-ltla.csv')) %>%
  rename_with(.fn = function(x){c('YEAR','LAD22NM','LAD22CD',
                                  'POPN', 
                                  'DISABLED', 'DISABLED_LIMITED_A_LOT', 'DISABLED_LIMITED_A_LITTLE', 'NOT_DISABLED', 'NOT_DISABLED_WITH_LTC', 'NOT_DISABLED_NO_LTC')}) %>%
  inner_join(df_lad22_rgn22_lu %>% select(LAD22CD, RGN22CD, RGN22NM), by = c('LAD22CD' = 'LAD22CD')) %>%
  transmute(YEAR, 
            RGN22CD, RGN22NM, 
            LAD22CD, LAD22NM, 
            POPN, 
            DISABLED, DISABLED_LIMITED_A_LOT, DISABLED_LIMITED_A_LITTLE, 
            NOT_DISABLED, NOT_DISABLED_WITH_LTC, NOT_DISABLED_NO_LTC)

df_ts038_ltla_summary <- df_ts038_ltla %>% 
  summarise(
    POPN = sum(POPN), 
    DISABLED = sum(DISABLED), DISABLED_LIMITED_A_LOT = sum(DISABLED_LIMITED_A_LOT), DISABLED_LIMITED_A_LITTLE = sum(DISABLED_LIMITED_A_LITTLE), 
    NOT_DISABLED = sum(NOT_DISABLED), NOT_DISABLED_WITH_LTC = sum(NOT_DISABLED_WITH_LTC), NOT_DISABLED_NO_LTC = sum(NOT_DISABLED_NO_LTC),
    .groups = 'keep') %>%
  transmute(LEVEL = 0, AREA_CODE = 'E92000001', AREA_NAME = 'ENGLAND', 
            POPN, 
            DISABLED, DISABLED_LIMITED_A_LOT, DISABLED_LIMITED_A_LITTLE, 
            NOT_DISABLED, NOT_DISABLED_WITH_LTC, NOT_DISABLED_NO_LTC) %>%
  bind_rows(
    df_ts038_ltla %>% 
      group_by(RGN22CD, RGN22NM) %>% 
      summarise(
        POPN = sum(POPN), 
        DISABLED = sum(DISABLED), DISABLED_LIMITED_A_LOT = sum(DISABLED_LIMITED_A_LOT), DISABLED_LIMITED_A_LITTLE = sum(DISABLED_LIMITED_A_LITTLE), 
        NOT_DISABLED = sum(NOT_DISABLED), NOT_DISABLED_WITH_LTC = sum(NOT_DISABLED_WITH_LTC), NOT_DISABLED_NO_LTC = sum(NOT_DISABLED_NO_LTC),
        .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 1, AREA_CODE = RGN22CD, AREA_NAME = RGN22NM, 
                POPN, 
                DISABLED, DISABLED_LIMITED_A_LOT, DISABLED_LIMITED_A_LITTLE, 
                NOT_DISABLED, NOT_DISABLED_WITH_LTC, NOT_DISABLED_NO_LTC)
    ) %>%
  bind_rows(
    df_ts038_ltla %>% 
      group_by(LAD22CD, LAD22NM) %>% 
      summarise(
        POPN = sum(POPN), 
        DISABLED = sum(DISABLED), DISABLED_LIMITED_A_LOT = sum(DISABLED_LIMITED_A_LOT), DISABLED_LIMITED_A_LITTLE = sum(DISABLED_LIMITED_A_LITTLE), 
        NOT_DISABLED = sum(NOT_DISABLED), NOT_DISABLED_WITH_LTC = sum(NOT_DISABLED_WITH_LTC), NOT_DISABLED_NO_LTC = sum(NOT_DISABLED_NO_LTC),
        .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 2, AREA_CODE = LAD22CD, AREA_NAME = LAD22NM, 
                POPN, 
                DISABLED, DISABLED_LIMITED_A_LOT, DISABLED_LIMITED_A_LITTLE, 
                NOT_DISABLED, NOT_DISABLED_WITH_LTC, NOT_DISABLED_NO_LTC)
  ) %>% 
  mutate(PCT_DISABLED = DISABLED / POPN,PCT_DISABLED_LIMITED_A_LOT = DISABLED_LIMITED_A_LOT / POPN, PCT_DISABLED_LIMITED_A_LITTLE = DISABLED_LIMITED_A_LITTLE / POPN, 
         PCT_NOT_DISABLED = NOT_DISABLED / POPN, PCT_NOT_DISABLED_WITH_LTC = NOT_DISABLED_WITH_LTC / POPN, PCT_NOT_DISABLED_NO_LTC = NOT_DISABLED_NO_LTC / POPN) %>%
  group_by(LEVEL) %>%
  mutate(
    RANK_DISABLED = rank(desc(PCT_DISABLED), ties.method = 'first'),
    RANK_DISABLED = sprintf('(%d of %d)', RANK_DISABLED, n()),
    RANK_DISABLED_LIMITED_A_LOT = rank(desc(PCT_DISABLED_LIMITED_A_LOT), ties.method = 'first'),
    RANK_DISABLED_LIMITED_A_LOT = sprintf('(%d of %d)', RANK_DISABLED_LIMITED_A_LOT, n()),
    RANK_DISABLED_LIMITED_A_LITTLE = rank(desc(PCT_DISABLED_LIMITED_A_LITTLE), ties.method = 'first'),
    RANK_DISABLED_LIMITED_A_LITTLE = sprintf('(%d of %d)', RANK_DISABLED_LIMITED_A_LITTLE, n()),
    RANK_NOT_DISABLED = rank(desc(PCT_NOT_DISABLED), ties.method = 'first'),
    RANK_NOT_DISABLED = sprintf('(%d of %d)', RANK_NOT_DISABLED, n()),
    RANK_NOT_DISABLED_WITH_LTC = rank(desc(PCT_NOT_DISABLED_WITH_LTC), ties.method = 'first'),
    RANK_NOT_DISABLED_WITH_LTC = sprintf('(%d of %d)', RANK_NOT_DISABLED_WITH_LTC, n()),
    RANK_NOT_DISABLED_NO_LTC = rank(desc(PCT_NOT_DISABLED_NO_LTC), ties.method = 'first'),
    RANK_NOT_DISABLED_NO_LTC = sprintf('(%d of %d)', RANK_NOT_DISABLED_NO_LTC, n())
  ) %>%
  ungroup() %>%
  filter(LEVEL < 2 | AREA_CODE %in% df_swahsn_lu$LAD22CD)

plt_ts038_disabled <- ggplot(
  df_ts038_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_DISABLED)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Population Disabled under the Equality Act for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.3,0.05)) %+%
  #  scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_DISABLED, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts038_disabled
ggsave('.\\outputs\\06_disability\\plt_ts038_disabled.png', plt_ts038_disabled, height = 210, width = 297, units = 'mm')

plt_ts038_disabled_limited_a_lot <- ggplot(
  df_ts038_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_DISABLED_LIMITED_A_LOT)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Population Disabled under the Equality Act whose Day-to-day Activities are Limited a lot for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.1,0.02)) %+%
  #  scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_DISABLED_LIMITED_A_LOT, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts038_disabled_limited_a_lot
ggsave('.\\outputs\\06_disability\\plt_ts038_disabled_limited_a_lot.png', plt_ts038_disabled_limited_a_lot, height = 210, width = 297, units = 'mm')

plt_ts038_disabled_limited_a_little <- ggplot(
  df_ts038_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_DISABLED_LIMITED_A_LITTLE)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Population Disabled under the Equality Act whose Day-to-day Activities are Limited a little for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.12,0.02)) %+%
  #  scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_DISABLED_LIMITED_A_LITTLE, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts038_disabled_limited_a_little
ggsave('.\\outputs\\06_disability\\plt_ts038_disabled_limited_a_little.png', plt_ts038_disabled_limited_a_little, height = 210, width = 297, units = 'mm')

plt_ts038_not_disabled <- ggplot(
  df_ts038_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_NOT_DISABLED)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Population Not Disabled under the Equality Act for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.8,0.2)) %+%
  #  scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_NOT_DISABLED, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts038_not_disabled
ggsave('.\\outputs\\06_disability\\plt_ts038_not_disabled.png', plt_ts038_not_disabled, height = 210, width = 297, units = 'mm')

plt_ts038_not_disabled_with_ltc <- ggplot(
  df_ts038_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_NOT_DISABLED_WITH_LTC)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Population Not Disabled under the Equality Act but with Long Term Physical or Mental Health Condition for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.1,0.02)) %+%
  #  scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_NOT_DISABLED_WITH_LTC, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts038_not_disabled_with_ltc
ggsave('.\\outputs\\06_disability\\plt_ts038_not_disabled_with_ltc.png', plt_ts038_not_disabled_with_ltc, height = 210, width = 297, units = 'mm')

plt_ts038_not_disabled_no_ltc <- ggplot(
  df_ts038_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_NOT_DISABLED_NO_LTC)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
  ) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Population Not Disabled under the Equality Act without Any Long Term Physical or Mental Health Condition for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.8,0.2)) %+%
  #  scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_NOT_DISABLED_NO_LTC, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts038_not_disabled_no_ltc
ggsave('.\\outputs\\06_disability\\plt_ts038_not_disabled_no_ltc.png', plt_ts038_not_disabled_no_ltc, height = 210, width = 297, units = 'mm')

write.csv(df_ts038_ltla_summary, '.\\outputs\\06_disability\\ts038_summary.csv', row.names = FALSE)

map_ts038_disabled <- ggplot(sf_lsoa21 %>% inner_join(df_ts038_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Population Disabled under the Equality Act for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Population',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_DISABLED),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\06_disability\\map_ts038_disabled_dark.png', map_ts038_disabled, height = 297, width = 210, units = 'mm')
map_ts038_disabled <- map_ts038_disabled %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\06_disability\\map_ts038_disabled_light.png', map_ts038_disabled, height = 297, width = 210, units = 'mm')

map_ts038_disabled_limited_a_lot <- ggplot(sf_lsoa21 %>% inner_join(df_ts038_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Population Disabled under the Equality Act whose Day-to-day Activities are Limited a lot for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Population',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_DISABLED_LIMITED_A_LOT),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\06_disability\\map_ts038_disabled_limited_a_lot_dark.png', map_ts038_disabled_limited_a_lot, height = 297, width = 210, units = 'mm')
map_ts038_disabled_limited_a_lot <- map_ts038_disabled_limited_a_lot %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\06_disability\\map_ts038_disabled_limited_a_lot_light.png', map_ts038_disabled_limited_a_lot, height = 297, width = 210, units = 'mm')

map_ts038_not_disabled_with_ltc <- ggplot(sf_lsoa21 %>% inner_join(df_ts038_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Population Not Disabled under the Equality Act but with Long Term Physical or Mental Health Condition for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Population',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_NOT_DISABLED_WITH_LTC),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\06_disability\\map_ts038_not_disabled_with_ltc_dark.png', map_ts038_not_disabled_with_ltc, height = 297, width = 210, units = 'mm')
map_ts038_not_disabled_with_ltc <- map_ts038_not_disabled_with_ltc %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\06_disability\\map_ts038_not_disabled_with_ltc_light.png', map_ts038_not_disabled_with_ltc, height = 297, width = 210, units = 'mm')
