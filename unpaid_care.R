# Unpaid Care
# ===========

dir.create(path = '.\\outputs\\07_unpaid_care', showWarnings = FALSE, recursive = TRUE)

# Load the TS039 - Unpaid Care data
df_ts039_lsoa <- read.csv(unzip('.\\data\\03_health\\census2021-ts039.zip', 'census2021-ts039-lsoa.csv')) %>%
  rename_with(.fn = function(x){c('YEAR','LSOA21NM','LSOA21CD',
                                  'POPN', 
                                  'NO_UNPAID_CARE', 'LE_19H_UNPAID_CARE', 'LE_09H_UNPAID_CARE', 'BETWEEN_10H_19H_UNPAID_CARE',
                                  'BETWEEN_20H_49H_UNPAID_CARE', 'BETWEEN_20H_34H_UNPAID_CARE', 'BETWEEN_35H_49H_UNPAID_CARE',
                                  'GE_50H_UNPAID_CARE')}) %>%
  mutate(
    PCT_NO_UNPAID_CARE = NO_UNPAID_CARE / POPN,
    PCT_LE_19H_UNPAID_CARE = LE_19H_UNPAID_CARE / POPN,
    PCT_LE_09H_UNPAID_CARE = LE_09H_UNPAID_CARE / POPN,
    PCT_BETWEEN_10H_19H_UNPAID_CARE = BETWEEN_10H_19H_UNPAID_CARE / POPN,
    PCT_BETWEEN_20H_49H_UNPAID_CARE = BETWEEN_20H_49H_UNPAID_CARE / POPN,
    PCT_BETWEEN_20H_34H_UNPAID_CARE = BETWEEN_20H_34H_UNPAID_CARE / POPN,
    PCT_BETWEEN_35H_49H_UNPAID_CARE = BETWEEN_35H_49H_UNPAID_CARE / POPN,
    PCT_GE_50H_UNPAID_CARE = GE_50H_UNPAID_CARE / POPN)

df_ts039_ltla <- read.csv(unzip('.\\data\\03_health\\census2021-ts039.zip', 'census2021-ts039-ltla.csv')) %>%
  rename_with(.fn = function(x){c('YEAR','LAD22NM','LAD22CD',
                                  'POPN', 
                                  'NO_UNPAID_CARE', 'LE_19H_UNPAID_CARE', 'LE_09H_UNPAID_CARE', 'BETWEEN_10H_19H_UNPAID_CARE',
                                  'BETWEEN_20H_49H_UNPAID_CARE', 'BETWEEN_20H_34H_UNPAID_CARE', 'BETWEEN_35H_49H_UNPAID_CARE',
                                  'GE_50H_UNPAID_CARE')}) %>%
  inner_join(df_lad22_rgn22_lu %>% select(LAD22CD, RGN22CD, RGN22NM), by = c('LAD22CD' = 'LAD22CD')) %>%
  transmute(YEAR, 
            RGN22CD, RGN22NM, 
            LAD22CD, LAD22NM, 
            POPN, 
            NO_UNPAID_CARE, LE_19H_UNPAID_CARE, LE_09H_UNPAID_CARE, BETWEEN_10H_19H_UNPAID_CARE,
            BETWEEN_20H_49H_UNPAID_CARE, BETWEEN_20H_34H_UNPAID_CARE, BETWEEN_35H_49H_UNPAID_CARE,
            GE_50H_UNPAID_CARE)

df_ts039_ltla_summary <- df_ts039_ltla %>% 
  summarise(
    POPN = sum(POPN), 
    NO_UNPAID_CARE = sum(NO_UNPAID_CARE), 
    LE_19H_UNPAID_CARE = sum(LE_19H_UNPAID_CARE), LE_09H_UNPAID_CARE = sum(LE_09H_UNPAID_CARE), 
    BETWEEN_10H_19H_UNPAID_CARE = sum(BETWEEN_10H_19H_UNPAID_CARE), BETWEEN_20H_49H_UNPAID_CARE = sum(BETWEEN_20H_49H_UNPAID_CARE), 
    BETWEEN_20H_34H_UNPAID_CARE = sum(BETWEEN_20H_34H_UNPAID_CARE), BETWEEN_35H_49H_UNPAID_CARE = sum(BETWEEN_35H_49H_UNPAID_CARE),
    GE_50H_UNPAID_CARE = sum(GE_50H_UNPAID_CARE),
    .groups = 'keep') %>%
  transmute(LEVEL = 0, AREA_CODE = 'E92000001', AREA_NAME = 'ENGLAND', 
            POPN, 
            NO_UNPAID_CARE, LE_19H_UNPAID_CARE, LE_09H_UNPAID_CARE, BETWEEN_10H_19H_UNPAID_CARE,
            BETWEEN_20H_49H_UNPAID_CARE, BETWEEN_20H_34H_UNPAID_CARE, BETWEEN_35H_49H_UNPAID_CARE,
            GE_50H_UNPAID_CARE) %>% 
  bind_rows(
    df_ts039_ltla %>% 
      group_by(RGN22CD, RGN22NM) %>% 
      summarise(
        POPN = sum(POPN), 
        NO_UNPAID_CARE = sum(NO_UNPAID_CARE), 
        LE_19H_UNPAID_CARE = sum(LE_19H_UNPAID_CARE), LE_09H_UNPAID_CARE = sum(LE_09H_UNPAID_CARE), 
        BETWEEN_10H_19H_UNPAID_CARE = sum(BETWEEN_10H_19H_UNPAID_CARE), BETWEEN_20H_49H_UNPAID_CARE = sum(BETWEEN_20H_49H_UNPAID_CARE), 
        BETWEEN_20H_34H_UNPAID_CARE = sum(BETWEEN_20H_34H_UNPAID_CARE), BETWEEN_35H_49H_UNPAID_CARE = sum(BETWEEN_35H_49H_UNPAID_CARE),
        GE_50H_UNPAID_CARE = sum(GE_50H_UNPAID_CARE),
        .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 1, AREA_CODE = RGN22CD, AREA_NAME = RGN22NM, 
                POPN, 
                NO_UNPAID_CARE, LE_19H_UNPAID_CARE, LE_09H_UNPAID_CARE, BETWEEN_10H_19H_UNPAID_CARE,
                BETWEEN_20H_49H_UNPAID_CARE, BETWEEN_20H_34H_UNPAID_CARE, BETWEEN_35H_49H_UNPAID_CARE,
                GE_50H_UNPAID_CARE)
    ) %>%
  bind_rows(
    df_ts039_ltla %>% 
      group_by(LAD22CD, LAD22NM) %>% 
      summarise(
        POPN = sum(POPN), 
        NO_UNPAID_CARE = sum(NO_UNPAID_CARE), 
        LE_19H_UNPAID_CARE = sum(LE_19H_UNPAID_CARE), LE_09H_UNPAID_CARE = sum(LE_09H_UNPAID_CARE), 
        BETWEEN_10H_19H_UNPAID_CARE = sum(BETWEEN_10H_19H_UNPAID_CARE), BETWEEN_20H_49H_UNPAID_CARE = sum(BETWEEN_20H_49H_UNPAID_CARE), 
        BETWEEN_20H_34H_UNPAID_CARE = sum(BETWEEN_20H_34H_UNPAID_CARE), BETWEEN_35H_49H_UNPAID_CARE = sum(BETWEEN_35H_49H_UNPAID_CARE),
        GE_50H_UNPAID_CARE = sum(GE_50H_UNPAID_CARE),
        .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 2, AREA_CODE = LAD22CD, AREA_NAME = LAD22NM, 
                POPN, 
                NO_UNPAID_CARE, LE_19H_UNPAID_CARE, LE_09H_UNPAID_CARE, BETWEEN_10H_19H_UNPAID_CARE,
                BETWEEN_20H_49H_UNPAID_CARE, BETWEEN_20H_34H_UNPAID_CARE, BETWEEN_35H_49H_UNPAID_CARE,
                GE_50H_UNPAID_CARE)
    ) %>% 
  mutate(
    PCT_NO_UNPAID_CARE = NO_UNPAID_CARE / POPN,
    PCT_LE_19H_UNPAID_CARE = LE_19H_UNPAID_CARE / POPN,
    PCT_LE_09H_UNPAID_CARE = LE_09H_UNPAID_CARE / POPN,
    PCT_BETWEEN_10H_19H_UNPAID_CARE = BETWEEN_10H_19H_UNPAID_CARE / POPN,
    PCT_BETWEEN_20H_49H_UNPAID_CARE = BETWEEN_20H_49H_UNPAID_CARE / POPN,
    PCT_BETWEEN_20H_34H_UNPAID_CARE = BETWEEN_20H_34H_UNPAID_CARE / POPN,
    PCT_BETWEEN_35H_49H_UNPAID_CARE = BETWEEN_35H_49H_UNPAID_CARE / POPN,
    PCT_GE_50H_UNPAID_CARE = GE_50H_UNPAID_CARE / POPN) %>% 
  group_by(LEVEL) %>%
  mutate(
    RANK_NO_UNPAID_CARE = rank(desc(PCT_NO_UNPAID_CARE), ties.method = 'first'),
    RANK_NO_UNPAID_CARE = sprintf('(%d of %d)', RANK_NO_UNPAID_CARE, n()),
    RANK_LE_19H_UNPAID_CARE = rank(desc(PCT_LE_19H_UNPAID_CARE), ties.method = 'first'),
    RANK_LE_19H_UNPAID_CARE = sprintf('(%d of %d)', RANK_LE_19H_UNPAID_CARE, n()),
    RANK_BETWEEN_10H_19H_UNPAID_CARE = rank(desc(PCT_BETWEEN_10H_19H_UNPAID_CARE), ties.method = 'first'),
    RANK_BETWEEN_10H_19H_UNPAID_CARE = sprintf('(%d of %d)', RANK_BETWEEN_10H_19H_UNPAID_CARE, n()),
    RANK_BETWEEN_20H_49H_UNPAID_CARE = rank(desc(PCT_BETWEEN_20H_49H_UNPAID_CARE), ties.method = 'first'),
    RANK_BETWEEN_20H_49H_UNPAID_CARE = sprintf('(%d of %d)', RANK_BETWEEN_20H_49H_UNPAID_CARE, n()),
    RANK_BETWEEN_20H_34H_UNPAID_CARE = rank(desc(PCT_BETWEEN_20H_34H_UNPAID_CARE), ties.method = 'first'),
    RANK_BETWEEN_20H_34H_UNPAID_CARE = sprintf('(%d of %d)', RANK_BETWEEN_20H_34H_UNPAID_CARE, n()),
    RANK_BETWEEN_35H_49H_UNPAID_CARE = rank(desc(PCT_BETWEEN_35H_49H_UNPAID_CARE), ties.method = 'first'),
    RANK_BETWEEN_35H_49H_UNPAID_CARE = sprintf('(%d of %d)', RANK_BETWEEN_35H_49H_UNPAID_CARE, n()),
    RANK_GE_50H_UNPAID_CARE = rank(desc(PCT_GE_50H_UNPAID_CARE), ties.method = 'first'),
    RANK_GE_50H_UNPAID_CARE = sprintf('(%d of %d)', RANK_GE_50H_UNPAID_CARE, n())
  ) %>%
  ungroup() %>%
  filter(LEVEL < 2 | AREA_CODE %in% df_swahsn_lu$LAD22CD)

plt_ts039_no_unpaid_care <- ggplot(
  df_ts039_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_NO_UNPAID_CARE)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
  ) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Population Not Delivering Any Unpaid Care for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.8,0.2)) %+%
  #scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_NO_UNPAID_CARE, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts039_no_unpaid_care
ggsave('.\\outputs\\07_unpaid_care\\plt_ts039_no_unpaid_care.png', plt_ts039_no_unpaid_care, height = 210, width = 297, units = 'mm')

plt_ts039_any_unpaid_care <- ggplot(
  df_ts039_ltla_summary %>% 
    arrange(LEVEL, desc(1 - PCT_NO_UNPAID_CARE)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Population Delivering Any Unpaid Care for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.1,0.02)) %+%
  #scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = 1 - PCT_NO_UNPAID_CARE, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts039_any_unpaid_care
ggsave('.\\outputs\\07_unpaid_care\\plt_ts039_any_unpaid_care.png', plt_ts039_any_unpaid_care, height = 210, width = 297, units = 'mm')

plt_ts039_le_19h_unpaid_care <- ggplot(
  df_ts039_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_LE_19H_UNPAID_CARE)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
  ) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Population Delivering Up to 19 hours of Unpaid Care a Week for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.06,0.01)) %+%
  #scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_LE_19H_UNPAID_CARE, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts039_le_19h_unpaid_care
ggsave('.\\outputs\\07_unpaid_care\\plt_ts039_le_19h_unpaid_care.png', plt_ts039_le_19h_unpaid_care, height = 210, width = 297, units = 'mm')

plt_ts039_between_20h_and_49h_unpaid_care <- ggplot(
  df_ts039_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_BETWEEN_20H_49H_UNPAID_CARE)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
  ) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Population Delivering Between 20 hours and 49 hours of Unpaid Care a Week for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = .1), breaks = seq(0,0.025,0.005)) %+%
  #scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_BETWEEN_20H_49H_UNPAID_CARE, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts039_between_20h_and_49h_unpaid_care
ggsave('.\\outputs\\07_unpaid_care\\plt_ts039_between_20h_and_49h_unpaid_care.png', plt_ts039_between_20h_and_49h_unpaid_care, height = 210, width = 297, units = 'mm')

plt_ts039_ge_50h_unpaid_care <- ggplot(
  df_ts039_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_GE_50H_UNPAID_CARE)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
  ) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Population Delivering Over 50 hours of Unpaid Care a Week for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = .1), breaks = seq(0,0.04,0.01)) %+%
  #scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_GE_50H_UNPAID_CARE, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts039_ge_50h_unpaid_care
ggsave('.\\outputs\\07_unpaid_care\\plt_ts039_ge_50h_unpaid_care.png', plt_ts039_ge_50h_unpaid_care, height = 210, width = 297, units = 'mm')

write.csv(df_ts039_ltla_summary, '.\\outputs\\07_unpaid_care\\ts039_summary.csv', row.names = FALSE)

map_ts039_no_unpaid_care <- ggplot(sf_lsoa21 %>% inner_join(df_ts039_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Population Not Delivering Any Unpaid Care for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Population',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_NO_UNPAID_CARE),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\07_unpaid_care\\map_ts039_no_unpaid_care_dark.png', map_ts039_no_unpaid_care, height = 297, width = 210, units = 'mm')
map_ts039_no_unpaid_care <- map_ts039_no_unpaid_care %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\07_unpaid_care\\map_ts039_no_unpaid_care_light.png', map_ts039_no_unpaid_care, height = 297, width = 210, units = 'mm')

map_ts039_any_unpaid_care <- ggplot(sf_lsoa21 %>% inner_join(df_ts039_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Population Delivering Any Level of Unpaid Care for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Population',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = 1 - PCT_NO_UNPAID_CARE),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\07_unpaid_care\\map_ts039_any_unpaid_care_dark.png', map_ts039_any_unpaid_care, height = 297, width = 210, units = 'mm')
map_ts039_any_unpaid_care <- map_ts039_any_unpaid_care %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\07_unpaid_care\\map_ts039_any_unpaid_care_light.png', map_ts039_any_unpaid_care, height = 297, width = 210, units = 'mm')

map_ts039_le_19h_unpaid_care <- ggplot(sf_lsoa21 %>% inner_join(df_ts039_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Population Delivering Up to 19 hours of Unpaid Care for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Population',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_LE_19H_UNPAID_CARE),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\07_unpaid_care\\map_ts039_le_19h_unpaid_care_dark.png', map_ts039_le_19h_unpaid_care, height = 297, width = 210, units = 'mm')
map_ts039_le_19h_unpaid_care <- map_ts039_le_19h_unpaid_care %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\07_unpaid_care\\map_ts039_le_19h_unpaid_care_light.png', map_ts039_le_19h_unpaid_care, height = 297, width = 210, units = 'mm')

map_ts039_between_20h_and_49h_unpaid_care <- ggplot(sf_lsoa21 %>% inner_join(df_ts039_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Population Delivering Between 20 hours and 49 hours of Unpaid Care for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Population',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_BETWEEN_20H_49H_UNPAID_CARE),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\07_unpaid_care\\map_ts039_between_20h_and_49h_unpaid_care_dark.png', map_ts039_between_20h_and_49h_unpaid_care, height = 297, width = 210, units = 'mm')
map_ts039_between_20h_and_49h_unpaid_care <- map_ts039_between_20h_and_49h_unpaid_care %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\07_unpaid_care\\map_ts039_between_20h_and_49h_unpaid_care_light.png', map_ts039_between_20h_and_49h_unpaid_care, height = 297, width = 210, units = 'mm')

map_ts039_ge_50h_unpaid_care <- ggplot(sf_lsoa21 %>% inner_join(df_ts039_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Population Delivering Over 50 hours of Unpaid Care for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Population',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_GE_50H_UNPAID_CARE),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\07_unpaid_care\\map_ts039_ge_50h_unpaid_care_dark.png', map_ts039_ge_50h_unpaid_care, height = 297, width = 210, units = 'mm')
map_ts039_ge_50h_unpaid_care <- map_ts039_ge_50h_unpaid_care %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\07_unpaid_care\\map_ts039_ge_50h_unpaid_care_light.png', map_ts039_ge_50h_unpaid_care, height = 297, width = 210, units = 'mm')
