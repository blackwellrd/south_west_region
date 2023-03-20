# Education
# =========

dir.create(path = '.\\outputs\\08_education', showWarnings = FALSE, recursive = TRUE)

# Load the TS067 - Education level data
df_ts067_lsoa <- read.csv(unzip('.\\data\\04_education\\census2021-ts067.zip', 'census2021-ts067-lsoa.csv')) %>%
  rename_with(.fn = function(x){c('YEAR','LSOA21NM','LSOA21CD',
                                  'POPN', 
                                  'NO_QUALS', 'LEVEL_1', 'LEVEL_2', 'APPRENTICESHIP',
                                  'LEVEL_3', 'LEVEL_4_PLUS', 'OTHER_QUALS')}) %>%
  mutate(
    PCT_NO_QUALS = NO_QUALS / POPN,
    PCT_LEVEL_1 = LEVEL_1 / POPN,
    PCT_LEVEL_2 = LEVEL_2 / POPN,
    PCT_APPRENTICESHIP = APPRENTICESHIP / POPN,
    PCT_LEVEL_3 = LEVEL_3 / POPN,
    PCT_LEVEL_4_PLUS = LEVEL_4_PLUS / POPN,
    PCT_OTHER_QUALS = OTHER_QUALS / POPN)

df_ts067_ltla <- read.csv(unzip('.\\data\\04_education\\census2021-ts067.zip', 'census2021-ts067-ltla.csv')) %>%
  rename_with(.fn = function(x){c('YEAR','LAD22NM','LAD22CD',
                                  'POPN', 
                                  'NO_QUALS', 'LEVEL_1', 'LEVEL_2', 'APPRENTICESHIP',
                                  'LEVEL_3', 'LEVEL_4_PLUS', 'OTHER_QUALS')}) %>%
  inner_join(df_lad22_rgn22_lu %>% select(LAD22CD, RGN22CD, RGN22NM), by = c('LAD22CD' = 'LAD22CD')) %>%
  transmute(YEAR, 
            RGN22CD, RGN22NM, 
            LAD22CD, LAD22NM, 
            POPN, 
            NO_QUALS, LEVEL_1, LEVEL_2, APPRENTICESHIP,
            LEVEL_3, LEVEL_4_PLUS, OTHER_QUALS)
  
df_ts067_ltla_summary <- df_ts067_ltla %>% 
  summarise(
    POPN = sum(POPN), 
    NO_QUALS = sum(NO_QUALS), LEVEL_1 = sum(LEVEL_1), LEVEL_2 = sum(LEVEL_2), 
    APPRENTICESHIP = sum(APPRENTICESHIP), LEVEL_3 = sum(LEVEL_3), LEVEL_4_PLUS = sum(LEVEL_4_PLUS), 
    OTHER_QUALS = sum(OTHER_QUALS),
    .groups = 'keep') %>%
  transmute(LEVEL = 0, AREA_CODE = 'E92000001', AREA_NAME = 'ENGLAND', 
            POPN, 
            NO_QUALS, LEVEL_1, LEVEL_2, APPRENTICESHIP,
            LEVEL_3, LEVEL_4_PLUS, OTHER_QUALS
  ) %>% 
  bind_rows(
    df_ts067_ltla %>% 
      group_by(RGN22CD, RGN22NM) %>% 
      summarise(
        POPN = sum(POPN), 
        NO_QUALS = sum(NO_QUALS), LEVEL_1 = sum(LEVEL_1), LEVEL_2 = sum(LEVEL_2), 
        APPRENTICESHIP = sum(APPRENTICESHIP), LEVEL_3 = sum(LEVEL_3), LEVEL_4_PLUS = sum(LEVEL_4_PLUS), 
        OTHER_QUALS = sum(OTHER_QUALS),
        .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 1, AREA_CODE = RGN22CD, AREA_NAME = RGN22NM, 
                POPN, 
                NO_QUALS, LEVEL_1, LEVEL_2, APPRENTICESHIP,
                LEVEL_3, LEVEL_4_PLUS, OTHER_QUALS)
  ) %>% 
  bind_rows(
    df_ts067_ltla %>% 
      group_by(LAD22CD, LAD22NM) %>% 
      summarise(
        POPN = sum(POPN), 
        NO_QUALS = sum(NO_QUALS), LEVEL_1 = sum(LEVEL_1), LEVEL_2 = sum(LEVEL_2), 
        APPRENTICESHIP = sum(APPRENTICESHIP), LEVEL_3 = sum(LEVEL_3), LEVEL_4_PLUS = sum(LEVEL_4_PLUS), 
        OTHER_QUALS = sum(OTHER_QUALS),
        .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 2, AREA_CODE = LAD22CD, AREA_NAME = LAD22NM, 
                POPN, 
                NO_QUALS, LEVEL_1, LEVEL_2, APPRENTICESHIP,
                LEVEL_3, LEVEL_4_PLUS, OTHER_QUALS)
  ) %>% 
  mutate(
    PCT_NO_QUALS = NO_QUALS / POPN, PCT_LEVEL_1 = LEVEL_1 / POPN,
    PCT_LEVEL_2 = LEVEL_2 / POPN, PCT_APPRENTICESHIP = APPRENTICESHIP / POPN,
    PCT_LEVEL_3 = LEVEL_3 / POPN, PCT_LEVEL_4_PLUS = LEVEL_4_PLUS / POPN,
    PCT_OTHER_QUALS = OTHER_QUALS / POPN
  ) %>% 
  group_by(LEVEL) %>%
  mutate(
    RANK_NO_QUALS = rank(desc(PCT_NO_QUALS), ties.method = 'first'),
    RANK_NO_QUALS = sprintf('(%d of %d)', RANK_NO_QUALS, n()),
    RANK_LEVEL_1 = rank(desc(PCT_LEVEL_1), ties.method = 'first'),
    RANK_LEVEL_1 = sprintf('(%d of %d)', RANK_LEVEL_1, n()),
    RANK_LEVEL_2 = rank(desc(PCT_LEVEL_2), ties.method = 'first'),
    RANK_LEVEL_2 = sprintf('(%d of %d)', RANK_LEVEL_2, n()),
    RANK_APPRENTICESHIP = rank(desc(PCT_APPRENTICESHIP), ties.method = 'first'),
    RANK_APPRENTICESHIP = sprintf('(%d of %d)', RANK_APPRENTICESHIP, n()),
    RANK_LEVEL_3 = rank(desc(PCT_LEVEL_3), ties.method = 'first'),
    RANK_LEVEL_3 = sprintf('(%d of %d)', RANK_LEVEL_3, n()),
    RANK_LEVEL_4_PLUS = rank(desc(PCT_LEVEL_4_PLUS), ties.method = 'first'),
    RANK_LEVEL_4_PLUS = sprintf('(%d of %d)', RANK_LEVEL_4_PLUS, n()),
    RANK_OTHER_QUALS = rank(desc(PCT_OTHER_QUALS), ties.method = 'first'),
    RANK_OTHER_QUALS = sprintf('(%d of %d)', RANK_OTHER_QUALS, n())
  ) %>%
  ungroup() %>%
  filter(LEVEL < 2 | AREA_CODE %in% df_swahsn_lu$LAD22CD)

plt_ts067_no_quals <- ggplot(
  df_ts067_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_NO_QUALS)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
  ) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Population with No Formal Educational Qualifications for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.2,0.05)) %+%
  #scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_NO_QUALS, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts067_no_quals
ggsave('.\\outputs\\08_education\\plt_ts067_no_quals.png', plt_ts067_no_quals, height = 210, width = 297, units = 'mm')

plt_ts067_level_4_plus <- ggplot(
  df_ts067_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_LEVEL_4_PLUS)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Population with Level 4 or above Qualifications for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.5,0.1)) %+%
  #scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_LEVEL_4_PLUS, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts067_level_4_plus
ggsave('.\\outputs\\08_education\\plt_ts067_level_4_plus.png', plt_ts067_level_4_plus, height = 210, width = 297, units = 'mm')

write.csv(df_ts067_ltla_summary, '.\\outputs\\08_education\\ts067_summary.csv', row.names = FALSE)

map_ts067_no_quals <- ggplot(sf_lsoa21 %>% inner_join(df_ts067_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Population with No Formal Educational Qualifications for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Population',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_NO_QUALS),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\08_education\\map_ts067_no_quals_dark.png', map_ts067_no_quals, height = 297, width = 210, units = 'mm')
map_ts067_no_quals <- map_ts067_no_quals %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\08_education\\map_ts067_no_quals_light.png', map_ts067_no_quals, height = 297, width = 210, units = 'mm')

map_ts067_level_4_plus <- ggplot(sf_lsoa21 %>% inner_join(df_ts067_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Population with Level 4 and above Educational Qualifications for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Population',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_LEVEL_4_PLUS),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\08_education\\map_ts067_level_4_plus_dark.png', map_ts067_level_4_plus, height = 297, width = 210, units = 'mm')
map_ts067_level_4_plus <- map_ts067_level_4_plus %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\08_education\\map_ts067_level_4_plus_light.png', map_ts067_level_4_plus, height = 297, width = 210, units = 'mm')
