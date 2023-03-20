# Tenure
# ======

dir.create(path = '.\\outputs\\11_tenure', showWarnings = FALSE, recursive = TRUE)
# Load the TS054 - Tenure data

df_ts054_lsoa <- read.csv(unzip('.\\data\\05_housing\\census2021-ts054.zip', 'census2021-ts054-lsoa.csv')) %>%
  rename_with(.fn = function(x){c('YEAR','LSOA21NM','LSOA21CD',
                                  'HOUSEHOLDS', 
                                  'OWNED', 'OWNED_OUTRIGHT', 'OWNED_MORTGAGE',
                                  'SHARED_OWNERSHIP', 'SHARED_OWNERSHIP_DISCARD',
                                  'RENTED_SOCIAL', 'RENTED_SOCIAL_LA', 'RENTED_SOCIAL_OTHER',
                                  'RENTED_PRIVATE', 'RENTED_PRIVATE_LANDLORD', 'RENTED_PRIVATE_OTHER',
                                  'RENT_FREE')}) %>%
  mutate(
    PCT_OWNED = OWNED / HOUSEHOLDS,
    PCT_OWNED_OUTRIGHT = OWNED_OUTRIGHT / HOUSEHOLDS,
    PCT_OWNED_MORTGAGE = OWNED_MORTGAGE / HOUSEHOLDS,
    PCT_SHARED_OWNERSHIP = SHARED_OWNERSHIP / HOUSEHOLDS,
    PCT_SHARED_OWNERSHIP_DISCARD = SHARED_OWNERSHIP_DISCARD / HOUSEHOLDS,
    PCT_RENTED_SOCIAL = RENTED_SOCIAL / HOUSEHOLDS,
    PCT_RENTED_SOCIAL_LA = RENTED_SOCIAL_LA / HOUSEHOLDS,
    PCT_RENTED_SOCIAL_OTHER = RENTED_SOCIAL_OTHER / HOUSEHOLDS,
    PCT_RENTED_PRIVATE = RENTED_PRIVATE / HOUSEHOLDS,
    PCT_RENTED_PRIVATE_LANDLORD = RENTED_PRIVATE_LANDLORD / HOUSEHOLDS,
    PCT_RENTED_PRIVATE_OTHER = RENTED_PRIVATE_OTHER / HOUSEHOLDS,
    PCT_RENT_FREE = RENT_FREE / HOUSEHOLDS
  )

df_ts054_ltla <- read.csv(unzip('.\\data\\05_housing\\census2021-ts054.zip', 'census2021-ts054-ltla.csv')) %>%
  rename_with(.fn = function(x){c('YEAR','LAD22NM','LAD22CD',
                                  'HOUSEHOLDS', 
                                  'OWNED', 'OWNED_OUTRIGHT', 'OWNED_MORTGAGE',
                                  'SHARED_OWNERSHIP', 'SHARED_OWNERSHIP_DISCARD',
                                  'RENTED_SOCIAL', 'RENTED_SOCIAL_LA', 'RENTED_SOCIAL_OTHER',
                                  'RENTED_PRIVATE', 'RENTED_PRIVATE_LANDLORD', 'RENTED_PRIVATE_OTHER',
                                  'RENT_FREE')}) %>%
  inner_join(df_lad22_rgn22_lu %>% select(LAD22CD, RGN22CD, RGN22NM), by = c('LAD22CD' = 'LAD22CD')) %>%
  transmute(YEAR, 
            RGN22CD, RGN22NM, 
            LAD22CD, LAD22NM, 
            HOUSEHOLDS, 
            OWNED, OWNED_OUTRIGHT, OWNED_MORTGAGE,
            SHARED_OWNERSHIP, SHARED_OWNERSHIP_DISCARD,
            RENTED_SOCIAL, RENTED_SOCIAL_LA, RENTED_SOCIAL_OTHER,
            RENTED_PRIVATE, RENTED_PRIVATE_LANDLORD, RENTED_PRIVATE_OTHER,
            RENT_FREE)
            
df_ts054_ltla_summary <- df_ts054_ltla %>% 
  summarise(
    HOUSEHOLDS = sum(HOUSEHOLDS), 
    OWNED = sum(OWNED), 
    OWNED_OUTRIGHT = sum(OWNED_OUTRIGHT), 
    OWNED_MORTGAGE = sum(OWNED_MORTGAGE),
    SHARED_OWNERSHIP = sum(SHARED_OWNERSHIP), 
    SHARED_OWNERSHIP_DISCARD = sum(SHARED_OWNERSHIP_DISCARD),
    RENTED_SOCIAL = sum(RENTED_SOCIAL), 
    RENTED_SOCIAL_LA = sum(RENTED_SOCIAL_LA), 
    RENTED_SOCIAL_OTHER = sum(RENTED_SOCIAL_OTHER),
    RENTED_PRIVATE = sum(RENTED_PRIVATE), 
    RENTED_PRIVATE_LANDLORD = sum(RENTED_PRIVATE_LANDLORD), 
    RENTED_PRIVATE_OTHER = sum(RENTED_PRIVATE_OTHER),
    RENT_FREE = sum(RENT_FREE),
    .groups = 'keep') %>%
  transmute(LEVEL = 0, AREA_CODE = 'E92000001', AREA_NAME = 'ENGLAND', 
            HOUSEHOLDS, 
            OWNED, OWNED_OUTRIGHT, OWNED_MORTGAGE,
            SHARED_OWNERSHIP, SHARED_OWNERSHIP_DISCARD,
            RENTED_SOCIAL, RENTED_SOCIAL_LA, RENTED_SOCIAL_OTHER,
            RENTED_PRIVATE, RENTED_PRIVATE_LANDLORD, RENTED_PRIVATE_OTHER,
            RENT_FREE
  ) %>% 
  bind_rows(
    df_ts054_ltla %>% 
      group_by(RGN22CD, RGN22NM) %>% 
      summarise(
        HOUSEHOLDS = sum(HOUSEHOLDS), 
        OWNED = sum(OWNED), 
        OWNED_OUTRIGHT = sum(OWNED_OUTRIGHT), 
        OWNED_MORTGAGE = sum(OWNED_MORTGAGE),
        SHARED_OWNERSHIP = sum(SHARED_OWNERSHIP), 
        SHARED_OWNERSHIP_DISCARD = sum(SHARED_OWNERSHIP_DISCARD),
        RENTED_SOCIAL = sum(RENTED_SOCIAL), 
        RENTED_SOCIAL_LA = sum(RENTED_SOCIAL_LA), 
        RENTED_SOCIAL_OTHER = sum(RENTED_SOCIAL_OTHER),
        RENTED_PRIVATE = sum(RENTED_PRIVATE), 
        RENTED_PRIVATE_LANDLORD = sum(RENTED_PRIVATE_LANDLORD), 
        RENTED_PRIVATE_OTHER = sum(RENTED_PRIVATE_OTHER),
        RENT_FREE = sum(RENT_FREE),
        .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 1, AREA_CODE = RGN22CD, AREA_NAME = RGN22NM, 
                HOUSEHOLDS, 
                OWNED, OWNED_OUTRIGHT, OWNED_MORTGAGE,
                SHARED_OWNERSHIP, SHARED_OWNERSHIP_DISCARD,
                RENTED_SOCIAL, RENTED_SOCIAL_LA, RENTED_SOCIAL_OTHER,
                RENTED_PRIVATE, RENTED_PRIVATE_LANDLORD, RENTED_PRIVATE_OTHER,
                RENT_FREE)
  ) %>% 
  bind_rows(
    df_ts054_ltla %>% 
      group_by(LAD22CD, LAD22NM) %>% 
      summarise(
        HOUSEHOLDS = sum(HOUSEHOLDS), 
        OWNED = sum(OWNED), 
        OWNED_OUTRIGHT = sum(OWNED_OUTRIGHT), 
        OWNED_MORTGAGE = sum(OWNED_MORTGAGE),
        SHARED_OWNERSHIP = sum(SHARED_OWNERSHIP), 
        SHARED_OWNERSHIP_DISCARD = sum(SHARED_OWNERSHIP_DISCARD),
        RENTED_SOCIAL = sum(RENTED_SOCIAL), 
        RENTED_SOCIAL_LA = sum(RENTED_SOCIAL_LA), 
        RENTED_SOCIAL_OTHER = sum(RENTED_SOCIAL_OTHER),
        RENTED_PRIVATE = sum(RENTED_PRIVATE), 
        RENTED_PRIVATE_LANDLORD = sum(RENTED_PRIVATE_LANDLORD), 
        RENTED_PRIVATE_OTHER = sum(RENTED_PRIVATE_OTHER),
        RENT_FREE = sum(RENT_FREE),
        .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 2, AREA_CODE = LAD22CD, AREA_NAME = LAD22NM, 
                HOUSEHOLDS, 
                OWNED, OWNED_OUTRIGHT, OWNED_MORTGAGE,
                SHARED_OWNERSHIP, SHARED_OWNERSHIP_DISCARD,
                RENTED_SOCIAL, RENTED_SOCIAL_LA, RENTED_SOCIAL_OTHER,
                RENTED_PRIVATE, RENTED_PRIVATE_LANDLORD, RENTED_PRIVATE_OTHER,
                RENT_FREE)
  ) %>% 
  mutate(
    PCT_OWNED = OWNED / HOUSEHOLDS, 
    PCT_OWNED_OUTRIGHT = OWNED_OUTRIGHT / HOUSEHOLDS, 
    PCT_OWNED_MORTGAGE = OWNED_MORTGAGE / HOUSEHOLDS,
    PCT_SHARED_OWNERSHIP = SHARED_OWNERSHIP / HOUSEHOLDS, 
    PCT_SHARED_OWNERSHIP_DISCARD = SHARED_OWNERSHIP_DISCARD / HOUSEHOLDS,
    PCT_RENTED_SOCIAL = RENTED_SOCIAL / HOUSEHOLDS, 
    PCT_RENTED_SOCIAL_LA = RENTED_SOCIAL_LA / HOUSEHOLDS, 
    PCT_RENTED_SOCIAL_OTHER = RENTED_SOCIAL_OTHER / HOUSEHOLDS,
    PCT_RENTED_PRIVATE = RENTED_PRIVATE / HOUSEHOLDS, 
    PCT_RENTED_PRIVATE_LANDLORD = RENTED_PRIVATE_LANDLORD / HOUSEHOLDS, 
    PCT_RENTED_PRIVATE_OTHER = RENTED_PRIVATE_OTHER / HOUSEHOLDS,
    PCT_RENT_FREE = RENT_FREE / HOUSEHOLDS
  ) %>% 
  group_by(LEVEL) %>%
  mutate(
    RANK_OWNED = rank(desc(PCT_OWNED ), ties.method = 'first'),
    RANK_OWNED = sprintf('(%d of %d)', RANK_OWNED , n()),
    RANK_OWNED_OUTRIGHT = rank(desc(PCT_OWNED_OUTRIGHT ), ties.method = 'first'),
    RANK_OWNED_OUTRIGHT = sprintf('(%d of %d)', RANK_OWNED_OUTRIGHT , n()),
    RANK_OWNED_MORTGAGE = rank(desc(PCT_OWNED_MORTGAGE ), ties.method = 'first'),
    RANK_OWNED_MORTGAGE = sprintf('(%d of %d)', RANK_OWNED_MORTGAGE , n()),
    RANK_SHARED_OWNERSHIP = rank(desc(PCT_SHARED_OWNERSHIP ), ties.method = 'first'),
    RANK_SHARED_OWNERSHIP = sprintf('(%d of %d)', RANK_SHARED_OWNERSHIP , n()),
    RANK_SHARED_OWNERSHIP_DISCARD = rank(desc(PCT_SHARED_OWNERSHIP_DISCARD ), ties.method = 'first'),
    RANK_SHARED_OWNERSHIP_DISCARD = sprintf('(%d of %d)', RANK_SHARED_OWNERSHIP_DISCARD , n()),
    RANK_RENTED_SOCIAL = rank(desc(PCT_RENTED_SOCIAL ), ties.method = 'first'),
    RANK_RENTED_SOCIAL = sprintf('(%d of %d)', RANK_RENTED_SOCIAL , n()),
    RANK_RENTED_SOCIAL_LA = rank(desc(PCT_RENTED_SOCIAL_LA ), ties.method = 'first'),
    RANK_RENTED_SOCIAL_LA = sprintf('(%d of %d)', RANK_RENTED_SOCIAL_LA , n()),
    RANK_RENTED_SOCIAL_OTHER = rank(desc(PCT_RENTED_SOCIAL_OTHER ), ties.method = 'first'),
    RANK_RENTED_SOCIAL_OTHER = sprintf('(%d of %d)', RANK_RENTED_SOCIAL_OTHER , n()),
    RANK_RENTED_PRIVATE = rank(desc(PCT_RENTED_PRIVATE ), ties.method = 'first'),
    RANK_RENTED_PRIVATE = sprintf('(%d of %d)', RANK_RENTED_PRIVATE , n()),
    RANK_RENTED_PRIVATE_LANDLORD = rank(desc(PCT_RENTED_PRIVATE_LANDLORD ), ties.method = 'first'),
    RANK_RENTED_PRIVATE_LANDLORD = sprintf('(%d of %d)', RANK_RENTED_PRIVATE_LANDLORD , n()),
    RANK_RENTED_PRIVATE_OTHER = rank(desc(PCT_RENTED_PRIVATE_OTHER ), ties.method = 'first'),
    RANK_RENTED_PRIVATE_OTHER = sprintf('(%d of %d)', RANK_RENTED_PRIVATE_OTHER , n()),
    RANK_RENT_FREE = rank(desc(PCT_RENT_FREE ), ties.method = 'first'),
    RANK_RENT_FREE = sprintf('(%d of %d)', RANK_RENT_FREE , n())
  ) %>%
  ungroup() %>%
  filter(LEVEL < 2 | AREA_CODE %in% df_swahsn_lu$LAD22CD)

plt_ts054_owned <- ggplot(
  df_ts054_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_OWNED)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
  ) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Households Owned by Occupier for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Households'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.8,0.2)) %+%
  #scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_OWNED, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts054_owned
ggsave('.\\outputs\\11_tenure\\plt_ts054_owned.png', plt_ts054_owned, height = 210, width = 297, units = 'mm')

plt_ts054_social_rented <- ggplot(
  df_ts054_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_RENTED_SOCIAL)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Households Social Rented for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Households'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.2,0.05)) %+%
  #scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_RENTED_SOCIAL, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts054_social_rented
ggsave('.\\outputs\\11_tenure\\plt_ts054_social_rented.png', plt_ts054_social_rented, height = 210, width = 297, units = 'mm')

plt_ts054_private_rented <- ggplot(
  df_ts054_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_RENTED_PRIVATE)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Households Private Rented for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Households'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.4,0.05)) %+%
  #scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_RENTED_PRIVATE, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts054_private_rented
ggsave('.\\outputs\\11_tenure\\plt_ts054_private_rented.png', plt_ts054_private_rented, height = 210, width = 297, units = 'mm')

write.csv(df_ts054_ltla_summary, '.\\outputs\\11_tenure\\ts054_summary.csv', row.names = FALSE)

map_ts054_owned <- ggplot(sf_lsoa21 %>% inner_join(df_ts054_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Households Owned by Occupier for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Households',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_OWNED),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\11_tenure\\map_ts054_owned_dark.png', map_ts054_owned, height = 297, width = 210, units = 'mm')
map_ts054_owned <- map_ts054_owned %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\11_tenure\\map_ts054_owned_light.png', map_ts054_owned, height = 297, width = 210, units = 'mm')

map_ts054_social_rented <- ggplot(sf_lsoa21 %>% inner_join(df_ts054_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Households Social Rented for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Households',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_RENTED_SOCIAL),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\11_tenure\\map_ts054_social_rented_dark.png', map_ts054_social_rented, height = 297, width = 210, units = 'mm')
map_ts054_social_rented <- map_ts054_social_rented %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\11_tenure\\map_ts054_social_rented_light.png', map_ts054_social_rented, height = 297, width = 210, units = 'mm')

map_ts054_private_rented <- ggplot(sf_lsoa21 %>% inner_join(df_ts054_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Households Private Rented for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Households',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_RENTED_PRIVATE),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\11_tenure\\map_ts054_private_rented_dark.png', map_ts054_private_rented, height = 297, width = 210, units = 'mm')
map_ts054_private_rented <- map_ts054_private_rented %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\11_tenure\\map_ts054_private_rented_light.png', map_ts054_private_rented, height = 297, width = 210, units = 'mm')
