# Car ownership
# =============

dir.create(path = '.\\outputs\\09_car_ownership', showWarnings = FALSE, recursive = TRUE)

# Load the TS045 - Car ownership data
df_ts045_lsoa <- read.csv(unzip('.\\data\\05_housing\\census2021-ts045.zip', 'census2021-ts045-lsoa.csv')) %>%
  rename_with(.fn = function(x){c('YEAR','LSOA21NM','LSOA21CD',
                                  'HOUSEHOLDS', 
                                  'NO_CARS', 'CARS_1', 'CARS_2', 'CARS_3_PLUS')}) %>%
  mutate(
    PCT_NO_CARS = NO_CARS / HOUSEHOLDS,
    PCT_CARS_1 = CARS_1 / HOUSEHOLDS,
    PCT_CARS_2 = CARS_2 / HOUSEHOLDS,
    PCT_CARS_3_PLUS = CARS_3_PLUS / HOUSEHOLDS)

df_ts045_ltla <- read.csv(unzip('.\\data\\05_housing\\census2021-ts045.zip', 'census2021-ts045-ltla.csv')) %>%
  rename_with(.fn = function(x){c('YEAR','LAD22NM','LAD22CD',
                                  'HOUSEHOLDS', 
                                  'NO_CARS', 'CARS_1', 'CARS_2', 'CARS_3_PLUS')}) %>%
  inner_join(df_lad22_rgn22_lu %>% select(LAD22CD, RGN22CD, RGN22NM), by = c('LAD22CD' = 'LAD22CD')) %>%
  transmute(YEAR, 
            RGN22CD, RGN22NM, 
            LAD22CD, LAD22NM, 
            HOUSEHOLDS, 
            NO_CARS, CARS_1, CARS_2, CARS_3_PLUS)
  
df_ts045_ltla_summary <- df_ts045_ltla %>% 
  summarise(
    HOUSEHOLDS = sum(HOUSEHOLDS), 
    NO_CARS = sum(NO_CARS), CARS_1 = sum(CARS_1), 
    CARS_2 = sum(CARS_2), CARS_3_PLUS = sum(CARS_3_PLUS),
    .groups = 'keep') %>%
  transmute(LEVEL = 0, AREA_CODE = 'E92000001', AREA_NAME = 'ENGLAND', 
            HOUSEHOLDS, 
            NO_CARS, CARS_1, CARS_2, CARS_3_PLUS
  ) %>% 
  bind_rows(
    df_ts045_ltla %>% 
      group_by(RGN22CD, RGN22NM) %>% 
      summarise(
        HOUSEHOLDS = sum(HOUSEHOLDS), 
        NO_CARS = sum(NO_CARS), CARS_1 = sum(CARS_1), 
        CARS_2 = sum(CARS_2), CARS_3_PLUS = sum(CARS_3_PLUS),
        .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 1, AREA_CODE = RGN22CD, AREA_NAME = RGN22NM, 
                HOUSEHOLDS, 
                NO_CARS, CARS_1, CARS_2, CARS_3_PLUS)
  ) %>% 
  bind_rows(
    df_ts045_ltla %>% 
      group_by(LAD22CD, LAD22NM) %>% 
      summarise(
        HOUSEHOLDS = sum(HOUSEHOLDS), 
        NO_CARS = sum(NO_CARS), CARS_1 = sum(CARS_1), 
        CARS_2 = sum(CARS_2), CARS_3_PLUS = sum(CARS_3_PLUS),
        .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 2, AREA_CODE = LAD22CD, AREA_NAME = LAD22NM, 
                HOUSEHOLDS, 
                NO_CARS, CARS_1, CARS_2, CARS_3_PLUS)
  ) %>% 
  mutate(
    PCT_NO_CARS = NO_CARS / HOUSEHOLDS, PCT_CARS_1 = CARS_1 / HOUSEHOLDS,
    PCT_CARS_2 = CARS_2 / HOUSEHOLDS, PCT_CARS_3_PLUS = CARS_3_PLUS / HOUSEHOLDS
  ) %>% 
  group_by(LEVEL) %>%
  mutate(
    RANK_NO_CARS = rank(desc(PCT_NO_CARS), ties.method = 'first'),
    RANK_NO_CARS = sprintf('(%d of %d)', RANK_NO_CARS, n()),
    RANK_CARS_1 = rank(desc(PCT_CARS_1), ties.method = 'first'),
    RANK_CARS_1 = sprintf('(%d of %d)', RANK_CARS_1, n()),
    RANK_CARS_2 = rank(desc(PCT_CARS_2), ties.method = 'first'),
    RANK_CARS_2 = sprintf('(%d of %d)', RANK_CARS_2, n()),
    RANK_CARS_3_PLUS = rank(desc(PCT_CARS_3_PLUS), ties.method = 'first'),
    RANK_CARS_3_PLUS = sprintf('(%d of %d)', RANK_CARS_3_PLUS, n())
  ) %>%
  ungroup() %>%
  filter(LEVEL < 2 | AREA_CODE %in% df_swahsn_lu$LAD22CD)

plt_ts045_no_cars <- ggplot(
  df_ts045_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_NO_CARS)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
  ) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Households with No Car for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Households'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.4,0.1)) %+%
  #scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_NO_CARS, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts045_no_cars
ggsave('.\\outputs\\09_car_ownership\\plt_ts045_no_cars.png', plt_ts045_no_cars, height = 210, width = 297, units = 'mm')

write.csv(df_ts045_ltla_summary, '.\\outputs\\09_car_ownership\\ts045_summary.csv', row.names = FALSE)

map_ts045_no_cars <- ggplot(sf_lsoa21 %>% inner_join(df_ts045_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Households with No Cars for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Households',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_NO_CARS),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\09_car_ownership\\map_ts045_no_cars_dark.png', map_ts045_no_cars, height = 297, width = 210, units = 'mm')
map_ts045_no_cars <- map_ts045_no_cars %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\09_car_ownership\\map_ts045_no_cars_light.png', map_ts045_no_cars, height = 297, width = 210, units = 'mm')
