# Population deprivation
# ======================

dir.create(path = '.\\outputs\\01_population_density', showWarnings = FALSE, recursive = TRUE)

# Load the OA21 data to obtain the areas in m2
df_lad22 <- read.csv('.\\data\\00_reference\\Local_Authority_Districts_(December_2022)_Boundaries_UK_BFC.csv') %>%
  rename_with(.fn = function(x){c('OBJECTID','LAD22CD','LAD22NM','BNG_E','BNG_N','LONG','LAT','GLOBALID','CIRCUMFERENCE_M','AREA_SQ_M')}) %>%
  mutate(AREA_SQ_KM = AREA_SQ_M / 1e06)

# Load the TS006 - Population density population density census data
df_ts006_ltla <- read.csv(unzip('.\\data\\01_demography_and_migration\\census2021-ts006.zip', 'census2021-ts006-ltla.csv')) %>%
  rename_with(.fn = function(x){c('YEAR','LAD22NM','LAD22CD','POPN_PER_SQ_KM')}) %>%
  left_join(df_lad22 %>% select(LAD22CD, LAD22NM, AREA_SQ_KM), by = c('LAD22CD' = 'LAD22CD','LAD22NM' = 'LAD22NM')) %>%
  left_join(df_ts009_ltla %>% select(LAD22CD, PERSONS_TOTAL), by = c('LAD22CD' = 'LAD22CD')) %>%
  inner_join(df_lad22_rgn22_lu %>% select(LAD22CD, RGN22CD, RGN22NM), by = c('LAD22CD' = 'LAD22CD')) %>%
  transmute(YEAR, RGN22CD, RGN22NM, LAD22CD, LAD22NM, POPN = PERSONS_TOTAL, AREA_SQ_KM, POPN_PER_SQ_KM)

df_ts006_lsoa <- read.csv(unzip('.\\data\\01_demography_and_migration\\census2021-ts006.zip', 'census2021-ts006-lsoa.csv')) %>%
  rename_with(.fn = function(x){c('YEAR','LSOA21NM','LSOA21CD','POPN_PER_SQ_KM')}) %>%
  left_join(sf_lsoa21 %>% st_drop_geometry() %>% select(LSOA21CD, SHAPE_Area), by = c('LSOA21CD' = 'LSOA21CD')) %>%
  transmute(YEAR, LSOA21CD, LSOA21NM, AREA_SQ_KM = SHAPE_Area / 1e06, POPN_PER_SQ_KM)

df_ts006_summary <- df_ts006_ltla %>% 
  summarise(POPN = sum(POPN), AREA_SQ_KM = sum(AREA_SQ_KM), .groups = 'keep') %>%
  transmute(LEVEL = 0, AREA_CODE = 'E92000001', AREA_NAME = 'ENGLAND', POPN, AREA_SQ_KM, POPN_PER_SQ_KM = POPN / AREA_SQ_KM) %>%
  bind_rows(
    df_ts006_ltla %>% 
      group_by(RGN22CD, RGN22NM) %>% 
      summarise(POPN = sum(POPN), AREA_SQ_KM = sum(AREA_SQ_KM), .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 1, AREA_CODE = RGN22CD, AREA_NAME = RGN22NM, POPN, AREA_SQ_KM, POPN_PER_SQ_KM = POPN / AREA_SQ_KM)
  ) %>%
  bind_rows(
    df_ts006_ltla %>% 
# Don't filter for local areas as we need all for ranking
#      semi_join(df_swahsn_lu, by = c('LAD22CD' = 'LAD22CD', 'LAD22NM' = 'LAD22NM')) %>%
      transmute(LEVEL = 2, AREA_CODE = LAD22CD, AREA_NAME = LAD22NM, POPN, AREA_SQ_KM, POPN_PER_SQ_KM = POPN / AREA_SQ_KM)
  ) %>%
  group_by(LEVEL) %>%
  mutate(
    RANK_DENSITY = rank(POPN_PER_SQ_KM, ties.method = 'first'),
    RANK_DENSITY_LABEL = sprintf('(%d of %d)', RANK_DENSITY, n())
  ) %>%
  ungroup() %>%
  filter(LEVEL < 2 | AREA_CODE %in% df_swahsn_lu$LAD22CD)

plt_ts006 <- ggplot(
  df_ts006_summary %>% 
    arrange(LEVEL, desc(POPN_PER_SQ_KM)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(title = str_wrap('Population per Square Km for England, Regions, and Local Authority Districts (LAD)', 80),
       x = 'Area Name', y = 'Popn / Sq. Km') %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = POPN_PER_SQ_KM, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts006
ggsave('.\\outputs\\01_population_density\\ts006_bar_chart.png', plt_ts006, height = 210, width = 297, units = 'mm')
write.csv(df_ts006_summary, '.\\outputs\\01_population_density\\ts006_summary.csv', row.names = FALSE)

map_ts006 <- ggplot(sf_lsoa21 %>% inner_join(df_ts006_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(15, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Population Density for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = 'Popn / Sq. Km',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = POPN_PER_SQ_KM),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, trans = "log1p", breaks = c(0,10,100,1000,10000,100000), labels = c('0','10','100','1,000','10,000','100,000'))
ggsave('.\\outputs\\01_population_density\\ts006_map_dark.png', map_ts006, height = 297, width = 210, units = 'mm')
map_ts006 <- map_ts006 %+% scale_fill_viridis_c(direction = -1, trans = "log1p", breaks = c(0,10,100,1000,10000,100000), labels = c('0','10','100','1,000','10,000','100,000'))
ggsave('.\\outputs\\01_population_density\\ts006_map_light.png', map_ts006, height = 297, width = 210, units = 'mm')

