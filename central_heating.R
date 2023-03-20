# Central heating
# ===============

dir.create(path = '.\\outputs\\10_central_heating', showWarnings = FALSE, recursive = TRUE)

# Load the TS046 - Central heating data
df_ts046_lsoa <- read.csv(unzip('.\\data\\05_housing\\census2021-ts046.zip', 'census2021-ts046-lsoa.csv')) %>%
  rename_with(.fn = function(x){c('YEAR','LSOA21NM','LSOA21CD',
                                  'HOUSEHOLDS', 
                                  'NO_CENTRAL_HEATING', 
                                  'MAINS_GAS', 'BOTTLED_GAS', 'ELECTRIC', 'OIL', 'WOOD',
                                  'SOLID_FUEL', 'RENEWABLE', 'COMMUNAL_HEAT', 'OTHER',
                                  'TWO_PLUS_TYPES_NOT_RENEWABLE', 'TWO_PLUS_TYPES_RENEWABLE')}) %>%
  mutate(
    PCT_NO_CENTRAL_HEATING = NO_CENTRAL_HEATING / HOUSEHOLDS,
    PCT_MAINS_GAS = MAINS_GAS / HOUSEHOLDS,
    PCT_BOTTLED_GAS = BOTTLED_GAS / HOUSEHOLDS,
    PCT_ELECTRIC = ELECTRIC / HOUSEHOLDS,
    PCT_OIL = OIL / HOUSEHOLDS,
    PCT_WOOD = WOOD / HOUSEHOLDS,
    PCT_SOLID_FUEL = SOLID_FUEL / HOUSEHOLDS,
    PCT_RENEWABLE = RENEWABLE / HOUSEHOLDS,
    PCT_COMMUNAL_HEAT = COMMUNAL_HEAT / HOUSEHOLDS,
    PCT_OTHER = OTHER / HOUSEHOLDS,
    PCT_TWO_PLUS_TYPES_NOT_RENEWABLE = TWO_PLUS_TYPES_NOT_RENEWABLE / HOUSEHOLDS,
    PCT_TWO_PLUS_TYPES_RENEWABLE = TWO_PLUS_TYPES_RENEWABLE / HOUSEHOLDS)

df_ts046_ltla <- read.csv(unzip('.\\data\\05_housing\\census2021-ts046.zip', 'census2021-ts046-ltla.csv')) %>%
  rename_with(.fn = function(x){c('YEAR','LAD22NM','LAD22CD',
                                  'HOUSEHOLDS', 
                                  'NO_CENTRAL_HEATING', 
                                  'MAINS_GAS', 'BOTTLED_GAS', 'ELECTRIC', 'OIL', 'WOOD',
                                  'SOLID_FUEL', 'RENEWABLE', 'COMMUNAL_HEAT', 'OTHER',
                                  'TWO_PLUS_TYPES_NOT_RENEWABLE', 'TWO_PLUS_TYPES_RENEWABLE')}) %>%
  inner_join(df_lad22_rgn22_lu %>% select(LAD22CD, RGN22CD, RGN22NM), by = c('LAD22CD' = 'LAD22CD')) %>%
  transmute(YEAR, 
            RGN22CD, RGN22NM, 
            LAD22CD, LAD22NM, 
            HOUSEHOLDS, 
            NO_CENTRAL_HEATING,
            MAINS_GAS, BOTTLED_GAS, ELECTRIC, OIL, WOOD,
            SOLID_FUEL, RENEWABLE, COMMUNAL_HEAT, OTHER,
            TWO_PLUS_TYPES_NOT_RENEWABLE, TWO_PLUS_TYPES_RENEWABLE)
            
df_ts046_ltla_summary <- df_ts046_ltla %>% 
  summarise(
    HOUSEHOLDS = sum(HOUSEHOLDS), 
    NO_CENTRAL_HEATING = sum(NO_CENTRAL_HEATING),
    MAINS_GAS = sum(MAINS_GAS), BOTTLED_GAS = sum(BOTTLED_GAS), ELECTRIC = sum(ELECTRIC), 
    OIL = sum(OIL), WOOD = sum(WOOD), SOLID_FUEL = sum(SOLID_FUEL), 
    RENEWABLE = sum(RENEWABLE), COMMUNAL_HEAT = sum(COMMUNAL_HEAT), OTHER = sum(OTHER),
    TWO_PLUS_TYPES_NOT_RENEWABLE = sum(TWO_PLUS_TYPES_NOT_RENEWABLE), 
    TWO_PLUS_TYPES_RENEWABLE = sum(TWO_PLUS_TYPES_RENEWABLE),
    .groups = 'keep') %>%
  transmute(LEVEL = 0, AREA_CODE = 'E92000001', AREA_NAME = 'ENGLAND', 
            HOUSEHOLDS, 
            NO_CENTRAL_HEATING,
            MAINS_GAS, BOTTLED_GAS, ELECTRIC, OIL, WOOD,
            SOLID_FUEL, RENEWABLE, COMMUNAL_HEAT, OTHER,
            TWO_PLUS_TYPES_NOT_RENEWABLE, TWO_PLUS_TYPES_RENEWABLE
  ) %>% 
  bind_rows(
    df_ts046_ltla %>% 
      group_by(RGN22CD, RGN22NM) %>% 
      summarise(
        HOUSEHOLDS = sum(HOUSEHOLDS), 
        NO_CENTRAL_HEATING = sum(NO_CENTRAL_HEATING),
        MAINS_GAS = sum(MAINS_GAS), BOTTLED_GAS = sum(BOTTLED_GAS), ELECTRIC = sum(ELECTRIC), 
        OIL = sum(OIL), WOOD = sum(WOOD), SOLID_FUEL = sum(SOLID_FUEL), 
        RENEWABLE = sum(RENEWABLE), COMMUNAL_HEAT = sum(COMMUNAL_HEAT), OTHER = sum(OTHER),
        TWO_PLUS_TYPES_NOT_RENEWABLE = sum(TWO_PLUS_TYPES_NOT_RENEWABLE), 
        TWO_PLUS_TYPES_RENEWABLE = sum(TWO_PLUS_TYPES_RENEWABLE),
        .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 1, AREA_CODE = RGN22CD, AREA_NAME = RGN22NM, 
                HOUSEHOLDS, 
                NO_CENTRAL_HEATING,
                MAINS_GAS, BOTTLED_GAS, ELECTRIC, OIL, WOOD,
                SOLID_FUEL, RENEWABLE, COMMUNAL_HEAT, OTHER,
                TWO_PLUS_TYPES_NOT_RENEWABLE, TWO_PLUS_TYPES_RENEWABLE)
  ) %>% 
  bind_rows(
    df_ts046_ltla %>% 
      group_by(LAD22CD, LAD22NM) %>% 
      summarise(
        HOUSEHOLDS = sum(HOUSEHOLDS), 
        NO_CENTRAL_HEATING = sum(NO_CENTRAL_HEATING),
        MAINS_GAS = sum(MAINS_GAS), BOTTLED_GAS = sum(BOTTLED_GAS), ELECTRIC = sum(ELECTRIC), 
        OIL = sum(OIL), WOOD = sum(WOOD), SOLID_FUEL = sum(SOLID_FUEL), 
        RENEWABLE = sum(RENEWABLE), COMMUNAL_HEAT = sum(COMMUNAL_HEAT), OTHER = sum(OTHER),
        TWO_PLUS_TYPES_NOT_RENEWABLE = sum(TWO_PLUS_TYPES_NOT_RENEWABLE), 
        TWO_PLUS_TYPES_RENEWABLE = sum(TWO_PLUS_TYPES_RENEWABLE),
        .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 2, AREA_CODE = LAD22CD, AREA_NAME = LAD22NM, 
                HOUSEHOLDS, 
                NO_CENTRAL_HEATING,
                MAINS_GAS, BOTTLED_GAS, ELECTRIC, OIL, WOOD,
                SOLID_FUEL, RENEWABLE, COMMUNAL_HEAT, OTHER,
                TWO_PLUS_TYPES_NOT_RENEWABLE, TWO_PLUS_TYPES_RENEWABLE)
  ) %>% 
  mutate(
    PCT_NO_CENTRAL_HEATING = NO_CENTRAL_HEATING / HOUSEHOLDS,
    PCT_MAINS_GAS = MAINS_GAS / HOUSEHOLDS,
    PCT_BOTTLED_GAS = BOTTLED_GAS / HOUSEHOLDS,
    PCT_ELECTRIC = ELECTRIC / HOUSEHOLDS,
    PCT_OIL = OIL / HOUSEHOLDS,
    PCT_WOOD = WOOD / HOUSEHOLDS,
    PCT_SOLID_FUEL = SOLID_FUEL / HOUSEHOLDS,
    PCT_RENEWABLE = RENEWABLE / HOUSEHOLDS,
    PCT_COMMUNAL_HEAT = COMMUNAL_HEAT / HOUSEHOLDS,
    PCT_OTHER = OTHER / HOUSEHOLDS,
    PCT_TWO_PLUS_TYPES_NOT_RENEWABLE = TWO_PLUS_TYPES_NOT_RENEWABLE / HOUSEHOLDS,
    PCT_TWO_PLUS_TYPES_RENEWABLE = TWO_PLUS_TYPES_RENEWABLE / HOUSEHOLDS
  ) %>% 
  group_by(LEVEL) %>%
  mutate(
    RANK_NO_CENTRAL_HEATING = rank(desc(PCT_NO_CENTRAL_HEATING), ties.method = 'first'),
    RANK_NO_CENTRAL_HEATING = sprintf('(%d of %d)', RANK_NO_CENTRAL_HEATING, n()),
    RANK_MAINS_GAS = rank(desc(PCT_MAINS_GAS), ties.method = 'first'),
    RANK_MAINS_GAS = sprintf('(%d of %d)', RANK_MAINS_GAS, n()),
    RANK_BOTTLED_GAS = rank(desc(PCT_BOTTLED_GAS), ties.method = 'first'),
    RANK_BOTTLED_GAS = sprintf('(%d of %d)', RANK_BOTTLED_GAS, n()),
    RANK_ELECTRIC = rank(desc(PCT_ELECTRIC), ties.method = 'first'),
    RANK_ELECTRIC = sprintf('(%d of %d)', RANK_ELECTRIC, n()),
    RANK_OIL = rank(desc(PCT_OIL), ties.method = 'first'),
    RANK_OIL = sprintf('(%d of %d)', RANK_OIL, n()),
    RANK_WOOD = rank(desc(PCT_WOOD), ties.method = 'first'),
    RANK_WOOD = sprintf('(%d of %d)', RANK_WOOD, n()),
    RANK_SOLID_FUEL = rank(desc(PCT_SOLID_FUEL), ties.method = 'first'),
    RANK_SOLID_FUEL = sprintf('(%d of %d)', RANK_SOLID_FUEL, n()),
    RANK_RENEWABLE = rank(desc(PCT_RENEWABLE), ties.method = 'first'),
    RANK_RENEWABLE = sprintf('(%d of %d)', RANK_RENEWABLE, n()),
    RANK_COMMUNAL_HEAT = rank(desc(PCT_COMMUNAL_HEAT), ties.method = 'first'),
    RANK_COMMUNAL_HEAT = sprintf('(%d of %d)', RANK_COMMUNAL_HEAT, n()),
    RANK_OTHER = rank(desc(PCT_OTHER), ties.method = 'first'),
    RANK_OTHER = sprintf('(%d of %d)', RANK_OTHER, n()),
    RANK_TWO_PLUS_TYPES_NOT_RENEWABLE = rank(desc(PCT_TWO_PLUS_TYPES_NOT_RENEWABLE), ties.method = 'first'),
    RANK_TWO_PLUS_TYPES_NOT_RENEWABLE = sprintf('(%d of %d)', RANK_TWO_PLUS_TYPES_NOT_RENEWABLE, n()),
    RANK_TWO_PLUS_TYPES_RENEWABLE = rank(desc(PCT_TWO_PLUS_TYPES_RENEWABLE), ties.method = 'first'),
    RANK_TWO_PLUS_TYPES_RENEWABLE = sprintf('(%d of %d)', RANK_TWO_PLUS_TYPES_RENEWABLE, n())
  ) %>%
  ungroup() %>%
  filter(LEVEL < 2 | AREA_CODE %in% df_swahsn_lu$LAD22CD)

plt_ts046_no_central_heating <- ggplot(
  df_ts046_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_NO_CENTRAL_HEATING)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
  ) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Households with No Central Heating for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Households'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.2,0.05)) %+%
  #scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_NO_CENTRAL_HEATING, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts046_no_central_heating
ggsave('.\\outputs\\10_central_heating\\plt_ts046_no_central_heating.png', plt_ts046_no_central_heating, height = 210, width = 297, units = 'mm')

write.csv(df_ts046_ltla_summary, '.\\outputs\\10_central_heating\\ts046_summary.csv', row.names = FALSE)

map_ts046_no_central_heating <- ggplot(sf_lsoa21 %>% inner_join(df_ts046_lsoa, by = c('LSOA21CD' = 'LSOA21CD', 'LSOA21NM' = 'LSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Households with No Central Heating for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Households',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_NO_CENTRAL_HEATING),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\10_central_heating\\map_ts046_no_central_heating_dark.png', map_ts046_no_central_heating, height = 297, width = 210, units = 'mm')
map_ts046_no_central_heating <- map_ts046_no_central_heating %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\10_central_heating\\map_ts046_no_central_heating_light.png', map_ts046_no_central_heating, height = 297, width = 210, units = 'mm')
