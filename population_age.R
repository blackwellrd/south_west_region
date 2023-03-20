# Population Age
# ==============

dir.create(path = '.\\outputs\\02_population_age', showWarnings = FALSE, recursive = TRUE)

selected_fields <- c(
  'LAD22CD', 'LAD22NM',
  paste0('FEMALES_AGED_', str_sub(paste0('0', seq(0,89,1)), -2)), 'FEMALES_AGED_90_PLUS',
  paste0('MALES_AGED_', str_sub(paste0('0', seq(0,89,1)), -2)), 'MALES_AGED_90_PLUS'
)

# Load the TS009 - Population age census data
df_ts009_ltla <- read.csv(unzip('.\\data\\01_demography_and_migration\\census2021-ts009.zip', 'census2021-ts009-ltla.csv')) %>%
  rename_with(.fn = function(x){c('YEAR','LAD22NM','LAD22CD',
                                  'PERSONS_TOTAL', 'PERSONS_AGED_00_04', paste0('PERSONS_AGED_', str_sub(paste0('0', seq(0,4,1)), -2)),
                                  'PERSONS_AGED_05_09', paste0('PERSONS_AGED_', str_sub(paste0('0', seq(5,9,1)), -2)), 'PERSONS_AGED_10_15', paste0('PERSONS_AGED_', seq(10,15,1)), 
                                  'PERSONS_AGED_16_19', paste0('PERSONS_AGED_', seq(16,19,1)), 'PERSONS_AGED_20_24', paste0('PERSONS_AGED_', seq(20,24,1)), 
                                  'PERSONS_AGED_25_34', paste0('PERSONS_AGED_', seq(25,34,1)), 'PERSONS_AGED_35_49', paste0('PERSONS_AGED_', seq(35,49,1)),
                                  'PERSONS_AGED_50_64', paste0('PERSONS_AGED_', seq(50,64,1)), 'PERSONS_AGED_65_74', paste0('PERSONS_AGED_', seq(65,74,1)),
                                  'PERSONS_AGED_75_84', paste0('PERSONS_AGED_', seq(75,84,1)), 'PERSONS_AGED_85_PLUS', paste0('PERSONS_AGED_', seq(85,89,1)), 
                                  'PERSONS_AGED_90_PLUS',
                                  'FEMALES_TOTAL', 'FEMALES_AGED_00_04', paste0('FEMALES_AGED_', str_sub(paste0('0', seq(0,4,1)), -2)),
                                  'FEMALES_AGED_05_09', paste0('FEMALES_AGED_', str_sub(paste0('0', seq(5,9,1)), -2)), 'FEMALES_AGED_10_15', paste0('FEMALES_AGED_', seq(10,15,1)), 
                                  'FEMALES_AGED_16_19', paste0('FEMALES_AGED_', seq(16,19,1)), 'FEMALES_AGED_20_24', paste0('FEMALES_AGED_', seq(20,24,1)), 
                                  'FEMALES_AGED_25_34', paste0('FEMALES_AGED_', seq(25,34,1)), 'FEMALES_AGED_35_49', paste0('FEMALES_AGED_', seq(35,49,1)),
                                  'FEMALES_AGED_50_64', paste0('FEMALES_AGED_', seq(50,64,1)), 'FEMALES_AGED_65_74', paste0('FEMALES_AGED_', seq(65,74,1)),
                                  'FEMALES_AGED_75_84', paste0('FEMALES_AGED_', seq(75,84,1)), 'FEMALES_AGED_85_PLUS', paste0('FEMALES_AGED_', seq(85,89,1)), 
                                  'FEMALES_AGED_90_PLUS',
                                  'MALES_TOTAL', 'MALES_AGED_00_04', paste0('MALES_AGED_', str_sub(paste0('0', seq(0,4,1)), -2)),
                                  'MALES_AGED_05_09', paste0('MALES_AGED_', str_sub(paste0('0', seq(5,9,1)), -2)), 'MALES_AGED_10_15', paste0('MALES_AGED_', seq(10,15,1)), 
                                  'MALES_AGED_16_19', paste0('MALES_AGED_', seq(16,19,1)), 'MALES_AGED_20_24', paste0('MALES_AGED_', seq(20,24,1)), 
                                  'MALES_AGED_25_34', paste0('MALES_AGED_', seq(25,34,1)), 'MALES_AGED_35_49', paste0('MALES_AGED_', seq(35,49,1)),
                                  'MALES_AGED_50_64', paste0('MALES_AGED_', seq(50,64,1)), 'MALES_AGED_65_74', paste0('MALES_AGED_', seq(65,74,1)),
                                  'MALES_AGED_75_84', paste0('MALES_AGED_', seq(75,84,1)), 'MALES_AGED_85_PLUS', paste0('MALES_AGED_', seq(85,89,1)), 
                                  'MALES_AGED_90_PLUS')})

df_ts009_ltla_syoa <- df_ts009_ltla %>% select(!!!quos(selected_fields)) %>% 
  pivot_longer(cols = contains('_AGED_'), names_to = 'AGE', values_to = 'POPN') %>%
  mutate(AGE = gsub('_PLUS', '', AGE)) %>%
  mutate(
    GENDER = ifelse(grepl('^FEMALE',AGE), 'FEMALE', 'MALE'),
    AGE = as.integer(str_sub(AGE, -2))
  ) %>%
  inner_join(df_lad22_rgn22_lu %>% select(LAD22CD, RGN22CD, RGN22NM), by = c('LAD22CD' = 'LAD22CD'))

df_ts009_ltla_syoa_pyramid_data <- df_ts009_ltla_syoa %>% 
  group_by(AGE, GENDER) %>%
  summarise(POPN = sum(POPN), .groups = 'keep') %>%
  ungroup() %>%
  transmute(LEVEL = 0, AREA_CODE = 'E92000001', AREA_NAME = 'ENGLAND', AGE , GENDER, POPN) %>% 
  bind_rows(
    df_ts009_ltla_syoa %>% 
      group_by(RGN22CD, RGN22NM, AGE, GENDER) %>%
      summarise(POPN = sum(POPN), .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 1, AREA_CODE = RGN22CD, AREA_NAME = RGN22NM, AGE , GENDER, POPN)
  ) %>%
  bind_rows(
    df_ts009_ltla_syoa %>% 
      inner_join(df_swahsn_lu, by = c('LAD22CD' = 'LAD22CD', 'LAD22NM' = 'LAD22NM')) %>%
      group_by(ICB22CD, ICB22NM, AGE, GENDER) %>%
      summarise(POPN = sum(POPN), .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 2, AREA_CODE = ICB22CD, AREA_NAME = ICB22NM, AGE , GENDER, POPN)
  ) %>%
  bind_rows(
    df_ts009_ltla_syoa %>% 
      semi_join(df_swahsn_lu, by = c('LAD22CD' = 'LAD22CD', 'LAD22NM' = 'LAD22NM')) %>%
      group_by(LAD22CD, LAD22NM, AGE, GENDER) %>%
      summarise(POPN = sum(POPN), .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 3, AREA_CODE = LAD22CD, AREA_NAME = LAD22NM, AGE , GENDER, POPN)
  )

df_ts009_ltla_syoa_pyramid_data <- df_ts009_ltla_syoa_pyramid_data %>% 
  inner_join(
    df_ts009_ltla_syoa_pyramid_data %>% 
      group_by(LEVEL, AREA_CODE, AREA_NAME) %>% 
      summarise(TOTAL_POPN = sum(POPN), .groups = 'keep') %>% 
      ungroup(),
    by = c('LEVEL' = 'LEVEL', 'AREA_CODE' = 'AREA_CODE', 'AREA_NAME' = 'AREA_NAME')
  ) %>% 
  mutate(PCT_POPN = POPN / TOTAL_POPN)

df_ts009_ltla_syoa_summary <- df_ts009_ltla_syoa_pyramid_data %>% 
  group_by(LEVEL, AREA_CODE, AREA_NAME, AGE) %>% 
  summarise(POPN = sum(POPN), .groups = 'keep') %>%
  ungroup() %>% 
  group_by(LEVEL, AREA_CODE, AREA_NAME) %>%
  summarise(VALUE = c(quantile(rep(AGE, POPN), probs = c(0.05, 0.25, 0.5, 0.75, 0.95), names = TRUE), POPN = sum(POPN)), .groups = 'keep') %>%
  mutate(METRIC = names(VALUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = 'METRIC', values_from = 'VALUE') %>%
  rename_with(.fn = function(x){c('LEVEL', 'AREA_CODE', 'AREA_NAME', 'C05', 'Q1', 'MED', 'Q3', 'C95', 'POPN')}) %>%
  mutate(IQR = (Q3 - Q1))

plt_ts009_boxplot <- ggplot(
  df_ts009_ltla_syoa_summary %>% 
    arrange(LEVEL, desc(MED)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(title = str_wrap('Boxplot of 5th Centile, 1st Quartile, Median, 3rd Quartile, and 95th Centile Age of Population for England, Regions, Local Integrated Care Boards (ICB) and Local Authority Districts (LAD)', 80),
       x = 'Area Name', y = 'Median Age') %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:3), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a', '3' = '#984ea3'), 
    labels = c('England','Region','Local ICB','Local LAD')) %+%
  geom_boxplot(aes(x = AREA_NAME, ymin = C05, ymax = C95, lower = Q1, upper = Q3, middle = MED, fill = as.factor(LEVEL), group = AREA_NAME), stat = 'identity') 
plt_ts009_boxplot
ggsave('.\\outputs\\02_population_age\\plt_ts009_boxplot_chart.png', plt_ts009_boxplot, height = 210, width = 297, units = 'mm')
write.csv(df_ts009_ltla_syoa_summary, '.\\outputs\\02_population_age\\ts009_summary.csv', row.names = FALSE)

########################
### Work in progress ###
########################

selected_fields <- c('MSOA21CD','MSOA21NM','ICB22CD','ICB22CDH','ICB22NM',paste0('PERSONS_AGED_0', c(0:9)),paste0('PERSONS_AGED_', c(10:99)), 'PERSONS_AGED_100_PLUS')  
df_ts007_msoa <- read.csv(unzip('.\\data\\01_demography_and_migration\\census2021-ts007.zip', 'census2021-ts007-msoa.csv')) %>% 
  rename_with(.fn = function(x){c('YEAR','MSOA21NM','MSOA21CD',
                                  'PERSONS_TOTAL', 'PERSONS_AGED_00_04', paste0('PERSONS_AGED_', str_sub(paste0('0', seq(0,4,1)), -2)),
                                  'PERSONS_AGED_05_09', paste0('PERSONS_AGED_', str_sub(paste0('0', seq(5,9,1)), -2)), 'PERSONS_AGED_10_15', paste0('PERSONS_AGED_', seq(10,15,1)), 
                                  'PERSONS_AGED_16_19', paste0('PERSONS_AGED_', seq(16,19,1)), 'PERSONS_AGED_20_24', paste0('PERSONS_AGED_', seq(20,24,1)), 
                                  'PERSONS_AGED_25_34', paste0('PERSONS_AGED_', seq(25,34,1)), 'PERSONS_AGED_35_49', paste0('PERSONS_AGED_', seq(35,49,1)),
                                  'PERSONS_AGED_50_64', paste0('PERSONS_AGED_', seq(50,64,1)), 'PERSONS_AGED_65_74', paste0('PERSONS_AGED_', seq(65,74,1)),
                                  'PERSONS_AGED_75_84', paste0('PERSONS_AGED_', seq(75,84,1)), 'PERSONS_AGED_85_PLUS', paste0('PERSONS_AGED_', seq(85,99,1)), 
                                  'PERSONS_AGED_100_PLUS')}) %>%
  left_join(df_oa21_lsoa21_msoa21_icb22 %>% distinct(msoa21cd, ICB22CD, ICB22CDH, ICB22NM), by = c('MSOA21CD' = 'msoa21cd')) %>%
  select(!!!quos(selected_fields)) %>% 
  pivot_longer(cols = contains('_AGED_'), names_to = 'AGE', values_to = 'POPN') %>%
  mutate(AGE = as.integer(gsub('PERSONS_AGED_', '', gsub('_PLUS', '', AGE))))


df_ts007_msoa_summary <- df_ts007_msoa %>%
  group_by(MSOA21CD, MSOA21NM, ICB22CD, ICB22CDH, ICB22NM) %>%
  summarise(VALUE = c(quantile(rep(AGE, POPN), probs = c(0.05, 0.25, 0.5, 0.75, 0.95), names = TRUE), POPN = sum(POPN)), .groups = 'keep') %>%
  mutate(
    METRIC = names(VALUE),
    VALUE = unname(VALUE)) %>%
  ungroup() %>%
  pivot_wider(names_from = 'METRIC', values_from = 'VALUE') %>%
  rename_with(.fn = function(x){c('MSOA21CD', 'MSOA21NM', 'ICB22CD', 'ICB22CDH', 'ICB22NM', 'C05', 'Q1', 'MED', 'Q3', 'C95', 'POPN')}) %>%
  mutate(IQR = (Q3 - Q1)) %>%
  mutate(
    MED_DECILE = ntile(desc(MED),10),
    IQR_DECILE = ntile(desc(IQR),10)
  )
########################################### TO DO ################################

ggplot(
  df_ts007_msoa_summary %>% 
    filter(!is.na(ICB22CDH)) %>% 
    group_by(ICB22CDH, ICB22NM, MED_DECILE) %>% 
    summarise(COUNT = n()) %>% 
    mutate(PCT = COUNT / sum(COUNT)) %>%
    left_join(
      df_ts007_msoa %>%
        group_by(ICB22CD, ICB22CDH, ICB22NM) %>%
        summarise(VALUE = c(quantile(rep(AGE, POPN), probs = c(0.05, 0.25, 0.5, 0.75, 0.95), names = TRUE), POPN = sum(POPN)), .groups = 'keep') %>%
        mutate(
          METRIC = names(VALUE),
          VALUE = unname(VALUE)) %>%
        ungroup() %>%
        pivot_wider(names_from = 'METRIC', values_from = 'VALUE') %>%
        rename_with(.fn = function(x){c('ICB22CD', 'ICB22CDH', 'ICB22NM', 'C05', 'Q1', 'MED', 'Q3', 'C95', 'POPN')}) %>%
        mutate(IQR = (Q3 - Q1))
    ) %>%
    arrange(desc(MED), ICB22CDH) %>%
    mutate(ICB22NM = factor(ICB22NM, levels = ICB22NM))
  ) %+%
  geom_bar(aes(x = as.factor(MED_DECILE), y = PCT, group = ICB22CDH), stat = 'identity') %+%
  facet_wrap(. ~ str_wrap(ICB22NM, 40))

df_ts007_msoa_summary %>% filter(is.na(ICB22CDH))
df_ts007_msoa_summary %>% group_by(ICB22CDH, ICB22NM, MED_DECILE) %>% summarise(count = n()) %>% mutate(pct = count / sum(count))

map_ts007_median_age_decile <- ggplot(sf_msoa21 %>% inner_join(df_ts007_msoa_summary, by = c('MSOA21CD' = 'MSOA21CD', 'MSOA21NM' = 'MSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Decile of Median Age (1 - Oldest) for England by Middle-layer Super Output Areas (MSOA) 2021', 80),
    fill = 'Median Age Decile',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = as.factor(MED_DECILE)),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_brewer(type = 'div', palette = 'RdYlGn')
map_ts007_median_age_decile
#ggsave('.\\outputs\\03_population_deprivation\\map_ts011_not_deprived_dark.png', map_ts011_not_deprived, height = 297, width = 210, units = 'mm')
#map_ts011_not_deprived <- map_ts011_not_deprived %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 0.1))
#ggsave('.\\outputs\\03_population_deprivation\\map_ts011_not_deprived_light.png', map_ts011_not_deprived, height = 297, width = 210, units = 'mm')

