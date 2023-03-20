# Industry employment
# ===================

dir.create(path = '.\\outputs\\13_industry_employment', showWarnings = FALSE, recursive = TRUE)

# Load the TS060 - Industry employment data
df_ts060_msoa <- read.csv(unzip('.\\data\\06_work_and_travel\\census2021-ts060.zip', 'census2021-ts060-msoa.csv')) %>%
  select(1, 2, 3, 4, 5, 9, 15, 40, 42, 47, 49, 54, 60, 63, 70, 74, 76, 84, 91, 93, 95, 99) %>%
  rename_with(.fn = function(x){c('YEAR','MSOA21NM','MSOA21CD',
                                  'EMPLOYED_POPN', 
                                  'A_AGRICULTURE_FORESTRY_FISHING','B_MINING_QUARRYING',
                                  'C_MANUFACTURING','D_ELECTRICITY_GAS',
                                  'D_WATER_WASTE','F_CONSTRUCTION',
                                  'G_WHOLESALE_CAR_REPAIR','H_TRANSPORT_STORAGE',
                                  'I_ACCOMMODATION_FOOD','J_INFORMATION_COMMUNICATION',
                                  'K_FINANCIAL','L_REAL_ESTATE',
                                  'M_PROFESSIONAL_SCIENTIFIC','N_ADMIN_SUPPORT',
                                  'O_PUBLIC_ADMIN_DEFENCE','P_EDUCATION',
                                  'Q_HEALTH_AND_CARE','RSTU_OTHER')}) %>%
  mutate(
    PCT_A_AGRICULTURE_FORESTRY_FISHING = A_AGRICULTURE_FORESTRY_FISHING / EMPLOYED_POPN,
    PCT_B_MINING_QUARRYING = B_MINING_QUARRYING / EMPLOYED_POPN,
    PCT_C_MANUFACTURING = C_MANUFACTURING / EMPLOYED_POPN,
    PCT_D_ELECTRICITY_GAS = D_ELECTRICITY_GAS / EMPLOYED_POPN,
    PCT_D_WATER_WASTE = D_WATER_WASTE / EMPLOYED_POPN,
    PCT_F_CONSTRUCTION = F_CONSTRUCTION / EMPLOYED_POPN,
    PCT_G_WHOLESALE_CAR_REPAIR = G_WHOLESALE_CAR_REPAIR / EMPLOYED_POPN,
    PCT_H_TRANSPORT_STORAGE = H_TRANSPORT_STORAGE / EMPLOYED_POPN,
    PCT_I_ACCOMMODATION_FOOD = I_ACCOMMODATION_FOOD / EMPLOYED_POPN,
    PCT_J_INFORMATION_COMMUNICATION = J_INFORMATION_COMMUNICATION / EMPLOYED_POPN,
    PCT_K_FINANCIAL = K_FINANCIAL / EMPLOYED_POPN,
    PCT_L_REAL_ESTATE = L_REAL_ESTATE / EMPLOYED_POPN,
    PCT_M_PROFESSIONAL_SCIENTIFIC = M_PROFESSIONAL_SCIENTIFIC / EMPLOYED_POPN,
    PCT_N_ADMIN_SUPPORT = N_ADMIN_SUPPORT / EMPLOYED_POPN,
    PCT_O_PUBLIC_ADMIN_DEFENCE = O_PUBLIC_ADMIN_DEFENCE / EMPLOYED_POPN,
    PCT_P_EDUCATION = P_EDUCATION / EMPLOYED_POPN,
    PCT_Q_HEALTH_AND_CARE = Q_HEALTH_AND_CARE / EMPLOYED_POPN,
    PCT_RSTU_OTHER = RSTU_OTHER / EMPLOYED_POPN
    )

df_ts060_ltla <- read.csv(unzip('.\\data\\06_work_and_travel\\census2021-ts060.zip', 'census2021-ts060-ltla.csv')) %>%
  select(1, 2, 3, 4, 5, 9, 15, 40, 42, 47, 49, 54, 60, 63, 70, 74, 76, 84, 91, 93, 95, 99) %>%
  rename_with(.fn = function(x){c('YEAR','LAD22NM','LAD22CD',
                                  'EMPLOYED_POPN', 
                                  'A_AGRICULTURE_FORESTRY_FISHING','B_MINING_QUARRYING',
                                  'C_MANUFACTURING','D_ELECTRICITY_GAS',
                                  'D_WATER_WASTE','F_CONSTRUCTION',
                                  'G_WHOLESALE_CAR_REPAIR','H_TRANSPORT_STORAGE',
                                  'I_ACCOMMODATION_FOOD','J_INFORMATION_COMMUNICATION',
                                  'K_FINANCIAL','L_REAL_ESTATE',
                                  'M_PROFESSIONAL_SCIENTIFIC','N_ADMIN_SUPPORT',
                                  'O_PUBLIC_ADMIN_DEFENCE','P_EDUCATION',
                                  'Q_HEALTH_AND_CARE','RSTU_OTHER')}) %>%
  inner_join(df_lad22_rgn22_lu %>% select(LAD22CD, RGN22CD, RGN22NM), by = c('LAD22CD' = 'LAD22CD')) %>%
  transmute(YEAR, 
            RGN22CD, RGN22NM, 
            LAD22CD, LAD22NM, 
            EMPLOYED_POPN, 
            A_AGRICULTURE_FORESTRY_FISHING, B_MINING_QUARRYING,
            C_MANUFACTURING, D_ELECTRICITY_GAS,
            D_WATER_WASTE, F_CONSTRUCTION,
            G_WHOLESALE_CAR_REPAIR, H_TRANSPORT_STORAGE,
            I_ACCOMMODATION_FOOD, J_INFORMATION_COMMUNICATION,
            K_FINANCIAL, L_REAL_ESTATE,
            M_PROFESSIONAL_SCIENTIFIC, N_ADMIN_SUPPORT,
            O_PUBLIC_ADMIN_DEFENCE, P_EDUCATION,
            Q_HEALTH_AND_CARE, RSTU_OTHER)
  
df_ts060_ltla_summary <- df_ts060_ltla %>% 
  summarise(
    EMPLOYED_POPN = sum(EMPLOYED_POPN), 
    A_AGRICULTURE_FORESTRY_FISHING = sum(A_AGRICULTURE_FORESTRY_FISHING), B_MINING_QUARRYING = sum(B_MINING_QUARRYING),
    C_MANUFACTURING = sum(C_MANUFACTURING), D_ELECTRICITY_GAS = sum(D_ELECTRICITY_GAS),
    D_WATER_WASTE = sum(D_WATER_WASTE), F_CONSTRUCTION = sum(F_CONSTRUCTION),
    G_WHOLESALE_CAR_REPAIR = sum(G_WHOLESALE_CAR_REPAIR), H_TRANSPORT_STORAGE = sum(H_TRANSPORT_STORAGE),
    I_ACCOMMODATION_FOOD = sum(I_ACCOMMODATION_FOOD), J_INFORMATION_COMMUNICATION = sum(J_INFORMATION_COMMUNICATION),
    K_FINANCIAL = sum(K_FINANCIAL), L_REAL_ESTATE = sum(L_REAL_ESTATE),
    M_PROFESSIONAL_SCIENTIFIC = sum(M_PROFESSIONAL_SCIENTIFIC), N_ADMIN_SUPPORT = sum(N_ADMIN_SUPPORT),
    O_PUBLIC_ADMIN_DEFENCE = sum(O_PUBLIC_ADMIN_DEFENCE), P_EDUCATION = sum(P_EDUCATION),
    Q_HEALTH_AND_CARE = sum(Q_HEALTH_AND_CARE), RSTU_OTHER = sum(RSTU_OTHER),
    .groups = 'keep') %>%
  transmute(LEVEL = 0, AREA_CODE = 'E92000001', AREA_NAME = 'ENGLAND', 
            EMPLOYED_POPN, 
            A_AGRICULTURE_FORESTRY_FISHING, B_MINING_QUARRYING,
            C_MANUFACTURING, D_ELECTRICITY_GAS,
            D_WATER_WASTE, F_CONSTRUCTION,
            G_WHOLESALE_CAR_REPAIR, H_TRANSPORT_STORAGE,
            I_ACCOMMODATION_FOOD, J_INFORMATION_COMMUNICATION,
            K_FINANCIAL, L_REAL_ESTATE,
            M_PROFESSIONAL_SCIENTIFIC, N_ADMIN_SUPPORT,
            O_PUBLIC_ADMIN_DEFENCE, P_EDUCATION,
            Q_HEALTH_AND_CARE, RSTU_OTHER
  ) %>% 
  bind_rows(
    df_ts060_ltla %>% 
      group_by(RGN22CD, RGN22NM) %>% 
      summarise(
        EMPLOYED_POPN = sum(EMPLOYED_POPN), 
        A_AGRICULTURE_FORESTRY_FISHING = sum(A_AGRICULTURE_FORESTRY_FISHING), B_MINING_QUARRYING = sum(B_MINING_QUARRYING),
        C_MANUFACTURING = sum(C_MANUFACTURING), D_ELECTRICITY_GAS = sum(D_ELECTRICITY_GAS),
        D_WATER_WASTE = sum(D_WATER_WASTE), F_CONSTRUCTION = sum(F_CONSTRUCTION),
        G_WHOLESALE_CAR_REPAIR = sum(G_WHOLESALE_CAR_REPAIR), H_TRANSPORT_STORAGE = sum(H_TRANSPORT_STORAGE),
        I_ACCOMMODATION_FOOD = sum(I_ACCOMMODATION_FOOD), J_INFORMATION_COMMUNICATION = sum(J_INFORMATION_COMMUNICATION),
        K_FINANCIAL = sum(K_FINANCIAL), L_REAL_ESTATE = sum(L_REAL_ESTATE),
        M_PROFESSIONAL_SCIENTIFIC = sum(M_PROFESSIONAL_SCIENTIFIC), N_ADMIN_SUPPORT = sum(N_ADMIN_SUPPORT),
        O_PUBLIC_ADMIN_DEFENCE = sum(O_PUBLIC_ADMIN_DEFENCE), P_EDUCATION = sum(P_EDUCATION),
        Q_HEALTH_AND_CARE = sum(Q_HEALTH_AND_CARE), RSTU_OTHER = sum(RSTU_OTHER),
        .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 1, AREA_CODE = RGN22CD, AREA_NAME = RGN22NM, 
                EMPLOYED_POPN, 
                A_AGRICULTURE_FORESTRY_FISHING, B_MINING_QUARRYING,
                C_MANUFACTURING, D_ELECTRICITY_GAS,
                D_WATER_WASTE, F_CONSTRUCTION,
                G_WHOLESALE_CAR_REPAIR, H_TRANSPORT_STORAGE,
                I_ACCOMMODATION_FOOD, J_INFORMATION_COMMUNICATION,
                K_FINANCIAL, L_REAL_ESTATE,
                M_PROFESSIONAL_SCIENTIFIC, N_ADMIN_SUPPORT,
                O_PUBLIC_ADMIN_DEFENCE, P_EDUCATION,
                Q_HEALTH_AND_CARE, RSTU_OTHER)
  ) %>% 
  bind_rows(
    df_ts060_ltla %>% 
      group_by(LAD22CD, LAD22NM) %>% 
      summarise(
        EMPLOYED_POPN = sum(EMPLOYED_POPN), 
        A_AGRICULTURE_FORESTRY_FISHING = sum(A_AGRICULTURE_FORESTRY_FISHING), B_MINING_QUARRYING = sum(B_MINING_QUARRYING),
        C_MANUFACTURING = sum(C_MANUFACTURING), D_ELECTRICITY_GAS = sum(D_ELECTRICITY_GAS),
        D_WATER_WASTE = sum(D_WATER_WASTE), F_CONSTRUCTION = sum(F_CONSTRUCTION),
        G_WHOLESALE_CAR_REPAIR = sum(G_WHOLESALE_CAR_REPAIR), H_TRANSPORT_STORAGE = sum(H_TRANSPORT_STORAGE),
        I_ACCOMMODATION_FOOD = sum(I_ACCOMMODATION_FOOD), J_INFORMATION_COMMUNICATION = sum(J_INFORMATION_COMMUNICATION),
        K_FINANCIAL = sum(K_FINANCIAL), L_REAL_ESTATE = sum(L_REAL_ESTATE),
        M_PROFESSIONAL_SCIENTIFIC = sum(M_PROFESSIONAL_SCIENTIFIC), N_ADMIN_SUPPORT = sum(N_ADMIN_SUPPORT),
        O_PUBLIC_ADMIN_DEFENCE = sum(O_PUBLIC_ADMIN_DEFENCE), P_EDUCATION = sum(P_EDUCATION),
        Q_HEALTH_AND_CARE = sum(Q_HEALTH_AND_CARE), RSTU_OTHER = sum(RSTU_OTHER),
        .groups = 'keep') %>%
      ungroup() %>%
      transmute(LEVEL = 2, AREA_CODE = LAD22CD, AREA_NAME = LAD22NM, 
                EMPLOYED_POPN, 
                A_AGRICULTURE_FORESTRY_FISHING, B_MINING_QUARRYING,
                C_MANUFACTURING, D_ELECTRICITY_GAS,
                D_WATER_WASTE, F_CONSTRUCTION,
                G_WHOLESALE_CAR_REPAIR, H_TRANSPORT_STORAGE,
                I_ACCOMMODATION_FOOD, J_INFORMATION_COMMUNICATION,
                K_FINANCIAL, L_REAL_ESTATE,
                M_PROFESSIONAL_SCIENTIFIC, N_ADMIN_SUPPORT,
                O_PUBLIC_ADMIN_DEFENCE, P_EDUCATION,
                Q_HEALTH_AND_CARE, RSTU_OTHER)
  ) %>% 
  mutate(
    PCT_A_AGRICULTURE_FORESTRY_FISHING = A_AGRICULTURE_FORESTRY_FISHING / EMPLOYED_POPN,  
    PCT_B_MINING_QUARRYING = B_MINING_QUARRYING / EMPLOYED_POPN,
    PCT_C_MANUFACTURING = C_MANUFACTURING / EMPLOYED_POPN,
    PCT_D_ELECTRICITY_GAS = D_ELECTRICITY_GAS / EMPLOYED_POPN,
    PCT_D_WATER_WASTE = D_WATER_WASTE / EMPLOYED_POPN,
    PCT_F_CONSTRUCTION = F_CONSTRUCTION / EMPLOYED_POPN,
    PCT_G_WHOLESALE_CAR_REPAIR = G_WHOLESALE_CAR_REPAIR / EMPLOYED_POPN,
    PCT_H_TRANSPORT_STORAGE = H_TRANSPORT_STORAGE / EMPLOYED_POPN,
    PCT_I_ACCOMMODATION_FOOD = I_ACCOMMODATION_FOOD / EMPLOYED_POPN,
    PCT_J_INFORMATION_COMMUNICATION = J_INFORMATION_COMMUNICATION / EMPLOYED_POPN,
    PCT_K_FINANCIAL = K_FINANCIAL / EMPLOYED_POPN,
    PCT_L_REAL_ESTATE = L_REAL_ESTATE / EMPLOYED_POPN,
    PCT_M_PROFESSIONAL_SCIENTIFIC = M_PROFESSIONAL_SCIENTIFIC / EMPLOYED_POPN,
    PCT_N_ADMIN_SUPPORT = N_ADMIN_SUPPORT / EMPLOYED_POPN,
    PCT_O_PUBLIC_ADMIN_DEFENCE = O_PUBLIC_ADMIN_DEFENCE / EMPLOYED_POPN,
    PCT_P_EDUCATION = P_EDUCATION / EMPLOYED_POPN,
    PCT_Q_HEALTH_AND_CARE = Q_HEALTH_AND_CARE / EMPLOYED_POPN,
    PCT_RSTU_OTHER = RSTU_OTHER / EMPLOYED_POPN
  ) %>% 
  group_by(LEVEL) %>%
  mutate(
    RANK_A_AGRICULTURE_FORESTRY_FISHING = rank(desc(PCT_A_AGRICULTURE_FORESTRY_FISHING ), ties.method = 'first'),	
    RANK_A_AGRICULTURE_FORESTRY_FISHING = sprintf('(%d of %d)', RANK_A_AGRICULTURE_FORESTRY_FISHING , n()),
    RANK_B_MINING_QUARRYING = rank(desc(PCT_B_MINING_QUARRYING ), ties.method = 'first'),	
    RANK_B_MINING_QUARRYING = sprintf('(%d of %d)', RANK_B_MINING_QUARRYING , n()),
    RANK_C_MANUFACTURING = rank(desc(PCT_C_MANUFACTURING ), ties.method = 'first'),	
    RANK_C_MANUFACTURING = sprintf('(%d of %d)', RANK_C_MANUFACTURING , n()),
    RANK_D_ELECTRICITY_GAS = rank(desc(PCT_D_ELECTRICITY_GAS ), ties.method = 'first'),	
    RANK_D_ELECTRICITY_GAS = sprintf('(%d of %d)', RANK_D_ELECTRICITY_GAS , n()),
    RANK_D_WATER_WASTE = rank(desc(PCT_D_WATER_WASTE ), ties.method = 'first'),	
    RANK_D_WATER_WASTE = sprintf('(%d of %d)', RANK_D_WATER_WASTE , n()),
    RANK_F_CONSTRUCTION = rank(desc(PCT_F_CONSTRUCTION ), ties.method = 'first'),	
    RANK_F_CONSTRUCTION = sprintf('(%d of %d)', RANK_F_CONSTRUCTION , n()),
    RANK_G_WHOLESALE_CAR_REPAIR = rank(desc(PCT_G_WHOLESALE_CAR_REPAIR ), ties.method = 'first'),	
    RANK_G_WHOLESALE_CAR_REPAIR = sprintf('(%d of %d)', RANK_G_WHOLESALE_CAR_REPAIR , n()),
    RANK_H_TRANSPORT_STORAGE = rank(desc(PCT_H_TRANSPORT_STORAGE ), ties.method = 'first'),	
    RANK_H_TRANSPORT_STORAGE = sprintf('(%d of %d)', RANK_H_TRANSPORT_STORAGE , n()),
    RANK_I_ACCOMMODATION_FOOD = rank(desc(PCT_I_ACCOMMODATION_FOOD ), ties.method = 'first'),	
    RANK_I_ACCOMMODATION_FOOD = sprintf('(%d of %d)', RANK_I_ACCOMMODATION_FOOD , n()),
    RANK_J_INFORMATION_COMMUNICATION = rank(desc(PCT_J_INFORMATION_COMMUNICATION ), ties.method = 'first'),	RANK_J_INFORMATION_COMMUNICATION = sprintf('(%d of %d)', RANK_J_INFORMATION_COMMUNICATION , n()),
    RANK_K_FINANCIAL = rank(desc(PCT_K_FINANCIAL ), ties.method = 'first'),	
    RANK_K_FINANCIAL = sprintf('(%d of %d)', RANK_K_FINANCIAL , n()),
    RANK_L_REAL_ESTATE = rank(desc(PCT_L_REAL_ESTATE ), ties.method = 'first'),	
    RANK_L_REAL_ESTATE = sprintf('(%d of %d)', RANK_L_REAL_ESTATE , n()),
    RANK_M_PROFESSIONAL_SCIENTIFIC = rank(desc(PCT_M_PROFESSIONAL_SCIENTIFIC ), ties.method = 'first'),	RANK_M_PROFESSIONAL_SCIENTIFIC = sprintf('(%d of %d)', RANK_M_PROFESSIONAL_SCIENTIFIC , n()),
    RANK_N_ADMIN_SUPPORT = rank(desc(PCT_N_ADMIN_SUPPORT ), ties.method = 'first'),	
    RANK_N_ADMIN_SUPPORT = sprintf('(%d of %d)', RANK_N_ADMIN_SUPPORT , n()),
    RANK_O_PUBLIC_ADMIN_DEFENCE = rank(desc(PCT_O_PUBLIC_ADMIN_DEFENCE ), ties.method = 'first'),	
    RANK_O_PUBLIC_ADMIN_DEFENCE = sprintf('(%d of %d)', RANK_O_PUBLIC_ADMIN_DEFENCE , n()),
    RANK_P_EDUCATION = rank(desc(PCT_P_EDUCATION ), ties.method = 'first'),	
    RANK_P_EDUCATION = sprintf('(%d of %d)', RANK_P_EDUCATION , n()),
    RANK_Q_HEALTH_AND_CARE = rank(desc(PCT_Q_HEALTH_AND_CARE ), ties.method = 'first'),	
    RANK_Q_HEALTH_AND_CARE = sprintf('(%d of %d)', RANK_Q_HEALTH_AND_CARE , n()),
    RANK_RSTU_OTHER = rank(desc(PCT_RSTU_OTHER ), ties.method = 'first'),	
    RANK_RSTU_OTHER = sprintf('(%d of %d)', RANK_RSTU_OTHER , n())
  ) %>%
  ungroup() %>%
  filter(LEVEL < 2 | AREA_CODE %in% df_swahsn_lu$LAD22CD)

########################## TODO ###################################
plt_ts058_home_worker <- ggplot(
  df_ts058_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_HOME_WORKER)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
  ) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Employed Population Working from Home for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Employed Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.4,0.1)) %+%
  #scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_HOME_WORKER, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts058_home_worker
ggsave('.\\outputs\\12_distance_to_work\\plt_ts058_home_worker.png', plt_ts058_home_worker, height = 210, width = 297, units = 'mm')

plt_ts058_lt_5km <- ggplot(
  df_ts058_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_DIST_LT_2KM + PCT_DIST_BETWEEN_2KM_AND_5KM)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
  ) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Employed Population Working Within 5km of Home for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Employed Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.4,0.1)) %+%
  #scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_DIST_LT_2KM + PCT_DIST_BETWEEN_2KM_AND_5KM, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts058_lt_5km
ggsave('.\\outputs\\12_distance_to_work\\plt_ts058_lt_5km.png', plt_ts058_lt_5km, height = 210, width = 297, units = 'mm')

plt_ts058_between_5km_and_20km <- ggplot(
  df_ts058_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_DIST_BETWEEN_5KM_AND_10KM + PCT_DIST_BETWEEN_10KM_AND_20KM)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Employed Population Working Between 5km and 20km of Home for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Employed Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.3,0.05)) %+%
  #scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_DIST_BETWEEN_5KM_AND_10KM + PCT_DIST_BETWEEN_10KM_AND_20KM, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts058_between_5km_and_20km
ggsave('.\\outputs\\12_distance_to_work\\plt_ts058_between_5km_and_20km.png', plt_ts058_between_5km_and_20km, height = 210, width = 297, units = 'mm')

plt_ts058_gt_20km <- ggplot(
  df_ts058_ltla_summary %>% 
    arrange(LEVEL, desc(PCT_DIST_BETWEEN_20KM_AND_30KM + PCT_DIST_BETWEEN_30KM_AND_40KM + PCT_DIST_BETWEEN_40KM_AND_60KM + PCT_DIST_GE_60KM)) %>%
    mutate(AREA_NAME = factor(AREA_NAME, levels = AREA_NAME))
  ) %+%
  theme_bw(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, vjust = 0.5, angle = 90)
  ) %+%
  labs(
    title = str_wrap('Percentage of Employed Population Working more than 20km from Home for England, Regions, and Local Authority Districts (LAD)', 80),
    x = 'Area Name', y = '% of Employed Population'
  ) %+%
  scale_y_continuous(labels = percent_format(accuracy = 1), breaks = seq(0,0.25,0.05)) %+%
  #scale_y_continuous(labels = percent_format(accuracy = 1)) %+%
  scale_fill_manual(
    name = 'Level',
    breaks = c(0:2), 
    values = c('0' = '#e41a1c', '1' = '#377eb8', '2' = '#4daf4a'), 
    labels = c('England','Region','Local LAD')) %+%
  geom_bar(aes(x = AREA_NAME, y = PCT_DIST_BETWEEN_20KM_AND_30KM + PCT_DIST_BETWEEN_30KM_AND_40KM + PCT_DIST_BETWEEN_40KM_AND_60KM + PCT_DIST_GE_60KM, fill = as.factor(LEVEL), group = LEVEL), stat = 'identity') 
plt_ts058_gt_20km
ggsave('.\\outputs\\12_distance_to_work\\plt_ts058_gt_20km.png', plt_ts058_gt_20km, height = 210, width = 297, units = 'mm')

write.csv(df_ts060_ltla_summary, '.\\outputs\\13_industry_employment\\ts060_summary.csv', row.names = FALSE)

map_ts058_home_worker <- ggplot(sf_msoa21 %>% inner_join(df_ts058_msoa, by = c('MSOA21CD' = 'MSOA21CD', 'MSOA21NM' = 'MSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Employed Population Working from Home for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Employed Population',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_HOME_WORKER),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\12_distance_to_work\\map_ts058_home_worker_dark.png', map_ts058_home_worker, height = 297, width = 210, units = 'mm')
map_ts058_home_worker <- map_ts058_home_worker %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\12_distance_to_work\\map_ts058_home_worker_light.png', map_ts058_home_worker, height = 297, width = 210, units = 'mm')

map_ts058_lt_5km <- ggplot(sf_msoa21 %>% inner_join(df_ts058_msoa, by = c('MSOA21CD' = 'MSOA21CD', 'MSOA21NM' = 'MSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Employed Population Working Within 5km of Home for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Employed Population',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_DIST_LT_2KM + PCT_DIST_BETWEEN_2KM_AND_5KM),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\12_distance_to_work\\map_ts058_lt_5km_dark.png', map_ts058_lt_5km, height = 297, width = 210, units = 'mm')
map_ts058_lt_5km <- map_ts058_lt_5km %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\12_distance_to_work\\map_ts058_lt_5km_light.png', map_ts058_lt_5km, height = 297, width = 210, units = 'mm')

map_ts058_between_5km_and_20km <- ggplot(sf_msoa21 %>% inner_join(df_ts058_msoa, by = c('MSOA21CD' = 'MSOA21CD', 'MSOA21NM' = 'MSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Employed Population Working Between 5km and 20km of Home for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Employed Population',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_DIST_BETWEEN_5KM_AND_10KM + PCT_DIST_BETWEEN_10KM_AND_20KM),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\12_distance_to_work\\map_ts058_between_5km_and_20km_dark.png', map_ts058_between_5km_and_20km, height = 297, width = 210, units = 'mm')
map_ts058_between_5km_and_20km <- map_ts058_between_5km_and_20km %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\12_distance_to_work\\map_ts058_between_5km_and_20km_light.png', map_ts058_between_5km_and_20km, height = 297, width = 210, units = 'mm')

map_ts058_gt_20km <- ggplot(sf_msoa21 %>% inner_join(df_ts058_msoa, by = c('MSOA21CD' = 'MSOA21CD', 'MSOA21NM' = 'MSOA21NM'))) %+%
  theme_void(base_size = 12) %+%
  theme(
    plot.title = element_text(hjust = 0.5), 
    plot.caption = element_text(hjust = 1),
    legend.position = 'bottom',
    legend.key.width = unit(10, 'mm')
  ) %+%
  #  lims(x = c(bbox$xmin, bbox$xmax), y = c(bbox$ymin, bbox$ymax)) %+%
  labs(
    title = str_wrap('Map of Percentage of Employed Population Working Between 5km and 20km of Home for England and Wales by Lower-layer Super Output Areas (LSOA) 2021', 80),
    fill = '% of Employed Population',
    caption = 'Source: Office for National Statistics licensed under the Open Government Licence v.3.0\nContains OS data © Crown copyright and database right [2023]'
  ) %+%
  geom_sf(
    aes(fill = PCT_DIST_BETWEEN_20KM_AND_30KM + PCT_DIST_BETWEEN_30KM_AND_40KM + PCT_DIST_BETWEEN_40KM_AND_60KM + PCT_DIST_GE_60KM),
    color = NA,
    size = 0.5
  ) %+%
  scale_fill_viridis_c(direction = 1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\12_distance_to_work\\map_ts058_gt_20km_dark.png', map_ts058_gt_20km, height = 297, width = 210, units = 'mm')
map_ts058_gt_20km <- map_ts058_gt_20km %+% scale_fill_viridis_c(direction = -1, labels = percent_format(accuracy = 1))
ggsave('.\\outputs\\12_distance_to_work\\map_ts058_gt_20km_light.png', map_ts058_gt_20km, height = 297, width = 210, units = 'mm')