library(tidyverse)
library(plotly)
library(sf)
library(rgdal)
library(scales)
library(readxl)

# Load and process references
# ===========================

tidy_up <- TRUE

df_lad22_rgn22_lu <- read.csv('.\\data\\00_reference\\Local_Authority_District_to_Region_(December_2022)_Lookup_in_England.csv') %>%
  rename_with(.fn = function(x){c('LAD22CD','LAD22NM','RGN22CD','RGN22NM','OBJECTID')})

sf_lad22 <- st_read(dsn = 'D:\\Data\\OpenGeography\\Shapefiles\\LAD22', layer = 'lad22') %>%
  st_transform(crs = 4326)

sf_lsoa21 <- st_read(dsn = 'D:\\Data\\OpenGeography\\Shapefiles\\LSOA21', layer = 'lsoa21') %>%
  st_transform(crs = 4326)

sf_msoa21 <- st_read(dsn = 'D:\\Data\\OpenGeography\\Shapefiles\\MSOA21', layer = 'msoa21') %>%
  st_transform(crs = 4326)

df_oa21_lsoa21_msoa21_icb22 <- read.csv('D:\\Data\\OpenGeography\\Lookups\\OA21_LSOA21_MSOA21_ICB22_LAD22\\OA21_LSOA21_MSOA21_ICB22_LAD22_EW_LU.csv')

# South West AHSN Local Authority Districts
# =========================================
cornwall_lad <- c('Cornwall','Isles of Scilly')
devon_lad <- c('Plymouth','Torbay','East Devon','Exeter','Mid Devon','North Devon','South Hams','Teignbridge','Torridge','West Devon')
somerset_lad <- c('Mendip','Sedgemoor','South Somerset','Somerset West and Taunton')
df_swahsn_lu <- sf_lad22 %>% st_drop_geometry() %>% select(LAD22CD, LAD22NM) %>% filter(LAD22NM %in% c(cornwall_lad, devon_lad, somerset_lad))
df_swahsn_lu$ICB22CD <- 'E54000037'
df_swahsn_lu$ICB22NM[df_swahsn_lu$LAD22NM %in% cornwall_lad] <- 'E54000036'
df_swahsn_lu$ICB22NM[df_swahsn_lu$LAD22NM %in% somerset_lad] <- 'E54000038'
df_swahsn_lu$ICB22NM <- 'NHS Devon ICB'
df_swahsn_lu$ICB22NM[df_swahsn_lu$LAD22NM %in% cornwall_lad] <- 'NHS Cornwall and IoS ICB'
df_swahsn_lu$ICB22NM[df_swahsn_lu$LAD22NM %in% somerset_lad] <- 'NHS Somerset ICB'

if(tidy_up){
  object_list <- c(ls(), 'df_ts009_ltla', 'object_list')
}

# ======================== #
# Demography and migration #
# ======================== #

# Population age
# ==============
source('population_age.R', local = TRUE)
if(tidy_up){
  rm(list = ls()[!(ls() %in% object_list)])
}


# Population density
# ==================
source('population_density.R', local = TRUE)
if(tidy_up){
  rm(list = ls()[!(ls() %in% object_list)])
}

# Population deprivation
# ======================
source('population_deprivation.R', local = TRUE)
if(tidy_up){
  rm(list = ls()[!(ls() %in% object_list)])
}

# ========================================== #
# Ethnicity, identity, language and religion #
# ========================================== #

# Ethnicity
# =========
source('ethnicity.R', local = TRUE)
if(tidy_up){
  rm(list = ls()[!(ls() %in% object_list)])
}

# ====== #
# Health #
# ====== #

# General Health
# ==============
source('general_health.R', local = TRUE)
if(tidy_up){
  rm(list = ls()[!(ls() %in% object_list)])
}

# Disability
# ==========
source('disability.R', local = TRUE)
if(tidy_up){
  rm(list = ls()[!(ls() %in% object_list)])
}

# Unpaid Care
# ===========
source('unpaid_care.R', local = TRUE)
if(tidy_up){
  rm(list = ls()[!(ls() %in% object_list)])
}

# ========= #
# Education #
# ========= #

# Education
# =========
source('education.R', local = TRUE)
if(tidy_up){
  rm(list = ls()[!(ls() %in% object_list)])
}

# ======= #
# Housing #
# ======= #

# Car ownership
# =============
source('car_ownership.R', local = TRUE)
if(tidy_up){
  rm(list = ls()[!(ls() %in% object_list)])
}

# Central heating
# ===============
source('central_heating.R', local = TRUE)
if(tidy_up){
  rm(list = ls()[!(ls() %in% object_list)])
}

# Tenure
# ======
source('tenure.R', local = TRUE)
if(tidy_up){
  rm(list = ls()[!(ls() %in% object_list)])
}

# =============== #
# Work and travel #
# =============== #

# Distance to work
# ================
source('distance_to_work.R', local = TRUE)
if(tidy_up){
  rm(list = ls()[!(ls() %in% object_list)])
}

# Industry employment
# ===================
source('industry_employment.R', local = TRUE)
if(tidy_up){
  rm(list = ls()[!(ls() %in% object_list)])
}

# Economic activity status
# ========================
# TS066 - Economic activity status data
source('economic_activity_status.R', local = TRUE)
if(tidy_up){
  rm(list = ls()[!(ls() %in% object_list)])
}
