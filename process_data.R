library(tidyverse)
library(plotly)
library(sf)
library(rgdal)
library(scales)
library(readxl)

# Load and process references
# ===========================

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

# ======================== #
# Demography and migration #
# ======================== #

# Population age
# ==============
source('population_age.R', local = TRUE)


# Population density
# ==================
source('population_density.R', local = TRUE)

# Population deprivation
# ======================
source('population_deprivation.R', local = TRUE)

# Ethnicity
# =========
source('ethnicity.R', local = TRUE)

# General Health
# ==============
source('general_health.R', local = TRUE)

# Disability
# ==========
