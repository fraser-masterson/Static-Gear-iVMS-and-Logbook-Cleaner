
require(sf)
require(readr)
require(dplyr)
require(knitr)
require(rmarkdown)
require(ggplot2)
require(raster)
require(concaveman)
require(tidyr)
require(ggspatial)
require(kableExtra)

#tinytex::install_tinytex() # Need to install this if it's the first time running this file on a device

# Columns required in Joined datasets -------------------------------------------

# uniqueID: RSS/VesselID concatenated with date (e.g., M055_01/04/2023)
# Vessel.Name: name of vessel (e.g., Whisky Galore)
# Departure.Date: date of departure in YYYY-mm-dd format (e.g., 2023-05-13)
# LE_KG_CRE: kg of crab landed during a trip
# LE_KG_LBE: kg of lobster landed during a trip
# LE_KG_WHE: kg of whelk landed during a trip
# Pot_No_CRE: number of crab pots hauled during a trip
# Pot_No_LBE: number of lobster pots hauled during a trip
# Pot_No_WHE: number of whelk pots hauled during a trip
# SI_Month: month of trip
# LE_YEAR: year of trip
# VE_REF: Vessel reference/ID (e.g., M055)
# SI_DATE: date of vms record in dd-mm-YYYY format
# SI_LONG: longitude of vms record
# SI_LATI: latitude of vms record

# -----------------------------------------------------------------------------

# Input parameters ------------------------------------------------------------
params = list(quarter = 'Custom', # Input quarter ('1', '2', '3', '4', 'Annual', 'All', or 'Custom')
              
              year = '2024', # Input year in 'YYYY' format (e.g., 2023, 2024, etc.); redundant if quarter = 'Custom' or 'All'
              DEFA = 'TRUE', # If report is for DEFA: TRUE, if public: FALSE
              species = 'Whelk', # 'Crab and lobster', or 'Whelk'
              
              # Only necessary if quarter = 'Custom'
              start_date = '01/01/2021', # 'dd/mm/YYYY'
              end_date = '01/01/2025') # 'dd/mm/YYYY'

reports_dir = "~/Documents/IoM 2025/Static Gear Reports/" # Directory of this code and report code files

joined_data_dir = "~/Documents/IoM 2025/Data/Cleaned/Joined data/" # Directory for Join csv files (e.g., Join5.csv)
cleaned_data_dir = "~/Documents/IoM 2025/Data/Cleaned/" # Directory for cleaned VMS and logbook data
reference_data_dir = "~/Documents/IoM 2025/Data/Cleaned/" # Directory for reference data csv files
shapefiles_dir = "~/Documents/IoM 2025/Data/Raw/Shapefiles/" # Directory for shapefiles
# -----------------------------------------------------------------------------


{  # Run this line to generate the report ----
  
  suppressWarnings({
    
    Join5 = read_csv(paste0(joined_data_dir, "Join5.csv"))
    Join10s.4 = read_csv(paste0(joined_data_dir, "Join10s_4.csv"))
    Join10m.5 = read_csv(paste0(joined_data_dir, "Join10m_5.csv"))
    
    logbook = read.csv(paste0(cleaned_data_dir, "combinedlog_O10_U10m.csv"))
    vms = read.csv(paste0(cleaned_data_dir, "ivms_cleaned.csv"))

    all_ref = read_csv(paste0(reference_data_dir, "IOM_Logbook _ReferencePeriod.csv"))
    
    nm12 <- st_read(paste0(shapefiles_dir, "IoM_12nm_marbdy_arc_bng.shp")) # 12nm territorial sea
    nm3 <- st_read(paste0(shapefiles_dir, "IoM_3nm_marbdy_arc_bng.shp")) # 3nm boundary
    IoM <- st_read(paste0(shapefiles_dir, "IOM_WGS.shp")) # IoM shapefile
    zones = st_read(paste0(shapefiles_dir, "IoM_lobster_zones.shp")) # 1nm lobster management zones
    whelk_zones = st_read(paste0(shapefiles_dir, "Whelk MGZ_16102023.shp")) # 12nm whelk management zones
    crab_zones = st_read(paste0(shapefiles_dir, "Crab_Grid.shp")) # common crab fishing grounds 1km2
    lobster_grid = st_read(paste0(shapefiles_dir, "Lobster_Grid.shp")) # common lobster fishing grounds 1km2
    
    
    nm12 = st_transform(nm12, crs = 4326)
    nm3 = st_transform(nm3, crs = 4326)
    IoM = st_transform(IoM, crs = 4326)
    IOM12NM = concaveman(nm12)
    IOM3NM = concaveman(nm3)
    
    
    if (params$quarter == '1') {
      dates = '1st January - 31st March'
    }
    if (params$quarter == '2') {
      dates = '1st April - 30th June'
    }
    if (params$quarter ==  '3') {
      dates = '1st July - 30th September'
    }
    if (params$quarter == '4') {
      dates = '1st October - 31st December'
    }
    if (params$quarter == 'Annual') {
      dates = '1st January - 31st December'
    }
    
    quarter = params$quarter
    {
      if (isTRUE(quarter == 'All')) {
        year = paste(min(Join5$LE_YEAR), '-', max(Join5$LE_YEAR), sep = '')
      }
      else if (isTRUE(quarter == 'Custom')) {
        year = paste(params$start_date, '-', params$end_date, sep = '')
      }
      else {
        year = params$year
      }
    }
    
    if (quarter == '1') {
      start_date = as.POSIXct(paste('01/01/', year, sep = ''), format = '%d/%m/%Y', tz = 'UTC')
      end_date = as.POSIXct(paste('31/03/', year, sep = ''), format = '%d/%m/%Y', tz = 'UTC')
    }
    if (quarter == '2') {
      start_date = as.POSIXct(paste('01/04/', year, sep = ''), format = '%d/%m/%Y', tz = 'UTC')
      end_date = as.POSIXct(paste('30/06/', year, sep = ''), format = '%d/%m/%Y', tz = 'UTC')
    }
    if (quarter == '3') {
      start_date = as.POSIXct(paste('01/07/', year, sep = ''), format = '%d/%m/%Y', tz = 'UTC')
      end_date = as.POSIXct(paste('30/09/', year, sep = ''), format = '%d/%m/%Y', tz = 'UTC')
    }
    if (quarter == '4') {
      start_date = as.POSIXct(paste('01/10/', year, sep = ''), format = '%d/%m/%Y', tz = 'UTC')
      end_date = as.POSIXct(paste('31/12/', year, sep = ''), format = '%d/%m/%Y', tz = 'UTC')
    }
    if (quarter == 'Annual') {
      start_date = as.POSIXct(paste('01/01/', year, sep = ''), format = '%d/%m/%Y', tz = 'UTC')
      end_date = as.POSIXct(paste('31/12/', year, sep = ''), format = '%d/%m/%Y', tz = 'UTC')
    }
    if (quarter == 'All') {
      start_date = min(na.omit(as.POSIXct(Join5$SI_DATE, format = '%d/%m/%Y', tz = 'UTC')))
      end_date = max(na.omit(as.POSIXct(Join5$SI_DATE, format = '%d/%m/%Y', tz = 'UTC')))
    }
    if (quarter == 'Custom') {
      start_date = as.POSIXct(params$start_date, format = '%d/%m/%Y', tz = 'UTC')
      end_date = as.POSIXct(params$end_date, format = '%d/%m/%Y', tz = 'UTC')
    }
    
    DEFA = params$DEFA
    species = params$species
    
    type = if_else(DEFA == 'TRUE', 'DEFA', 'Public')
    
    add_suffix <- function(n) {
      if (n %% 100 %in% 11:13) return("th")
      switch(as.character(n %% 10),
             "1" = "st",
             "2" = "nd",
             "3" = "rd",
             "th")
    }
    
    format_with_suffix <- function(x) {
      day <- as.integer(format(x, "%d"))
      suffix <- add_suffix(day)
      paste0(day, suffix, format(x, " %B %Y"))
    }
    
    
    
    {
      if (isTRUE(quarter == 'Annual')) {
        render(input = paste0(reports_dir, species, " report.Rmd", sep = ''),
               output_file = paste0(reports_dir, "Reports/IoM_", gsub(' ', '_', species), '_', type, '_Annual_', year, '.pdf', sep = ''),
               params = params)
      }
      else if (isTRUE(quarter == '1' | quarter == '2' | quarter == '3' | quarter == '4')) {
        render(input = paste0(reports_dir, species, " report.Rmd", sep = ''),
               output_file = paste0(reports_dir, "Reports/IoM_", gsub(' ', '_', species), '_', type, "_Q", quarter, '_', year, '.pdf', sep = ''),
               params = params)
      }
      else if (isTRUE(quarter == 'All')) {
        render(input = paste0(reports_dir, species, " report.Rmd", sep = ''),
               output_file = paste0(reports_dir, "Reports/IoM_", gsub(' ', '_', species), '_', type, '_All_', year, '.pdf', sep = ''),
               params = params)
      }
      else if (isTRUE(quarter == 'Custom')) {
        render(input = paste0(reports_dir, species, " report.Rmd", sep = ''),
               output_file = paste0(reports_dir, "Reports/IoM_", gsub(' ', '_', species), '_', type, '_Custom_', gsub('/', '_', year), '.pdf', sep = ''),
               params = params)
      }
      else {
        simpleError('Quarter name not recognised, check input value.')
      }
    }
  })
}



