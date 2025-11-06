

rm(list = ls()) # clear the environment


# Raw data directories ##################################

under10log_dir = '~/Documents/IoM 2025/Data/Raw/LB_Under10m/Under10m.csv' # directory for .csv containing all logbook entries for vessels under 10 m (e.g., '~/Documents/Under10m.csv')
over10log_dir = '~/Documents/IoM 2025/Data/Raw/LB_Over10m/O10_Logbook.csv' # directory for .csv containing all logbook entries for vessels over 10 m (e.g., '~/Documents/Over10m.csv')
potlimit_dir = '~/Documents/IoM 2025/Data/Raw/Pots/PotLimitsVessel.csv' # directory for .csv containing all pot limits for vessels (e.g., '~/Documents/PotLimits.csv')
iVMS_dir = '~/Documents/IoM 2025/Data/Raw/iVMS' # directory for iVMS folder containing all .csv files of iVMS data (must have 'iVMS' in file names)

uk_shapefile_dir = '~/Documents/IoM 2025/Data/Raw/Shapefiles/CoastlineUK_and_IOM.shp'
nm12_shapefile_dir = '~/Documents/IoM 2025/Data/Raw/Shapefiles/Base/IoM_12nm_marbdy_arc_bng.shp'
nm3_shapefile_dir = '~/Documents/IoM 2025/Data/Raw/Shapefiles/Base/IoM_3nm_marbdy_arc_bng.shp'
nm3_clippedshapefile_dir = '~/Documents/IoM 2025/Data/Raw/Shapefiles/Base/IoM_3nm_clipWGS.shp'
IoM_shapefile_dir = '~/Documents/IoM 2025/Data/Raw/Shapefiles/Base/IOM_WGS.shp'

years = c(2023, 2024) # list of years for output files to include (e.g., c(2023, 2024))
excluded_vessels = c("M123", "M180", "M221", "M243") # vessels to exclude from iVMS data (default was c("M123", "M180", "M221", "M243"))

script_dir = '~/Documents/IoM 2025/Raw Data Clean' # folder directory of cleaning script (cleaning_script_FM.R)
output_dir = '~/Documents/IoM 2025/Cleaned data test' # folder directory for cleaned files to be created in


# Run the cleaning script ###############################

source(paste0(script_dir, '/cleaning_script_FM.R')) 
