
# This will be a full script that will allow for raw iVMS and logbook data to be cleaned, formatted and joined for reports
# Original scripts were made by Isobel Bloor and Matt Coleman. This new script has been created by Fraser Masterson.

require(dplyr)
require(tidyverse)
require(reshape)
require(stringr)
require(data.table)
require(vmstools)
require(epiDisplay)
require(mapdata)
require(sf)
require(tmap)
require(sp)
require(concaveman)
require(ggplot2)
require(sqldf)
require(plyr)
require(doBy)


suppressWarnings(suppressMessages({
  
  # 1A: Logbook Cleaning (Under 10s) ----
  print('Cleaning logbook under 10s...')
  
  u10 <- read.csv(under10log_dir) 
  
  u10$LE_YEAR <-  format(as.Date(u10$Departure.Date, format = "%d/%m/%Y"), "%Y")
  u10$LE_YEAR <- as.numeric(u10$LE_YEAR)
  
  
  #Filter to static gear types
  geartype<-c("FPO","LHP","LX","LHM","LTL", "GNS", "LL", "LLS")
  u10 <- filter(u10, Gear.Type %in% geartype)
  
  
  #Create a unique trip reference using RSSnumber and departure date
  u10$uniqueID <- paste0(u10$RSS.No, "_", u10$Departure.Date)
  
  
  #Find records with the major errors for CRE and LBE
  #So crab lobster and whelk should only be caught in pots (and scallops- new for 2025 and Nephrops)
  crustacean.notpots <- filter(u10, FAOSpeciesCode %in% c("CRE", "LBE", "WHE", "SCE", "NEP") & Gear.Type %in% c("GNS", "LHM", "LHP", "LL", "LLS", "LTL", "LX"))
  #So fish should only be caught on lines and not in pots
  fish.inpots <- filter(u10, FAOSpeciesCode %in% c("DGH", "GUX", "HER", "HKE", "LIN", "MAC", "MON", "POK", "POL", "RJA", "RJC", "SAN", "SCR",  "SQC", "SQR", "TUR", "USB",  "WRA") & Gear.Type %in% c("FPO")) #116
  
  crustacean.notpots.1 <- unique(crustacean.notpots$uniqueID)
  fish.inpots.1 <- unique(fish.inpots$uniqueID)
  
  (grouped_list.cru <-   
      filter(u10, uniqueID %in% crustacean.notpots.1) %>%
      dplyr::select(Vessel.Name, RSS.No, Gear.Type, Gear.Characteristics, Species.Name,  Sum.of.Weight, uniqueID) %>%
      arrange(Gear.Type) %>%
      group_split(uniqueID))
  
  (grouped_list.fish <-   
      filter(u10, uniqueID %in% fish.inpots.1) %>%
      dplyr::select(Vessel.Name, RSS.No, Gear.Type, Gear.Characteristics, Species.Name,  Sum.of.Weight, uniqueID) %>%
      arrange(Gear.Type) %>%
      group_split(uniqueID))
  
  # concatenate species and gear and remove anything with CRE,LBE, WHE in line gears and remove anything fish in a pot
  
  pre.weights <- u10  %>%
    dplyr::group_by(FAOSpeciesCode, Gear.Type) %>%
    dplyr::summarise(Weight.kg = sum(Sum.of.Weight)) %>%
    as.data.frame()
  
  u10$spe.gear <- paste0(u10$FAOSpeciesCode, "_", u10$Gear.Type)
  
  #These are all the combinations of species and gear code that are impossible so just remove - at worst it is 14 CRE/LBE records
  exclude.spe.gear <- c("CRE_GNS", "CRE_LHM", "CRE_LHP", "CRE_LL", "CRE_LLS", "CRE_LTL", "CRE_LX", "LBE_GNS", "LBE_LHM", "LBE_LHP", "LBE_LL", "LBE_LLS", "LBE_LTL", "LBE_LX", "WHE_GNS", "WHE_LHM", "WHE_LHP", "WHE_LL", "WHE_LLS", "WHE_LTL", "WHE_LX", "NEP_GNS", "NEP_LHM", "NEP_LHP", "NEP_LL", "NEP_LLS", "NEP_LTL", "NEP_LX", "SCE_GNS", "SCE_LHM", "SCE_LHP", "SCE_LL", "SCE_LLS", "SCE_LTL", "SCE_LX", "LOQ_GNS", "LOQ_LHM", "LOQ_LHP", "LOQ_LL", "LOQ_LLS", "LOQ_LTL", "LOQ_LX", "DGH_FPO", "GUX_FPO", "HER_FPO", "HKE_FPO", "LIN_FPO", "MAC_FPO", "MON_FPO", "POK_FPO", "POL_FPO", "RJA_FPO", "RJC_FPO", "SAN_FPO",  "SCR_FPO",  "SQC_FPO", "SQR_FPO", "TUR_FPO", "USB_FPO", "WRA_FPO")
  
  
  #Remove any gear combinations that are not plausible
  u10.1 <-  filter(u10, !(spe.gear %in% exclude.spe.gear))
  
  post.weights <- u10.1  %>%
    dplyr::group_by(FAOSpeciesCode, Gear.Type) %>%
    dplyr::summarise(Weight.kg = sum(Sum.of.Weight)) %>%
    as.data.frame()
  
  
  #Identify straight duplicates
  uni.u10 <-  u10.1 %>%
    dplyr::group_by(across(c("RSS.No", "Departure.Date", "FAOSpeciesCode", "Gear.Type", "Sum.of.Weight", "Gear.Characteristics"))) %>%
    filter(n() ==1) %>%
    as.data.frame #10321
  
  dup.u10 <- u10.1 %>%
    dplyr::group_by(across(c("RSS.No", "Departure.Date", "FAOSpeciesCode", "Gear.Type", "Sum.of.Weight", "Gear.Characteristics"))) %>%
    filter(n() >1) %>%
    as.data.frame #185
  
  dup.u10<- dup.u10[order(dup.u10$RSS.No,dup.u10$Departure.Date, dup.u10$Sum.of.Weight),] 
  
  #remove duplicate
  dup.u10.2 <-
    dup.u10 %>%
    dplyr::group_by(across(c("RSS.No", "Departure.Date", "FAOSpeciesCode", "Gear.Type", "Sum.of.Weight", "Gear.Characteristics"))) %>%
    filter(row_number()==1) #89
  
  #Bind unique rows with rows minus the duplicates
  u10.2 <- rbind(uni.u10, dup.u10.2) #10410
  
  #Split Mesh Size into new column called Mesh.Size
  # Extract parts before and after the delimiter, ignoring spaces. so essentially This extracts everything before the first comma (including leading and trailing spaces). and str_trim(): Removes any leading or trailing spaces from the extracted result.
  u10.2a <- u10.2 %>%
    mutate(
      Mesh.Size = str_extract(Gear.Characteristics, "^\\s*[^,]+\\s*") %>% str_trim(),  # Everything before the delimiter
      Gear.Characteristics = str_extract(Gear.Characteristics, "\\s*[^,]+$") %>% str_trim()       # Everything after the delimiter
    )
  
  #From the Mesh.Size column extract only those values that start with Mesh Size :
  u10.2b <- u10.2a %>%
    mutate(
      Mesh.Size = if_else(str_starts(Mesh.Size, "Mesh size : "), Mesh.Size, NA_character_)
    )
  
  #This replaces part of a string that matches a specific pattern : GN :  or Number of lines :  or GM : and replaces it with an empty string.
  u10.3 <- u10.2b %>%
    mutate(
      Gear.Characteristics = str_replace(
        Gear.Characteristics,
        pattern = "GN : |Number of lines : |GM : ",
        replacement = ""
      ),
      Mesh.Size = str_replace(
        Mesh.Size,
        pattern = "Mesh size : ",
        replacement = ""
      )
      
    )
  
  u10.3$Mesh.Size <- as.numeric(u10.3$Mesh.Size)
  u10.3$Gear.Characteristics <- as.numeric(u10.3$Gear.Characteristics)
  
  u10.2 <- copy(u10.3)
  
  #IDENTIFY NON-EXACT DUPLICATES:
  
  dup.u10.ne <- u10.2 %>%
    dplyr::group_by(across(c("RSS.No", "Departure.Date", "FAOSpeciesCode", "Gear.Type"))) %>%
    filter(n() >1) %>%
    as.data.frame #161
  
  dup.u10.ne<- dup.u10.ne[order(dup.u10.ne$RSS.No,dup.u10.ne$Departure.Date, dup.u10.ne$Sum.of.Weight),]
  
  #Create a subset of the logbook dataset (this is basically the Log Event ID (LE_ID), The main species (SP) and the Landing weight estimate of all species (LE_KG_ALL))
  sub <- u10.2[,c("LE_YEAR","Gear.Type", "uniqueID","FAOSpeciesCode","Sum.of.Weight")]
  
  #Then Change the names of the columns in this subset of data so that "LE_KG_ALL" is now labelled as "Value"
  names(sub) <- c("LE_YEAR", "LE_Gear","LE_ID","SP","value")
  
  sub.seas.1 <- NULL 
  
  for(i in unique(sub$LE_YEAR)) {
    
    sub.seas <- filter(sub, LE_YEAR == i)
    tmp <- cast(sub.seas, LE_ID ~ SP, fun.aggregate=sum)
    sub.seas.1 <-bind_rows(sub.seas.1, tmp) 
    sub.seas.1[is.na(sub.seas.1)] <- 0
    
  }
  
  new2 <- vector()
  
  for ( i in c(2:length(names(sub.seas.1)))) {
    new1 <- paste("LE_KG_",names(sub.seas.1)[i],sep="")
    new2 <- c(new2,new1)
  }
  
  names(sub.seas.1) <- c(names(sub.seas.1)[1], new2)
  colnames(sub.seas.1)[1] <- "uniqueID"
  
  logbooknew <- merge(u10.2[, !names(u10.2) %in% c("FAOSpeciesCode", "Species.Name", "Sum.of.Weight")], sub.seas.1, by="uniqueID", all=T)
  
  sub <- u10.2[,c("LE_YEAR", "uniqueID","FAOSpeciesCode","Gear.Characteristics")]
  names(sub) <- c("LE_YEAR", "LE_ID","SP","value")
  
  sub.seas.1a <- NULL
  
  for(i in unique(sub$LE_YEAR)) {
    sub.seas <- filter(sub, LE_YEAR == i)
    tmp <- cast(sub.seas, LE_ID ~ SP, fun.aggregate=sum)
    sub.seas.1a <-bind_rows(sub.seas.1a, tmp) 
    sub.seas.1a[is.na(sub.seas.1a)] <- 0
  }
  
  new2 <- vector()
  
  for ( i in c(2:length(names(sub.seas.1a)))) {
    new1 <- paste("Pot_No_",names(sub.seas.1a)[i],sep="")
    new2 <- c(new2,new1)
  }
  
  names(sub.seas.1a) <- c(names(sub.seas.1a)[1], new2)
  colnames(sub.seas.1a)[1] <- "uniqueID"
  
  #Remove Duplicates where there were previously two records of landings/effort for a given day
  logbooknew.1 <- logbooknew %>% distinct(uniqueID,.keep_all = T) # 6732 =  alot of records would have had one line for each species and not one line for each day of landings
  newlogbook2 <- merge(logbooknew.1[, !names(logbooknew.1) %in% c("Gear.Characteristics")], sub.seas.1a, by=c("uniqueID"), all=T)
  
  trip.id.n <- u10.2 %>%
    dplyr::group_by(uniqueID) %>%
    dplyr::summarise('records' = length(unique(Trip.Identifier)))
  
  newlogbook3 <- left_join(newlogbook2, trip.id.n)
  
  species.rec.n <- u10.2 %>%
    dplyr::group_by(uniqueID, FAOSpeciesCode) %>%
    dplyr::summarise(entries = n()) %>%
    as.data.frame
  
  test <- filter(species.rec.n, entries > 1)
  
  test.1 <- pivot_wider(data = test, 
                        names_from = FAOSpeciesCode, 
                        values_from = entries, 
                        values_fill = 0,
                        names_prefix = "Dup_") %>%
    as.data.frame
  
  #Join back to dataset
  newlogbook4 <- newlogbook3 %>%
    left_join(test.1, by = "uniqueID") %>%
    mutate(across(starts_with("Dup_"), ~replace_na(.,0)))
  
  
  #multiple gear codes - record in a new column
  gear.concate <- u10.2 %>%
    dplyr::group_by(uniqueID) %>%
    dplyr::summarise(Gear.Codes = str_c(unique(Gear.Type), collapse = ".")) %>%
    as.data.frame
  
  
  newlogbook5 <- left_join(newlogbook4, gear.concate, by = "uniqueID")
  
  key_columns.CL <- c("LE_KG_CRE", "LE_KG_LBE")
  key_columns.CLW <- c("LE_KG_CRE", "LE_KG_LBE", "LE_KG_WHE")
  key_columns.WHE <- c("LE_KG_WHE")
  other_columns.CL <- grep("LE_KG_", names(newlogbook5[!(names(newlogbook5) %in% key_columns.CL)]), value = TRUE)
  other_columns.CLW <- grep("LE_KG_", names(newlogbook5[!(names(newlogbook5) %in% c(key_columns.CL, key_columns.WHE))]), value = TRUE)
  
  
  newlogbook6 <- newlogbook5 %>%
    mutate(Single.Sp.trip = case_when(
      rowSums(dplyr::select(., starts_with("LE_KG_")) > 0) == 1 ~ 1,
      rowSums(dplyr::select(., all_of(key_columns.CL)) >=1) == 2 & rowSums(dplyr::select(., all_of(other_columns.CL)) != 0) == 0 ~ 2, 
      rowSums(dplyr::select(., all_of(key_columns.CL)) >=1) >= 1 & LE_KG_WHE > 0 & rowSums(dplyr::select(., all_of(other_columns.CLW)) != 0) == 0 ~ 3,
      rowSums(dplyr::select(., all_of(other_columns.CLW)) >=1) >= 1 & rowSums(dplyr::select(., all_of(key_columns.CLW)) != 0) == 0 ~ 4,
      rowSums(dplyr::select(., all_of(other_columns.CLW)) >=1) >= 1 & rowSums(dplyr::select(., all_of(key_columns.CLW)) >= 1) >= 1  ~ 5,
      TRUE ~ 0))
  
  #1 = any single species trip (crab, lobster or whelk)
  #2 = Crab and lobster only
  #3 = Crab and/or lobster & whelk
  #4 = multiple fish only (any species)
  #5 = fish with Crab and/or lobster and/or whelk
  
  #Some columns need removing to match with O10 dataset
  newlogbook7 <- newlogbook6 %>% dplyr::select(-c(find.duplicate, xcheck.duplicate, NOTE))
  newlogbook7 <- filter(newlogbook7, LE_YEAR %in% years) 
  write_csv(newlogbook7, paste0(output_dir, "/u10log_cleaned.csv"))
  
  
  
  
  # 1B: Logbook Cleaning (Over 10s) ----
  
  print('Logbook under 10s cleaning complete. Cleaning logbook over 10s...')
  
  over10 <- read.csv(over10log_dir)
  
  #Create a unique trip reference using RSSnumber and departure date
  over10$uniqueID <- paste0(over10$RSS.No, "_", over10$Departure.Date)
  
  #Create Year column
  over10$LE_YEAR <- format(as.Date(over10$Departure.Date, format = "%d/%m/%Y"), "%Y")
  over10$LE_YEAR <- as.numeric(over10$LE_YEAR)
  over10 <- filter(over10, LE_YEAR %in% years) 
  
  #Convert unique ID to factor
  over10$uniqueID <- as.factor(over10$uniqueID)
  #Convert RSS number to factor
  over10$RSS.No<-as.factor(over10$RSS.No)
  
  #Identify straight duplicates
  uni.df <-  over10 %>%
    dplyr::group_by(across(c("RSS.No", "uniqueID", "Gear.Type", "LE_KG_CRE","LE_KG_LBE","LE_KG_WHE","Pot_No_CRE","Pot_No_LBE", "Pot_No_WHE"))) %>%
    filter(n() ==1) %>%
    as.data.frame #628
  
  dup.df <- over10 %>%
    dplyr::group_by(across(c("RSS.No", "uniqueID", "Gear.Type", "LE_KG_CRE","LE_KG_LBE","LE_KG_WHE","Pot_No_CRE","Pot_No_LBE", "Pot_No_WHE"))) %>%
    filter(n() >1) %>%
    as.data.frame #0
  
  dup.df<- dup.df[order(dup.df$RSS.No,dup.df$uniqueID, dup.df$LE_KG_CRE, dup.df$LE_KG_LBE, dup.df$LE_KG_WHE),] #153
  
  #remove duplicate
  dup.df.2 <-
    dup.df %>%
    dplyr::group_by(across(c("RSS.No", "uniqueID", "Gear.Type", "LE_KG_CRE","LE_KG_LBE","LE_KG_WHE","Pot_No_CRE","Pot_No_LBE", "Pot_No_WHE"))) %>%
    filter(row_number()==1) 
  
  #Bind unique rows with rows minus the duplicates
  over10.a <- rbind(uni.df, dup.df.2)
  
  #dplyr::summarise into 1 row if there are two entries for a single date and also adds records columns so we know if vessels submitted one sheet per species or not
  
  over10.1 <- over10.a %>%
    dplyr::group_by(Trip.Identifier, Vessel.Name, Nationality, Fisheries.Authority.Name, X.Administration.Port..Port.Code., X.Administration.Port..Port.Name., CFR , RSS.No, PLN, Departure.Date, X.Port.of.Departure..Port.Name., X.Port.of.Departure..Port.Code., Arrival.Date, X.Port.of.Arrival..Port.Name., X.Port.of.Arrival..Port.Code., FAO.Area, ICES, Gear.Type, Gear.Type.Description, SizeDistributionCategory, CatchType, LE_YEAR, uniqueID) %>%
    dplyr::summarise(across(everything(), ~ sum(., na.rm = TRUE)), # Applies sum across all columns
                     records = n()) %>%
    as.data.frame()
  
  checkdupsp <- over10.1 %>% filter(records > 1) %>% pull(uniqueID) %>% as.character
  
  over10.1$Gear.Codes <- over10.1$Gear.Type
  
  key_columns.CL <- c("LE_KG_CRE", "LE_KG_LBE")
  
  over10.2 <- over10.1 %>%
    mutate(Single.Sp.trip = case_when(
      rowSums(dplyr::select(., starts_with("LE_KG_")) > 0) == 1 ~ 1, #single species trips
      rowSums(dplyr::select(., all_of(key_columns.CL)) >=1) >= 1 & LE_KG_WHE == 0  ~ 2, #crab and lobster but no whelk
      rowSums(dplyr::select(., all_of(key_columns.CL)) >=1) & LE_KG_WHE > 0  ~ 3, #crab and/or lobster and whelk 
      TRUE ~ 0))
  
  over10.2 <- filter(over10.2, Single.Sp.trip > 0) 
  
  o10 <- over10 %>%
    dplyr::group_by(uniqueID) %>%
    mutate(
      Dup_CRE = ifelse(
        (n_distinct(LE_KG_CRE) >= 2 & LE_KG_CRE >0)  | (n_distinct(Pot_No_CRE) >= 2 & Pot_No_CRE >0), 
        2, 
        1
      ),
      Dup_LBE = ifelse(
        (n_distinct(LE_KG_LBE) >= 2 & LE_KG_LBE >0) | (n_distinct(Pot_No_LBE) >= 2 & Pot_No_LBE >0) , 
        2, 
        1
      ),
      Dup_WHE = ifelse(
        (n_distinct(LE_KG_WHE) >= 2 & LE_KG_WHE >0) | (n_distinct(Pot_No_WHE) >= 2  & Pot_No_WHE >0), 
        2, 
        1
      )
    ) %>%
    as.data.frame
  
  
  o10.1 <- dplyr::select(o10, c(uniqueID, Dup_CRE, Dup_LBE, Dup_WHE))
  o10.2 <- o10.1 %>%
    distinct(uniqueID, .keep_all = TRUE)
  
  over10.3 <- left_join(over10.2, o10.2, by = 'uniqueID')
  
  write_csv(over10.3, paste0(output_dir, "/010log_cleaned.csv"))
  print('Logbook over 10s cleaning complete.')
  
  
  
  # 1C: Logbook Joining ----
  
  newlogbook2 <- read.csv(paste0(output_dir, "/u10log_cleaned.csv"))
  over10.1 <- read.csv(paste0(output_dir, "/010log_cleaned.csv"))
  
  df4 <- plyr::rbind.fill(newlogbook2, over10.1)
  
  pot.limit <- read.csv(potlimit_dir)
  
  df5 <- left_join(df4, pot.limit, by = "RSS.No")
  
  write_csv(df5, paste0(output_dir, "/combinedlog_O10_U10m.csv"))
  print('Logbook joining complete.')
  
  
  
  
  # 2: iVMS Cleaning ----
  
  print('Cleaning iVMS data...')
  
  
  # Load spatial data 
  
  uk <- st_read(uk_shapefile_dir)
  the_crs <- 4326
  uk_wgs <- st_transform(uk, crs = the_crs)
  landPoly <- as(st_geometry(uk_wgs), "Spatial")
  
  ukioma <- NULL
  for(d in 1:6){
    ukioma <- rbind(ukioma, landPoly@polygons[[d]]@Polygons[[1]]@coords, c(NA, NA))
  }
  
  
  tbl <-
    list.files(path = iVMS_dir,
               pattern = "*.csv", 
               full.names = T) %>% 
    map_df(~read_csv(., col_types = cols(.default = "c"))) #5029508
  
  df1 <- as.data.frame(tbl)
  
  df2 <- df1[!(is.na(df1$SightingDate)),]
  
  ivms_pots <- copy(df2) 
  
  #Extract Date from DateTime Stamp
  ivms_pots$SightingDate <- str_remove(ivms_pots$SightingDate, ".000")
  ivms_pots$Date<- as.Date(ivms_pots$SightingDate)
  ivms_pots$Date<- format(ivms_pots$Date, "%d/%m/%Y")
  
  ivms_pots$Time<- format(substr(ivms_pots$SightingDate, 12, 19), format = "%H:%M:%S")
  ivms_pots<-ivms_pots %>% dplyr::select("RSSNo" ,"Date", "Time", "Latitude", "Longitude","Course", "Speed")
  
  #Renames columns to match vmstools 
  setnames(ivms_pots, old = c('RSSNo','Date','Time','Latitude', 'Longitude', 'Course', "Speed"), 
           new = c('VE_REF','SI_DATE','SI_TIME', 'SI_LATI','SI_LONG','SI_HE','SI_SP')) 
  
  ivms_pots$SI_HE<-as.numeric(ivms_pots$SI_HE)
  ivms_pots$SI_SP<-as.numeric(ivms_pots$SI_SP)
  
  #Create a unique ID from RSS number and date
  ivms_pots$uniqueID <- paste0(ivms_pots$VE_REF, "_", ivms_pots$SI_DATE)
  
  tacsat <- ivms_pots
  
  tacsat$SI_SP <- as.numeric(tacsat$SI_SP)
  
  #Check logbooks which haven't merged in Script 3 and plot them out on a map
  
  nm12 <- st_read(nm12_shapefile_dir)
  nm3 <- st_read(nm3_shapefile_dir)
  IoM <- st_read(IoM_shapefile_dir)
  
  nm12 = st_transform(nm12, crs = 4326)
  nm3 = st_transform(nm3, crs = 4326)
  IoM = st_transform(IoM, crs = 4326)
  
  tacsat$SI_LATI <- as.numeric(tacsat$SI_LATI)
  tacsat$SI_LONG <- as.numeric(tacsat$SI_LONG)
  
  # CLEAN IVMS DATA
  interval <- "3mins"
  spThres       <- 20   #Maximum speed threshold in analyses in knots - speeds not possible
  
  # 2.2.1 Keep track of removed points
  
  remrecsTacsat <-
    matrix(
      NA,
      nrow = 7, ncol = 2,
      dimnames =
        list(
          c("total", "duplicates", "notPossible", "land", "harbour", "pseudoDuplicates", "speed"),
          c("rows", "percentage"))
    )
  remrecsTacsat["total", ] <- c(nrow(tacsat), "100%") #2573794
  
  # 2.2.2 Remove duplicate records 
  
  tacsat$SI_DATIM <-
    as.POSIXct(
      paste(tacsat$SI_DATE, tacsat$SI_TIME),
      tz = "GMT",
      format = "%d/%m/%Y  %H:%M:%S"
    )
  
  uniqueTacsat <-
    paste(tacsat$VE_REF, tacsat$SI_DATIM)
  
  
  tacsat <- tacsat[!duplicated(uniqueTacsat), ]
  
  
  remrecsTacsat["duplicates",] <-
    c(
      nrow(tacsat),
      100 +
        round(
          (nrow(tacsat) - as.numeric(remrecsTacsat["total", 1])) /
            as.numeric(remrecsTacsat["total", 1]) *
            100,
          2)
    )
  
  remrecsTacsat
  
  # 2.2.3 Remove points that cannot be possible
  idx <- which(abs(tacsat$SI_LATI) > 90 | abs(tacsat$SI_LONG) > 180)
  idx <- unique(c(idx, which(tacsat$SI_SP > spThres)))
  if (length(idx) > 0) tacsat <- tacsat[-idx,]
  
  remrecsTacsat["notPossible",] <-
    c(
      nrow(tacsat),
      100 +
        round(
          (nrow(tacsat) - as.numeric(remrecsTacsat["total",1])) /
            as.numeric(remrecsTacsat["total",1]) *
            100,
          2)
    )
  
  
  # Remove points on land
  
  idx <- point.in.polygon(
    point.x = tacsat$SI_LONG, point.y = tacsat$SI_LATI,
    pol.x = ukioma[, 1], pol.y = ukioma[, 2]
  )
  pol <- tacsat[idx > 0, ]
  
  tacsat <- tacsat[which(idx == 0), ]
  
  remrecsTacsat["land", ] <-
    c(
      nrow(tacsat),
      100 +
        round(
          (nrow(tacsat) - as.numeric(remrecsTacsat["total", 1])) /
            as.numeric(remrecsTacsat["total",1]) * 100,
          2)
    )
  
  
  
  # Remove points in harbour
  
  # Clip harbours
  
  #Create region-specific harbours. creates a buffer based on the range. range is in KM.
  harboursSUP<-rbind(data.frame(harbour="Douglas",lon=-4.47363, lat=54.14673,range=0.64),
                     data.frame(harbour="Derby_1",lon=-4.62172,lat=54.07608,range=0.250),
                     data.frame(harbour="Derby_2", lon = -4.61509, lat = 54.07533, range= 0.044),
                     data.frame(harbour = "Ctown",lon=-4.65055, lat=54.07267, range=0.132),
                     data.frame(harbour = "PSM_1", lon =-4.73601, lat = 54.07302, range = 0.168), 
                     data.frame(harbour = "PSM_2", lon = -4.73216, lat = 54.07103, range = 0.148), 
                     data.frame(harbour = "ERI", lon = -4.76395, lat = 54.08488, range = 0.5), 
                     data.frame(harbour ="PEEL", lon = -4.69799653, lat = 54.22530299, range = 0.445), 
                     data.frame(harbour = "RAM", lon = -4.38099, lat = 54.32245, range = 0.4),
                     data.frame(harbour = "MXShearwater_Stock",lon = -4.7322166667, lat = 54.0759166667, range = 0.05),
                     data.frame(harbour = "Two_Boys_Stock",lon = -4.77085, lat = 54.08575, range = 0.05))
  
  
  #index points in harbour
  idx <- pointInHarbour(tacsat$SI_LONG,tacsat$SI_LATI,harboursSUP,saveHarbourList=F)
  
  #save points in harbour if wanted
  pih <- tacsat[which(idx == 1),]
  
  
  #make tacsat only points not in harbour
  tacsat <- tacsat[which(idx == 0),]
  tacsat$SI_HARB <- 0
  
  #write out points in harbour and on land to check
  NotAtSea <- rbind(pih, pol)
  
  
  #some problems with iVMS tracking en route from UK to IOM. Remove all pings outside IOM region #### - This relates to Boy Shayne to and from England
  #ACTION: we need to have a think about if this is the best approach (would a lot of this not get removed by speed filters anyway)
  idx <- which(tacsat$SI_LATI > 54.5 | tacsat$SI_LATI < 53.9 | tacsat$SI_LONG <
                 -5.1 | tacsat$SI_LONG > -4.00)
  
  tacsat.1 <- tacsat[-idx,]
  tacsat.2 <- tacsat[idx,]
  
  tacsat <- tacsat[-idx,]
  
  
  remrecsTacsat["harbour",] <-
    c(
      nrow(tacsat[tacsat$SI_HARB == 0,]),
      100 +
        round(
          (nrow(tacsat[tacsat$SI_HARB == 0,]) - as.numeric(remrecsTacsat["total",1])) /
            as.numeric(remrecsTacsat["total",1]) *
            100,
          2)
    )
  
  #remove pseudo duplicates 
  
  lowThres <- 1 #Minimum difference in time interval in minutes to prevent pseudo duplicates
  highThres <- 7 # Maximum difference in time interval in minutes to prevent intervals being too large to be realistic##### Any pings higher than this are reset to this value #
  
  #sort tacsat so we can calculate intervals between pings
  tacsat <- sortTacsat(tacsat)
  
  #caculate intervals between pings by vessels
  tacsatp <- intervalTacsat(tacsat,level="vessel",fill.na=T)
  
  tacsatp <- filter(tacsatp, INTV < highThres & INTV > lowThres)
  
  tacsat <- copy(tacsatp)
  
  remrecsTacsat["pseudoDuplicates",] <-
    c(
      nrow(tacsat[tacsat$SI_HARB == 0,]),
      100 +
        round(
          (nrow(tacsat[tacsat$SI_HARB == 0,]) - as.numeric(remrecsTacsat["total",1])) /
            as.numeric(remrecsTacsat["total",1]) *
            100,
          2)
    )
  
  
  # Speed 
  
  spThres <- 3 #Matt/Isobel decided that 3knots is the maximum speed allowed in the dataset
  tacsat <- filter(tacsat, SI_SP < spThres)
  
  remrecsTacsat["speed", ] <-
    c(
      nrow(tacsat),
      100 +
        round(
          (nrow(tacsat) - as.numeric(remrecsTacsat["total", 1])) /
            as.numeric(remrecsTacsat["total",1]) * 100,2)
    ) 
  
  
  #Convert to polygons using convex hull
  IOM12NM = concaveman(nm12)
  IOM3NM = concaveman(nm3)
  
  # Subtract the overlapping area of polygon2 from polygon1
  IOM12NM.USE<- st_difference(IOM12NM, IOM3NM)
  
  #Load clipped 0-3 nm limit
  nm3 <- st_read(nm3_clippedshapefile_dir)
  nm3 = st_transform(nm3, crs = 4326)
  
  #Convert vms data to spatial point file 
  tacsat <- st_as_sf(tacsat, coords = c("SI_LONG", "SI_LATI"), crs = 4326)
  
  #assign a 1 if within 0-3 in column within03 and assign a 1 if within 3-12 in Within312 column
  tacsat$Within03 = as.integer(st_within(tacsat, nm3) %>% lengths > 0)
  tacsat$Within312 = as.integer(st_within(tacsat, IOM12NM.USE) %>% lengths > 0)
  
  
  # use the following code to buffer. This then assigns a 1 to those near the 0-3 and a 0 to those outside the 0-12.
  tacsat$Within03.1 <- as.integer(st_is_within_distance(tacsat, nm3, 100) %>% lengths > 0)
  tacsat$Within03 <- ifelse(tacsat$Within03 == 0 & tacsat$Within03.1 == 1, 1, tacsat$Within03)
  
  tacsat$Within03.1 <- NULL  
  
  coords <- st_coordinates(tacsat)
  
  # Add latitude and longitude columns to the data
  tacsat$SI_LONG <- coords[, "X"]
  tacsat$SI_LATI <- coords[, "Y"]
  
  tacsat <- as.data.frame(tacsat)
  tacsat$geometry <- NULL
  
  write_csv(tacsat, paste0(output_dir, "/ivms_cleaned.csv"))
  print('iVMS data cleaned. Joining with logbook data...')
  
  
  
  # 3: iVMS and Logbook Joining ----
  
  iVMS = tacsat
  logbook = df5
  
  
  #Create Month and filter logbook data & Vessels that dont have iVMS
  logbook$SI_Month<-format(as.Date(logbook$Departure.Date, format="%d/%m/%Y"),"%m")
  logbook$SI_Month<-as.numeric(logbook$SI_Month)
  
  too.small <- excluded_vessels 
  
  logbook <- filter(logbook, !(RSS.No %in% too.small))
  duplicate_rows <- logbook[duplicated(logbook$uniqueID) | duplicated(logbook$uniqueID, fromLast = TRUE),] #0 duplicated unique IDs
  
  LHP.1 <- filter(logbook, Gear.Type == "LHP")  
  
  logbook <- filter(logbook, !(Single.Sp.trip == 1 & (LE_KG_CRE == 0 & LE_KG_LBE == 0 & LE_KG_WHE == 0) | Single.Sp.trip == 4)) 
  
  Join2a <- sqldf("SELECT * FROM logbook
                INNER JOIN iVMS
                ON  logbook.uniqueID = iVMS.uniqueID")
  
  duplicated_names <- duplicated(colnames(Join2a))
  Join2a<- Join2a[!duplicated_names]
  
  
  #anti-join for vms and logbooks and then run merge 2 but for closest/nearest date.
  loganti2a <- anti_join(logbook, iVMS, by = "uniqueID")
  check.vms <- unique(loganti2a$uniqueID)
  
  vms.merged <- unique(Join2a$uniqueID)
  vmsnotmerge <- filter(iVMS, !(uniqueID %in% vms.merged))
  vms.merge <- filter(iVMS, uniqueID %in% vms.merged)
  
  #so join on RSS_NO and then on nearest date
  colnames(loganti2a)[colnames(loganti2a) == "RSS.No"] <- "VE_REF"# change to VE_REF as I think SQL doesn't work with '.'
  colnames(loganti2a)[colnames(loganti2a) == "Departure.Date"] <- "Departure_Date"# remove . from column name
  
  #Convert dates to format used by SQLlite (needed for Join2b - we don't directly use date in Join 2a)
  loganti2a$Departure_Date <- as.Date(loganti2a$Departure_Date, format = "%d/%m/%Y")
  vmsnotmerge$SI_DATE <- as.Date(vmsnotmerge$SI_DATE, format = "%d/%m/%Y")
  
  
  # Get unique groups
  vms.og <-  read.csv("~/Documents/IoM 2025/Data/Cleaned/iVMS_PreClean.csv")
  check.check.1 <- filter(vms.og, uniqueID %in% check.vms)
  length(unique(check.check.1$uniqueID)) #so 184 of the 305 records not merged do have vms pings for that vessel date pre-cleaning- so plot out to see
  #100/305*184 ~ 60%
  
  Points.sf.1 <- st_as_sf(check.check.1, coords = c("SI_LONG", "SI_LATI"), crs = 4326)
  
  nm3 <- st_read(nm3_shapefile_dir)
  nm3 = st_transform(nm3, crs = 4326)
  
  
  #sort in ascending order of dates (i.e. earliest date first)
  Join2b <- sqldf("SELECT * FROM loganti2a
                INNER JOIN vmsnotmerge
                ON  loganti2a.VE_REF = vmsnotmerge.VE_REF
                WHERE ABS(JULIANDAY(loganti2a.Departure_Date) - JULIANDAY(vmsnotmerge.SI_DATE)) = (
                SELECT MIN(ABS(JULIANDAY(loganti2a.Departure_Date) - JULIANDAY(vmsnotmerge.SI_DATE)))
                FROM loganti2a
                WHERE vmsnotmerge.VE_REF = loganti2a.VE_REF)
                ORDER BY loganti2a.VE_REF, vmsnotmerge.SI_DATE ASC") #27367
  
  
  duplicated_names <- duplicated(colnames(Join2b))
  # Remove Duplicate Column Names
  Join2b<- Join2b[!duplicated_names]
  
  
  #so we are going +/- 1 days max
  Join2b$date.diff <- Join2b$SI_DATE - Join2b$Departure_Date
  
  Join2b.1 <- filter(Join2b, abs(date.diff) <=1)
  
  Points.sf.1 <- st_as_sf(Join2b.1, coords = c("SI_LONG", "SI_LATI"), crs = 4326)
  
  # Filter data
  Join2b.2 <- Join2b.1 %>%
    dplyr::group_by(uniqueID) %>%
    filter(
      n() == 1 |               # Keep if only one row for the ID
        (n() > 1 & date.diff == -1) # For duplicates, keep rows where value == "A"
    ) %>%
    ungroup() %>% # Remove grouping for final output
    as.data.frame
  
  
  #Remove columns that are in DF2 not in DF1
  Join2b.2$UniqueID1 <- NULL
  Join2b.2$date.diff <- NULL
  
  #match column names
  colnames(Join2b.2)[colnames(Join2b.2) == "Departure_Date"] <- "Departure.Date"
  Join2a$RSS.No <- NULL
  
  Join3 <- rbind(Join2a, Join2b.2) 
  
  #Replace any NAs with 0s
  Join4 <- Join3 %>%
    mutate(across(c(starts_with("LE_KG"),starts_with("Pot_No_")), ~tidyr::replace_na(., 0)))
  
  
  #Sums the number of vms pings for each vessel trip that are within the 0-3 and within the 3-12
  sum_Join <- Join4 %>%
    dplyr::group_by(VE_REF, uniqueID) %>%
    dplyr::summarise(across(starts_with("Within"),   ~ ifelse(all(is.na(.x)), NA, sum(.x, na.rm = TRUE)), .names = "sum_{.col}")) %>%
    as.data.frame
  
  #Add the two together to get total (n) of vms points
  sum_Join$Tot.vms.p <- sum_Join$sum_Within03 + sum_Join$sum_Within312
  
  #Choose only these columns from sum_Join to add  (uniqueID, sum_Within03, sum_Within312, Tot.vms.p)
  Join5 <- left_join(Join4, sum_Join[, 2:5], by = "uniqueID")
  
  #Validate whether the number of pots reported is within the limits for each vessel/species and zone fished
  Join5$WHE.POT.OK <- ifelse((Join5$LE_KG_WHE >0 & Join5$sum_Within312 > 0) &  Join5$Pot_No_WHE > Join5$W012, 1, ifelse((Join5$LE_KG_WHE >0 & Join5$sum_Within312 ==  0 & Join5$sum_Within03 >0) &  Join5$Pot_No_WHE > Join5$W03, 2, 0))
  #so 0 = within limit, 
  #1 = fishing in 3-12 (and yes or no fishing in 0-3 nm) and more pots reported than 0-12 allocation
  #2 = fishing exclusively in 0-3 nm and more pots reported than allocation
  
  Join5 %>%
    filter(WHE.POT.OK >0) %>%    # Filter rows where WHE.POT.OK is greater than 1
    dplyr::select(VE_REF, uniqueID,  W03, W012, Pot_No_WHE, WHE.POT.OK, sum_Within03, sum_Within312, records, Dup_WHE, Single.Sp.trip) %>%
    distinct(uniqueID, .keep_all = TRUE)  # Keep only the first row for each uniqueID# Select specific column
  
  Join5$CL.POT.OK <- ifelse(((Join5$LE_KG_CRE >0 | Join5$LE_KG_LBE >0) & Join5$sum_Within312 > 0) &  (Join5$Pot_No_CRE > Join5$CL012 | Join5$Pot_No_LBE > Join5$CL012), 1, ifelse(((Join5$LE_KG_CRE >0 | Join5$LE_KG_LBE >0) & Join5$sum_Within312 ==  0 & Join5$sum_Within03 >0) &  (Join5$Pot_No_CRE > Join5$CL03 | Join5$Pot_No_LBE > Join5$CL03), 2, 0))
  
  Join5 %>%
    filter((CL.POT.OK >0 | WHE.POT.OK >0) & records == 1) %>%    # Filter rows where WHE.POT.OK is greater than 1
    dplyr::select(VE_REF, uniqueID,  CL03, CL012, W03, W012, Pot_No_CRE, Pot_No_LBE, Pot_No_WHE, CL.POT.OK, WHE.POT.OK, sum_Within03, sum_Within312, Single.Sp.trip) %>%
    distinct(uniqueID, .keep_all = TRUE) %>%  # Keep only the first row for each uniqueID# Select specific column
    arrange(CL.POT.OK, WHE.POT.OK, Single.Sp.trip)
  
  # Create the table of unique values
  df_unique <- Join5 %>%
    group_by(Single.Sp.trip) %>%              
    summarise(Unique_Values = list(unique(uniqueID)))
  
  #1 = any single species trip
  #2 = Crab and lobster only
  #3 = Crab and/or lobster & whelk
  #4 = fish only (any species)
  #5 = fish with Crab and/or lobster and/or whelk
  
  
  #For mixed crab and lobster trips an 80% cut off will be used for crab targeted trips
  Join5$Ratio.CL <- ifelse(Join5$Single.Sp.trip == 2 , round((100/(Join5$LE_KG_CRE + Join5$LE_KG_LBE)) * Join5$LE_KG_CRE,1), 0)
  
  df_unique <- Join5 %>%
    filter(Single.Sp.trip == 2 &Ratio.CL >= 80) %>%
    #filter(Single.Sp.trip == 2 &Ratio.CL <= 25) %>%
    group_by(Single.Sp.trip) %>%                # Group by 'Category'
    dplyr::summarise(Unique_Values = list(unique(uniqueID)))
  
  #Split here into 3 & 5 and Crab ration under 80 % and over 20% 
  #treat differently
  Mixed.trips <- filter(Join5, Single.Sp.trip %in% c(3,5))
  Mixed.trips$target.sp <- "mixed"
  
  #Single or targeted species trip - assign Crab if 2 but over 80% ration assign lobster if 2 but uner 20% and assign Crab, Lobster or whelk to each single species trip. 
  
  Join6 <- filter(Join5, Single.Sp.trip %in% c(1,2))#202065
  Join7a <- filter(Join5, Single.Sp.trip %in% c(3,5)) #8048
  Join7b <- filter(Join5, Single.Sp.trip %in% c(4)) # this would be for fish species if you wanted to include it
  
  #Sums the weight of the mixed trips for crab, lobster or whelk
  mixed.trips <- Join7a %>%
    group_by(uniqueID) %>%
    dplyr::summarise(Crab.kg = sum(max(LE_KG_CRE)),
                     Lobster.kg  = sum(max(LE_KG_LBE)),
                     Whelk.kg  = sum(max(LE_KG_WHE))) %>%
    as.data.frame
  
  #Sum weights in tonnes for single species trips
  single.trips <- Join6 %>%
    group_by(uniqueID) %>%
    summarise(Crab.kg = sum(max(LE_KG_CRE)),
              Lobster.kg  = sum(max(LE_KG_LBE)),
              Whelk.kg  = sum(max(LE_KG_WHE))) %>%
    as.data.frame
  
  Join8s <- filter(Join6, Single.Sp.trip == 1) 
  Join8m <- filter(Join6, Single.Sp.trip == 2) 
  
  
  Join8m.c <- filter(Join8m, Ratio.CL >= 95)#7619
  Join8m.m <- filter(Join8m, Ratio.CL > 30 & Ratio.CL < 95) #31859
  Join8m.l <- filter(Join8m, Ratio.CL <= 30) #4130
  
  
  #Create pots per ping for each species in single trip and kg.pot.day
  Join8s.1 <- Join8s %>%
    mutate(pots_ping.CRE = if_else(Pot_No_CRE == 0 | Tot.vms.p == 0, 0, Pot_No_CRE/Tot.vms.p),
           pots_ping.LBE = if_else(Pot_No_LBE == 0 | Tot.vms.p == 0, 0, Pot_No_LBE/Tot.vms.p),
           pots_ping.WHE = if_else(Pot_No_WHE == 0 | Tot.vms.p == 0, 0, Pot_No_WHE/Tot.vms.p),
           kg_pot.day.CRE = round(if_else(Pot_No_CRE == 0 | LE_KG_CRE == 0, 0, LE_KG_CRE / Pot_No_CRE),2),
           kg_pot.day.LBE = round(if_else(Pot_No_LBE == 0 | LE_KG_LBE == 0, 0, LE_KG_LBE / Pot_No_LBE),2),
           kg_pot.day.WHE = round(if_else(Pot_No_WHE == 0 | LE_KG_WHE == 0, 0, LE_KG_WHE / Pot_No_WHE),2))
  
  #Summarise by trip
  #Round to nearest whole pot
  Join9s.1 <- Join8s.1 %>%
    group_by(uniqueID, W03, W012, CL03, CL012) %>%
    dplyr::summarise(Pots03nm.CRE = round(sum(pots_ping.CRE[Within03 == 1], na.rm = TRUE),0),
                     Pots012nm.CRE = round(sum(pots_ping.CRE[Within312 == 1], na.rm = TRUE),0),
                     Pots03nm.LBE = round(sum(pots_ping.LBE[Within03 == 1], na.rm = TRUE),0),
                     Pots012nm.LBE = round(sum(pots_ping.LBE[Within312 == 1], na.rm = TRUE),0),
                     Pots03nm.WHE = round(sum(pots_ping.WHE[Within03 == 1], na.rm = TRUE),0),
                     Pots012nm.WHE = round(sum(pots_ping.WHE[Within312 == 1], na.rm = TRUE),0)) %>%
    as.data.frame
  
  
  Join10s <- Join8s.1 %>%
    dplyr::left_join(Join9s.1 %>% dplyr::select(uniqueID, Pots03nm.CRE, Pots012nm.CRE, Pots03nm.LBE, Pots012nm.LBE, Pots03nm.WHE, Pots012nm.WHE), by = "uniqueID")
  
  Join10s.1 <- Join10s %>%
    mutate(pots.O.03 = if_else(Pots03nm.CRE > CL03 | Pots03nm.LBE > CL03 | Pots03nm.WHE > W03, 1, 0),
           pots.O.312 = if_else(Pots012nm.CRE > CL012 | Pots012nm.LBE > CL012  | Pots012nm.WHE > W012 , 1, 0,))
  
  #so this should only reasign excess pots to 3-12 from 0-3 nm if there was any fished points within the 3-12mm
  Join10s.2 <- Join10s.1%>%
    mutate(Pots03nm.CRE.1 = ifelse(Pots03nm.CRE > CL03, CL03, Pots03nm.CRE),
           Pots012nm.CRE.1 = ifelse(Pots03nm.CRE > CL03 & sum_Within312 > 0, Pots012nm.CRE + (Pots03nm.CRE - CL03) , Pots012nm.CRE),
           Pots03nm.LBE.1 = ifelse(Pots03nm.LBE > CL03, CL03, Pots03nm.LBE),
           Pots012nm.LBE.1 = ifelse(Pots03nm.LBE > CL03 & sum_Within312 > 0, Pots012nm.LBE + (Pots03nm.LBE - CL03) , Pots012nm.LBE),
           Pots03nm.WHE.1 = ifelse(Pots03nm.WHE > W03, W03, Pots03nm.WHE),
           Pots012nm.WHE.1 = ifelse(Pots03nm.WHE > W03 & sum_Within312 > 0, Pots012nm.WHE + (Pots03nm.WHE - W03) , Pots012nm.WHE),)
  
  
  Join10s.3 <- Join10s.2 %>%
    mutate(pots_ping.CRE.1 = if_else((Pots03nm.CRE.1 == 0 & Pots012nm.CRE.1 == 0)| Tot.vms.p == 0, 0, (Pots03nm.CRE.1 + Pots012nm.CRE.1)/Tot.vms.p),
           pots_ping.LBE.1 = if_else((Pots03nm.LBE.1 == 0 & Pots012nm.LBE.1 == 0)| Tot.vms.p == 0, 0, (Pots03nm.LBE.1 + Pots012nm.LBE.1)/Tot.vms.p),
           pots_ping.WHE.1 = if_else((Pots03nm.WHE.1 == 0 & Pots012nm.WHE.1 == 0) | Tot.vms.p == 0, 0, (Pots03nm.WHE.1 + Pots012nm.WHE.1)/Tot.vms.p),
           kg_pot.day.CRE.1 = round(if_else((Pots03nm.CRE.1 == 0 & Pots012nm.CRE.1 == 0) | LE_KG_CRE == 0, 0, LE_KG_CRE / (Pots03nm.CRE.1 + Pots012nm.CRE.1)),2),
           kg_pot.day.LBE.1 = round(if_else((Pots03nm.LBE.1 == 0 & Pots012nm.LBE.1 == 0) | LE_KG_LBE == 0, 0, LE_KG_LBE / (Pots03nm.LBE.1 + Pots012nm.LBE.1)),2),
           kg_pot.day.WHE.1 = round(if_else((Pots03nm.WHE.1 == 0 & Pots012nm.WHE.1 == 0) | LE_KG_WHE == 0, 0, LE_KG_WHE / (Pots03nm.WHE.1 + Pots012nm.WHE.1)),2))
  
  Join10s.4 <- Join10s.3 %>%
    dplyr::select(-pots_ping.CRE, -pots_ping.LBE, -pots_ping.WHE, -kg_pot.day.CRE, -kg_pot.day.LBE,  -kg_pot.day.WHE, -Pots03nm.CRE, -Pots012nm.CRE, -Pots03nm.LBE, -Pots012nm.LBE, -Pots03nm.WHE, -Pots012nm.WHE) 
  #Add target species column
  Join10s.4 <- Join10s.4 %>%
    mutate(
      target.sp = case_when(
        LE_KG_CRE > 0 ~ "Crab",
        LE_KG_LBE > 0 ~ "Lobster",
        LE_KG_WHE > 0 ~ "Whelk",
        TRUE ~ "None" # Default category if no column > 0
      )
    )
  
  
  #For mixed crab and lobster trips an 80% cut off will be used for crab targeted trips
  Join8m$Ratio.CL <- round((100/(Join8m$LE_KG_CRE + Join8m$LE_KG_LBE)) * Join8m$LE_KG_CRE,1)
  
  Join8m.c <- filter(Join8m, Ratio.CL >= 95)#7619
  Join8m.m <- filter(Join8m, Ratio.CL > 30 & Ratio.CL < 95) #31859
  Join8m.l <- filter(Join8m, Ratio.CL <= 30) #4130
  
  Join8m.c$Ratio.CL <- NULL
  Join8m.l$Ratio.CL <- NULL
  
  Join8m.c$target.sp <- "Crab"
  Join8m.l$target.sp <- "Lobster"
  
  Join8m.cl <- rbind(Join8m.c,Join8m.l)
  
  Join8m.c <- Join8m.cl %>%
    mutate(pots_ping.CRE = if_else(Pot_No_CRE == 0 | Tot.vms.p == 0, 0, Pot_No_CRE/Tot.vms.p),
           pots_ping.LBE = if_else(Pot_No_LBE == 0 | Tot.vms.p == 0, 0, Pot_No_LBE/Tot.vms.p),
           pots_ping.WHE = if_else(Pot_No_WHE == 0 | Tot.vms.p == 0, 0, Pot_No_WHE/Tot.vms.p),
           kg_pot.day.CRE = round(if_else(Pot_No_CRE == 0 | LE_KG_CRE == 0, 0, LE_KG_CRE / Pot_No_CRE),2),
           kg_pot.day.LBE = round(if_else(Pot_No_LBE == 0 | LE_KG_LBE == 0, 0, LE_KG_LBE / Pot_No_LBE),2),
           kg_pot.day.WHE = round(if_else(Pot_No_WHE == 0 | LE_KG_WHE == 0, 0, LE_KG_WHE / Pot_No_WHE),2))
  
  #Summarise by trip
  #Round to nearest whole pot
  Join9m.1 <- Join8m.c %>%
    group_by(uniqueID, W03, W012, CL03, CL012) %>%
    dplyr::summarise(Pots03nm.CRE = round(sum(pots_ping.CRE[Within03 == 1], na.rm = TRUE),0),
                     Pots012nm.CRE = round(sum(pots_ping.CRE[Within312 == 1], na.rm = TRUE),0),
                     Pots03nm.LBE = round(sum(pots_ping.LBE[Within03 == 1], na.rm = TRUE),0),
                     Pots012nm.LBE = round(sum(pots_ping.LBE[Within312 == 1], na.rm = TRUE),0),
                     Pots03nm.WHE = round(sum(pots_ping.WHE[Within03 == 1], na.rm = TRUE),0),
                     Pots012nm.WHE = round(sum(pots_ping.WHE[Within312 == 1], na.rm = TRUE),0)) %>%
    as.data.frame
  
  
  Join10m.1 <- Join8m.c %>%
    dplyr::left_join(Join9m.1 %>% dplyr::select(uniqueID, Pots03nm.CRE, Pots012nm.CRE, Pots03nm.LBE, Pots012nm.LBE, Pots03nm.WHE, Pots012nm.WHE), by = "uniqueID")
  
  Join10m.2 <- Join10m.1 %>%
    mutate(pots.O.03 = if_else(Pots03nm.CRE > CL03 | Pots03nm.LBE > CL03 | Pots03nm.WHE > W03, 1, 0),
           pots.O.312 = if_else(Pots012nm.CRE > CL012 | Pots012nm.LBE > CL012  | Pots012nm.WHE > W012 , 1, 0,))
  
  #so this should only reasign excess pots to 3-12 from 0-3 nm if there was any fished points within the 3-12mm
  Join10m.3 <- Join10m.2%>%
    mutate(Pots03nm.CRE.1 = ifelse(Pots03nm.CRE > CL03, CL03, Pots03nm.CRE),
           Pots012nm.CRE.1 = ifelse(Pots03nm.CRE > CL03 & sum_Within312 > 0, Pots012nm.CRE + (Pots03nm.CRE - CL03) , Pots012nm.CRE),
           Pots03nm.LBE.1 = ifelse(Pots03nm.LBE > CL03, CL03, Pots03nm.LBE),
           Pots012nm.LBE.1 = ifelse(Pots03nm.LBE > CL03 & sum_Within312 > 0, Pots012nm.LBE + (Pots03nm.LBE - CL03) , Pots012nm.LBE),
           Pots03nm.WHE.1 = ifelse(Pots03nm.WHE > W03, W03, Pots03nm.WHE),
           Pots012nm.WHE.1 = ifelse(Pots03nm.WHE > W03 & sum_Within312 > 0, Pots012nm.WHE + (Pots03nm.WHE - W03) , Pots012nm.WHE),)
  
  
  Join10m.4 <- Join10m.3 %>%
    mutate(pots_ping.CRE.1 = if_else((Pots03nm.CRE.1 == 0 & Pots012nm.CRE.1 == 0)| Tot.vms.p == 0, 0, (Pots03nm.CRE.1 + Pots012nm.CRE.1)/Tot.vms.p),
           pots_ping.LBE.1 = if_else((Pots03nm.LBE.1 == 0 & Pots012nm.LBE.1 == 0)| Tot.vms.p == 0, 0, (Pots03nm.LBE.1 + Pots012nm.LBE.1)/Tot.vms.p),
           pots_ping.WHE.1 = if_else((Pots03nm.WHE.1 == 0 & Pots012nm.WHE.1 == 0) | Tot.vms.p == 0, 0, (Pots03nm.WHE.1 + Pots012nm.WHE.1)/Tot.vms.p),
           kg_pot.day.CRE.1 = round(if_else((Pots03nm.CRE.1 == 0 & Pots012nm.CRE.1 == 0) | LE_KG_CRE == 0, 0, LE_KG_CRE / (Pots03nm.CRE.1 + Pots012nm.CRE.1)),2),
           kg_pot.day.LBE.1 = round(if_else((Pots03nm.LBE.1 == 0 & Pots012nm.LBE.1 == 0) | LE_KG_LBE == 0, 0, LE_KG_LBE / (Pots03nm.LBE.1 + Pots012nm.LBE.1)),2),
           kg_pot.day.WHE.1 = round(if_else((Pots03nm.WHE.1 == 0 & Pots012nm.WHE.1 == 0) | LE_KG_WHE == 0, 0, LE_KG_WHE / (Pots03nm.WHE.1 + Pots012nm.WHE.1)),2))
  
  
  
  Join10m.5 <- Join10m.4%>%
    dplyr::select(-pots_ping.CRE, -pots_ping.LBE, -pots_ping.WHE, -kg_pot.day.CRE, -kg_pot.day.LBE,  -kg_pot.day.WHE, -Pots03nm.CRE, -Pots012nm.CRE, -Pots03nm.LBE, -Pots012nm.LBE, -Pots03nm.WHE, -Pots012nm.WHE) 
  
  
  if (!"Ratio.CL" %in% names(Join10m.5)) {
    Join10m.5$Ratio.CL <- NA
  }
  
  Join11 <- rbind(Join10s.4, Join10m.5)
  
  
  # Filter rows where any column meets the condition
  filtered_df <- Join11 %>%
    filter((Pot_No_CRE == 0 & LE_KG_CRE >0) | (Pot_No_LBE == 0 & LE_KG_LBE >0) | (Pot_No_WHE ==0 & LE_KG_WHE >0))
  
  
  write.csv(Join5, file = paste0(output_dir, "/Join5.csv"))
  write.csv(Join10s.4, file = paste0(output_dir, "/Join10s_4.csv"))
  write.csv(Join10m.5, file = paste0(output_dir, "/Join10m_5.csv"))
  
  print(paste('Data cleaning and formatting complete. Output files created in folder: ', output_dir, sep = ''))
  
  rm(list = ls()) # clear the environment
  
}))
  
