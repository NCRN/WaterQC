# Watershed attributes script
# Calculates watershed attributes for each NCRN monitoring site
# Dan Myers, 1/3/2024

# Load packages
library(sf)
library(raster)
library(tidyr)
library(dplyr)
library(terra)
library(lubridate)

### Read data ##################################################################
# NCRN Watersheds
sheds <- st_read("NCRN_watersheds_2023_07_06.shp") %>% arrange(NEW_IMLOCI)

# Lat/Lon
latLon <- read.csv("NCRN_Water_Sites_New_Location_for_Watershed_Delineation_20230630 - no dups.csv") %>% 
  arrange(NEW_IMLOCID) %>%
  mutate("IMLOCID_Water" = NEW_IMLOCID)

# Stream order (NHDPlus HR)
streams <- st_read("NHDFlowline_streamOrder_5070.shp")

# NPS boundary (NPS_-_Land_Resources_Division_Boundary_and_Tract_Data_Service)
nps <- st_read("nps_boundary_fixed_geoms_small_5070.shp")

# Trees (Dynamic World 2022)
trees <- raster("dw_composite2022_NCRN_extent_5070_trees.tif")

# Crops (Dynamic World 2022)
crops <- raster("dw_composite2022_NCRN_extent_5070_crops.tif")

# Built lands (Dynamic World 2022)
built <- raster("dw_composite2022_NCRN_extent_5070_built.tif")

# Impervious (NLCD 2019)
imp <- raster("nlcd19_impervious_5070.tif")

# Population (Landscan 2021 night)
pop <- raster("landscan_2021_night_5070.tif")

# Structures (USA Structures Dataset)
struc <- st_read("structures_point_ncrn_extent.shp")

# Protected area (Protected Areas Database)
prot <- st_read("PADUS3_0Combined_Region1_fixed_geoms.shp")

# Water quality
fileName <- "20231128_wqp_wqx_bss_wq_npsncrn" # Leave out .csv extension
wdata <- read.csv(paste(fileName,".csv",sep=""))

# State monitoring locations (Water Quality Portal)
wqx <- st_read("stations_WQX_2019-2022.shp")

# USGS monitoring locations (Water Quality Portal)
usgs <- st_read("stations_USGS_2019-2022.shp")

### Create attributes table ####################################################
sheds_table <- data.frame(
  MonitoringLocationIdentifier = NA,
  IMLOCID_Water = sheds$NEW_IMLOCI,
  NEW_NAME = sheds$NEW_NAME,
  Latitude = NA,
  Longitude = NA,
  MonitoringLocationIdentifier_IMLOCID_Water_Latitude_Longitude = NA,
  Stream_Order = NA,
  Watershed_Acres = NA,
  Watershed_SQKM = NA,
  Watershed_SQMI = NA,
  NPS_Watershed_Ownership_Acres = NA,
  NPS_Watershed_Ownership_Percent = NA,
  Stream_Miles_in_Watershed = NA,
  Watershed_Percent_Forest_DW = NA,
  Watershed_Percent_Agriculture_DW = NA,
  Watershed_Percent_Urban_DW = NA,
  Watershed_Percent_Impervious_NLCD2019 = NA,
  Watershed_Landscan_Population_Sum = NA,
  Watershed_USA_Structures_Count = NA,
  Watershed_Protected_Area_SQKM_PAD = NA,
  Watershed_Protected_Area_Percent_PAD = NA,
  Annual_Median_Water_Temp = NA,
  Annual_Median_pH = NA,
  Annual_Median_ANC = NA,
  Annual_Median_Specific_Conductance = NA,
  Annual_Median_DO = NA,
  Annual_Median_TN = NA,
  Annual_Median_TP = NA,
  Annual_Median_Flow = NA,
  State_Monitoring_Stations_in_Watershed = NA,
  USGS_Monitoring_Stations_in_Watershed = NA,
  Years_Monitored_by_NCRN = NA,
  Notes = NA
)

# Fix PRWI_NOCH name
sheds_table$NEW_NAME[sheds_table$IMLOCID_Water=="NCRN_PRWI_NOCH"] <- "North Branch Chopawamsic Below Confluence with Middle Branch"

# Watershed info
sheds_table$MonitoringLocationIdentifier <- paste("11NPSWRD_WQX-", sheds_table$IMLOCID_Water, sep="")

# Lat/Lon
temp <- left_join(sheds_table, latLon, by="IMLOCID_Water")
sheds_table$Latitude <- temp$LATITUDE
sheds_table$Longitude <- temp$LONGITUDE

# Combined name
sheds_table$MonitoringLocationIdentifier_IMLOCID_Water_Latitude_Longitude <-
  paste('"',sheds_table$MonitoringLocationIdentifier,",", sheds_table$IMLOCID_Water, ",",sheds_table$Latitude, ",",sheds_table$Longitude, '"',sep="")

### Calculate attributes #######################################################
# Watershed areas
sheds_table$Watershed_SQKM <- st_area(sheds) / 1000000
sheds_table$Watershed_Acres <- sheds_table$Watershed_SQKM * 247.105
sheds_table$Watershed_SQMI <- sheds_table$Watershed_SQKM * 0.38610156251654370596

# NPS ownership
nps_int <- st_intersection(sheds, nps)
nps_int$area_acres <- st_area(nps_int) / 1000000 * 247.105
nps_sum <- nps_int %>% mutate("IMLOCID_Water" = NEW_IMLOCI) %>%
  group_by(IMLOCID_Water) %>%
  summarise(area_acres = sum(area_acres, na.rm=T))
temp <- left_join(sheds_table, nps_sum, by="IMLOCID_Water")
sheds_table$NPS_Watershed_Ownership_Acres <- temp$area_acres
sheds_table$NPS_Watershed_Ownership_Percent <- sheds_table$NPS_Watershed_Ownership_Acres /
  sheds_table$Watershed_Acres * 100


# Stream miles and order
streams_int <- st_intersection(st_zm(streams), sheds)
streams_int$miles <- st_length(streams_int) / 1000 * 0.621371
streams_poly <- st_join(sheds,streams_int)
streams_poly <- streams_poly %>% mutate("IMLOCID_Water" = NEW_IMLOCI.x) %>%
  group_by(IMLOCID_Water) %>%
  summarise(miles = sum(miles, na.rm=T),
            order=max(NHDPlusFlo, na.rm=T))
temp <- left_join(sheds_table, streams_poly, by="IMLOCID_Water")
sheds_table$Stream_Miles_in_Watershed <- temp$miles
sheds_table$Stream_Order <- temp$order
sheds_table$Stream_Order[is.infinite(sheds_table$Stream_Order)] <- 1 # Fill in any small stream gaps

# Fix CATO_SVSR (should be 4th order not 5th)
sheds_table$Stream_Order[sheds_table$IMLOCID_Water=="NCRN_CHOH_SVSR"] <- 4

# Land cover (takes a few minutes)
trees_area <- NA
crops_area <- NA
built_area <- NA
for (i in 1:nrow(sheds)){
  trees_area[i] <- terra::extract(trees, sheds[i,], mean, na.rm=T)
  crops_area[i] <- terra::extract(crops, sheds[i,], mean, na.rm=T)
  built_area[i] <- terra::extract(built, sheds[i,], mean, na.rm=T)
  print(i)
}
sheds_table$Watershed_Percent_Forest_DW <- trees_area*100
sheds_table$Watershed_Percent_Agriculture_DW <- crops_area*100
sheds_table$Watershed_Percent_Urban_DW <- built_area*100

# Percent impervious
imp_area <- NA
for (i in 1:nrow(sheds)){
  imp_area[i] <- terra::extract(imp, sheds[i,], mean, na.rm=T)
  print(i)
}
sheds_table$Watershed_Percent_Impervious_NLCD2019 <- imp_area

# Landscan population sum
pop_area <- NA
for (i in 1:nrow(sheds)){
  pop_area[i] <- terra::extract(pop, sheds[i,], sum, na.rm=T)
  print(i)
}
sheds_table$Watershed_Landscan_Population_Sum <- pop_area

# Structures
struc_int <- st_intersection(struc, sheds) %>% mutate("IMLOCID_Water" = NEW_IMLOCI) %>%
  group_by(IMLOCID_Water) %>%
  summarise(sites = n())
temp <- left_join(sheds_table, struc_int, by="IMLOCID_Water")
sheds_table$Watershed_USA_Structures_Count <- temp$sites
sheds_table$Watershed_USA_Structures_Count[
  is.na(sheds_table$Watershed_USA_Structures_Count)] <- 0

# Protected area
prot_int <- st_intersection(sheds, prot)
prot_int$area_km2 <- st_area(prot_int) / 1000000
prot_sum <- prot_int %>% mutate("IMLOCID_Water" = NEW_IMLOCI) %>%
  group_by(IMLOCID_Water) %>%
  summarise(area_km2 = sum(area_km2, na.rm=T))
temp <- left_join(sheds_table, prot_sum, by="IMLOCID_Water")
sheds_table$Watershed_Protected_Area_SQKM_PAD <- temp$area_km2
sheds_table$Watershed_Protected_Area_Percent_PAD <- sheds_table$Watershed_Protected_Area_SQKM_PAD /
  sheds_table$Watershed_SQKM * 100

# State monitoring stations
wqx_int <- st_intersection(wqx, sheds) %>% mutate("IMLOCID_Water" = NEW_IMLOCI) %>%
  group_by(IMLOCID_Water) %>%
  summarise(sites = n())
temp <- left_join(sheds_table, wqx_int, by="IMLOCID_Water")
sheds_table$State_Monitoring_Stations_in_Watershed <- temp$sites
sheds_table$State_Monitoring_Stations_in_Watershed[
  is.na(sheds_table$State_Monitoring_Stations_in_Watershed)] <- 0

# USGS monitoring stations
usgs_int <- st_intersection(usgs, sheds) %>% mutate("IMLOCID_Water" = NEW_IMLOCI) %>%
  group_by(IMLOCID_Water) %>%
  summarise(sites = n())
temp <- left_join(sheds_table, usgs_int, by="IMLOCID_Water")
sheds_table$USGS_Monitoring_Stations_in_Watershed <- temp$sites
sheds_table$USGS_Monitoring_Stations_in_Watershed[
  is.na(sheds_table$USGS_Monitoring_Stations_in_Watershed)] <- 0


### Water quality ##############################################################
# Format dates as date
wdata$ActivityStartDate <- as.Date(wdata$ActivityStartDate)

# Save character results for later
wdata$char_results <- wdata$ResultMeasureValue

# Format result as numeric
wdata$ResultMeasureValue <- as.numeric(wdata$ResultMeasureValue)

# Charlie's string parsing script to create comment fields
st_visit_regex <- '(?<=Station_Visit_Comment":").*?(?=",)'
act_regex <- '(?<=Activity_Comment":").*?(?=",)'
samp_regex <- '(?<=Sampleability":").*?(?=",)'
flow_regex <- '(?<=Flow_Status_Choice_List":").*?(?=")'
wdata <- wdata %>% dplyr::mutate(
  Station_Visit_Comment = stringr::str_extract(ActivityCommentText, st_visit_regex)
  ,Activity_Comment = stringr::str_extract(ActivityCommentText, act_regex)
  ,Sampleability = stringr::str_extract(ActivityCommentText, samp_regex)
  ,Flow_Status_Choice_List = stringr::str_extract(ActivityCommentText, flow_regex)
)

# Select WQ data by project identifier
wqdata <- wdata[wdata$ProjectIdentifier=="USNPS NCRN Perennial stream water monitoring",] # 

# Choose WQ columns
wqdata <- wqdata[c("ActivityStartDate", "MonitoringLocationIdentifier", "CharacteristicName",
                   "ResultMeasureValue", "MeasureQualifierCode", "ResultValueTypeName",
                   "LaboratoryName", "ResultCommentText")]

# Take median of multiple measurements across stream
wdata_avgd <- wqdata %>%
  group_by(MonitoringLocationIdentifier) %>%
  group_by(ActivityStartDate, .add=T) %>%
  group_by(CharacteristicName, .add=T) %>%
  summarise(ResultMeasureValue = median(ResultMeasureValue, na.rm=T)) # Changed from mean to median

# Calculate medians and add to table
temp <- wdata_avgd %>% filter(CharacteristicName=="Temperature, water") %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarise(Median=summary(ResultMeasureValue)[3]) %>%
  mutate("IMLOCID_Water" = MonitoringLocationIdentifier)
temp2 <- left_join(sheds_table, temp, by="IMLOCID_Water")
sheds_table$Annual_Median_Water_Temp <- temp2$Median

temp <- wdata_avgd %>% filter(CharacteristicName=="pH") %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarise(Median=summary(ResultMeasureValue)[3]) %>%
  mutate("IMLOCID_Water" = MonitoringLocationIdentifier)
temp2 <- left_join(sheds_table, temp, by="IMLOCID_Water")
sheds_table$Annual_Median_pH <- temp2$Median

temp <- wdata_avgd %>% filter(CharacteristicName=="Acid Neutralizing Capacity (ANC)") %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarise(Median=summary(ResultMeasureValue)[3]) %>%
  mutate("IMLOCID_Water" = MonitoringLocationIdentifier)
temp2 <- left_join(sheds_table, temp, by="IMLOCID_Water")
sheds_table$Annual_Median_ANC <- temp2$Median

temp <- wdata_avgd %>% filter(CharacteristicName=="Specific conductance") %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarise(Median=summary(ResultMeasureValue)[3]) %>%
  mutate("IMLOCID_Water" = MonitoringLocationIdentifier)
temp2 <- left_join(sheds_table, temp, by="IMLOCID_Water")
sheds_table$Annual_Median_Specific_Conductance <- temp2$Median

temp <- wdata_avgd %>% filter(CharacteristicName=="Dissolved oxygen (DO)") %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarise(Median=summary(ResultMeasureValue)[3]) %>%
  mutate("IMLOCID_Water" = MonitoringLocationIdentifier)
temp2 <- left_join(sheds_table, temp, by="IMLOCID_Water")
sheds_table$Annual_Median_DO <- temp2$Median

temp <- wdata_avgd %>% filter(CharacteristicName=="Total Nitrogen, mixed forms") %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarise(Median=summary(ResultMeasureValue)[3]) %>%
  mutate("IMLOCID_Water" = MonitoringLocationIdentifier)
temp2 <- left_join(sheds_table, temp, by="IMLOCID_Water")
sheds_table$Annual_Median_TN <- temp2$Median

temp <- wdata_avgd %>% filter(CharacteristicName=="Total Phosphorus, mixed forms") %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarise(Median=summary(ResultMeasureValue)[3]) %>%
  mutate("IMLOCID_Water" = MonitoringLocationIdentifier)
temp2 <- left_join(sheds_table, temp, by="IMLOCID_Water")
sheds_table$Annual_Median_TP <- temp2$Median

temp <- wdata_avgd %>% filter(CharacteristicName=="Base flow discharge") %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarise(Median=summary(ResultMeasureValue)[3]) %>%
  mutate("IMLOCID_Water" = MonitoringLocationIdentifier)
temp2 <- left_join(sheds_table, temp, by="IMLOCID_Water")
sheds_table$Annual_Median_Flow <- temp2$Median

# Calculate years monitored
temp <- wdata_avgd %>% 
  group_by(MonitoringLocationIdentifier) %>%
  summarise(start1=min(year(ActivityStartDate), na.rm=T),
            end1=max(year(ActivityStartDate), na.rm=T)) %>%
  mutate("IMLOCID_Water" = MonitoringLocationIdentifier)
temp$years_mon <- paste(temp$start1,"-",temp$end1,sep="")
temp2 <- left_join(sheds_table, temp, by="IMLOCID_Water")
sheds_table$Years_Monitored_by_NCRN <- temp2$years_mon


### Save as csv ################################################################
write.csv(sheds_table, "NCRN-Water-Sites-Analysis_2024_01_17.csv", row.names=F, quote = F)
