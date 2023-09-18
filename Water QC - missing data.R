# Water QC - missing data
# Dan Myers, 9/13/2023

# This script flags water data activities that did not have a full record
# data


################################################################################
### Format data ################################################################
################################################################################

# Load packages
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(htmlwidgets)

# Set working directory
# setwd("R:/NPS_NCRN_VitalSigns/Analyses/Projects/New WQ data/")

# Read readme
# readLines("readme.txt")

# Read data
fileName <- "20230915_wqp_wqx_bss_wq_npsncrn" # Leave out .csv extension
wdata <- read.csv(paste(fileName,"_Flagged.csv",sep=""))

# Format dates as date
wdata$ActivityStartDate <- as.Date(wdata$ActivityStartDate)


################################################################################
### Extract WQ data#############################################################
################################################################################

# Select WQ data by project identifier
wqdata <- wdata[wdata$ProjectIdentifier=="USNPS NCRN Perennial stream water monitoring",]

################################################################################
### Create a "Flag Assessment" field that justifies reason for blank data#######
################################################################################

# Create fields for "sampleability" and "Flow, severity (choice list)"
samp <- wqdata[wqdata$CharacteristicName=="sampleability",]
samp <- samp[c("ActivityMediaSubdivisionName","ResultMeasureValue")]
colnames(samp) <- c("ActivityMediaSubdivisionName","sampleability")

flow <- wqdata[wqdata$CharacteristicName=="Flow, severity (choice list)",]
flow <- flow[c("ActivityMediaSubdivisionName","ResultMeasureValue")]
colnames(flow) <- c("ActivityMediaSubdivisionName","Flow_severity_choice_list")

# Add them to the water quality data
wqdata <- left_join(wqdata, samp, by="ActivityMediaSubdivisionName")
wqdata <- left_join(wqdata, flow, by="ActivityMediaSubdivisionName")
wqdata$sampleability[is.na(wqdata$sampleability)] <- 0
wqdata$Flow_severity_choice_list[is.na(wqdata$Flow_severity_choice_list)] <- 0

# Create a list of conditions that would lead us to expect a measurement that event
conditions <- (wqdata$ResultDetectionConditionText == "" | (wqdata$ResultDetectionConditionText != "" & wqdata$ResultMeasureValue != "")) & 
  wqdata$sampleability=="Actively sampled" & !wqdata$Flow_severity_choice_list %in% c("DRY","INTERSTITIAL","FLOOD")  

################################################################################
### Plot number of characteristics for each activity over time##################
################################################################################

# Summarize characteristics
chars <- wqdata[conditions,] %>%
  group_by(ActivityIdentifier) %>%
  group_by(ActivityStartDate, .add=T) %>%
  summarise(`Count of characteristics` = length(unique(CharacteristicName[ResultMeasureValue != ""]))) # All unique characteristics that have data

# Note measurement type
chars$Type <- substr(chars$ActivityIdentifier,30,100)

# For Flow Tracker entries 
# Discharge, wetted width, depth, and velocity
type <- "NCRN_WQ_FLTK"
plot(chars$ActivityStartDate[chars$Type==type], chars$`Count of characteristics`[chars$Type==type],
     xlab="Date", ylab="Count of characteristics",main=type)

# For YSI entries (having at least the first ones)
# TDS, DO, DO%, salinity, water temperature, pH, SC, conductivity, barometric pressure, distance from right bank, sequential increment from right bank
type <- "NCRN_WQ_YSI|1st"
plot(chars$ActivityStartDate[chars$Type==type], chars$`Count of characteristics`[chars$Type==type],
     xlab="Date", ylab="Count of characteristics",main=type)

# For lab measurements (not QC's)
# Nitrogen, one of the phosphoruses, and ANC (plus ammonia early on)
type <- "NCRN_WQ_LAB"
plot(chars$ActivityStartDate[chars$Type==type], chars$`Count of characteristics`[chars$Type==type],
     xlab="Date", ylab="Count of characteristics",main=type)

# For habitat and environment measurements
# Algae choice, algae description, stream appearance choice, channel flow choice
type <- "NCRN_WQ_HABINV"
plot(chars$ActivityStartDate[chars$Type==type], chars$`Count of characteristics`[chars$Type==type],
     xlab="Date", ylab="Count of characteristics",main=type)

# For air temperature measurement
type <- "NCRN_WQ_KES"
plot(chars$ActivityStartDate[chars$Type==type], chars$`Count of characteristics`[chars$Type==type],
     xlab="Date", ylab="Count of characteristics",main=type)

# Save as csv
write.csv(chars, "Count of water characteristics for each activity ID.csv",row.names=F)

################################################################################
### Investigate missing data by characteristic with pivot table (wide format)###
################################################################################

# Summarize by characteristic
chars2 <- wqdata[conditions,] %>%
  group_by(ActivityMediaSubdivisionName) %>%
  group_by(CharacteristicName, .add=T) %>%
  # group_by(ActivityCommentText, .add=T) %>% # To add comments in
  summarise(Has_data = (sum(ResultMeasureValue != "")>0)) # 1 if there's data, 0 if it's all blank

# Fill in blank characteristic names
chars2$CharacteristicName[chars2$CharacteristicName==""] <- "Unnamed characteristic"

# Pivot by characteristic name
pw <- chars2 %>%
  pivot_wider(names_from=CharacteristicName, values_from=Has_data)

# Convert to character
pw_char <- pw %>%
  mutate_if(is.logical, as.character)

# Mark data with no row records
pw_char[pw==1] <- ""
pw_char[pw==F] <- "Blank"
pw_char[is.na(pw)] <- "No record"

# Write to csv
write.csv(pw_char, "Record of characteristics for each activity.csv",row.names=F)

################################################################################
### Summarize by characteristics ###############################################
################################################################################

# Summarize by characteristic
chars3 <- wqdata[conditions,] %>%
  group_by(ActivityStartDate) %>%
  group_by(MonitoringLocationIdentifier,.add=T) %>%
  group_by(ActivityMediaSubdivisionName, .add=T) %>%
  group_by(CharacteristicName, .add=T) %>%
  summarise(Has_data = (sum(ResultMeasureValue != "")>0)) # 1 if there's data, 0 if it's all blank

# Fill in blank characteristic names
chars3$CharacteristicName[chars3$CharacteristicName==""] <- "Unnamed characteristic"

# Pivot by characteristic name
pw2 <- chars3 %>%
  pivot_wider(names_from=CharacteristicName, values_from=Has_data)

# Create summary data frame
Summary <- data.frame(
  Recorded = colSums(pw2[4:length(pw2)]==1, na.rm=T),
  Blank = colSums(pw2[4:length(pw2)]==0, na.rm=T),
  `No record` = colSums(is.na(pw2[4:length(pw2)])))

# Add characteristic names
Summary$CharacteristicName <- row.names(Summary)
Summary <- Summary[c(4,1,2,3)]
row.names(Summary) <- NULL

# Write to csv
write.csv(Summary, "Number of recorded, blank, and no-records for each characteristic.csv", row.names=F)

################################################################################
### Investigate missing data by characteristic (long format)####################
################################################################################

# Convert to character
pw_char2 <- pw2 %>%
  mutate_if(is.logical, as.character)

# Mark data with no row records
pw_char2[pw2==1] <- ""
pw_char2[pw2==F] <- "Blank"
pw_char2[is.na(pw2)] <- "No record"

# Pivot back longer with the "no record" rows now included
pw_char3 <- pw_char2 %>%
  pivot_longer(!c(ActivityMediaSubdivisionName, ActivityStartDate,MonitoringLocationIdentifier),
               names_to = "CharacteristicName",
               values_to = "Status")

# Create comments table (Activity level)
coms <- wqdata[conditions,c("ActivityMediaSubdivisionName","ActivityCommentText")]
coms <- coms[coms$ActivityCommentText != "" & coms$ActivityCommentText != "||",]

# Remove duplicate comments (Activity level)
coms <- coms[!duplicated(coms),] %>% 
  arrange(ActivityMediaSubdivisionName,ActivityCommentText)

# Combine multiple comments (Activity level)
for (i in 1:(nrow(coms)-1)){
  if (coms$ActivityMediaSubdivisionName[i] == coms$ActivityMediaSubdivisionName[i+1]){
    
    # Combine them, separated by a "| ", then remove the copied comment
    coms$ActivityCommentText[i+1] <- paste(coms$ActivityCommentText[i], coms$ActivityCommentText[i+1], sep="| ")
    coms$ActivityCommentText[i] <- ""
  }
}

# Remove duplicate rows (Activity level)
coms <- coms[coms$ActivityCommentText != "",]


# Create comments table (Result level)
coms2 <- wqdata[conditions,c("ActivityMediaSubdivisionName","CharacteristicName","ResultCommentText")]
coms2 <- coms2[coms2$ResultCommentText != "",]

# Remove duplicate comments (Result level)
coms2 <- coms2[!duplicated(coms2),] %>% 
  arrange(ActivityMediaSubdivisionName,CharacteristicName,ResultCommentText)

# Combine multiple comments (Result level)
for (i in 1:(nrow(coms2)-1)){
  if (coms2$ActivityMediaSubdivisionName[i] == coms2$ActivityMediaSubdivisionName[i+1] &
      coms2$CharacteristicName[i] == coms2$CharacteristicName[i+1]){
    
    # Combine them, separated by a "| ", then remove the copied comment
    coms2$ResultCommentText[i+1] <- paste(coms2$ResultCommentText[i], coms2$ResultCommentText[i+1], sep="| ")
    coms2$ResultCommentText[i] <- ""
  }
}

# Remove duplicate rows (Result level)
coms2 <- coms2[coms2$ResultCommentText != "",]


# Join comment to long table and remove records that aren't missing
pw_coms <- left_join(pw_char3,coms,by=c("ActivityMediaSubdivisionName"))
pw_coms <- left_join(pw_coms,coms2,by=c("ActivityMediaSubdivisionName","CharacteristicName"))
pw_coms <- pw_coms[pw_coms$Status != "",]

# Remove sites not currently monitored
current_sites <- wqdata %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarise(start_date = min(ActivityStartDate, na.rm=T),
            end_date = max(ActivityStartDate, na.rm=T))
current_sites <- current_sites[year(current_sites$end_date)==2023,]

# Remove rows that make sense there's no record there
pw_coms2 <- pw_coms[
  
  # Currently monitored sites
  pw_coms$MonitoringLocationIdentifier %in% current_sites$MonitoringLocationIdentifier &
  
  # Characteristics which should normally always be there
  (pw_coms$CharacteristicName %in% c("Conductivity","Salinity",
                                    "Acid Neutralizing Capacity (ANC)","RBP Stream Velocity",
                                    "Base flow discharge","Wetted Width","Cross-Section Depth",
                                    "Temperature, air","Dissolved oxygen (DO)",
                                    "Dissolved oxygen saturation","Specific conductance",
                                    "Temperature, water","pH","Total Nitrogen, mixed forms") |
  
    # Total phosphorus before 2009-01-13
  (pw_coms$CharacteristicName == "Total Phosphorus, mixed forms" &
     pw_coms$ActivityStartDate >= as.Date("2009-01-13")) |
    
    # TDS before 2016-05-05
  (pw_coms$CharacteristicName == "Solids, Dissolved (TDS)"  &
     pw_coms$ActivityStartDate >= as.Date("2016-05-05")) |
    
   # Barometric pressure started 2013-12-05
  (pw_coms$CharacteristicName == "Barometric pressure"  &
     pw_coms$ActivityStartDate >= as.Date("2013-12-05"))),
]

# Write to csv
write.csv(pw_coms2, "Long table of blanks and no-records.csv", na="", row.names=F)

################################################################################
### Plot by characteristics presence/absence ###################################
################################################################################

# Summarize by characteristic and date
chars4 <- wqdata[conditions,] %>%
  group_by(ActivityMediaSubdivisionName) %>%
  group_by(ActivityStartDate, .add=T) %>%
  group_by(CharacteristicName, .add=T) %>%
  summarise(Has_data = (sum(ResultMeasureValue != "")>0)) # 1 if there's data, 0 if it's all blank

# Fill in blank characteristic names
chars4$CharacteristicName[chars4$CharacteristicName==""] <- "Unnamed characteristic"

# Create vector of unique characteristics
chars_names <- unique(chars4$CharacteristicName)

# Pivot by characteristic name
pw3 <- chars4 %>%
  pivot_wider(names_from=CharacteristicName, values_from=Has_data)

# Convert to numeric
pw3 <- pw3 %>%
  mutate_if(is.logical, as.numeric)

# Change NA to False
pw3[is.na(pw3)] <- 0

# Pivot back longer with the "no record" rows now included (split panels between 2 figures)
pw4 <- pw3[1:18] %>%
  pivot_longer(!c(ActivityMediaSubdivisionName, ActivityStartDate),
               names_to = "CharacteristicName",
               values_to = "Present")

pw5 <- pw3[c(1,2,19:35)] %>%
  pivot_longer(!c(ActivityMediaSubdivisionName, ActivityStartDate),
               names_to = "CharacteristicName",
               values_to = "Present")

# Shorten names
pw4$CharacteristicName <- substr(pw4$CharacteristicName,1,25)
pw5$CharacteristicName <- substr(pw5$CharacteristicName,1,25)


# Create ggplot for first figure
a <- ggplot(pw4, aes(x=ActivityStartDate, z=ActivityMediaSubdivisionName))
a <- a + geom_point(aes(y=Present),shape=1,stroke=0.25) +
  facet_wrap(vars(CharacteristicName))

# Create ggplot for second figure
b <- ggplot(pw5, aes(x=ActivityStartDate, z=ActivityMediaSubdivisionName))
b <- b + geom_point(aes(y=Present),shape=1,stroke=0.25) +
  facet_wrap(vars(CharacteristicName))

# Save plotly to html
saveWidget(ggplotly(a), file = "Characteristic time series A.html",background='r')
saveWidget(ggplotly(b), file = "Characteristic time series B.html",background='r')


################################################################################
### Update EDD dataset with QC Determination column#############################
################################################################################

# Create variable and populate OOR (out of range) values
wdata$`QC Determination` <- NA
wdata$`QC Determination`[wdata$Flag..outside.20.80.quantiles.==1] <- "OOR"

# Assign QC Determination and remove unneeded columns
wdata_join <- full_join(wdata, pw_coms2[1:4], by=c("ActivityMediaSubdivisionName", "CharacteristicName"))
wdata_join$`QC Determination`[!is.na(wdata_join$Status)] <- wdata_join$Status[!is.na(wdata_join$Status)]
wdata_qc <- wdata_join[!colnames(wdata_join) %in% c("Month","Flag..outside.20.80.quantiles.","Status")]

# Save to csv file
write.csv(wdata_qc, paste(fileName,"_QC_Determination.csv",sep=""), na="", row.names=F)
