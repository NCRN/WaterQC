# Water QC - missing data
# Dan Myers, 8/31/2023

# This script flags water data activities that did not have a full record
# data


################################################################################
### Format data ################################################################
################################################################################

# Load packages
library(dplyr)
library(tidyr)
library(lubridate)

# Set working directory
# setwd("R:/NPS_NCRN_VitalSigns/Analyses/Projects/New WQ data/")

# Read readme
# readLines("readme.txt")

# Read data
fileName <- "20230825_wqp_wqx_bss_wq_npsncrn" # Leave out .csv extension
wdata <- read.csv(paste(fileName,".csv",sep=""))

# Format dates as date
wdata$ActivityStartDate <- as.Date(wdata$ActivityStartDate)

# Format result as numeric
# wdata$ResultMeasureValue <- as.numeric(wdata$ResultMeasureValue)


################################################################################
### Extract WQ data#############################################################
################################################################################

# Select WQ data by project identifier
wqdata <- wdata[wdata$ProjectIdentifier=="USNPS NCRN Perennial stream water monitoring",]


################################################################################
### Plot number of characteristics for each activity over time##################
################################################################################

# Summarize characteristics
chars <- wqdata %>%
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
### Investigate missing data by characteristic with pivot table#################
################################################################################

# Summarize by characteristic
chars2 <- wqdata %>%
  group_by(ActivityMediaSubdivisionName) %>%
  group_by(CharacteristicName, .add=T) %>%
  group_by(ActivityCommentText, .add=T) %>% # To add comments in
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
write.csv(pw_char, "Record of characteristics and comments for each activity.csv",row.names=F)

################################################################################
### Summarize by characteristics ###############################################
################################################################################

# Summarize by characteristic
chars3 <- wqdata %>%
  group_by(ActivityMediaSubdivisionName) %>%
  group_by(CharacteristicName, .add=T) %>%
  summarise(Has_data = (sum(ResultMeasureValue != "")>0)) # 1 if there's data, 0 if it's all blank

# Fill in blank characteristic names
chars3$CharacteristicName[chars3$CharacteristicName==""] <- "Unnamed characteristic"

# Pivot by characteristic name
pw2 <- chars3 %>%
  pivot_wider(names_from=CharacteristicName, values_from=Has_data)

# Create summary data frame
Summary <- data.frame(
  Recorded = colSums(pw2[3:length(pw2)]==1, na.rm=T),
  Blank = colSums(pw2[3:length(pw2)]==0, na.rm=T),
  `No record` = colSums(is.na(pw2[3:length(pw2)])))

# Add characteristic names
Summary$CharacteristicName <- row.names(Summary)
Summary <- Summary[c(4,1,2,3)]
row.names(Summary) <- NULL

# Write to csv
write.csv(Summary, "Number of recorded, blank, and no-records for each characteristic.csv", row.names=F)
