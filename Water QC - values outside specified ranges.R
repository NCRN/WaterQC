# Water QC - values outside specified ranges
# Dan Myers, 8/31/2023

# This script reads in a table of user-specified boundaries for water quality
# values, and adds a field to the dataset flagging abnormal values


################################################################################
### Format data ################################################################
################################################################################

# Load packages
library(dplyr)
library(lubridate)

# Set working directory
# setwd("R:/NPS_NCRN_VitalSigns/Analyses/Projects/New WQ data/")

# Read readme
# readLines("readme.txt")

# Read data
fileName <- "20230906_wqp_wqx_bss_wq_npsncrn" # Leave out .csv extension
wdata <- read.csv(paste(fileName,".csv",sep=""))

# Format dates as date
wdata$ActivityStartDate <- as.Date(wdata$ActivityStartDate)

# Save character results for later
char_results <- wdata$ResultMeasureValue

# Format result as numeric
wdata$ResultMeasureValue <- as.numeric(wdata$ResultMeasureValue)


################################################################################
### Explore fields #############################################################
################################################################################

# Get column names
colnames(wdata)

# Get min and max dates
min(wdata$ActivityStartDate)
max(wdata$ActivityStartDate)

# Get sites
unique(wdata$MonitoringLocationIdentifier)

# Check out result identifiers (keys)
head(wdata$ResultIdentifier)
max(wdata$ResultIdentifier)

# Check out project identifiers
unique(wdata$ProjectIdentifier)

# Check out site visit names
head(wdata$ActivityMediaSubdivisionName)

# Check out activity identifiers
head(wdata$ActivityIdentifier)

# Check out characteristic names
unique(wdata$CharacteristicName)

# Check out result units
unique(wdata$ResultMeasure.MeasureUnitCode)

# Check out times
head(wdata$ActivityStartTime.Time)

# Check out site names
unique(wdata$MonitoringLocationName)

# Latitude and longitude (numeric)
head(wdata$ActivityLocation.LatitudeMeasure)
head(wdata$ActivityLocation.LongitudeMeasure)

# Method identifiers
unique(wdata$ResultAnalyticalMethod.MethodIdentifier)

# Quality control codes
unique(wdata$MeasureQualifierCode)

# Taxon
unique(wdata$SubjectTaxonomicName)[1:5]

# Activity comments
unique(wdata$ActivityCommentText)[1:5]

# Result comments
unique(wdata$ResultCommentText)[1:5]

# Sample comments (electrofishing pass number)
unique(wdata$SampleCollectionMethod.MethodDescriptionText)

# Quantification limit
unique(wdata$DetectionQuantitationLimitMeasure.MeasureValue)


################################################################################
### Extract WQ data#############################################################
################################################################################

# Select WQ data by project identifier
wqdata <- wdata[wdata$ProjectIdentifier=="USNPS NCRN Perennial stream water monitoring",]

################################################################################
### Explore sites, dates, and characteristics###################################
################################################################################

# View site monitoring extents
site_info <- wqdata %>% 
  group_by(MonitoringLocationIdentifier) %>%
  summarise(start_date = min(ActivityStartDate, na.rm=T),
            end_date = max(ActivityStartDate, na.rm=T))

# Add if currently monitored
site_info$current <- 0
site_info$current[year(site_info$end_date)==2023] <- 1

# Add how many years of data
site_info$years <- year(site_info$end_date) - year(site_info$start_date)

# Write to csv
write.csv(site_info, "Information about sites.csv", row.names=F)

# View characteristics info
char_info <- wqdata %>%
  group_by(CharacteristicName) %>%
  group_by(ResultMeasure.MeasureUnitCode, .add=T) %>%
  summarise(Min=summary(ResultMeasureValue)[1],
            Q1=summary(ResultMeasureValue)[2],
            Median=summary(ResultMeasureValue)[3],
            Mean=summary(ResultMeasureValue)[4],
            Q3=summary(ResultMeasureValue)[5],
            Max=summary(ResultMeasureValue)[6],
            NAs=summary(ResultMeasureValue)[7],
            n=n(),
            SD=sd(ResultMeasureValue,na.rm=T))

# Remove non-numeric variables
char_info <- char_info[!is.na(char_info$Min) & char_info$n>100,]
char_info <- char_info[char_info$CharacteristicName!="Weather Condition (WMO Code 4501) (choice list)",]

# Write to csv
write.csv(char_info, "Information about characters.csv", row.names=F)

################################################################################
### Identify samples with odd values ###########################################
################################################################################

# Read in reasonable ranges sheet
ranges <- read.csv("User specified water value boundaries.csv")

# Initialize a data frame of odd values
odds <- data.frame()

# Start a loop to search for odd values for each characteristic
for (name in ranges$CharacteristicName){
  
  # Get the reasonable ranges for that characteristic
  rmin <- ranges$Reasonable_min[ranges$CharacteristicName==name]
  rmax <- ranges$Reasonable_max[ranges$CharacteristicName==name]
  
  # Extract data for that characteristic
  all <- wqdata[wqdata$CharacteristicName==name,]
  
  # Remove NA values
  all <- all[!is.na(all$ResultMeasureValue),]
  
  # Find the low and high values
  low <- all[all$ResultMeasureValue<rmin,]
  high <- all[all$ResultMeasureValue>rmax,]
  
  # Add too low values to the data frame (if they exist)
  if (nrow(low)>0){
    odd_low <- data.frame(CharacteristicName = low$CharacteristicName,
                      ActivityIdentifier = low$ActivityIdentifier,
                      ResultMeasureValue = low$ResultMeasureValue,
                      ResultMeasure.MeasureUnitCode = low$ResultMeasure.MeasureUnitCode,
                      Reason_flagged = "Very low")
    odds <- rbind(odds, odd_low)
  }
  
  # Add too high values to the data frame (if they exist)
  if (nrow(high)>0){
    odd_high <- data.frame(CharacteristicName = high$CharacteristicName,
                          ActivityIdentifier = high$ActivityIdentifier,
                          ResultMeasureValue = high$ResultMeasureValue,
                          ResultMeasure.MeasureUnitCode = high$ResultMeasure.MeasureUnitCode,
                          Reason_flagged = "Very high")
    odds <- rbind(odds, odd_high)
  }
}

# Save as csv
write.csv(odds, "Abnormal water values 2005-2022.csv", row.names=F)

################################################################################
### Add column to EDD flagging records #########################################
################################################################################
# Create variable
wdata$`Flag (outside user specified boundary)` <- 0

# Assign flag to abnormal values
for (i in 1:nrow(odds)){
  wdata$`Flag (outside user specified boundary)`[
    wdata$ActivityIdentifier==odds$ActivityIdentifier[i] & 
    wdata$CharacteristicName==odds$CharacteristicName[i] &
    wdata$ResultMeasureValue==odds$ResultMeasureValue[i]] <- 1
}

# Add character results back in
wdata$ResultMeasureValue <- char_results

# Save to csv file
write.csv(wdata, paste(fileName,"_Flagged.csv",sep=""), na="", row.names=F)
