# Water QC - values outside quantiles
# Dan Myers, 9/13/2023

# This script flags water data results that are outside the quantiles for
# each site and characteristic


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
wdata <- read.csv(paste(fileName,"_Flagged.csv",sep=""))

# Format dates as date
wdata$ActivityStartDate <- as.Date(wdata$ActivityStartDate)
wdata$Month <- month(wdata$ActivityStartDate)

# Save character results for later
char_results <- wdata$ResultMeasureValue

# Format result as numeric
wdata$ResultMeasureValue <- as.numeric(wdata$ResultMeasureValue)


################################################################################
### Extract WQ data#############################################################
################################################################################

# Select WQ data by project identifier
wqdata <- wdata[wdata$ProjectIdentifier=="USNPS NCRN Perennial stream water monitoring",]


################################################################################
### Identify samples outside 20-80 quantiles####################################
################################################################################

# Summarize sites and characteristics
sites_chars <- wqdata %>%
  group_by(MonitoringLocationIdentifier) %>%
  group_by(CharacteristicName, .add=T) %>%
  group_by(Month, .add=T) %>%
  summarise(Q05=quantile(ResultMeasureValue,0.05,na.rm=T),
            Q10=quantile(ResultMeasureValue,0.1,na.rm=T),
            Q20=quantile(ResultMeasureValue,0.2,na.rm=T), 
            Q80=quantile(ResultMeasureValue,0.8,na.rm=T),
            Q90=quantile(ResultMeasureValue,0.9,na.rm=T),
            Q95=quantile(ResultMeasureValue,0.95,na.rm=T))

# Remove NAs and extra choice list characteristics
sites_chars <- sites_chars[!is.na(sites_chars$Q20),]
sites_chars <- sites_chars[!sites_chars$CharacteristicName %in% c(
  "Weather Condition (WMO Code 4501) (choice list)",
  "RBP2, Low G, Riparian Vegetative Zone Width, Left Bank (choice list)",
  "RBP2, Low G, Riparian Vegetative Zone Width, Right Bank (choice list)"),]


# Save as csv
write.csv(sites_chars, "Quantiles for water values 2005-2022.csv", row.names=F)

################################################################################
### Add column to EDD flagging records #########################################
################################################################################
# Create variable
wdata$`Flag (outside 20-80 quantiles)` <- 0
wdata$`Flag (outside 10-90 quantiles)` <- 0
wdata$`Flag (outside 05-95 quantiles)` <- 0

# Assign flag to abnormal values
for (i in 1:nrow(sites_chars)){
  wdata$`Flag (outside 20-80 quantiles)`[
    wdata$MonitoringLocationIdentifier==sites_chars$MonitoringLocationIdentifier[i] & 
    wdata$CharacteristicName==sites_chars$CharacteristicName[i] &
    wdata$Month==sites_chars$Month[i] &
    (wdata$ResultMeasureValue < sites_chars$Q20[i] | wdata$ResultMeasureValue > sites_chars$Q80[i])] <- 1

  wdata$`Flag (outside 10-90 quantiles)`[
    wdata$MonitoringLocationIdentifier==sites_chars$MonitoringLocationIdentifier[i] & 
      wdata$CharacteristicName==sites_chars$CharacteristicName[i] &
      wdata$Month==sites_chars$Month[i] &
      (wdata$ResultMeasureValue < sites_chars$Q10[i] | wdata$ResultMeasureValue > sites_chars$Q90[i])] <- 1
}

# Add character results back in
wdata$ResultMeasureValue <- char_results

# Save to csv file
write.csv(wdata, paste(fileName,"_Flagged.csv",sep=""), na="", row.names=F)
