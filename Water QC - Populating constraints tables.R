# Water QC - Populating constraints tables
# Dan Myers, 11/29/2023

# This script populates a SoftConstraints.csv table with low and high values
# for each site. Users can specify the desired quantiles for each. It requires
# the blank SoftConstraints.csv, EDD, and Lookup.csv (on GitHub) as inputs.

################################################################################
### Format data ################################################################
################################################################################

# Load packages
library(tidyr)
library(dplyr)
library(lubridate)

# Read data
fileName <- "20231128_wqp_wqx_bss_wq_npsncrn" # Leave out .csv extension
wdata <- read.csv(paste(fileName,".csv",sep=""))

# Format dates as date
wdata$ActivityStartDate <- as.Date(wdata$ActivityStartDate)
wdata$Month <- month(wdata$ActivityStartDate)

# Save character results for later
char_results <- wdata$ResultMeasureValue

# Format result as numeric
wdata$ResultMeasureValue <- as.numeric(wdata$ResultMeasureValue)

# Open soft constraints file
s_con <- read.csv("SoftConstraints.csv")

# Open Lookup table
lookup <- read.csv("Lookup.csv")

################################################################################
### Extract WQ data#############################################################
################################################################################

# Select WQ data by project identifier
wqdata <- wdata[wdata$ProjectIdentifier=="USNPS NCRN Perennial stream water monitoring",]


################################################################################
### Identify low and high values ###############################################
################################################################################

# Summarize sites and characteristics
low <- wqdata %>%
  group_by(MonitoringLocationIdentifier) %>%
  group_by(CharacteristicName, .add=T) %>%
  summarise(low=quantile(ResultMeasureValue,0.05,na.rm=T))

high <- wqdata %>%
  group_by(MonitoringLocationIdentifier) %>%
  group_by(CharacteristicName, .add=T) %>%
  summarise(high=quantile(ResultMeasureValue,0.95,na.rm=T))


# Remove NAs and extra choice list characteristics
low <- low[!is.na(low$low),]
low <- low[!low$CharacteristicName %in% c(
  "Weather Condition (WMO Code 4501) (choice list)",
  "RBP2, Low G, Riparian Vegetative Zone Width, Left Bank (choice list)",
  "RBP2, Low G, Riparian Vegetative Zone Width, Right Bank (choice list)"),]

high <- high[!is.na(high$high),]
high <- high[!high$CharacteristicName %in% c(
  "Weather Condition (WMO Code 4501) (choice list)",
  "RBP2, Low G, Riparian Vegetative Zone Width, Left Bank (choice list)",
  "RBP2, Low G, Riparian Vegetative Zone Width, Right Bank (choice list)"),]

pw_low <- low %>%
  pivot_wider(names_from="CharacteristicName", values_from="low")

pw_high <- high %>%
  pivot_wider(names_from="CharacteristicName", values_from="high")


################################################################################
### Match columns ##############################################################
################################################################################

# Extract column names
low_cn <- data.frame(CharacteristicName = colnames(pw_low))
high_cn <- data.frame(CharacteristicName = colnames(pw_high))

# Lookup constraint names
low_cn <- left_join (low_cn, lookup[lookup$Type=="low",], by="CharacteristicName")
high_cn <- left_join (high_cn, lookup[lookup$Type=="high",], by="CharacteristicName")

# Rename columns
colnames(pw_low) <- low_cn$SoftConstraints_name
colnames(pw_low)[1] <- "MonitoringLocationIdentifier"
colnames(pw_high) <- high_cn$SoftConstraints_name
colnames(pw_high)[1] <- "MonitoringLocationIdentifier"

# Remove NA columns
pw_low <- pw_low[,!is.na(colnames(pw_low))]
pw_high <- pw_high[,!is.na(colnames(pw_high))]

# Join together
lowHigh <- left_join(pw_low, pw_high, by="MonitoringLocationIdentifier")
colnames(lowHigh)[1] <- "Location_ID"

# Add turbidity columns (if needed)
if (!"turbidity_soft_constraint_low" %in% colnames(lowHigh)){
  lowHigh[,length(lowHigh)+1] <- NA
  colnames(lowHigh)[length(lowHigh)] <- "turbidity_soft_constraint_low"
  lowHigh[,length(lowHigh)+1] <- NA
  colnames(lowHigh)[length(lowHigh)] <- "turbidity_soft_constraint_high"
}

# Sort columns
sorted <- lowHigh[,colnames(s_con)]

# Fill soft constraints table
sites <- data.frame(Location_ID = s_con[,1])
s_con_filled <- left_join(sites, sorted, by="Location_ID")

# Write as csv
write.csv(s_con_filled,"SoftConstraints-filled.csv",row.names=F, quote=F)
