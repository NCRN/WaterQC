# Water QC - Characteristics across stream
# Dan Myers, 9/14/2023

library(dplyr)
library(tidyr)

# Read data
fileName <- "20230915_wqp_wqx_bss_wq_npsncrn" # Leave out .csv extension
wdata <- read.csv(paste(fileName,"_Flagged.csv",sep=""))
wqdata <- wdata[wdata$ProjectIdentifier=="USNPS NCRN Perennial stream water monitoring",]

################################################################################
### Calculate whether differences between number of characteristics#############
################################################################################

# Select only desired YSI characteristics
wqdata <- wqdata[wqdata$CharacteristicName %in% c(
  "Specific conductance", "Salinity", "Temperature, water", "Conductivity", "pH",
  "Dissolved oxygen saturation","Dissolved oxygen (DO)","Solids, Dissolved (TDS)"
),]

# Summarize characteristics
chars <- wqdata[,] %>%
  group_by(ActivityIdentifier) %>%
  group_by(ActivityMediaSubdivisionName, .add=T) %>%
  summarise(`Count of characteristics` = length(unique(CharacteristicName[ResultMeasureValue != ""]))) # All unique characteristics that have data

# Note measurement type
chars$Type <- substr(chars$ActivityIdentifier,30,100)
chars <- chars[chars$Type %in% c("NCRN_WQ_YSI|1st", "NCRN_WQ_YSI|2nd", "NCRN_WQ_YSI|3rd",
               "NCRN_WQ_YSI|4th", "NCRN_WQ_YSI|5th", "NCRN_WQ_YSI|6th",
               "NCRN_WQ_YSI|7th", "NCRN_WQ_YSI|8th"),]

# Put all on one row
pw <- pivot_wider(chars[2:length(chars)], names_from = "Type", 
                  values_from = "Count of characteristics")

# Identify where the number of characteristics at each YSI reading is not equal
pw$Equal_number_of_chars <- NA
for (i in 1:nrow(pw)){
  pw$Equal_number_of_chars[i] <- max(pw[i,2:9], na.rm=T) == min(pw[i,2:9], na.rm=T)
}

# Write csv
write.csv(pw, "YSI measurements across stream.csv",na="")

# It appears frequent that there will be different number of characteristics for each YSI measurement
# across the stream, primarily for conductivity and TDS, but also salinity.

################################################################################
### Calculate variability of across stream measurements ########################
################################################################################

# Convert to numeric
wqdata$ResultMeasureValue <- as.numeric(wqdata$ResultMeasureValue)

# Summarize variability
meas <- wqdata %>%
  group_by(CharacteristicName) %>%
  group_by(ActivityMediaSubdivisionName, .add=T) %>%
  summarise(min=min(ResultMeasureValue, na.rm=T), 
            max=max(ResultMeasureValue, na.rm=T),
            dif=(max(ResultMeasureValue, na.rm=T)-min(ResultMeasureValue, na.rm=T)),
            n=sum(ResultMeasureValue>-999, na.rm=T))

# Remove ones without multiple measurements
meas <- meas[meas$n>1,]

# Write to csv
# write.csv(meas, "Differences in measurements across stream.csv")

# Summarize variability
meas_sum <- meas %>%
  group_by(CharacteristicName) %>%
  summarise(median = median(dif),
            max = max(dif))
meas_sum

# Summarize characteristics
chars2 <- wqdata[,] %>%
  group_by(ActivityIdentifier) %>%
  group_by(ActivityMediaSubdivisionName, .add=T) %>%
  group_by(CharacteristicName,.add=T) %>%
  summarise(Result=ResultMeasureValue) # All unique characteristics that have data

# Note measurement type
chars2$Type <- substr(chars2$ActivityIdentifier,30,100)
chars2 <- chars2[chars2$Type %in% c("NCRN_WQ_YSI|1st", "NCRN_WQ_YSI|2nd", "NCRN_WQ_YSI|3rd",
                                 "NCRN_WQ_YSI|4th", "NCRN_WQ_YSI|5th", "NCRN_WQ_YSI|6th",
                                 "NCRN_WQ_YSI|7th", "NCRN_WQ_YSI|8th"),]

# Put all on one row
pw2 <- pivot_wider(chars2[2:length(chars2)], names_from = "Type", 
                  values_from = "Result")

# Add difference and number of measurements across stream
pw2$Max_difference <- NA
pw2$n <- NA
for ( i in 1:nrow(pw2)){
  pw2$Max_difference[i] <- max(pw2[i,3:10], na.rm=T) - min(pw2[i,3:10], na.rm=T)
  pw2$n[i] <- sum(!is.na(pw2[i,3:10]))
  
}

# Remove events with only one measurement
pw2 <- pw2[pw2$n>1,]

# Import user specified reasonable ranges
reas <- read.csv("User-specified reasonable variation in YSI measurements across stream.csv",header=T)

# Identify which events have a very large variation
large_var <- pw2
select_all <- data.frame()
for (i in 1:nrow(reas)){
  selection <- large_var[large_var$CharacteristicName==reas$CharacteristicName[i] & large_var$Max_difference > reas$Max_difference[i],]
  select_all <- rbind(select_all, selection)
}

# Summarize selection
select_all %>%
  group_by(CharacteristicName) %>%
  count()

# Write csv
write.csv(select_all, "Events having large variation in YSI's across stream.csv", na="", row.names = F)
