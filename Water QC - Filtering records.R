# Water QC - Filtering records.R
# Dan Myers, 10/12/2023

# This script filters water quality records.


################################################################################
### Format data ################################################################
################################################################################

# Load packages
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)

# Set working directory
setwd("R:/NPS_NCRN_VitalSigns/Analyses/Projects/New WQ data/")

# Read readme
# readLines("readme.txt")

# Read EDD data
fileName <- "20231003_wqp_wqx_bss_wq_npsncrn" # Leave out .csv extension
wdata <- read.csv(paste(fileName,".csv",sep=""))

# Read lab data
labDataNew <- read.xlsx("NCRN_Water_New_Nutrient_Data_20231002.xlsx")
# labDataNew <- read.csv("NCRN_Water_New_Nutrient_Data_20231002.csv")
labDataNew$Sample.Date <- as.Date(labDataNew$Sample.Date, origin="1899-12-30")
labDataNew$Received.Date <- as.Date(labDataNew$Received.Date, origin="1899-12-30")
labDataNew$Analysis.Date <- as.Date(labDataNew$Analysis.Date, origin="1899-12-30")

lab20160926 <- read.xlsx("NCRN CBL NPStoret Import_20160926_20161004.xlsx")
lab20160926$Sample.Date <- as.Date(lab20160926$Sample.Date, origin="1899-12-30")

lab20161017 <- read.xlsx("NCRN CBL NPStoret Import_20161017_20161018.xlsx")
lab20161017$Sample.Date <- as.Date(lab20161017$Sample.Date, origin="1899-12-30")

# Format dates as date
# wdata$ActivityStartDate <- as.Date(wdata$ActivityStartDate)

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

################################################################################
### Extract WQ data#############################################################
################################################################################

# Select WQ data by project identifier
wqdata <- wdata[wdata$ProjectIdentifier=="USNPS NCRN Perennial stream water monitoring",]

# Choose WQ columns
wqdata <- wqdata[c("ActivityStartDate", "MonitoringLocationIdentifier", "CharacteristicName",
                   "ResultMeasureValue", "MeasureQualifierCode", "ResultValueTypeName",
                   "LaboratoryName")]

# Choose lab columns
labDataNew <- labDataNew[c("New.LocID", "Sample.Date", "Received.Date", "Analysis.Date", "Parameter", "Result")]
labDataNew$Result <- round(as.numeric(labDataNew$Result),4)

################################################################################
### Filter WQ data #############################################################
################################################################################

# Problem 1: TP and TN records for 3 sites are duplicated 2018-06-25 and 2018-06-26.
#            However, only the 06-26 ones all match the lab report. And, the 06-25 ones were sent to the lab in June 2019, a year later.
wqdata %>% filter(MonitoringLocationIdentifier %in% c("NCRN_NACE_HECR", "NCRN_NACE_OXRU", "NCRN_NACE_STCK") &
                  ActivityStartDate=="2018-06-25" &
                  CharacteristicName %in% c("Total Phosphorus, mixed forms", "Total Nitrogen, mixed forms"))

wqdata %>% filter(MonitoringLocationIdentifier %in% c("NCRN_NACE_HECR", "NCRN_NACE_OXRU", "NCRN_NACE_STCK") &
                    ActivityStartDate=="2018-06-26" &
                    CharacteristicName %in% c("Total Phosphorus, mixed forms", "Total Nitrogen, mixed forms"))

labDataNew %>% filter(New.LocID %in% c("NCRN_NACE_HECR", "NCRN_NACE_OXRU", "NCRN_NACE_STCK") &
                      Sample.Date == "2018-06-25")

# Upon looking at June 2019 records, it seems that there were no TN or TP for those NACE
# sites that were monitored 2019-06-25. Thus, I assume that those NACE samples from
# 2018-06-25 actually belong in 2019-06-25

labDataNew %>% filter(New.LocID %in% c("NCRN_NACE_HECR", "NCRN_NACE_OXRU", "NCRN_NACE_STCK") &
                        Sample.Date == "2019-06-25" &
                        Parameter %in% c("TN","TP"))

# Solution 1: Move the 2018-06-25 TN and TP records for HECR, OXRU, and STCK to 2019-06-25.


################################################################################

# Problem 2: 2016-09-26 is a ROCR day but has TN and TP for CATO_BLBZ.
wqdata %>% filter(MonitoringLocationIdentifier %in% c("NCRN_CATO_BLBZ") &
                    ActivityStartDate=="2016-09-26" &
                    CharacteristicName %in% c("Total Phosphorus, mixed forms", "Total Nitrogen, mixed forms"))

wqdata %>% filter(MonitoringLocationIdentifier %in% c("NCRN_CATO_BLBZ") &
                    ActivityStartDate=="2016-10-04" &
                    CharacteristicName %in% c("Total Phosphorus, mixed forms", "Total Nitrogen, mixed forms"))

lab20160926 %>% filter(Sample.ID %in% c("NCRN_CATO_WHST") &
                        Sample.Date == "2016-10-04")
# Solution 2: Delete the 2016-09-26 CATO_BLBZ TN and TP records, as they are already in 2016-10-04.


################################################################################

# Problem 3: 2020-11-04 is a Western sites day but has ANC, TN, and TP data for NACE_HECR and NACE_STCK.
wqdata %>% filter(MonitoringLocationIdentifier %in% c("NCRN_NACE_HECR", "NCRN_NACE_STCK") &
                    ActivityStartDate=="2020-11-04" &
                    CharacteristicName %in% c("Total Phosphorus, mixed forms", "Total Nitrogen, mixed forms", "Acid Neutralizing Capacity (ANC)"))

# NACE_HECR and NACE_STCK were monitored 2020-10-28, but lack ANC, TN, and TP data
wqdata %>% filter(MonitoringLocationIdentifier %in% c("NCRN_NACE_HECR", "NCRN_NACE_STCK") &
                    ActivityStartDate=="2020-10-28" &
                    CharacteristicName %in% c("Total Phosphorus, mixed forms", "Total Nitrogen, mixed forms", "Acid Neutralizing Capacity (ANC)"))

# The lab results had the wrong date
labDataNew %>% filter(New.LocID %in% c("NCRN_NACE_HECR", "NCRN_NACE_STCK") &
                        Sample.Date == "2020-11-04" &
                        Parameter %in% c("TN","TP", "ANC"))

# Solution 3: Move 2020-11-04 ANC, TP, and TN for NACE_HECR and NACE_STCK to 2020-10-28


################################################################################

# Problem 4: 2017-10-16 was a ROCR day but TN and TP field duplicate data for GWMP_MIRU. 
#            The MIRU TP duplicate is already entered in 2017-10-17.
wqdata %>% filter(MonitoringLocationIdentifier %in% c("NCRN_GWMP_MIRU") &
                    ActivityStartDate=="2017-10-16" &
                    CharacteristicName %in% c("Total Phosphorus, mixed forms", "Total Nitrogen, mixed forms"))

wqdata %>% filter(MonitoringLocationIdentifier %in% c("NCRN_GWMP_MIRU") &
                    ActivityStartDate=="2017-10-17" &
                    CharacteristicName %in% c("Total Phosphorus, mixed forms", "Total Nitrogen, mixed forms"))

# The lab import correctly says 10-17. However, these somehow ended up in the EDD database as 10-16.
# Plus, there is an 3rd CBL record in the database for 10-17 (TP=0.0158) which doesn't exist in the lab import.
labDataNew %>% filter(New.LocID %in% c("NCRN_GWMP_MIRU") &
                        Sample.Date == "2017-10-17" &
                        Parameter %in% c("TN","TP"))

# Though there is a 2017-10-17 MIRU TP lab dup =0.0158 in the raw CBL data (NPS_NCRN_101917.xlsx), as well as a MIRU TN=0.77 lab dup.

# Solution 4: The best I can come up with would be to remove the 2017-10-16 MIRU TN and TP from the EDD.
#             Then, remove the 2017-10-17 MIRU TP=0.0158 from the EDD. Though there could be broader
#             questions for consistency of when and how lab dup's are included.


################################################################################

# Problem 5: 2016-10-18 was a PRWI day but with TN and TP for GWMP_MIRU. 
#            The correct day for these is 2016-10-17, but they’re already there.
wqdata %>% filter(MonitoringLocationIdentifier %in% c("NCRN_GWMP_MIRU") &
                    ActivityStartDate=="2016-10-18" &
                    CharacteristicName %in% c("Total Phosphorus, mixed forms", "Total Nitrogen, mixed forms"))

wqdata %>% filter(MonitoringLocationIdentifier %in% c("NCRN_GWMP_MIRU") &
                    ActivityStartDate=="2016-10-17" &
                    CharacteristicName %in% c("Total Phosphorus, mixed forms", "Total Nitrogen, mixed forms"))

lab20161017 %>% filter(Sample.ID %in% c("NCRN_GWMP_MIRU") &
                        Sample.Date == "2016-10-17")

# Solution 5: Delete the 2018-10-18 MIRU TN and TP from the EDD.


################################################################################

# Problem 6: 2017-04-17 appears to be a NACE day but with ROCR_PHBR and ROCR_ROC3 TN and TP data.
#            These data already exist correctly in 2017-04-18.
wqdata %>% filter(MonitoringLocationIdentifier %in% c("NCRN_ROCR_PHBR", "ROCR_ROC3") &
                    ActivityStartDate=="2017-04-17"  &
                    CharacteristicName %in% c("Total Phosphorus, mixed forms", "Total Nitrogen, mixed forms"))

wqdata %>% filter(MonitoringLocationIdentifier %in% c("NCRN_ROCR_PHBR", "ROCR_ROC3") &
                    ActivityStartDate=="2017-04-18"  &
                    CharacteristicName %in% c("Total Phosphorus, mixed forms", "Total Nitrogen, mixed forms"))

# Solution 6: The 2017-04-17 TN and TP data for PHBR and ROCR can be deleted.

################################################################################

# Problem 7 - 2018-06-11 is a Western sites day but with TN and TP for ROCR_LUBR.
wqdata %>% filter(MonitoringLocationIdentifier %in% c("NCRN_ROCR_LUBR") &
                    ActivityStartDate=="2018-06-11"  &
                    CharacteristicName %in% c("Total Phosphorus, mixed forms", "Total Nitrogen, mixed forms"))

wqdata %>% filter(MonitoringLocationIdentifier %in% c("NCRN_ROCR_LUBR") &
                    ActivityStartDate=="2018-06-12"  &
                    CharacteristicName %in% c("Total Phosphorus, mixed forms", "Total Nitrogen, mixed forms"))

labDataNew %>% filter(New.LocID %in% c("NCRN_ROCR_LUBR") &
                        Sample.Date == "2018-06-12" &
                        Parameter %in% c("TN","TP"))
# Solution 7: 2018-06-11 TN and TP data for LUBR can be deleted because they already exist correctly in 2016-06-12.
# I'm starting to see a pattern where data from the "CBL QAQC" tab of the FY2017-2018 NPstoret reports are entered
# in the wrong sample date that doesn't match their sampling event (but match another day in the report). This
# may be because that tab provides general, rather than site-specific, sample dates.
# Instead of going through these 1-by-1, I wonder if there's a way to batch re-enter them or remove incorrect ones.

################################################################################

# Problem 8 - 2017-04-18 is a ROCR day but with TN and TP for NACE_STCK
wqdata %>% filter(MonitoringLocationIdentifier %in% c("NCRN_NACE_STCK") &
                    ActivityStartDate=="2017-04-18"  &
                    CharacteristicName %in% c("Total Phosphorus, mixed forms", "Total Nitrogen, mixed forms"))

wqdata %>% filter(MonitoringLocationIdentifier %in% c("NCRN_NACE_STCK") &
                    ActivityStartDate=="2017-04-17"  &
                    CharacteristicName %in% c("Total Phosphorus, mixed forms", "Total Nitrogen, mixed forms"))

# Solution 8 - Remove NACE_STCK TN and TP from 2017-04-18. Also, this is related
#              to Problem #6 and the QAQC mis-dating mentioned above.

################################################################################

# Problem 9: 2017-07-24 - Has a mix of GWMP_PIRU YSI and lab; NACE OXRU TN and TP; 
# YSI and lab for ROCR_NOST, ROCR_DUOA, ROCR_ROC3; and TN and TP for ROCR_PHBR.
wqdata %>% filter(ActivityStartDate=="2017-07-24"  &
                    CharacteristicName %in% c("Total Phosphorus, mixed forms", "Total Nitrogen, mixed forms"))
# Solution: I'll have to see the PDF datasheets for this one. I'm not sure which
# day each belong. The lab results in "NCRN CBL NPStoret Import_20170717_20170718_20170725"
# have mixed dates too. It appears the NACE_OXRU ones can be deleted because they
# exist in 2017-07-18.

################################################################################

# Problem 10: 2017-07-17 - Likely related to the above (2017-07-24), this is a largely 
# ROCR-dominated sample but with TN and TP records for NACE_OXRU.
wqdata %>% filter(MonitoringLocationIdentifier %in% c("NCRN_NACE_OXRU") &
                    ActivityStartDate=="2017-07-17"  &
                    CharacteristicName %in% c("Total Phosphorus, mixed forms", "Total Nitrogen, mixed forms"))

wqdata %>% filter(MonitoringLocationIdentifier %in% c("NCRN_NACE_OXRU") &
                    ActivityStartDate=="2017-07-18"  &
                    CharacteristicName %in% c("Total Phosphorus, mixed forms", "Total Nitrogen, mixed forms"))
# Solution: The rest of the YSI data for NACE_OXRU are on 2017-07-18, so I believe
# these lab QC records belong there. However, they are already there, so the OXRU
# TN and TP for 2017-07-17 can be deleted.

################################################################################

# Problem 11: 2018-04-10 is a ROCR day but with TN and TP for NACE_OXRU. The OXRU
# data already exist in 2018-04-09
wqdata %>% filter(MonitoringLocationIdentifier %in% c("NCRN_NACE_OXRU") &
                    ActivityStartDate=="2018-04-10"  &
                    CharacteristicName %in% c("Total Phosphorus, mixed forms", "Total Nitrogen, mixed forms"))

labDataNew %>% filter(New.LocID %in% c("NCRN_NACE_OXRU") &
                        Sample.Date == "2018-04-09" &
                        Parameter %in% c("TN","TP"))

# Solution: The 04-10 samples are lab QAQC spikes that were entered in the wrong 
# date, following the same pattern as most of the above. Should we just remove
# lab QAQC data from the database?

################################################################################

# Problem 12: 2017-01-30 is a PRWI day but with GWMP_TURU TN and TP.
wqdata %>% filter(MonitoringLocationIdentifier %in% c("NCRN_GWMP_TURU") &
                    ActivityStartDate=="2017-01-30"  &
                    CharacteristicName %in% c("Total Phosphorus, mixed forms", "Total Nitrogen, mixed forms"))
# Solution: These are lab QAQC spike data entered in the wrong date. Since so many
# errors are because of this, it may make sense to just remove the lab QC data from
# the dataset, since it's kinda extra, and then run the missing data check again.
# This could allow more precise identification of other errors.