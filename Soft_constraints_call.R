# Soft Constraints Call
# Dan Myers, 12/18/2023

# Calls SoftConstraintsFunc.R and then formats the table.

# Load packages
library(rlang)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(brms)
library(tidybayes)
library(ggpubr)

# Read data
Water_Data<-read_csv("20231128_wqp_wqx_bss_wq_npsncrn.csv")

# Get site and characteristic names
site_names <- unique(Water_Data$MonitoringLocationIdentifier)
char_names <- unique(Water_Data$CharacteristicName)

# Select inputs
site <- "NCRN_GWMP_MIRU"
water_char <- "Specific conductance"
distribution = "gamma" # "gamma" or "gaussian"

# Run function
source("SoftConstraintsFunc.R")
Soft_Limits <- SoftConstraintsFunc(Water_Data, site, water_char, distribution)
