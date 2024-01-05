# Land cover and water quality figure with EDD dataset
# Dan Myers, 9/8/2023

################################################################################
### Format data ################################################################
################################################################################

# Load packages
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(raster)
library(trend)
library(mgcv)
library(ggplot2)
library(viridis)

# Source functions
source("Functions.R")

# Read data
fileName <- "20231128_wqp_wqx_bss_wq_npsncrn" # Leave out .csv extension
wdata <- read.csv(paste(fileName,".csv",sep=""))

# Format dates as date
wdata$ActivityStartDate <- as.Date(wdata$ActivityStartDate)

# Save character results for later
char_results <- wdata$ResultMeasureValue

# Format result as numeric
wdata$ResultMeasureValue <- as.numeric(wdata$ResultMeasureValue)

# Remove extra rows
wdata <- wdata[wdata$ProjectIdentifier=="USNPS NCRN Perennial stream water monitoring",]
wdata <- wdata[!is.na(wdata$ResultMeasureValue),]
wdata <- wdata[!wdata$CharacteristicName %in% c("Chlorine",
  "Weather Condition (WMO Code 4501) (choice list)",                      
  "RBP2, Low G, Riparian Vegetative Zone Width, Left Bank (choice list)", 
  "RBP2, Low G, Riparian Vegetative Zone Width, Right Bank (choice list)"),]

# Take average of multiple measurements across stream
wdata_avgd <- wdata %>%
  group_by(MonitoringLocationIdentifier) %>%
  group_by(ActivityStartDate, .add=T) %>%
  group_by(CharacteristicName, .add=T) %>%
  summarise(ResultMeasureValue = median(ResultMeasureValue, na.rm=T)) # Changed from mean to median

# Remove sites not currently monitored
current_sites <- wdata_avgd %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarise(start_date = min(ActivityStartDate, na.rm=T),
    end_date = max(ActivityStartDate, na.rm=T))
current_sites <- current_sites[year(current_sites$end_date)==2023,]
wdata_avgd <- wdata_avgd[wdata_avgd$MonitoringLocationIdentifier %in% current_sites$MonitoringLocationIdentifier,]

################################################################################
### Calculate average number of measurements per site###########################
################################################################################

# Summarize measurements per site
meas_per_site <- wdata_avgd %>%
  group_by(CharacteristicName) %>%
  group_by(MonitoringLocationIdentifier, .add=T) %>%
  summarise(n=sum(ResultMeasureValue>-1000,na.rm=T))

# Summarize measurements per characteristic
meas_per_site %>%
  group_by(CharacteristicName) %>%
  summarise(Mean = mean(n, na.rm=T), StDev = sd(n, na.rm=T))

################################################################################
### Analyze specific conductance ###############################################
################################################################################

# Load background conditions and percent protected
back_sc <- read.csv("NCRN_Monitoring_Locations_with_background_SC_2023_12_21.csv")
shed_sums <- read.csv("Watershed conditions 2023_09_19.csv") %>%
  rename(MonitoringLocationIdentifier=NEW_IMLOCI)

# Remove extra sites (Make sure the field and site names are up-to-date)
back_sc$MonitoringLocationIdentifier <- back_sc$IMLOCID
back_sc <- back_sc[back_sc$MonitoringLocationIdentifier %in% current_sites$MonitoringLocationIdentifier,]
back_sc <- back_sc[!duplicated(back_sc$IMLOCID),]
shed_sums <- shed_sums[shed_sums$MonitoringLocationIdentifier %in% current_sites$MonitoringLocationIdentifier,]

CharacteristicName <- "Specific conductance"

outputs <- medians_func(current_sites, wdata_avgd, CharacteristicName, back_sc, shed_sums)


ylabs <- "Specific cond. (uS/cm)"
ylim <- 1600
threshold <- 171

plotting_func(wdata_avgd, outputs, ylabs,ylim, threshold, CharacteristicName)

################################################################################
################################################################################
################################################################################

# # Water quality and urbanization trends figure with EDD dataset
# # Dan Myers, 9/10/2023
# 
# # Summarise by site and year
# wq_annual <- wdata_avgd[wdata_avgd$CharacteristicName==CharacteristicName,] %>%
#   group_by(CharacteristicName, .add=FALSE) %>%
#   group_by(MonitoringLocationIdentifier, .add=TRUE) %>%
#   group_by(year(ActivityStartDate), .add=TRUE) %>%
#   summarise(annual_avg = median(as.numeric(ResultMeasureValue), na.rm=T), 
#             n=sum(as.numeric(ResultMeasureValue)>-1000, na.rm=T)) %>%
#   rename(year_wq=`year(ActivityStartDate)`) %>%
#   arrange(MonitoringLocationIdentifier, year_wq)
# 
# # Calculate Sen's slopes
# sens_slopes <- wq_annual %>%
#   group_by(MonitoringLocationIdentifier) %>%
#   summarise(sen_slope = sens.slope(annual_avg)$estimates,
#             z = sens.slope(annual_avg)$statistic,
#             mk_p = sens.slope(annual_avg)$p.value,
#             n=sens.slope(annual_avg)$parameter)
# 
# ### Join spc with built areas
# # Attempt it with increase only
# # lc_chg[lc_chg<0] = 0 # Since even increase of existing urban area's intensity is development
# lc_chg <- read.csv("watersheds_nlcd06-19_change_2023_09_15.csv") %>% na.omit()
# 
# sheds_nlcd06 <- read.csv("watersheds_nlcd06_stats_2023_09_15.csv") %>% na.omit()
# sheds_nlcd19 <- read.csv("watersheds_nlcd19_stats_2023_09_15.csv") %>% na.omit()
# sheds <- st_read("NCRN_watersheds_2023_07_06.shp")
# 
# # Calculate total urban change (expansion)
# urb_df <- data.frame(MonitoringLocationIdentifier=lc_chg$X, urb_chg=rowSums(lc_chg[,4:7]))
# 
# # Calculate urban intensification (infill)
# inf <- data.frame(MonitoringLocationIdentifier=as.numeric(lc_chg$X))
# for (i in 1:nrow(lc_chg[,4:7])){
#   for (j in 1:ncol(lc_chg[,4:7])){
#     if ((lc_chg[,4:7]>0)[i,j]){
#       inf[i,j] <- lc_chg[,4:7][i,j]
#     }
#   }
# }
# urb_df$urb_inf <- rowSums(inf[],na.rm=T)
# 
# # Calculate combined urbanization rate (expansion + infill)
# urb_df$urb_com <- urb_df$urb_chg + urb_df$urb_inf
# 
# # Calculate total urban (avg of years)
# urb_df$urb_tot <- rowSums((sheds_nlcd06[,4:7]+sheds_nlcd19[,4:7])/2)
# 
# # Make data frame
# urb_df[,2:5] <- round(urb_df[,2:5],3)
# urb_df <- urb_df[urb_df$MonitoringLocationIdentifier %in% current_sites$MonitoringLocationIdentifier,]
# 
# # Join LULC with WQ
# spc_join <- left_join(x=sens_slopes, y=urb_df, by="MonitoringLocationIdentifier")
# 
# # Plot it
# trends_func(spc_join)

################################################################################
################################################################################
################################################################################
# Proportion of streams with median SC above the critical value, by park.
data.frame(name=substr(outputs$MonitoringLocationIdentifier,6,9), 
                    medians=outputs$medians) %>%
  group_by(name) %>%
  summarise(pct_above = (sum(medians>threshold) / length(medians)*100))

################################################################################
### Analyze salinity ########### ###############################################
################################################################################

# Extract salinity
CharacteristicName2 <- "Salinity"

outputs2 <- medians_func(current_sites, wdata_avgd, CharacteristicName2, back_sc, shed_sums)


ylabs2 <- "Salinity (ppt)"
ylim2 <- 12
threshold2 <- 0.5

boxplot_func(wdata_avgd, outputs2, ylabs2,ylim2, threshold2, CharacteristicName2)


################################################################################
### Road salt analysis##########################################################
################################################################################

# Turn-off scientific notation
options(scipen=999)  

# Read data
salt_all <- read.csv("Watershed road salt annual.csv")

# Remove inactive sites
salt <- salt_all[salt_all$NEW_IMLOCI %in% current_sites$MonitoringLocationIdentifier,]

### Comparison plots
# Plot it
windows(6.5, 6.5)
par(mar=c(6,4,1,1), mgp=c(2,1,0), mfrow=c(2,1))

# Set up data
box_data_raw <- t(salt[,17:31]) *0.453592 # Convert lbs to kg
colnames(box_data_raw) <- substr(current_sites$MonitoringLocationIdentifier,6,14)

# Order data
meds <- rep(NA,length(current_sites$MonitoringLocationIdentifier))
for (j in 1:length(meds)){
  meds[j] <- median(box_data_raw[,j], na.rm=T) 
}
box_data <- box_data_raw[,order(meds)]

# Make box plot
boxplot(box_data/10000, las=2, ylab="salt x10^5 kg / km2 / yr", cex.axis=0.75)
grid()
boxplot(box_data/10000, las=2, add=T, cex.axis=0.75)
title("a)",adj=0.01, line=-1.2, cex.main=1.5)


### Scatterplots with WQ data
# Calculate 2005-2019 avg
avg_salt <- data.frame(MonitoringLocationIdentifier=substr(current_sites$MonitoringLocationIdentifier,1,14), avg_lbs = rowMeans(salt[,17:31]), area_km = salt$Area_km)
avg_salt$avg_kg = avg_salt$avg*0.453592

# Join data
plot_data <- left_join(avg_salt,outputs, by="MonitoringLocationIdentifier")
plot_data$avg_kg <- plot_data$avg_kg / 10000

# Remove unneeded site and order
plot_data <- plot_data[plot_data$MonitoringLocationIdentifier != "NCRN_GWMP_SPRU",]
plot_data <- plot_data[order(plot_data$medians),]

# Make plot
par(mar=c(3,4,0,1))
plot(plot_data$avg_kg, plot_data$medians, xlab="salt x10^5 kg / km2 / yr", 
     ylab = "Sp. Cond. (uS/cm)", type="n",ylim=c(0,max(plot_data$q90th, na.rm=T)))
grid()
points(plot_data$avg_kg, plot_data$q1, col="blue")
points(plot_data$avg_kg, plot_data$medians, col="orange")
points(plot_data$avg_kg, plot_data$q3, col="red")
points(plot_data$avg_kg, plot_data$q90th, col="darkred")

# Add models
y = plot_data$q1
x = plot_data$avg_kg
model <- lm(y ~ x+I(x^2))
myPredict <- predict( model ) 
ix <- sort(x,index.return=T)$ix
lines(x[ix], myPredict[ix], col="blue", lwd=1 )  
summary(model)

y = plot_data$medians
x = plot_data$avg_kg
model <- lm(y ~ x+I(x^2))
myPredict <- predict( model ) 
ix <- sort(x,index.return=T)$ix
lines(x[ix], myPredict[ix], col="orange", lwd=1 )  
summary(model)

y = plot_data$q3
x = plot_data$avg_kg
model <- lm(y ~ x+I(x^2))
myPredict <- predict( model ) 
ix <- sort(x,index.return=T)$ix
lines(x[ix], myPredict[ix], col="red", lwd=1 )  
summary(model)

y = plot_data$q90th
x = plot_data$avg_kg
model <- lm(y ~ x+I(x^2))
myPredict <- predict( model ) 
ix <- sort(x,index.return=T)$ix
lines(x[ix], myPredict[ix], col="darkred", lwd=1 )  
summary(model)

# Add legend
legend("bottomright", legend=c("Extreme (Q90)", "Third quartile", "Median", "First quartile"),
       col=c("darkred","red","orange","blue"), pch=1, bg="white", cex=0.8)
title("b)",adj=0.01, line=-1.2, cex.main=1.5)


### Time series plots
# Plot time series (1-20)
windows(15,10)
par(mfrow=c(5,4), mar=c(3,4,3,1))
for (i in 1:20){
  plot(1992:2019,t(salt[i,4:31]),
       xlab="", ylab="lbs salt / km2",
       main=salt[i,1], type='l')
}

# Plot time series (21-37)
windows(15,10)
par(mfrow=c(5,4), mar=c(3,4,3,1))
for (i in 21:37){
  plot(1992:2019,t(salt[i,4:31]),
       xlab="", ylab="lbs salt / km2",
       main=salt[i,1], type='l')
}

################################################################################
### Make supplementary SC time series###########################################
################################################################################

# Make plots
# sc_series_func(wdata_avgd, current_sites, 1, CharacteristicName)
# sc_series_func(wdata_avgd, current_sites, 2, CharacteristicName)
# sc_series_func(wdata_avgd, current_sites, 3, CharacteristicName)



################################################################################
### Output watershed conditions table###########################################
################################################################################
# Extract current sites
shed_sums_current <- shed_sums[shed_sums$MonitoringLocationIdentifier %in% current_sites$MonitoringLocationIdentifier,]

# Create watershed conditions data frame
wat <- data.frame(
  Site_name = substr(shed_sums_current$MonitoringLocationIdentifier,6,14),
  Trees = round(shed_sums_current$Watershed.Percent.Forest..DW.,0),
  Crops = round(shed_sums_current$Watershed.Percent.Agriculture..DW.,0),
  Built = round(shed_sums_current$Watershed.Percent.Urban..DW.,0),
  Protect. = round(shed_sums_current$Prot_area_pct..PAD.,0),
  Area = round(shed_sums_current$Area_km,1)) %>%
  arrange(Site_name)

# Write to csv
# write.csv(wat, "Watershed conditions table 1.csv", row.names=F)


################################################################################
### Count number of measurements per site#######################################
################################################################################
# Extract data (SC)
data1 <- wdata_avgd[wdata_avgd$MonitoringLocationIdentifier %in% current_sites$MonitoringLocationIdentifier &
             wdata_avgd$CharacteristicName ==CharacteristicName,]

# Summarize data (SC)
data1_sum <- data1 %>%
  group_by(MonitoringLocationIdentifier)%>%
  summarise(n = sum(ResultMeasureValue>-999, na.rm=T))

# Calculate mean and SD (SC)
mean(data1_sum$n)
sd(data1_sum$n)


# Extract data (Salinity)
data2 <- wdata_avgd[wdata_avgd$MonitoringLocationIdentifier %in% current_sites$MonitoringLocationIdentifier &
                      wdata_avgd$CharacteristicName ==CharacteristicName2,]

# Summarize data (Salinity)
data2_sum <- data2 %>%
  group_by(MonitoringLocationIdentifier)%>%
  summarise(n = sum(ResultMeasureValue>-999, na.rm=T))

# Calculate mean and SD (Salinity)
mean(data2_sum$n)
sd(data2_sum$n)


################################################################################
### GAMs #######################################################################
################################################################################
CharacteristicName <- "Specific conductance" 

for (plot_num in 1:3){
  # Select site groups and plot size
  if (plot_num==1){
    site_nums <- 1:15
    windows(6.5,7.5)
    par(mfrow=c(5,3),mgp=c(2,1,0),mar=c(3,3,2,1))
  } else if (plot_num==2){
    site_nums <- 16:30
    windows(6.5,7.5)
    par(mfrow=c(5,3),mgp=c(2,1,0),mar=c(3,3,2,1))
  } else if (plot_num==3){
    site_nums <- 31:37
    windows(6.5,4.5)
    par(mfrow=c(3,3),mgp=c(2,1,0),mar=c(3,3,2,1))
  }
  
  # Get site names
  sites_unique <- unique(current_sites$MonitoringLocationIdentifier)
  sites_unique <- sites_unique[sites_unique!="NCRN_GWMP_SPRU"]
  
  # Start ticker
  ticker <- 1
  
  # Start loop
  for (i in site_nums){
    
    ### Extract a time series
    # Choose site
    site <- sites_unique[i]
    
    # Query time series
    site_data <- wdata_avgd[wdata_avgd$MonitoringLocationIdentifier == site &
                              wdata_avgd$CharacteristicName == CharacteristicName,]
    
    # Format outputs
    time_series = data.frame(
      Date = site_data$ActivityStartDate,
      Value = site_data$ResultMeasureValue) %>%
      arrange(Date)
    
    # Plot time series
    plot(time_series$Date, time_series$Value, ylab="SC (uS/cm)", main=substr(site,6,14),
         type='n', xlab="",ylim=c(quantile(time_series$Value,0.01,na.rm=T), quantile(time_series$Value,0.99,na.rm=T)))
    grid()
    lines(time_series$Date, time_series$Value)
    
    # Set up GAM data
    x <- site_data$ActivityStartDate
    y <- site_data$ResultMeasureValue
    
    # Remove NAs
    x <- x[!is.na(x) & !is.na(y)]
    y <- y[!is.na(x) & !is.na(y)]
    
    # Convert to decimal date
    x <- decimal_date(x)
    
    # Create GAM for whole time period
    g <- gam(y ~ s(x),gamma=0.1)
    
    # Plot it
    # plot(x,y,type='l', xlab="", ylab=char1, main=site1)
    lines(time_series$Date,g$fitted.values,type='l', col="gold",lwd=2)
    
    # Add legend
    if (ticker==1){
      legend("bottomright",legend=c("Data","GAM"),col=c("black","gold"),lwd=c(1,2),bty="n")
      ticker <- 0
    }
  }
}


################################################################################
# Set up variables
slopes_df <- data.frame(MonitoringLocationIdentifier=unique(current_sites$MonitoringLocationIdentifier[current_sites$MonitoringLocationIdentifier!="NCRN_GWMP_SPRU"]),
                        slopes_avg = NA, slopes_newer = NA, p_value = NA, R2=NA)
slopes_ticker <- 1

# Choose start year for newer slopes
newer_start <- 2017

### Try for median annual
for (plot_num in 1:3){
  # Select site groups and plot size
  if (plot_num==1){
    site_nums <- 1:15
    windows(6.5,7.5)
    par(mfrow=c(5,3),mgp=c(2,1,0),mar=c(3,3,2,1))
  } else if (plot_num==2){
    site_nums <- 16:30
    windows(6.5,7.5)
    par(mfrow=c(5,3),mgp=c(2,1,0),mar=c(3,3,2,1))
  } else if (plot_num==3){
    site_nums <- 31:37
    windows(6.5,4.5)
    par(mfrow=c(3,3),mgp=c(2,1,0),mar=c(3,3,2,1))
  }
  
  # Get site names
  sites_unique <- unique(current_sites$MonitoringLocationIdentifier)
  sites_unique <- sites_unique[sites_unique!="NCRN_GWMP_SPRU"]
  
  # Start ticker
  ticker <- 1
  
  # Start loop
  for (i in site_nums){
    
    ### Extract a time series
    # Choose site
    site <- sites_unique[i]
    
    # Query time series
    site_data <- wdata_avgd[wdata_avgd$MonitoringLocationIdentifier == site &
                              wdata_avgd$CharacteristicName == CharacteristicName,]
    
    # Aggregate to annual median
    site_data$ActivityStartDate <- year(site_data$ActivityStartDate)
    site_data <- site_data %>%
      group_by(MonitoringLocationIdentifier) %>%
      group_by(ActivityStartDate) %>%
      summarise(ResultMeasureValue = median(ResultMeasureValue, na.rm=T))
    
    # Format outputs
    time_series = data.frame(
      Date = site_data$ActivityStartDate,
      Value = site_data$ResultMeasureValue) %>%
      arrange(Date)
    
    # Plot time series
    plot(time_series$Date, time_series$Value, ylab="SC (uS/cm)", main=substr(site,6,14),
         type='n', xlab="")
    grid()
    lines(time_series$Date, time_series$Value)
    
    # Set up GAM data
    x <- site_data$ActivityStartDate
    y <- site_data$ResultMeasureValue
    
    # Remove NAs
    x <- x[!is.na(x) & !is.na(y)]
    y <- y[!is.na(x) & !is.na(y)]
    
    # Convert to decimal date
    # x <- decimal_date(x)
    
    # Create GAM for whole time period
    g <- gam(y ~ s(x),gamma=1)
    
    # Plot it
    # plot(x,y,type='l', xlab="", ylab=char1, main=site1)
    lines(time_series$Date,g$fitted.values,type='l', col="gold",lwd=2)
    
    # Add legend
    if (ticker==1){
      legend("bottomright",legend=c("Data","GAM"),col=c("black","gold"),lwd=c(1,2),bty="n")
      ticker <- 0
    }
    
    ### Do slopes calcs
    slope <- NA
    slope_weighted <- NA
    slope_avg <- NA
    
    for (j in 1:(length(g$model$x)-1)){
      slope[j] <- (g$model$y[j+1] - g$model$y[j]) / (g$model$x[j+1] - g$model$x[j])
      slope_weighted[j] <- slope[j] * (g$model$x[j+1] - g$model$x[j]) / (g$model$x[length(g$model$x)] - g$model$x[1])
    }
    
    # Calculate average slope (all years) (uS/cm/yr)
    slope_avg <- sum(slope_weighted)
    
    
    ### Calculate average slope (2015-2022...since Murphy 2019 says do minimum 8 years?)
    slope <- NA
    slope_weighted <- NA
    slope_newer <- NA
    
    # Calculate slope (newer)
    yr_start <- length(g$model$x[g$model$x>=newer_start])
    
    for (j in yr_start:(length(g$model$x)-1)){
      slope[j] <- (g$model$y[j+1] - g$model$y[j]) / (g$model$x[j+1] - g$model$x[j])
      slope_weighted[j] <- slope[j] * (g$model$x[j+1] - g$model$x[j]) / (g$model$x[length(g$model$x)] - g$model$x[yr_start])
    }
    
    # Calculate average slope (newer) (uS/cm/yr)
    slope_newer <- sum(slope_weighted,na.rm=T)
    
    # Calculate p-value and R2
    gs <- summary(g)
    slopes_df$p_value[slopes_ticker] <- gs$s.pv
    slopes_df$R2[slopes_ticker] <- gs$r.sq
    
    # Add to data frame
    slopes_df$slopes_avg[slopes_ticker] <- slope_avg
    slopes_df$slopes_newer[slopes_ticker] <- slope_newer
    slopes_ticker <- slopes_ticker + 1
  }
}


################################################################################
# Load watershed conditions
lc_chg <- read.csv("watersheds_nlcd06-19_change_2023_09_15.csv") %>% na.omit()

sheds_nlcd06 <- read.csv("watersheds_nlcd06_stats_2023_09_15.csv") %>% na.omit()
sheds_nlcd19 <- read.csv("watersheds_nlcd19_stats_2023_09_15.csv") %>% na.omit()
sheds <- st_read("NCRN_watersheds_2023_07_06_wgs84.shp")
states <- st_read("cb_2018_us_state_20m.shp")
nps <- st_read("nps_boundary_fixed_geoms_region.shp")
sc <- read.csv("NCRN_Monitoring_Locations_with_background_SC_2023_12_21.csv")

# Calculate total urban change (expansion)
urb_df <- data.frame(MonitoringLocationIdentifier=lc_chg$X, urb_chg=rowSums(lc_chg[,4:7]))

# Calculate urban intensification (infill)
inf <- data.frame(MonitoringLocationIdentifier=as.numeric(lc_chg$X))
for (i in 1:nrow(lc_chg[,4:7])){
  for (j in 1:ncol(lc_chg[,4:7])){
    if ((lc_chg[,4:7]>0)[i,j]){
      inf[i,j] <- lc_chg[,4:7][i,j]
    }
  }
}
urb_df$urb_inf <- rowSums(inf[],na.rm=T)

# Calculate combined urbanization rate (expansion + infill)
urb_df$urb_com <- urb_df$urb_chg + urb_df$urb_inf

# Calculate total urban (avg of years)
urb_df$urb_tot <- rowSums((sheds_nlcd06[,4:7]+sheds_nlcd19[,4:7])/2)

# Make data frame
urb_df[,2:5] <- round(urb_df[,2:5],3)
urb_df <- urb_df[urb_df$MonitoringLocationIdentifier %in% current_sites$MonitoringLocationIdentifier,]
spc_join <- left_join(x=slopes_df, y=urb_df, by="MonitoringLocationIdentifier")


################################################################################
# Plot GAM slopes
plot(slopes_df$slopes_avg, slopes_df$slopes_newer, xlab="Avg. slope 2005-2023 (uS/cm/yr)",
     ylab=paste("Avg. slope ",newer_start,"-2023",sep=""))
abline(h=0); abline(v=0)

ggplot(data=spc_join, mapping=aes(x=slopes_avg, y=slopes_newer)) +
  geom_point(aes(colour=urb_tot))


################################################################################
# Plot on map

# Join with GIS
sheds$MonitoringLocationIdentifier <- sheds$NEW_IMLOCI
# sheds_join <- left_join(sheds,spc_join, by="MonitoringLocationIdentifier")
sc$MonitoringLocationIdentifier <- sc$IMLOCID
spc_join <- left_join(spc_join, sc, by="MonitoringLocationIdentifier")
spc_join <- left_join(spc_join, sheds, by="MonitoringLocationIdentifier")
spc_join <- left_join(spc_join, shed_sums[c("MonitoringLocationIdentifier","Watershed.Percent.Urban..DW.")], by="MonitoringLocationIdentifier")

# # Plot
# ggplot(data=spc_join, mapping=aes(x=LONGITUDE, y=LATITUDE)) +
#   geom_point(aes(colour=slopes_newer))  # or slopes_avg
# 
# # +
#   geom_polygon(aes(fill=slope_avg))# +
#   scale_fill_gradient2(low="red",high="blue",mid="white",limits=c(-100,100))

# Select colors (all)
cols <- rep(NA,nrow(spc_join))
cols[spc_join$slopes_avg<=100] <- "blue"
cols[spc_join$slopes_avg<=5] <- "cyan"
cols[spc_join$slopes_avg<=0] <- "orchid1"
cols[spc_join$slopes_avg<=-5] <- "red"
cols[spc_join$p_value>0.05] <- "grey"
spc_join$cols <- cols

# Select colors (newer)
cols2 <- rep(NA,nrow(spc_join))
cols2[spc_join$slopes_newer<=100] <- "blue"
cols2[spc_join$slopes_newer<=5] <- "cyan"
cols2[spc_join$slopes_newer<=0] <- "orchid1"
cols2[spc_join$slopes_newer<=-5] <- "red"
cols2[spc_join$p_value>0.05] <- "grey"
spc_join$cols2 <- cols2

# Add to plot que and sort
spc_join_plot <- rbind(
  spc_join[spc_join$cols=="grey",],
  spc_join[spc_join$cols=="cyan",],
  spc_join[spc_join$cols=="orchid1",],
  spc_join[spc_join$cols=="blue",],
  spc_join[spc_join$cols=="red",]
)

spc_join_plot2 <- rbind(
  spc_join[spc_join$cols2=="grey",],
  spc_join[spc_join$cols2=="cyan",],
  spc_join[spc_join$cols2=="orchid1",],
  spc_join[spc_join$cols2=="blue",],
  spc_join[spc_join$cols2=="red",]
)
  

# Plot
windows(6.5,4)
par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1),pty='s')
plot(spc_join_plot$LONGITUDE,spc_join_plot$LATITUDE,col=spc_join_plot$cols,type='n',main="",
     xlab="Lon",ylab="Lat")
plot(nps$geometry,add=T,border="palegreen")
plot(states$geometry, add=T,lwd=3,border="black")
points(spc_join_plot$LONGITUDE,spc_join_plot$LATITUDE,col=spc_join_plot$cols,lwd=2)

legend("topright",legend=c(">5","0 to 5","-5 to 0", "<-5", "Insignificant","National Parks","States"),
       col=c("blue","cyan","orchid1","red","grey","palegreen","black"),
       pch=c("o","o","o","o","o",NA,NA),bg="white",title="SC change (uS/cm/yr)",
       lwd=c(NA,NA,NA,NA,NA,1,1),cex=0.7)

text(
  x=c(-77.66,-77.7,-77.5,-77.48,-77.5,-77.4,-77.3,-77.13,-77.14,-76.95),
  y=c(39.25,39.55,39.57,39.4,38.88,38.66,38.9,39.03,38.86,38.76),
  labels=c("HAFE","ANTI","CATO","MONO","MANA","PRWI","WOTR","ROCR","GWMP","NACE"),
  cex=0.75
)
title("a)",adj=0.05, line=-1.1, cex.main=1.25)

plot(spc_join_plot2$LONGITUDE,spc_join_plot2$LATITUDE,col=spc_join_plot2$cols,type='n', main="",
     xlab="Lon",ylab="Lat")
plot(nps$geometry,add=T,border="palegreen")
plot(states$geometry, add=T,lwd=3,border="black")
points(spc_join_plot2$LONGITUDE,spc_join_plot2$LATITUDE,col=spc_join_plot2$cols2,pch=1,lwd=2)

text(
  x=c(-77.66,-77.7,-77.5,-77.48,-77.5,-77.4,-77.3,-77.13,-77.14,-76.95),
  y=c(39.25,39.55,39.57,39.4,38.88,38.66,38.9,39.03,38.86,38.76),
  labels=c("HAFE","ANTI","CATO","MONO","MANA","PRWI","WOTR","ROCR","GWMP","NACE"),
  cex=0.75
)
title("b)",adj=0.05, line=-1.1, cex.main=1.25)

### Plot it
trends_func(spc_join)

################################################################################
# Calculate where GAM slope increasing or decreasing
nrow(spc_join[spc_join$slopes_avg>0 & spc_join$p_value<0.05,]) # Significantly increasing 2005-2022
nrow(spc_join[spc_join$slopes_avg<0 & spc_join$p_value<0.05,]) # Significantly decreasing 2005-2022
min(spc_join$slopes_avg[spc_join$p_value<0.05])
max(spc_join$slopes_avg[spc_join$p_value<0.05])

nrow(spc_join[spc_join$slopes_newer>0 & spc_join$p_value<0.05,]) # Significantly increasing 2017-2022
nrow(spc_join[spc_join$slopes_newer<0 & spc_join$p_value<0.05,]) # Significantly decreasing 2017-2022
min(spc_join$slopes_newer[spc_join$p_value<0.05])
max(spc_join$slopes_newer[spc_join$p_value<0.05])
