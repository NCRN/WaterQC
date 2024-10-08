# Land cover and water quality figures with EDD dataset
# Dan Myers, 8/13/2024

# Load packages
library(dplyr)
library(tidyr)
library(lubridate)
library(sf)
library(mblm)
library(quantreg)

################################################################################
### Read and format data #######################################################
################################################################################

### Water quality data #########################################################
# Read WQ data
fileName <- "20240129_wqp_wqx_bss_wq_npsncrn" # Leave out .csv extension
wdata <- read.csv(paste(fileName,".csv",sep=""))

# Format dates as date
wdata$ActivityStartDate <- as.Date(wdata$ActivityStartDate)

# Format result as numeric
wdata$ResultMeasureValue <- as.numeric(wdata$ResultMeasureValue)

# Remove extra rows
wdata <- wdata[wdata$ProjectIdentifier=="USNPS NCRN Perennial stream water monitoring",]
wdata <- wdata[!is.na(wdata$ResultMeasureValue),]
wdata <- wdata[!wdata$CharacteristicName %in% c("Chlorine",
  "Weather Condition (WMO Code 4501) (choice list)",                      
  "RBP2, Low G, Riparian Vegetative Zone Width, Left Bank (choice list)", 
  "RBP2, Low G, Riparian Vegetative Zone Width, Right Bank (choice list)"),]

# Take median of multiple measurements across stream
wdata_avgd <- wdata %>%
  group_by(MonitoringLocationIdentifier) %>%
  group_by(ActivityStartDate, .add=T) %>%
  group_by(CharacteristicName, .add=T) %>%
  summarise(ResultMeasureValue = median(ResultMeasureValue, na.rm=T))

# Remove sites not currently monitored
current_sites <- wdata_avgd %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarise(start_date = min(ActivityStartDate, na.rm=T),
    end_date = max(ActivityStartDate, na.rm=T))
current_sites <- current_sites[year(current_sites$end_date)==2023,]
current_sites <- current_sites[current_sites$MonitoringLocationIdentifier !="NCRN_GWMP_SPRU",] # Remove this site with too little data
wdata_avgd <- wdata_avgd[wdata_avgd$MonitoringLocationIdentifier %in% current_sites$MonitoringLocationIdentifier,]

# Choose characteristic
CharacteristicName <- "Specific conductance" 


### If selecting only specific months of WQ data ###############################
# Save original before month selection
wdata_avgd_all <- wdata_avgd
desired_years <- min(year(wdata_avgd_all$ActivityStartDate), na.rm=T):max(year(wdata_avgd_all$ActivityStartDate), na.rm=T)
desired_months <- 1:12

# Select only certain months (comment or uncomment these, otherwise it will use all the months)
# desired_months <- c(5,6,7,8,9,10) # Warm season
# desired_months <- c(11,12,1,2,3,4) # Cold season
# desired_months <- c(1,4,7,10) ; desired_years <- c(2005:2018) # Quarterly through 2018

# Extract the data for those months
wdata_avgd <- wdata_avgd_all[month(wdata_avgd_all$ActivityStartDate) %in% desired_months &
                             year(wdata_avgd_all$ActivityStartDate) %in% desired_years,]


### Watershed LULC, road salt, and background SC data ##########################
# Load data
shed_sums <- read.csv("Watershed conditions 2024_03_27.csv") %>%
  rename(MonitoringLocationIdentifier=NEW_IMLOCI)
shed_sums <- shed_sums[shed_sums$MonitoringLocationIdentifier %in% current_sites$MonitoringLocationIdentifier,]

# Calculate total urban change (expansion)
urb_df <- data.frame(MonitoringLocationIdentifier=shed_sums$MonitoringLocationIdentifier, 
                     urb_chg=rowSums(shed_sums[,c(  "Change.Developed..open..21.",
                                                    "Change.Developed..low..22.",
                                                    "Change.Developed..medium..23.",
                                                    "Change.Developed..high..24.")]))

# Calculate urban intensification (infill)
inf <- data.frame(MonitoringLocationIdentifier=shed_sums$MonitoringLocationIdentifier,
                  open=NA,low=NA,med=NA,high=NA)
for (i in 1:nrow(shed_sums)){
  for (j in 1:ncol(shed_sums[,c(  "Change.Developed..open..21.",
                                  "Change.Developed..low..22.",
                                  "Change.Developed..medium..23.",
                                  "Change.Developed..high..24.")])){
    if ((shed_sums[,c(  "Change.Developed..open..21.",
                        "Change.Developed..low..22.",
                        "Change.Developed..medium..23.",
                        "Change.Developed..high..24.")]>0)[i,j]){
      inf[i,j+1] <- shed_sums[,c(  "Change.Developed..open..21.",
                                 "Change.Developed..low..22.",
                                 "Change.Developed..medium..23.",
                                 "Change.Developed..high..24.")][i,j]
    }
  }
}
urb_df$urb_inf <- rowSums(inf[,c("open","low","med","high")],na.rm=T)

# Calculate combined urbanization rate (expansion + infill)
urb_df$urb_com <- urb_df$urb_chg + urb_df$urb_inf

# Calculate total urban area (average of years)
urb_df$urb_tot <- rowSums((shed_sums[,c("NLCD06.Developed..open..21.",
                                        "NLCD06.Developed..low..22.",
                                        "NLCD06.Developed..medium..23.",
                                        "NLCD06.Developed..high..24.")] +
                             shed_sums[,c("NLCD19.Developed..open..21.",
                                          "NLCD19.Developed..low..22.",
                                          "NLCD19.Developed..medium..23.",
                                          "NLCD19.Developed..high..24.")])/2)

urb_df$ag_tot <- rowSums((shed_sums[,c("NLCD06.Pasture..81.", "NLCD06.Crops..82.")]+
                          shed_sums[,c("NLCD19.Pasture..81.", "NLCD19.Crops..82.")])/2)

urb_df$for_tot <- rowSums((shed_sums[,c("NLCD06.Deciduous.forest..41.",
                                        "NLCD06.Evergreen.forest..42.",
                                        "NLCD06.Mixed.forest..43.")]+
                             shed_sums[,c("NLCD19.Deciduous.forest..41.",
                                          "NLCD19.Evergreen.forest..42.",
                                          "NLCD19.Mixed.forest..43.")])/2)

# Add sub-types
urb_df$open <- (shed_sums$NLCD06.Developed..open..21. + shed_sums$NLCD19.Developed..open..21.)/2
urb_df$low <- (shed_sums$NLCD19.Developed..low..22. + shed_sums$NLCD19.Developed..low..22.)/2
urb_df$med <- (shed_sums$NLCD06.Developed..medium..23. + shed_sums$NLCD19.Developed..medium..23.)/2
urb_df$high <- (shed_sums$NLCD06.Developed..high..24. + shed_sums$NLCD19.Developed..high..24.)/2

# Format data frame and remove extra sites
urb_df <- urb_df[urb_df$MonitoringLocationIdentifier %in% current_sites$MonitoringLocationIdentifier,]


### Background SC and watershed conditions data ################################
# Calculate SC medians and IQR
for (i in 1:nrow(current_sites)){
  sel_data <- wdata_avgd$ResultMeasureValue[wdata_avgd$MonitoringLocationIdentifier==current_sites$MonitoringLocationIdentifier[i] &
                                              wdata_avgd$CharacteristicName==CharacteristicName]
  current_sites$medians_raw[i] <- median(sel_data,na.rm=T)
  current_sites$q1_raw[i] <- quantile(sel_data,0.25,na.rm=T)
  current_sites$q3_raw[i] <- quantile(sel_data,0.75,na.rm=T)
  current_sites$q90th_raw[i] <- quantile(sel_data,0.90,na.rm=T)
}

# Add background SC and watershed protected area
current_sites <- left_join(current_sites, shed_sums, by="MonitoringLocationIdentifier")
current_sites <- left_join(current_sites, urb_df, by="MonitoringLocationIdentifier")

# Correct for background SC
if (CharacteristicName=="Specific conductance"){
  current_sites$medians <- current_sites$medians_raw - current_sites$AvgPredEC
  current_sites$q1 <- current_sites$q1_raw - current_sites$AvgPredEC
  current_sites$q3 <- current_sites$q3_raw - current_sites$AvgPredEC
  current_sites$q90th <- current_sites$q90th_raw - current_sites$AvgPredEC
} else { 
  current_sites$medians <- current_sites$medians_raw
  current_sites$q1 <- current_sites$q1_raw
  current_sites$q3 <- current_sites$q3_raw
  current_sites$q90th <- current_sites$q90th_raw
}

# Shorten outputs
outputs <- data.frame(MonitoringLocationIdentifier=current_sites$MonitoringLocationIdentifier,
                      medians = current_sites$medians,
                      q1 = current_sites$q1,
                      q3 = current_sites$q3,
                      q90th = current_sites$q90th,
                      pct_prot = current_sites$Prot_area_pct..PAD., # Changed from NPS ownership to PAD
                      pct_urb = current_sites$urb_tot * 100,
                      pct_ag = current_sites$ag_tot * 100,
                      pct_for = current_sites$for_tot * 100)


### Road salt data #############################################################
# Calculate 2005-2019 avg salt
avg_salt <- data.frame(MonitoringLocationIdentifier=substr(current_sites$MonitoringLocationIdentifier,1,14), 
                       avg_lbs = rowMeans(shed_sums[,c(
                         "X2005","X2006","X2007","X2008","X2009","X2010",
                         "X2011","X2012","X2013","X2014","X2015","X2016",
                         "X2017","X2018","X2019")]), area_km = shed_sums$Area_km)
avg_salt$avg_kg = avg_salt$avg*0.453592 # convert lbs to kg


################################################################################
### Time series plots ##########################################################
################################################################################
# Set up data frame and ticker
slopes_df <- data.frame(MonitoringLocationIdentifier=unique(current_sites$MonitoringLocationIdentifier[current_sites$MonitoringLocationIdentifier!="NCRN_GWMP_SPRU"]),
                        slopes_avg = NA, p_value = NA, MAD=NA, lci=NA, uci=NA, ci=NA)
slopes_ticker <- 1

# Start loop through figures
for (plot_num in 1:3){
  
  # Select site groups to plot and plot size
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
  
  # Start ticker for legend placement
  ticker <- 1
  
  # Start loop through sites
  for (i in site_nums){
    
    # Choose site
    site <- sites_unique[i]
    
    # Query time series
    site_data <- wdata_avgd[wdata_avgd$MonitoringLocationIdentifier == site &
                              wdata_avgd$CharacteristicName == CharacteristicName,]
    
    # Format time series
    time_series = data.frame(
      Date = site_data$ActivityStartDate,
      Value = site_data$ResultMeasureValue) %>%
      arrange(Date)
    
    # Plot time series
    plot(time_series$Date, time_series$Value, ylab="SC (µS/cm)", main=substr(site,6,14),
         type='n', xlab="",ylim=c(quantile(time_series$Value,0.04,na.rm=T), quantile(time_series$Value,0.96,na.rm=T)))
    grid()
    lines(time_series$Date, time_series$Value, col="darkgrey")
    
    # Set up trend data
    x <- site_data$ActivityStartDate
    y <- site_data$ResultMeasureValue
    
    # Remove NAs
    x <- x[!is.na(x) & !is.na(y)]
    y <- y[!is.na(x) & !is.na(y)]
    
    # Convert to decimal date
    x <- decimal_date(x)
    
    # Remove zero and negative values
    x <- x[y>=1]
    time_series <- time_series[y>=1,]
    y <- y[y>=1] 
    
    # Create trend model for whole time period
    g <- mblm(y ~ x)
    
    # Add legend to first plot of figure
    if (ticker==1){
      legend("bottomright",legend=c("Data","Trend"),col=c("darkgrey","gold"),lwd=c(1,2),bty="n")
      ticker <- 0
    }
    
    # Calculate p-value and performance statistic from summary
    gs <- summary(g)
    slopes_df$p_value[slopes_ticker] <- gs$coefficients[2,4]
    slopes_df$MAD[slopes_ticker] <- gs$coefficients[2,2]
    
    # Add to data frame
    slopes_df$slopes_avg[slopes_ticker] <- gs$coefficients[2,1]
    
    # Add confidence interval
    uci <- confint(g)[2,2]
    lci <- confint(g)[2,1]
    ci <- uci - lci
    slopes_df$lci[slopes_ticker] <- lci
    slopes_df$uci[slopes_ticker] <- uci
    slopes_df$ci[slopes_ticker] <- ci
    
    # Up ticker
    slopes_ticker <- slopes_ticker + 1
    
    # Make p-values for display
    if (gs$coefficients[2,4]<0.001){
      p_disp <- "p<0.001"
    } else {
      p_disp <- paste("p=",round(gs$coefficients[2,4],3),sep="")
    }
    
    # Add model info to the plot
    # if (gs$coefficients[2,4] < 0.05){
      lines(time_series$Date,g$fitted.values,type='l', col="gold",lwd=2)
      title(paste("Trend=",round(gs$coefficients[2,1],2),", ",p_disp, ",\nLCI=",
                  round(lci,2), ", UCI=",round(uci,2), sep=""),adj=0.1, line=-1.9, cex.main=1)
    # }
    
    # Box it in
    box()
  }
}

################################################################################
### Quantile Regressions #######################################################
################################################################################

# Join LULC with WQ from time series and salt data
current_sites <- left_join(x=current_sites, y=slopes_df, by="MonitoringLocationIdentifier")

# Rename some fields
current_sites$NPS_pct <- current_sites$NPS.Watershed.Ownership.Percent
current_sites$sen_slope <- current_sites$slopes_avg
current_sites$mk_p <- current_sites$p_value
current_sites$urb_tot2 <- current_sites$urb_tot*100 # Just displayed as percent

# Join with salt data and convert to tons
current_sites <- left_join(current_sites,avg_salt, by="MonitoringLocationIdentifier")
current_sites$avg_ton <- current_sites$avg_kg / 1000

# Organize SC data
qr_data_sc <- wdata_avgd[wdata_avgd$CharacteristicName == "Specific conductance",] %>%
  select(MonitoringLocationIdentifier, ActivityStartDate, ResultMeasureValue)

# Organize LULC data
qr_data_lulc <- current_sites %>%
  select(MonitoringLocationIdentifier, Prot_area_pct..PAD.,urb_tot2, for_tot, AvgPredEC)
qr_data_lulc$for_tot <- qr_data_lulc$for_tot*100

# Join them together
qr_data <- left_join(qr_data_sc, qr_data_lulc, by="MonitoringLocationIdentifier")

# Correct for background
qr_data$Result_cor <- qr_data$ResultMeasureValue - qr_data$AvgPredEC

# Begin quantile regression using quantreg
qr_model10_urb <- rq(qr_data$Result_cor ~ qr_data$urb_tot2, tau=0.1)
qr_model25_urb <- rq(qr_data$Result_cor ~ qr_data$urb_tot2, tau=0.25)
qr_model50_urb <- rq(qr_data$Result_cor ~ qr_data$urb_tot2, tau=0.5)
qr_model75_urb <- rq(qr_data$Result_cor ~ qr_data$urb_tot2, tau=0.75)
qr_model90_urb <- rq(qr_data$Result_cor ~ qr_data$urb_tot2, tau=0.9)

# Begin quantile regression using quantreg
qr_model10_for <- rq(qr_data$Result_cor ~ qr_data$for_tot, tau=0.1)
qr_model25_for <- rq(qr_data$Result_cor ~ qr_data$for_tot, tau=0.25)
qr_model50_for <- rq(qr_data$Result_cor ~ qr_data$for_tot, tau=0.5)
qr_model75_for <- rq(qr_data$Result_cor ~ qr_data$for_tot, tau=0.75)
qr_model90_for <- rq(qr_data$Result_cor ~ qr_data$for_tot, tau=0.9)

# Begin quantile regression using quantreg
qr_model10_pro <- rq(qr_data$Result_cor ~ qr_data$Prot_area_pct..PAD., tau=0.1)
qr_model25_pro <- rq(qr_data$Result_cor ~ qr_data$Prot_area_pct..PAD., tau=0.25)
qr_model50_pro <- rq(qr_data$Result_cor ~ qr_data$Prot_area_pct..PAD., tau=0.5)
qr_model75_pro <- rq(qr_data$Result_cor ~ qr_data$Prot_area_pct..PAD., tau=0.75)
qr_model90_pro <- rq(qr_data$Result_cor ~ qr_data$Prot_area_pct..PAD., tau=0.9)

# Do for change in SC
qr_data2 <- current_sites %>%
  select(MonitoringLocationIdentifier, urb_tot2, sen_slope, avg_ton, ci)
qr_data2 <- qr_data2[qr_data2$ci <= 5,]

qr_model10_ur2 <- rq(qr_data2$sen_slope ~ qr_data2$urb_tot2, tau=0.1)
qr_model25_ur2 <- rq(qr_data2$sen_slope ~ qr_data2$urb_tot2, tau=0.25)
qr_model50_ur2 <- rq(qr_data2$sen_slope ~ qr_data2$urb_tot2, tau=0.5)
qr_model75_ur2 <- rq(qr_data2$sen_slope ~ qr_data2$urb_tot2, tau=0.75)
qr_model90_ur2 <- rq(qr_data2$sen_slope ~ qr_data2$urb_tot2, tau=0.9)

qr_model10_sal <- rq(qr_data2$sen_slope ~ qr_data2$avg_ton, tau=0.1)
qr_model25_sal <- rq(qr_data2$sen_slope ~ qr_data2$avg_ton, tau=0.25)
qr_model50_sal <- rq(qr_data2$sen_slope ~ qr_data2$avg_ton, tau=0.5)
qr_model75_sal <- rq(qr_data2$sen_slope ~ qr_data2$avg_ton, tau=0.75)
qr_model90_sal <- rq(qr_data2$sen_slope ~ qr_data2$avg_ton, tau=0.9)

# Get quantile regression stats
summary(qr_model50_urb)
summary(qr_model50_for)
summary(qr_model50_pro)
# summary(qr_model50_ur2)
summary(qr_model50_sal, se="nid")


################################################################################
### Watershed protection plots #################################################
################################################################################

# Assign plotting specs
ylabs <- "SC (µS/cm)"
ylim=1600
threshold <- 171

### Set up plot and layout
windows(5.5,6.5)
nf <- layout(matrix(c(1,1,1,1,1,1, # top
                      2,2,3,3,4,4, # middle
                      5,5,5,6,6,6), # bottom
                    nrow=3, ncol=6,byrow=TRUE))

### Boxplot ##################################################################
# Pivot the data
pw <- wdata_avgd[wdata_avgd$CharacteristicName==CharacteristicName,] %>%
  pivot_wider(names_from="MonitoringLocationIdentifier",
              values_from="ResultMeasureValue")

# Create a NA data frame to convert the list to
pw_df <- data.frame() 

# Populate the data frame
for (i in 1:ncol(pw)){
  site_data <- unlist(pw[[i]])
  nrows <- length(site_data) 
  pw_df[1:nrows,i] <- site_data # Add column
}
colnames(pw_df) <- substr(colnames(pw),6,14)
pw_df <- pw_df[3:length(pw_df)]

# Sort sites by median
medians <- apply(pw_df,2,median, na.rm=T)
order1 <- row_number(medians)
order1_df <- data.frame(order1 = order1, sort1 = 1:length(order1)) %>% arrange(order1)
toPlot <- pw_df[,order1_df$sort1]

# Make the boxplot
par(mar=c(5,4,0,1)+0.1,mgp=c(3,1,0))
boxplot(toPlot, names=colnames(toPlot),las=2,ylim=c(0,ylim),
        ylab=ylabs,cex.axis=1,xlim=c(1.5,36.5))
grid()
boxplot(toPlot, names=colnames(toPlot),las=2,ylim=c(0,ylim),
        ylab=ylabs,cex.axis=1, add=T,xlim=c(1.5,36.5))

# Add Morgan 2007 line and title
abline(h=threshold, col="red",lty=2)
abline(h=1500, col="blue", lty=2)
title("a)",adj=0.02, line=-1.2, cex.main=1.5)
box()

# Add legend
legend("topleft",legend=c("Measurement","Critical value","Brackish"),pch=c("o","-","-"),
       col=c("black","red","blue"),inset=c(0.06,0),bg="white", pt.cex=c(1,1.5,1.5))

# Make scatterplot (% urb)
medians <- current_sites$medians
q1 <- current_sites$q1
q3 <- current_sites$q3
x <- current_sites$urb_tot*100
par(mgp=c(2,1,0),mar=c(3,3,4,0.5) + 0.1)
plot(x,medians,ylim=c(min(q1,na.rm=T),max(q3,na.rm=T)),type="n",xlab="Urban (%)",
     ylab="SC over background (µS/cm)")
grid()
points(x,medians)
lines(rep(x,times=1,each=3),t(matrix(c(q1,q3,rep(NA,length(q1))),ncol=3,byrow=F)))

# Add model
abline(qr_model10_urb, col="purple")
abline(qr_model25_urb, col="blue")
abline(qr_model50_urb, col="orange")
abline(qr_model75_urb, col="red")
abline(qr_model90_urb, col="darkred")

title("b)",adj=0.02, line=-1.2, cex.main=1.5)
box()

# Make scatterplot (% forest)
par(mgp=c(2,1,0),mar=c(3,2.75,4,0.75) + 0.1)
x <- current_sites$for_tot*100
plot(x,medians,ylim=c(min(q1,na.rm=T),max(q3,na.rm=T)),type="n",xlab="Forest (%)",
     ylab="")
grid()
points(x,medians)
lines(rep(x,times=1,each=3),t(matrix(c(q1,q3,rep(NA,length(q1))),ncol=3,byrow=F)))

# Add model
abline(qr_model10_for, col="purple")
abline(qr_model25_for, col="blue")
abline(qr_model50_for, col="orange")
abline(qr_model75_for, col="red")
abline(qr_model90_for, col="darkred")

title(" c)",adj=0.02, line=-1.2, cex.main=1.5)

# Add legend
legend("topright",legend=c("Median","IQR","Q90","Q75","Q50","Q25","Q10"),
       col=c("black","black","darkred","red","orange","blue","purple"),pch=c("o","|","-","-","-","-","-"),pt.cex=c(1,1,2,2,2,2,2),
       bg="white", inset=c(0.0,0),cex=0.75)
box()

# Make scatterplot (% protected)
par(mgp=c(2,1,0),mar=c(3,2.5,4,1) + 0.1)
x <- current_sites$Prot_area_pct..PAD.
plot(x,medians,ylim=c(min(q1,na.rm=T),max(q3,na.rm=T)),type="n",xlab="Protected area (%)",
     ylab="")
grid()
points(x,medians)
lines(rep(x,times=1,each=3),t(matrix(c(q1,q3,rep(NA,length(q1))),ncol=3,byrow=F)))

# Add model
abline(qr_model10_pro, col="purple")
abline(qr_model25_pro, col="blue")
abline(qr_model50_pro, col="orange")
abline(qr_model75_pro, col="red")
abline(qr_model90_pro, col="darkred")

title("d)",adj=0.02, line=-1.2, cex.main=1.5)
box()


################################################################################
### Trend analysis plots #######################################################
################################################################################


### Plot it ####################################################################
# Turn-off scientific notation
options(scipen=999) 

# Start plot
par(mar=c(3,3,2,0.5), mgp=c(2,1,0))

# Add trends plot
current_sites$cols<- "grey"
current_sites$cols[current_sites$ci<=5] <- "black"

plot(current_sites$urb_tot*100, current_sites$sen_slope,
     xlab="Total urban area (% watershed)", ylab = "ΔSC (µS/cm per year)",
     main=NULL,type="n")
grid()
points(current_sites$urb_tot*100, current_sites$sen_slope,
       col=current_sites$cols,
       cex=(current_sites$urb_com*25+0.5), pch=0) 

title("e)",adj=0.02, line=-1.2, cex.main=1.5)

# Add model
abline(qr_model25_ur2, col="blue")
abline(qr_model50_ur2, col="orange")
abline(qr_model75_ur2, col="red")
# abline(qr_model90_ur2, col="darkred")

# Add legend
legend("topleft", legend=c("0%", "10%"), title="Urban growth rate\n(ΔSC CI <5 µS/cm/yr)", 
       pch=0, pt.cex=c(0.5,3),bg="white", inset=c(0.11,0),cex=0.9)
box()

# Start road salt plot
par(mgp=c(2,1,0),mar=c(3,2.5,2,1) + 0.1)
plot(current_sites$avg_ton, current_sites$sen_slope, type="n", xlab="Salt applications (tons/km2/yr)",
     ylab="")
grid()
points(current_sites$avg_ton, current_sites$sen_slope, col="grey",pch=2)
points(current_sites$avg_ton[current_sites$ci<=5], current_sites$sen_slope[current_sites$ci<=5], col="black",pch=2)
title("f)",adj=0.02, line=-1.2, cex.main=1.5)

# Add model
abline(qr_model25_sal, col="blue")
abline(qr_model50_sal, col="orange")
abline(qr_model75_sal, col="red")

legend("bottomright",legend=c("ΔSC CI <5 µS/cm/yr", "ΔSC CI >5 µS/cm/yr"),pch=2,bg="white",col=c("black","grey"))

################################################################################
### Output watershed conditions table###########################################
################################################################################
# Create watershed conditions data frame
wat <- data.frame(
  Site_name = substr(current_sites$MonitoringLocationIdentifier,6,14),
  Trees = round(current_sites$for_tot*100,0),
  Crops = round(current_sites$ag_tot*100,0),
  Urban = round(current_sites$urb_tot*100,0),
  Protect. = round(current_sites$Prot_area_pct..PAD.,0),
  Area = round(current_sites$Area_km,1)) %>%
  arrange(Site_name)

# Write to csv
# write.csv(wat, "Watershed conditions table 1 NLCD.csv", row.names=F)


################################################################################
### Count number of measurements per site ######################################
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

################################################################################
### Plot with urban expansion on x axis ########################################
################################################################################

# Turn-off scientific notation
options(scipen=999) 

# Start plot
windows(4, 4)
par(mar=c(3,3,1,1), mgp=c(2,1,0))

# Add trends plot
current_sites$cols<- "grey"
current_sites$cols[current_sites$ci<=5] <- "black"

plot(current_sites$urb_com*100, current_sites$sen_slope,
     xlab="", ylab = "",
     main=NULL,type="n")
grid()
points(current_sites$urb_com*100, current_sites$sen_slope,
       col=current_sites$cols,
       cex=(current_sites$urb_tot*1.5+0.25), pch=0) 

# Add legend
legend("topleft", legend=c("0%", "100%"), title="Total urban area\n(ΔSC CI <5 µS/cm/yr)", 
       pch=0, pt.cex=c(0.25,1.75),bg="white", inset=c(0.4,0))


### Same plot but for urban area & growth vs. deicing salt application
# Start plot
windows(8, 4)
par(mar=c(3,3,1,1), mgp=c(2,1,0), mfrow=c(1,2))

plot(current_sites$urb_tot2, current_sites$avg_ton,
     xlab="Total urban area (%)", 
     ylab = "Salt applications (tons/km2/yr)",
     main=NULL,type="n")
grid()
points(current_sites$urb_tot2, current_sites$avg_ton,
       col=current_sites$cols,
       cex=(current_sites$urb_tot*1.5+0.25), pch=0) 

# Add legend
legend("topleft", legend=c("0%", "100%"), title="Total urban area\n(ΔSC CI <5 µS/cm/yr)", 
       pch=0, pt.cex=c(0.25,1.75),bg="white", inset=c(0.4,0))

plot(current_sites$urb_com*100, current_sites$avg_ton,
     xlab="Urban growth rate (% expansion + infill 2006-2019)", 
     ylab = "Salt applications (tons/km2/yr)",
     main=NULL,type="n")
grid()
points(current_sites$urb_com*100, current_sites$avg_ton,
       col=current_sites$cols,
       cex=(current_sites$urb_tot*1.5+0.25), pch=0) 

################################################################################
### Plots with imperviousness, road density, etc. ##############################
################################################################################
# Analyses with median SC
windows(6.5,6.5)
par(mfrow=c(3,3))
plot(current_sites$Watershed_Percent_Impervious_NLCD2019, current_sites$medians,ylab="SC over background (µS/cm)",xlab="% impervious")
plot(current_sites$urb_tot2, current_sites$medians,ylab="SC over background (µS/cm)",xlab="% total urban area")
plot(current_sites$open*100, current_sites$medians,ylab="SC over background (µS/cm)",xlab="% open space")
plot(current_sites$low*100, current_sites$medians,ylab="SC over background (µS/cm)",xlab="% low intensity")
plot(current_sites$med*100, current_sites$medians,ylab="SC over background (µS/cm)",xlab="% medium intensity")
plot(current_sites$high*100, current_sites$medians,ylab="SC over background (µS/cm)",xlab="% high intensity")
plot(current_sites$roads_km_km2, current_sites$medians,ylab="SC over background (µS/cm)",xlab="Road density (km/km2)")

summary(lm(current_sites$medians ~ current_sites$urb_tot2))
summary(lm(current_sites$medians ~ current_sites$Watershed_Percent_Impervious_NLCD2019))
summary(lm(current_sites$medians ~ current_sites$open))
summary(lm(current_sites$medians ~ current_sites$low))
summary(lm(current_sites$medians ~ current_sites$med))
summary(lm(current_sites$medians ~ current_sites$high))
summary(lm(current_sites$medians ~ current_sites$roads_km_km2))

# Analyses with SC trends
windows(6.5,6.5)
par(mfrow=c(3,3))
plot(current_sites$Watershed_Percent_Impervious_NLCD2019, current_sites$sen_slope,ylab="SC trend",xlab="% impervious")
plot(current_sites$urb_tot2, current_sites$sen_slope,ylab="SC trend",xlab="% total urban area")
plot(current_sites$open*100, current_sites$sen_slope,ylab="SC trend",xlab="% open space")
plot(current_sites$low*100, current_sites$sen_slope,ylab="SC trend",xlab="% low intensity")
plot(current_sites$med*100, current_sites$sen_slope,ylab="SC trend",xlab="% medium intensity")
plot(current_sites$high*100, current_sites$sen_slope,ylab="SC trend",xlab="% high intensity")
plot(current_sites$roads_km_km2, current_sites$sen_slope,ylab="SC trend",xlab="Road density (km/km2)")

summary(lm(current_sites$sen_slope ~ current_sites$urb_tot2))
summary(lm(current_sites$sen_slope ~ current_sites$Watershed_Percent_Impervious_NLCD2019))
summary(lm(current_sites$sen_slope ~ current_sites$open))
summary(lm(current_sites$sen_slope ~ current_sites$low))
summary(lm(current_sites$sen_slope ~ current_sites$med))
summary(lm(current_sites$sen_slope ~ current_sites$high))
summary(lm(current_sites$sen_slope ~ current_sites$roads_km_km2))


# Create extra watershed conditions data frame
wat2 <- data.frame(
  Site_name = substr(current_sites$MonitoringLocationIdentifier,6,14),
  Impervious = round(current_sites$Watershed_Percent_Impervious_NLCD2019,0),
  Open_space = round(current_sites$open*100,0),
  Low_intensity = round(current_sites$low*100,0),
  Med_intensity = round(current_sites$med*100,0),
  High_intensity = round(current_sites$high*100,0),
  Road_density = round(current_sites$roads_km_km2,1),
  Background_SC = round(current_sites$AvgPredEC,0),
  Deicing_salt = round(current_sites$avg_ton,0)) %>%
  arrange(Site_name)

# Write to csv
# write.csv(wat2, "Watershed conditions table 2 with salt and SC.csv", row.names=F)

### Calculate number of sites with positive or negative trends (including CI's)
# Positive trends
print(current_sites[current_sites$sen_slope>0 & current_sites$ci<5 & current_sites$lci>0,], n=37)

# Negative trends
print(current_sites[current_sites$sen_slope<0 & current_sites$ci<5 & current_sites$uci<0,], n=37)
