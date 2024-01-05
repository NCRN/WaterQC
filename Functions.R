# Functions.R
# Dan Myers, 10/27/2023

# This script contains functions to be sourced for Task 3 analyses.

################################################################################
### medians_func() #############################################################
################################################################################

medians_func <- function(current_sites, wdata_avgd, CharacteristicName, back_sc, shed_sums){
  # Calculate medians and IQR
  for (i in 1:nrow(current_sites)){
    sel_data <- wdata_avgd$ResultMeasureValue[wdata_avgd$MonitoringLocationIdentifier==current_sites$MonitoringLocationIdentifier[i] &
                                                wdata_avgd$CharacteristicName==CharacteristicName]
    current_sites$medians_raw[i] <- median(sel_data,na.rm=T)
    current_sites$q1_raw[i] <- quantile(sel_data,0.25,na.rm=T)
    current_sites$q3_raw[i] <- quantile(sel_data,0.75,na.rm=T)
    current_sites$q90th_raw[i] <- quantile(sel_data,0.90,na.rm=T)
  }
  
  # Add background SC and watershed protected area
  current_sites <- left_join(current_sites, back_sc, by="MonitoringLocationIdentifier")
  current_sites <- left_join(current_sites, shed_sums, by="MonitoringLocationIdentifier")
  
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
  output <- data.frame(MonitoringLocationIdentifier=current_sites$MonitoringLocationIdentifier,
                       medians = current_sites$medians,
                       q1 = current_sites$q1,
                       q3 = current_sites$q3,
                       q90th = current_sites$q90th,
                       pct_prot = current_sites$Prot_area_pct..PAD., # Changed from NPS ownership to PAD
                       pct_urb = current_sites$Watershed.Percent.Urban..DW.,
                       pct_ag = current_sites$Watershed.Percent.Agriculture..DW.,
                       pct_for = current_sites$Watershed.Percent.Forest..DW.)
  return(output)
}

################################################################################
### plotting_func() ############################################################
################################################################################

plotting_func <- function(wdata_avgd, outputs, ylabs,ylim, threshold, CharacteristicName){
  
  ### Set up plot
  windows(6.5,6.5)
  
  nf <- layout(matrix(c(1,1, # top
                        2,3, # middle
                        4,5), # bottom
                      nrow=3, ncol=2,byrow=TRUE))
  
  ### Boxplots
  # Pivot
  pw <- wdata_avgd[wdata_avgd$CharacteristicName==CharacteristicName,] %>%
    pivot_wider(names_from="MonitoringLocationIdentifier",
                values_from="ResultMeasureValue")
  
  # Create a NA data frame to convert the list to
  pw_df <- data.frame() 
  
  # Populate data frame
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
  
  # Make boxplot
  par(mar=c(6,4,1,1)+1,mgp=c(3,1,0))
  boxplot(toPlot, names=colnames(toPlot),las=2,ylim=c(0,ylim),
          ylab=ylabs,cex.axis=1)
  grid()
  boxplot(toPlot, names=colnames(toPlot),las=2,ylim=c(0,ylim),
          ylab=ylabs,cex.axis=1, add=T)
  
  # Add Morgan 2007 line and title
  abline(h=threshold, col="red",lty=2)
  title("a)",adj=0.02, line=-1.2, cex.main=1.5)
  
  # Add legend
  legend("topleft",legend=c("Measurement","Critical value"),pch=c("o","-"),
         col=c("black","red"),inset=c(0.06,0),bg="white")
  
  # Make scatterplot (% urb)
  medians <- outputs$medians
  q1 <- outputs$q1
  q3 <- outputs$q3
  x <- outputs$pct_urb
  par(mgp=c(2,1,0),mar=c(3,3,1,1) + 0.1)
  plot(x,medians,ylim=c(min(q1,na.rm=T),max(q3,na.rm=T)),type="n",xlab="Urban (%)",
       ylab=ylabs)
  grid()
  points(x,medians)
  lines(rep(x,times=1,each=3),t(matrix(c(q1,q3,rep(NA,length(q1))),ncol=3,byrow=F)))
  
  # Add model
  lm1 <- lm(medians ~ x)
  abline(lm1,col="blue",lwd=1)
  print(summary(lm1))
  
  title("b)",adj=0.02, line=-1.2, cex.main=1.5)
  
  ## Plot it (% ag)
  x <- outputs$pct_ag
  plot(x,medians,ylim=c(min(q1,na.rm=T),max(q3,na.rm=T)),type="n",xlab="Agriculture (%)",
       ylab=ylabs)
  grid()
  points(x,medians)
  lines(rep(x,times=1,each=3),t(matrix(c(q1,q3,rep(NA,length(q1))),ncol=3,byrow=F)))
  
  title("c)",adj=0.02, line=-1.2, cex.main=1.5)
  
  # Add legend
  legend("topright",legend=c("Median - background","IQR - background","Model"),
         col=c("black","black","blue"),pch=c("o","|","-"),
         bg="white")
  
  
  ## Plot it (% forest)
  x <- outputs$pct_for
  plot(x,medians,ylim=c(min(q1,na.rm=T),max(q3,na.rm=T)),type="n",xlab="Forest (%)",
       ylab=ylabs)
  grid()
  points(x,medians)
  lines(rep(x,times=1,each=3),t(matrix(c(q1,q3,rep(NA,length(q1))),ncol=3,byrow=F)))
  
  # Add model
  lm1 <- lm(medians ~ x)
  abline(lm1,col="blue",lwd=1)
  print(summary(lm1))
  
  title("d)",adj=0.02, line=-1.2, cex.main=1.5)
  
  
  ## Plot it (% protected)
  x <- outputs$pct_prot
  plot(x,medians,ylim=c(min(q1,na.rm=T),max(q3,na.rm=T)),type="n",xlab="Protected area (%)",
       ylab=ylabs)
  grid()
  points(x,medians)
  lines(rep(x,times=1,each=3),t(matrix(c(q1,q3,rep(NA,length(q1))),ncol=3,byrow=F)))
  
  # Add model
  lm1 <- lm(medians ~ x)
  abline(lm1,col="blue",lwd=1)
  print(summary(lm1))
  
  title("e)",adj=0.02, line=-1.2, cex.main=1.5)
}


################################################################################
### trends_func() ##############################################################
################################################################################
trends_func <- function(spc_join){
  
  # Set frames
  windows(6.5,6.5)
  par(mfrow=c(2,2), mgp=c(2,1,0),mar=c(3,3,1,1) + 0.1)
  
  # Plot relationship to urban change (expansion, sprawl)
  plot(spc_join$urb_chg*100, spc_join$slopes_avg,
       xlab="Urban expansion rate (% area 2006-2019)", ylab = "SC change (uS/cm per year)",
       main=NULL,col="grey")
  grid()
  # points(spc_join$urb_chg[spc_join$sen_p<=0.05]*100, spc_join$sen_slope[spc_join$sen_p<=0.05])
  points(spc_join$urb_chg[spc_join$p_value<=0.05]*100, spc_join$slopes_avg[spc_join$p_value<=0.05])
  
  legend("topright",legend=c("Significant site", "Not signif. site"),col=c("black","grey"),pch=c(1,1),bg="white")
  title("a)",adj=0.05, line=-1.5, cex.main=1.25)
  
  
  # Plot relationship to urban infill (intensification)
  plot(spc_join$urb_inf*100, spc_join$slopes_avg,
       xlab="Urban infill rate (% area 2006-2019)", ylab = "SC change (uS/cm per year)",
       main=NULL,col="grey")
  grid()
  # points(spc_join$urb_inf[spc_join$sen_p<=0.05]*100, spc_join$sen_slope[spc_join$sen_p<=0.05])
  points(spc_join$urb_inf[spc_join$p_value<=0.05]*100, spc_join$slopes_avg[spc_join$p_value<=0.05])
  
  # legend("topright",legend=c("Significant site", "Not signif. site"),col=c("black","grey"),pch=c(1,1),bg="white")
  title("b)",adj=0.05, line=-1.5, cex.main=1.25)
  
  
  # Plot relationship to urban combined (infill + expansion)
  plot(spc_join$urb_com*100, spc_join$slopes_avg,
       xlab="Urban expansion+infill rates", ylab = "SC change (uS/cm per year)",
       main=NULL,col="grey")
  grid()
  # points(spc_join$urb_com[spc_join$sen_p<=0.05]*100, spc_join$sen_slope[spc_join$sen_p<=0.05])
  points(spc_join$urb_com[spc_join$p_value<=0.05]*100, spc_join$slopes_avg[spc_join$p_value<=0.05])
  
  # legend("topright",legend=c("Significant site", "Not signif. site"),col=c("black","grey"),pch=c(1,1),bg="white")
  title("c)",adj=0.05, line=-1.5, cex.main=1.25)
  
  # Relationship with total urban area
  plot(spc_join$urb_tot*100, spc_join$slopes_avg,
       xlab="Total urban area (% watershed)", ylab = "SC change (uS/cm per year)",
       main=NULL,col="grey")
  grid()
  # points(spc_join$urb_tot[spc_join$sen_p<=0.05]*100, spc_join$sen_slope[spc_join$sen_p<=0.05])
  points(spc_join$urb_tot[spc_join$p_value<=0.05]*100, spc_join$slopes_avg[spc_join$p_value<=0.05])
  
  title("d)",adj=0.05, line=-1.5, cex.main=1.25)
  
  # Relationship with NPS percent
  windows(6.5,6.5)
  plot(spc_join$NPS_pct, spc_join$slopes_avg,
       xlab="NPS land (% watershed)", ylab = "SC change (uS/cm per year)",
       main=NULL,col="grey")
  grid()
  points(spc_join$NPS_pct[spc_join$p_value<=0.05], spc_join$slopes_avg[spc_join$p_value<=0.05])
  
  title("e)",adj=0.05, line=-1.5, cex.main=1.25)
  
  # The other model (urban expansion) is insignificant
  ind1 <- spc_join$urb_chg[spc_join$p_value<=0.05]*100
  dep1 <- spc_join$slopes_avg[spc_join$p_value<=0.05]
  lm1 <- lm(dep1 ~ ind1)
  print("expansion");print(summary(lm1))
  
  # The other model (urban infill) is insignificant
  ind2 <- spc_join$urb_inf[spc_join$p_value<=0.05]*100
  dep2 <- spc_join$slopes_avg[spc_join$p_value<=0.05]
  lm2 <- lm(dep2 ~ ind2)
  print("infill");print(summary(lm2))
  
  # The other model (urban infill) is insignificant
  ind3 <- spc_join$urb_com[spc_join$p_value<=0.05]*100
  dep3 <- spc_join$slopes_avg[spc_join$p_value<=0.05]
  lm3 <- lm(dep3 ~ ind3)
  print("expansion+infill");print(summary(lm3))
  
  # Add model
  ind4 <- spc_join$urb_tot[spc_join$p_value<=0.05]*100
  dep4 <- spc_join$slopes_avg[spc_join$p_value<=0.05]
  lm4 <- lm(dep4 ~ ind4)
  # abline(lm4, lty=1)
  print("total area");print(summary(lm4))
  
  # Add model
  ind5 <- spc_join$NPS_pct[spc_join$p_value<=0.05]
  dep5 <- spc_join$slopes_avg[spc_join$p_value<=0.05]
  lm5 <- lm(dep5 ~ ind5)
  print("NPS_pct");print(summary(lm5))
  
  ##############################################################################
  
  # Relationship with total urban area and newer SC change
  windows(6.5,6.5)
  plot(spc_join$Watershed.Percent.Urban..DW., spc_join$slopes_newer,
       xlab="Built area (% watershed)", ylab = "SC change (uS/cm per year 2017-2022)",
       main=NULL,col="grey")
  grid()
  points(spc_join$Watershed.Percent.Urban..DW.[spc_join$p_value<=0.05], spc_join$slopes_newer[spc_join$p_value<=0.05])
  
  # Add labels
  toLabel <- spc_join$slopes_newer < -10
  text(
    x=spc_join$Watershed.Percent.Urban..DW.[toLabel] + c(-1,-1,-1,-1), #MICR, KLVA, LUBR, ROC3
    y=spc_join$slopes_newer[toLabel] + c(0,0,-0.5,+0.5),
    labels=spc_join$MonitoringLocationIdentifier[toLabel],
    adj=1
  )
}


################################################################################
### boxplot_func() #############################################################
################################################################################

boxplot_func <- function(wdata_avgd, outputs, ylabs,ylim, threshold, CharacteristicName){
  
  ### Set up plot
  windows(6.5,6.5)
  
  ### Boxplots
  # Pivot
  pw <- wdata_avgd[wdata_avgd$CharacteristicName==CharacteristicName,] %>%
    pivot_wider(names_from="MonitoringLocationIdentifier",
                values_from="ResultMeasureValue")
  
  # Create a NA data frame to convert the list to
  pw_df <- data.frame() 
  
  # Populate data frame
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
  
  # Make boxplot
  par(mar=c(6,4,1,1)+1,mgp=c(3,1,0))
  boxplot(toPlot, names=colnames(toPlot),las=2,ylim=c(0,ylim),
          ylab=ylabs,cex.axis=0.65)
  grid()
  boxplot(toPlot, names=colnames(toPlot),las=2,ylim=c(0,ylim),
          ylab=ylabs,cex.axis=0.65, add=T)
  
  # Add Morgan 2007 line and title
  abline(h=threshold, col="red",lty=2)
  
  # Add legend
  legend("topleft",legend=c("Measurement","Fresh-brackish threshold"),pch=c("o","-"),
         col=c("black","red"),inset=c(0.06,0),bg="white")
} 


################################################################################
### SC time series function ####################################################
################################################################################

sc_series_func <- function(wdata_avgd, current_sites, plot_num, CharacteristicName){
  
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
         type='n',log='y', xlab="")
    grid()
    lines(time_series$Date, time_series$Value)
  }
}
