SoftConstraintsFunc <- function(Water_Data, site, water_char, distribution){

  # Extract data
  Demo_Data<-Water_Data %>% 
    filter(MonitoringLocationIdentifier==site,
           CharacteristicName ==water_char) %>% 
    select(MonitoringLocationIdentifier,ActivityStartDate,CharacteristicName,ResultMeasureValue  ) %>% 
    mutate(ResultMeasureValue=as.numeric(ResultMeasureValue))
  
  nrow(Demo_Data)
  
  Demo_Data %>% pull(ActivityStartDate) %>% unique %>% length()
  
  Demo_Data %>% filter(is.na(ResultMeasureValue))
  
  # Plot time series
  A <- ggplot(Demo_Data, aes(x=ActivityStartDate, y=ResultMeasureValue)) +
    geom_point() +
    ggtitle(paste(site,water_char))


  Demo_Data<-Demo_Data %>%
    mutate(Year=year(ActivityStartDate),
           Month=month(ActivityStartDate),
           DecDate=decimal_date(ActivityStartDate),
           YrFraction=decimal_date(ActivityStartDate)-Year,
           Date_cos=cos(2*pi*YrFraction),
           Date_sin=sin(2*pi*YrFraction))

  # Gamma
  if (distribution=="gamma"){
    DO_Model<-brm(ResultMeasureValue~s(DecDate, k=4) + s(Date_cos,Date_sin, k=4),
                  data = Demo_Data,
                  family=Gamma(link="log"),
                  chains = 4,
                  cores=4,
                  control = list(adapt_delta = 0.99, max_treedepth=15),
                  warmup=500,
                  iter=2000,
    )
  }

  # Gaussian
  if (distribution=="gaussian"){
    DO_Model<-brm(ResultMeasureValue~s(DecDate, k=4) + s(Date_cos,Date_sin, k=4),
                  data = Demo_Data,
                  family=gaussian(),
                  chains = 4,
                  cores=4,
                  control = list(adapt_delta = 0.99, max_treedepth=15),
                  warmup=500,
                  iter=2000,
    )
  }

  # Plot the model
  B <- Demo_Data%>% filter(!is.na(ResultMeasureValue)) %>% add_predicted_draws(DO_Model ) %>%
    ggplot(aes(x=ActivityStartDate, y=ResultMeasureValue)) +
    stat_lineribbon(aes(y=.prediction), .width=c(.9),
                    alpha=.8, color="black")+
    geom_point(data=Demo_Data, color="blue")+
    scale_fill_brewer(palette="Greys")+
    theme_bw()

  # Create predictions
  Prediction_Data<-
    data.frame(ActivityStartDate=seq(ymd("2023-01-01"), ymd("2023-12-31"), by="days"),
               Year=2023) %>%
    mutate( Month=month(ActivityStartDate),
            DecDate=decimal_date(ActivityStartDate),
            YrFraction=decimal_date(ActivityStartDate)-Year,
            Date_cos=cos(2*pi*YrFraction),
            Date_sin=sin(2*pi*YrFraction))
  
  Predictions<-predicted_draws(DO_Model, Prediction_Data) %>%
    summarize(Median=quantile(.prediction, probs=.5), Lower=quantile(.prediction, probs=.1),
              Upper=quantile(.prediction, probs=.9, .by=ActivityStartingDate)) %>% ungroup()
  
  head(Predictions)
  
  # Plot predictions
  C <- Predictions %>% ggplot(aes(x=ActivityStartDate))+
    geom_ribbon(aes(ymin=Lower, ymax=Upper), fill="grey")+
    geom_line(aes(y=Median))+
    theme_bw() +
    ggtitle(distribution)
  
  # Print the plots
  windows(6.5,9)
  print(
    ggarrange(A,B,C, ncol=1, nrow=3)
  )
  
  Soft_Limits<-Predictions %>%
    summarise(Lower=median(Lower), Upper=median(Upper), .by=Month)
  
  # Generate table of outputs
  Soft_Limits
  return(Soft_Limits)
}