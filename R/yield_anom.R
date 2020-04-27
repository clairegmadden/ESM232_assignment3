

# set up function to calculate almond yield anomaly

# define variables and parameters
# tn is minumum temperature (t) of the month(n)
# tx is the maximum temperature (t) of the month(x)
# p is precip

#add error checking for year, month etc how things are labled in toehr climate data 

# function in paper : Y = -0.015tn,2 - 0.0046t(^2)n,2 - 0.07P1 + 0.0043P(^2)1 +0.28


yield_anom = function(tn1 = -0.015, tn2 = -0.0046, p1 = -0.07, p2 = 0.0043, int = 0.28, clim_data) {
  
  clim_monthly <- clim_data %>% 
  group_by(month, year) %>% 
  summarize(meantmin = mean(tmin_c),
            precip=mean(precip)) %>%  #maybe check if this is supposed to be mean rather than sum (slack notes)  
    ungroup()
  
  clim_monthly$precip = ifelse(clim_monthly$precip < 0, return("Precipitation cannot be less than zero"), clim_monthly$precip)
  
  precip_2 <- clim_monthly %>% 
  select(-meantmin) %>% 
  filter(month == 2)
  
  temp_1 <- clim_monthly %>% 
  select(-precip) %>% 
  filter(month == 1)
  
    
  df_anom <- data.frame(year = temp_1$year, y_anom = NA) 
  
  
    for(i in 1:nrow(df_anom)) {
    
  df_anom$y_anom[i] <- tn1*temp_1$meantmin[i] + 
    tn2*temp_1$meantmin[i]^2 + 
    p1*precip_2$precip[i] + 
    p2*precip_2$precip[i]^2 + 
    int
    
  }
  

  
  y_anom_yr <- dplyr::select(df_anom, c(year,y_anom))
  
  min_anom <- min(y_anom_yr$y_anom)
  max_anom <- max(y_anom_yr$y_anom)
  
  return(list(y_anom_yr, min_anom, max_anom))
  
}




# to plot yield anom, you can make the first part of the output into a dataframe to plot


