#Function to create desired plot for ts analysis
#default plot will be all data from CHD with bars for corresponding grab samples
library(ggpubr)
build_ts_plot <- function(sensor_df = sensor_data, chem_df = tidy_chem, 
                          site_name= "CHD", param = "turb",
                          start_dt = "2023-09-07 08:00",
                          end_dt = "2023-10-27 08:00",
                          grabs = FALSE, 
                          color = "#0072B2"){
  
  #Convert datetime to posixct
  start_dt <- as.POSIXct(start_dt,tz = "MST", format = "%Y-%m-%d %H:%M")
  start_date <- as.Date(start_dt)
  end_dt <- as.POSIXct(end_dt,tz = "MST", format = "%Y-%m-%d %H:%M")
  end_date <- as.Date(end_dt)
  
  #Common name to sensor name
  param_chosen <- filter(sensor_meta, grepl(param,param_common , ignore.case = TRUE))
  
  #filter all sensor data to grab just the site and date range desired
  site_sensor <- filter(sensor_df, grepl(site_name, site, ignore.case = TRUE))%>%
    filter(timestamp >= start_dt & timestamp <= end_dt)%>%
    #pivot wider for graphing
    pivot_wider(id_cols = timestamp, names_from = parameter, values_from = value)
  
  
  #filter all discrete data to grab just the site and date range desired
  site_discrete <- filter(chem_df,grepl(site_name, site_code, ignore.case = TRUE))%>%
    filter( dt >= start_date &dt <= end_date)%>%
    rename(timestamp = dt)
  if(grabs == FALSE){
    sensor_ts <- ggplot(data = site_sensor, aes(x = timestamp, y = .data[[param_chosen$param_sonde[1]]])) +
      geom_line(color = color) +
      theme_minimal(base_size = 20) +
      ylab(param_chosen$param_w_units)+
      xlab("Date")
    
    sensor_ts
  }else{
    sensor_ts <- ggplot(data = site_sensor, aes(x = timestamp, y = .data[[param_chosen$param_sonde[1]]])) +
      geom_line(color = color) +
      geom_vline(data = site_discrete, aes(xintercept = as.numeric(timestamp)), color = "black", linetype = "longdash", size = 1) +
      theme_minimal(base_size = 20) +
      ylab(param_chosen$param_w_units)+
      xlab("Date")
    
    sensor_ts
  }
  #create plot

  
}
# Test function
build_ts_plot(site_name = "pbd", param = "turb", color = "#00946c", start_dt = "2023-07-23 00:00", end_dt = "2023-09-15 00:00", grabs = TRUE)
