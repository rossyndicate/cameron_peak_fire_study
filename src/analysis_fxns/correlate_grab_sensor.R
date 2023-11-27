correlate_grab_sonde <-function(sites = "all", sensor_param = "turb",grab_param = "Turbidity",  timestep = "15min" ){
  
  #Common name to sensor name
  param_chosen <- filter(sensor_meta, grepl(sonde_param, param_common , ignore.case = TRUE))
  
  #filter all sensor data to grab just the site and date range desired
  site_sensor <- filter(sensor_df, grepl(site_name, site, ignore.case = TRUE))%>%
    filter(timestamp >= start_dt & timestamp <= end_dt)%>%
    #pivot wider for graphing
    pivot_wider(id_cols = timestamp, names_from = parameter, values_from = value)
  
  
  #filter all discrete data to grab just the site and date range desired
  site_discrete <- filter(chem_df,grepl(site_name, site_code, ignore.case = TRUE))%>%
    filter( dt >= start_date &dt <= end_date)%>%
    rename(timestamp = dt)
  
  if(sites == "all"){
    #filter all sensor data to grab just the site and date range desired
    
    # filter for the desired parameter
    sensor_filtered <- filter(sensor_data, parameter %in% param_chosen$param_sonde )%>%
      #using padr, thicken to get all 15 min timedata and then pad (adjust times to 15 min interval)
      thicken(interval = "15 min", by = "timestamp")%>%
      pad(by = "timestamp_15_min", interval = "15 min")%>%
      #only select needed data
      select(site, dt = timestamp_15_min, sensor_value = value, parameter)
    

    #grab water sample data related to sites with sensor locations, 
    #round to 15 min interval and grab the parameter
    discrete_filtered <- tidy_chem %>%
      filter(!is.na(dt))%>%
      mutate(site= tolower(site_code),
             dt_round = round_date(dt, "15 minutes"))%>%
      select(site,grab_dt = dt_round, .data[[grab_param]] )%>%
      filter(site %in% unique(sensor_filtered$site))
      
    
    # Modify the calculate_averages function to include a site filter
    calculate_averages <- function( grab_dt, site) {
      sensor_filtered %>%
        # grab the data 90 min after a grab for a particular site
        filter(dt >= grab_dt & dt <= grab_dt + minutes(15 * 6) & site == site)%>%
        #take the median
        reframe(site = site, grab_dt = grab_dt, avg_value = median(sensor_value, na.rm = TRUE))%>%
        distinct()
    }
    
    #grab only dt and site
    grabs_dt <- discrete_filtered%>%
      select(grab_dt, site)
    
    combined_df <- pmap_dfr(grabs_dt, calculate_averages)%>%
      left_join(discrete_filtered, by = c("grab_dt", "site"))%>%
      na.omit()

    combined_df %>%
      
      ggplot(aes(x = Turbidity, y = avg_value, color = site)) +
      geom_point() +
      #Currently only doing it by a single site
      stat_poly_line() +
      stat_poly_eq(use_label(c("eq", "R2"))) +
      labs(title = "Turbidity",
           x = "Grab Turb",
           y = "Sensor Turb") +
      theme_minimal() 
  }
}