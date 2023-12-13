# Correlate grab sample and sensor data
# This function will generate a plot showing the linear relationship between grab sample data processed by RMRS and 
# sonde data collected by In Situ Inc sondes
# @param sites A string of the chosen site names or all.
# @param sensor_param A string of the sensor parameter name. Use the call: sensor_meta$param_common, to get a list of available parameters
# @param grab_param A string of the grab sample parameter name. Use the call: chem_units$simple, to get a list of available parameters
# @param timestep  A string of one of: 15min, 1hour, 1day. 15 min grabs the nearest data point after site visit, 1 hour aggregates 4 nearest data points after the site visit and daily aggregates all data on that day
# @param matched_axes Logical, if TRUE plot will have same Y and X limits on axes, this should only be used when directly comparing variables (ie: Turb vs Turbidity or SC vs Spec Cond),
#                                 False should be used on non identical variables (ie: NO3 vs Turb, or Cl vs SC)
# @return plot of sensor vs lab data and printing of linear model results
# @examples
# correlate_grab_sonde(sites = "pbd", sensor_param = "spec_cond", grab_param = "SC", timestep = "15min")
# correlate_grab_sonde(sites = "all", sensor_param = "spec_cond", grab_param = "SC", timestep = "1hour")
# correlate_grab_sonde(sites = "all", sensor_param = "turb", grab_param = "Turbidity", timestep = "15min", matched_axes = FALSE)


correlate_grab_sonde <-function(sites = "all", sensor_param = "turb",grab_param = "Turbidity",  timestep = "15min", matched_axes = TRUE){
  
  #Common name to sensor name
  param_chosen <- filter(sensor_meta, param_common %in% sensor_param)%>%
    mutate(lab_param = filter(chem_units,  simple %in% grab_param)%>%pull(simple), 
           lab_w_units = filter(chem_units, simple %in% grab_param)%>%pull(combined))
rm(sensor_param, grab_param)
  
  if(sites == "all"){
    # filter for the desired parameter for all sites
    sensor_filtered <- filter(sensor_data, parameter %in% param_chosen$param_sonde )%>%
      #using padr, thicken to get all 15 min timedata and then pad (adjust times to 15 min interval)
      thicken(interval = "15 min", by = "timestamp")%>%
      pad(by = "timestamp_15_min", interval = "15 min")%>%
      #only select needed data
      select(site, dt = timestamp_15_min, sensor_value = value, parameter)
    
  }else if(tolower(sites) %in% tolower(sonde_sites)){
    # filter for the desired parameter for selected sites
    sensor_filtered <- filter(sensor_data, parameter %in% param_chosen$param_sonde & site %in% tolower(sites))%>%
      #using padr, thicken to get all 15 min timedata and then pad (adjust times to 15 min interval)
      thicken(interval = "15 min", by = "timestamp")%>%
      pad(by = "timestamp_15_min", interval = "15 min")%>%
      #only select needed data
      select(site, dt = timestamp_15_min, sensor_value = value, parameter)
  }else{
    cat("User Input Error: Sites \nOptions: 'all' or choose specfic site\n")
  }

discrete_filtered <- tidy_chem %>%
  filter(!is.na(dt))%>%
  mutate(site= tolower(site_code),
         dt_round = round_date(dt, "15 minutes"))%>%
  #grab only sonde sites
  filter(site %in% unique(sensor_filtered$site))%>%
  # grab site, dt and selected parameter (GRAB PARAM)
  select(site,grab_dt = dt_round, .data[[param_chosen$lab_param]] )
    # Function to grab the four data points after a site visit, average them  and return DT, site, avged value


#matching up grab sample and sensor data
    calculate_averages <- function( grab_dt, site_select) {
      # convert to date to filter sensor data to only data on same date as sample
      grab_date = as.Date(grab_dt, tz = "MST")
      
      #if timestep is hour then grab hour of data (4 points) after site visit
      if(timestep == "1hour"){
      sensor_filtered %>%
        #create date for match up
        mutate(date = as.Date(dt, tz = "MST")) %>%
        filter(grepl(site_select, site, ignore.case = TRUE) & date == grab_date & dt > grab_dt )%>%
        dplyr::arrange(dt)%>%
        # grab the data 5 data points after a site visit
        dplyr::slice_head(n = 4)%>%
        #take the median
        reframe(site = site, grab_dt = grab_dt, avg_value = mean(sensor_value, na.rm = TRUE))%>%
        distinct()
        
    }#if timestep is hour then grab 1 data after site visit
      else if(timestep == "15min"){
      sensor_filtered %>%
        #create date for match up
        mutate(date = as.Date(dt, tz = "MST")) %>%
        filter(grepl(site_select, site, ignore.case = TRUE) & date == grab_date & dt > grab_dt )%>%
        dplyr::arrange(dt)%>%
        na.omit()%>%
        # grab the data point after site visit
        dplyr::slice_head(n = 1)%>%
        #take the median
        reframe(site = site, grab_dt = grab_dt, avg_value = sensor_value)%>%
        distinct()
    } #if timestep is 1day then grab all the data from that day
      else if(timestep == "1day"){
      sensor_filtered %>%
        #create date for match up
        mutate(date = as.Date(dt, tz = "MST")) %>%
        filter(grepl(site_select, site, ignore.case = TRUE) & date == grab_date)%>%
        dplyr::arrange(dt)%>%
        na.omit()%>%
        #take the median
        reframe(site = site, grab_dt = grab_dt, avg_value = mean(sensor_value, na.rm = TRUE))%>%
        distinct()
  }
}
    
    if(timestep %in% c("15min", "1hour", "1day")){
      
      discrete_vs_sensor <- discrete_filtered%>%
        #grab only dt and site from discrete samples for calculate avg fucn
        select(grab_dt, site_select = site)%>%
        # map over calc avg fucn
        pmap_dfr(., calculate_averages)%>%
        # join with lab data by site and grab dt
        left_join(discrete_filtered, by = c("grab_dt", "site"))%>%
        #remove NAs
        na.omit()
    
    #simple linear regression
      lm_results <- lm(discrete_vs_sensor[[param_chosen$lab_param]] ~ avg_value, data = discrete_vs_sensor)
      lm_tidy <- tidy(lm_results)
    #print results of linear regression
      cat("\nLinear Model Results:\nLab ", param_chosen$lab_param, " vs Sensor ",param_chosen$param_sonde,
          "\nn =",nrow(discrete_vs_sensor), "\nMultiple R-squared: ", round(summary(lm_results)$r.squared, digits = 4), "\np-value: ", round(lm_tidy$p.value[2], digits = 2), "\n"  )
      
      
      #grab min and max values to make axes equal in plot
      min_value <- min(discrete_vs_sensor[[param_chosen$lab_param]], discrete_vs_sensor$avg_value)
      max_value <- max(discrete_vs_sensor[[param_chosen$lab_param]], discrete_vs_sensor$avg_value)
      #plot sensor vs grab data
     data_plot <-  discrete_vs_sensor %>%
        ggplot(aes(x = .data[[param_chosen$lab_param]], y = avg_value)) +
        geom_point(aes(color = site)) +
        stat_poly_line(se = FALSE) +
        stat_poly_eq(use_label(c("eq", "R2"))) +
        labs(title = paste0("Lab ", param_chosen$lab_param, " vs Sensor ",param_chosen$param_sonde  ),
              subtitle = paste0("Sensor Timestep: ", timestep),
             x = paste0("Lab: ", param_chosen$lab_w_units),
             y = paste0("Sensor: ", param_chosen$param_w_units)) +
        theme_minimal() 
     if(matched_axes == TRUE){
       data_plot+
         geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
         coord_cartesian(xlim = c(min_value, max_value), ylim = c(min_value, max_value))
     }else{
       data_plot
     }
    
        
      
    }else{
      cat("User Input Error: timestep\nOptions: 15min, 1hour\n")
    }
  
}

#test function
#correlate_grab_sonde(sites = "all", sensor_param = "spec_cond", grab_param = "SC", timestep = "15min", matched_axes = FALSE)