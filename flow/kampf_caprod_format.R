# This script is to automate the manual measurement cleaning
# for data sent from Megan/Stephanie

kampf_manual <- read_csv("data/discharge_google.csv")%>%
 dplyr::select(site_code = site, datetime, manual_stage_cm, notes = comments )%>%
  filter(site_code %in% c("bl4","aspen", "greyrock" ) & manual_stage_cm != is.na(manual_stage_cm) )%>%
  mutate(datetime = as.character(as.POSIXct(datetime, tz = "MST",  format = "%m/%d/%Y %H:%M")))%>%
  distinct(datetime, .keep_all = TRUE)%>%
  mutate(first_measurement_DT = NA, 
         last_measurement_DT = NA, 
         first_measurement_cm = NA, 
         last_measurement_cm =NA)
  
  site_list <- data_frame(site_name = unique(kampf_manual$site_code))
    

  
  create_start_end <- function(site_name){
    
    #subset by indv site
    manual_by_site <- filter(kampf_manual, site_code == site_name)%>%
      arrange(datetime)
    
    #setting first measurement to be first date time/stage
    #then setting last measurement to be the following date/time and stage
    for (i in 1:(length(manual_by_site$site_code)-1)) {
      manual_by_site$first_measurement_DT[i] = manual_by_site$datetime[i] 
      manual_by_site$last_measurement_DT[i] = manual_by_site$datetime[i+1]  
      manual_by_site$first_measurement_cm[i] = manual_by_site$manual_stage_cm[i] 
      manual_by_site$last_measurement_cm[i] = manual_by_site$manual_stage_cm[i+1] 
    }
    
    #correct dates to be in POSIXCT and then remove last row
    final_by_site <- manual_by_site%>%
      mutate(first_measurement_DT = as.POSIXct(first_measurement_DT, tz = "MST"),
             last_measurement_DT = as.POSIXct(last_measurement_DT, tz = "MST"))%>%
      head(length(manual_by_site$site_code)-1)
    
    return(final_by_site)
    
  }
  
  manual_measurements_kampf <- map_dfr(site_list$site_name, create_start_end)

