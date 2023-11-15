# This script is to automate the manual measurement cleaning
# for data sent from Megan/Stephanie

site_list <- data_frame(site_name = c("bl4","aspen", "greyrock" ))

kampf_manual <- read_csv("data/flow/flow_kampf/manual_stage_q.csv")%>%
 dplyr::select(site_code = site, datetime, manual_stage_cm, notes = comments )%>%
  filter(site_code %in% site_list$site_name & manual_stage_cm != is.na(manual_stage_cm) )%>%
  mutate(datetime = as.character(as.POSIXct(datetime, tz = "MST",  format = "%m/%d/%Y %H:%M")))%>%
  distinct(datetime, .keep_all = TRUE)%>%
  mutate(first_measurement_DT = NA, 
         last_measurement_DT = NA, 
         first_measurement_cm = NA, 
         last_measurement_cm =NA)
  

    

  
  create_first_last <- function(site_name){
    
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
  
  #final dataframe
  manual_measurements_kampf <- map_dfr(site_list$site_name, create_first_last)%>%
    dplyr::select(site_code, first_measurement_DT, last_measurement_DT, first_measurement_cm, last_measurement_cm, notes)
  
  
  #Creating stage vs Q dataframe
  kampf_stage_Q <- read_csv("data/flow/flow_kampf/manual_stage_q.csv")%>%
    dplyr::select(site_code = site, datetime, manual_stage_cm, discharge_Ls, notes = comments )%>%
    filter(site_code %in% site_list$site_name & manual_stage_cm != is.na(manual_stage_cm)
           & discharge_Ls != is.na(discharge_Ls))%>%
    mutate(datetime = as.character(as.POSIXct(datetime, tz = "MST",  format = "%m/%d/%Y %H:%M")))
  
  kampf_stage_Q_means<- kampf_stage_Q%>%
    group_by(datetime, site_code)%>%
    mutate(mean_Ls = mean(discharge_Ls))%>%
    ungroup()%>%
    distinct(datetime, .keep_all = TRUE)%>%
    mutate(q_cfs = (mean_Ls/28.316847))%>%
    dplyr::select(site_code, datetime, manual_stage_cm, mean_Ls, q_cfs, notes)

  
  #Export!
#  write_csv(kampf_stage_Q_means, "data/kampf_test_manual_stage_Q.csv")
# write_csv(manual_measurements_kampf, "data/kampf_test_manual_stage.csv")

