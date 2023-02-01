# Larimer county Q downloader
### Larimer County portal:
# https://larimerco-ns5.trilynx-novastar.systems/novastar/operator/#/map



#adapted from B's code: https://github.com/Lake-Sunapee-Protective-Association/nh-des-dam-data

source("package_loader.R")


#sites with Q
clp_Q_site_nums <- tibble(station_ids = c("11531", "11530", "11525", 
                    "11514", "11004", "11515",
                    "11518", "11517", "11516",
                    "6770", "11083", "11082",
                    "11009","7130","10408", "11021"))
bigt_Q_site_nums <- tibble(station_ids = c("11567","11566","11564","11563","11562",
                     "11561","11560", "11559","11558",
                     "95100", "95000","3640","3610","3570"," 3530" ))


#station dashboard url
station_dashboard <- "https://larimerco-ns5.trilynx-novastar.systems/novastar/operator/#/stationDashboard/"

create_url <- function(site_num){
  end_dt <- "2023-01-24T13:30:00-07:00"
  start_url <- "https://larimerco-ns5.trilynx-novastar.systems/novastar/data/api/v1/stationSummaries?forOperatorStationDashboard=true&stationNumId="

  final_url <- tibble( site_num = site_num, 
                       site_url = paste0(start_url, site_num))
  return(final_url)
}
#create urls with site num for meta grab
clp_url_meta <- map_dfr(clp_Q_site_nums, create_url)%>%
  as.data.frame()


#Function to create URL to pull datetime and sensor data
create_url_dt <- function(site_num){
  end_dt <- "2023-01-24T13:30:00-07:00"
  start_url <- "https://larimerco-ns5.trilynx-novastar.systems/novastar/data/api/v1/stationSummaries?forOperatorStationDashboard=true&stationNumId="
  mid_url <- "&periodStart=2000-01-01T00:00:00-07:00&periodEnd="
  
  final_url <- tibble( site_num = site_num, 
                      site_url = paste0(start_url, site_num, mid_url, end_dt))
  return(final_url)
}

#get all urls for Poudre and Big Thompson flow sites
clp_urls_dt <- map_dfr(clp_Q_site_nums, create_url_dt)%>%
  as.data.frame()
bigt_urls_dt <- map_dfr(bigt_Q_site_nums, create_url_dt)




clp_url_meta_test<- clp_url_meta
#dataframe to store all json metadata to be used later
final_meta <- data.frame()

for( i in 1:length(clp_url_meta_test$site_url)){
  
  #create request to novastar website using url created above
  request <-GET(url = clp_url_meta_test$site_url[i])
  #gives content of httr pull
  total_list <- content(request)
  
  
  sensor_list <- total_list[["stationSummaries"]][[1]][["dataTypes"]]
  
  station_meta <- as.data.frame(do.call(rbind, sensor_list))%>%
    mutate(name = as.character(name))%>%
    distinct(name)%>%
    mutate(sensor_list_num = row_number())%>%
    pivot_wider( names_from = "name", values_from = "sensor_list_num")%>%
    mutate(id = total_list[["stationSummaries"]][[1]][["id"]], 
           numid = total_list[["stationSummaries"]][[1]][["numId"]], 
           name = total_list[["stationSummaries"]][[1]][["name"]],
           elevation = total_list[["stationSummaries"]][[1]][["elevation"]],
           lat = total_list[["stationSummaries"]][[1]][["latitude"]],
           long = total_list[["stationSummaries"]][[1]][["longitude"]], 
           site_num = clp_url_meta_test$site_num[i])
  final_meta <- plyr::rbind.fill(final_meta, station_meta)

  Sys.sleep(10)  
}




get_q <- function(site_num){
 unlist(discharge_list[[i]])
}




final_df_q <- data.frame()

#get_q <- function(site_url, site_num){

clp_urls_dt_test <- head(clp_urls_dt, 3)



  for(i in 1:length(clp_urls_dt_test$site_url)){
  
  #create request to novastar website using url created above
  request <-GET(url = clp_urls_dt_test$site_url[i])
  
  #trouble shooting
  #request <-GET(url = site_url)

  #request<- GET(url = "https://larimerco-ns5.trilynx-novastar.systems/novastar/data/api/v1/stationSummaries?forOperatorStationDashboard=true&stationNumId=6770&periodStart=2000-01-01T09:33:34-07:00&periodEnd=2023-01-25T09:33:34-07:00")
  #gives content of httr pull
  total_list <- content(request)
  
  #find list number where q is located
  discharge_sensor_num <- filter(final_meta, numid == clp_urls_dt_test$site_num[i])
  #discharge_sensor_num <- filter(final_meta, numid == "6770")
  
  q_list <- as.integer(as.character(discharge_sensor_num$DischargeRiver[1])) 
  
  q_list
    # find list where discharge data is stored
    
  discharge_list <- total_list[["stationSummaries"]][[1]][["ts"]][[q_list]][["data"]]
  
  q_df <- map_dfr(seq(1, length(discharge_list), 1), get_q)%>%
    mutate(numid = clp_urls_dt_test$site_num[i])
  
  Sys.sleep(10)
  
  final_df_q<- rbind(final_df_q, q_df)
  
}

final_df_q<- final_df_q%>%
  mutate(datetime = as.POSIXct(dt), 
         q_cfs = v)

plot_test_Q <- final_df_q%>%
  ggplot(aes( x= datetime, y = q_cfs))+
  geom_point()+
  theme_bw()+
  facet_wrap(~numid)
plot(plot_test_Q)

