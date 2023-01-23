# Larimer county Q downloader
### Larimer County portal:
# https://larimerco-ns5.trilynx-novastar.systems/novastar/operator/#/map



#adapted from B's code: https://github.com/Lake-Sunapee-Protective-Association/nh-des-dam-data

source("package_loader.R")
package_load(pack_req)

#start and end dates
start_date = as.Date("2010-01-01 00:00")
end_date = format(Sys.time(), "%Y-%m-%d %H:%M")


#station dashboard url
station_dashboard <- "https://larimerco-ns5.trilynx-novastar.systems/novastar/operator/#/stationDashboard/"

station_choice <- paste0(station_dashboard, 
                         #insert choice number here
                         "11021")
discharge_choice <- POST(
  url = station_choice, 
  
)
test_url <- "https://larimerco-ns5.trilynx-novastar.systems/novastar/data/api/v1/stationSummaries?forOperatorStationDashboard=true&stationNumId=11560&periodStart=2022-01-16T13:30:00-07:00&periodEnd=2023-01-24T13:30:00-07:00"
text_Q_pull <-GET(url = test_url)
text_Q_pull

#Function to create URL based on each site with flow
create_url <- function(site_num, start_dt, end_dt){
  start_url <- "https://larimerco-ns5.trilynx-novastar.systems/novastar/data/api/v1/stationSummaries?forOperatorStationDashboard=true&stationNumId="
  mid_url <- "&periodStart="
  end_url <- "&periodEnd="
  
  final_url <- paste0(start_url, site_num, mid_url, start_dt, end_url, end_dt)
  return(final_url)
}

#gives content of httr pull
named_listQ <- content(text_Q_pull)
# find list where discharge data is stored
test2_Q <- named_listQ[["stationSummaries"]][[1]][["ts"]][[3]][["data"]]
# find station metadata
get_meta() <- function(){
  station_meta <- tibble(
    id = named_listQ[["stationSummaries"]][[1]][["id"]], 
    numid = named_listQ[["stationSummaries"]][[1]][["numId"]], 
    name = named_listQ[["stationSummaries"]][[1]][["name"]]
  )
  
}


get_q <- function(i){
  
 unlist(test2_Q[[i]])
  
}

testing_wooooo <- map_dfr(seq(1, length(test2_Q), 1), get_q)%>%
  mutate(numid = named_listQ[["stationSummaries"]][[1]][["numId"]])
