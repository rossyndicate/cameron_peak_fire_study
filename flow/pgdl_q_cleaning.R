#This script will pull in all the downloaded CSVS of Q, precip, etc from:
###Larimer County portal: 
#https://larimerco-ns5.trilynx-novastar.systems/novastar/operator/#/map
##DWR:
#https://dwr.state.co.us/tools/stations

###USGS: Pulled in using dataRetrieval pakage

#Greeley: Hand delivered

##Fort Collins



#Info:
#Workflow to pull all data automatically in progress
#csv data was grabbed manually
# Date range: 2019-01-01 to 2023-01-19

#Setup
source("package_loader.R")
package_load(pack_req)


#---CODE----#

#parameter code for discharge
pcode_Q <- "00060"
#sites
site_names <- data.frame( site_no = c("06752260", "06752280", "06751490", "06746095","06746110", "06751145","06751150"), 
                          site_name = c("CLP at FC","CLP above BoxEld" ,"N fork near Livermore", "JW above Res" ,"JW below Res" , "N fork above Halligan", "N fork below Halligan") )

clp_usgs_sites <- c("06752260", "06752280", "06751490", "06746095","06746110", "06751145","06751150")
#start date
start_date <- as.POSIXct("2019-01-01" , tz = "UTC")
#end date = today
end_date <- lubridate::force_tz(Sys.Date(), tzone = "UTC")

#function to select Q from NWIS given site code. Dates selected above
read_q <- function(sites){
  site_Q <- readNWISuv(siteNumbers = sites, 
             parameterCd = pcode_Q, 
             tz = "America/Denver")
  return(site_Q)
  Sys.sleep(10)
}

#Pull in all Q for sites selected above
#May take a sec to load
clp_usgs_Q <- map(clp_usgs_sites, read_q)%>%
  list_rbind()
#make sure all sites are there
unique(clp_usgs_Q$site_no)

clean_clp_usgs_Q <- clp_usgs_Q %>%
  rename(Q_cfs = X_00060_00000,
         Q_flag = X_00060_00000_cd,
         tz = tz_cd) %>%
  left_join(site_names, by = "site_no")

plot_all_sites <- clean_clp_usgs_Q %>%
  ggplot(aes(
    x = dateTime,
    y = Q_cfs,
    group = site_name,
    color = site_name
  )) +
  geom_line() +
  theme_bw() +
  facet_wrap( ~ site_name)

plot(plot_all_sites)

clp_usgs_Q_final <- clean_clp_usgs_Q %>% 
  select(agency = agency_cd, 
         site_no, 
         DT = dateTime, 
         Q_cfs, Q_flag, tz, 
         site_name)

write.csv(clp_usgs_Q_final, file = "data/usgs_clp_Q.csv")

#------- Pull in Larimer County Data -----#




#------ Pull in DWR Data ------#
#https://dwr.state.co.us/tools/stations
