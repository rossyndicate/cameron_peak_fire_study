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


# Set the HUC code and parameter code
huc_code <- "10190007"
parameter_code <- "00060"

# Use the whatNWISdata function to get site information for all sites in the HUC with the specified parameter code
usgs_clp_site_info <- whatNWISdata(parameterCd = parameter_code, huc = huc_code)%>%
  distinct(site_no ,.keep_all = TRUE)%>%
  filter(end_date >= "2020-01-01")%>%
  select(site_no, site_name = station_nm, lat = dec_lat_va, long = dec_long_va, start_date = begin_date, end_date)

clp_usgs_sites<- select(usgs_clp_site_info, site_no)
site_names <- select(usgs_clp_site_info, c(site_no, site_name))

#save for later use
#write_csv(usgs_clp_site_info, "data/Q_modeling/usgs_sites_simple.csv")



#function to select Q from NWIS given site code. Dates autoselected for oldest and newest data available
read_q <- function(sites){
  site_Q <- readNWISuv(siteNumbers = sites, 
             parameterCd = parameter_code, 
             tz = "America/Denver")
  return(site_Q)
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

#plot(plot_all_sites)

#ggsave("output/USGS_sites.jpg", width = 20, height = 12)

clp_usgs_Q_final <- clean_clp_usgs_Q %>% 
  select(agency = agency_cd, 
         site_no, 
         DT = dateTime, 
         Q_cfs, Q_flag, tz, 
         site_name)

write_csv(clp_usgs_Q_final, file = "data/Q_modeling/usgs_clp_Q.csv")


#------- Pull in Larimer County Data -----#

# URL of the site to select specific stations
station_url <- "https://larimerco-ns5.trilynx-novastar.systems/novastar/operator/#/stationDashboard/"



#------ Pull in DWR Data ------#
#https://dwr.state.co.us/tools/stations


#----- Kampf site locations ----#
kampf_meta <- read_csv("data/Kampf_metadata.csv")%>%
  select(name = Name, 
         type = Type, 
         lat = Latitude, 
         long = Longitude, 
         start_date = Sensor_install, 
         active = Sensor_remove, 
         comments = Comments)%>%
  filter(type == "stream" &active == "still running")%>%
  distinct(name, .keep_all = TRUE)%>%
  select(name, lat, long, start_date)

write_csv(kampf_meta, "data/Q_modeling/kampf_stream_sites_simple.csv")


#----- Pull in Greeley data ------#

brnr_ptrr_daily_q <- read_csv("data/Q_modeling/BRNR_PTRR_Q.csv")%>%
  mutate(inflow = replace_na(inflow, 0), 
         date = as.Date(date, format = "%m/%d/%y"))

plot_brnr_ptrr <- brnr_ptrr_daily_q%>%
  ggplot(aes( x= date ))+
  geom_line(aes(y= inflow), color = "blue")+
  geom_line(aes(y= outflow), color = "red")+
  theme_bw(base_size = 20)+
  facet_wrap(~site)+
  ylab("Q in cfs")
plot(plot_brnr_ptrr)  

ggsave("output/brnr_ptrr_Q.jpg", width = 12, height = 8)
