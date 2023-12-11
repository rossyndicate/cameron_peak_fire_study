# This script will pull in all the downloaded CSVS of Q, precip, etc from:
### Larimer County portal:
# https://larimerco-ns5.trilynx-novastar.systems/novastar/operator/#/map
## DWR:
# https://dwr.state.co.us/tools/stations

### USGS: Pulled in using dataRetrieval pakage

# Greeley: Hand delivered

## Fort Collins



# Info:
# Workflow to pull all data automatically in progress
# csv data was grabbed manually
# Date range: 2019-01-01 to 2023-01-19

# Setup
source("package_loader.R")



#---CODE----#


# Set the HUC code and parameter code
huc_code <- c("10190007", "10190006")
watershed <- c("Cache La Poudre", "Big Thompson")
parameter_code <- "00060"

# Use the whatNWISdata function to get site information for all sites in the HUC with the specified parameter code
clean_usgs_site_info <- function(huc_code, watershed){
 usgs_meta <- whatNWISdata(parameterCd = parameter_code, huc = huc_code) %>%
  distinct(site_no, .keep_all = TRUE) %>%
  filter(end_date >= "2020-01-01") %>%
  select(
    site_no,
    site_name = station_nm,
    lat = dec_lat_va,
    long = dec_long_va,
    start_date = begin_date,
    end_date
  )%>%
   mutate(watershed = watershed)
  return(usgs_meta)
}

usgs_site_meta <- map2(huc_code, watershed, clean_usgs_site_info)%>%
  list_rbind()
usgs_site_nums <- usgs_site_meta%>%
  select(site_no)

# save for later use
# write_csv(usgs_clp_site_info, "data/Q_modeling/usgs_sites_simple.csv")



# function to select Q from NWIS given site code. Dates autoselected for oldest and newest data available
read_q <- function(sites) {
  site_Q <- readNWISuv(
    siteNumbers = sites,
    parameterCd = parameter_code,
    tz = "America/Denver"
  )
  return(site_Q)
}

# Pull in all Q for sites selected above
# May take a sec to load
usgs_Q <- map(usgs_site_nums, read_q) %>%
  list_rbind()
# make sure all sites are there
unique(usgs_Q$site_no)
usgs_site_nums
clean_usgs_Q <- usgs_Q %>%
  rename(
    Q_cfs = X_00060_00000,
    Q_flag = X_00060_00000_cd,
    tz = tz_cd
  ) %>%
  left_join(usgs_site_meta, by = "site_no")

plot_all_sites <- clean_usgs_Q %>%
  ggplot(aes(
    x = dateTime,
    y = Q_cfs,
    group = site_name,
    color = site_name
  )) +
  geom_line() +
  theme_bw() +
  facet_wrap(~site_name)

# plot(plot_all_sites)

# ggsave("output/USGS_sites.jpg", width = 20, height = 12)

usgs_Q_final <- clean_usgs_Q %>%
  select(
    agency = agency_cd,
    site_no,
    DT = dateTime,
    Q_cfs, Q_flag, tz,
    site_name, watershed
  )

write_csv(usgs_Q_final, file = "data/Q_modeling/usgs_Q.csv")


#------- Pull in Larimer County Data -----#

# URL of the site to select specific stations
larimer_q <- read_feather("data/Q_modeling/larimer_co_2015-01-25_2023-01-30")
larimer_p <- read_feather("data/Q_modeling/larimer_co_precip_2015-01-25_2023-01-30")


#------ Pull in DWR Data ------#

DWR <- read_feather("data/Q_modeling/active_DWR_020723")

#----- Kampf site locations ----#
kampf_meta <- read_csv("data/Kampf_metadata.csv") %>%
  select(
    name = Name,
    type = Type,
    lat = Latitude,
    long = Longitude,
    start_date = Sensor_install,
    active = Sensor_remove,
    comments = Comments
  ) %>%
  filter(type == "stream" & active == "still running") %>%
  distinct(name, .keep_all = TRUE) %>%
  select(name, lat, long, start_date)

write_csv(kampf_meta, "data/Q_modeling/kampf_stream_sites_simple.csv")


#----- Pull in Greeley data ------#

brnr_ptrr_daily_q <- read_csv("data/Q_modeling/BRNR_PTRR_Q.csv") %>%
  mutate(
    inflow = replace_na(inflow, 0),
    Date = as.Date(date, format = "%m/%d/%y"), 
    site_name = case_when(site == "BRNR" ~ "Barnes Meadow Reservoir", 
                          site == "PTRR" ~ "Peterson Lake"))

plot_brnr_ptrr <- brnr_ptrr_daily_q %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = inflow), color = "blue") +
  geom_line(aes(y = outflow), color = "red") +
  theme_bw(base_size = 20) +
  facet_wrap(~site) +
  ylab("Q in cfs")
plot(plot_brnr_ptrr)

ggsave("output/brnr_ptrr_Q.jpg", width = 12, height = 8)




### Map!

all_q_sites <- read.csv("data/Q_modeling/compiled_q_locations.csv")%>%
  filter(!is.na(long))%>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

clp_flowlines <- st_read("data/cpf_sbs/clp_flowlines.shp")

mapview::mapview(all_q_sites, zcol = "data")+
  mapview::mapview(clp_flowlines)


## Joining all datasets ###

# Q dataframe

#column names: 
# Date (or datetime): all posixct
# q_cfs: avg value for day
# site_number: reference site number in all_q_sites
# site_name: reference name in all_q_sites
# agency: Larimer. loveland...etc
# Source: Larimer, USGS, DWR, Greeley

#seperate dfs: DWR, larimer_q, brnr_ptrr_daily_q, usgs_Q

prepped_larimer_q <- larimer_q%>%
  mutate(Date = as.Date(datetime))%>%
  group_by(Date, locId, name, agency)%>%
  summarise(daily_q_cfs = mean(q_cfs))%>%
  mutate(source = "Larimer County")%>%
  rename(site_name = name, 
         site_number = locId)%>%
  ungroup()

prepped_usgs <- clean_usgs_Q%>%
   mutate(Date = as.Date(dateTime))%>%
  group_by(Date, site_no, site_name, agency_cd)%>%
  summarise(daily_q_cfs = mean(Q_cfs))%>%
  mutate(source = "USGS")%>%
  rename(agency = agency_cd, 
         site_number = site_no)%>%
  ungroup()

prepped_dwr <- DWR%>%
  mutate(Date = as.Date(datetime))%>%
  group_by(Date, station_num, abbrev, data_source)%>%
  summarise(daily_q_cfs = mean(value))%>%
  mutate(source = "dwr")%>%
  rename(agency = data_source, 
         site_number = station_num, 
         site_name = abbrev)%>%
  ungroup()%>%
  mutate(site_number = as.character(site_number))

prepped_greeley <- brnr_ptrr_daily_q%>%
  select(Date, daily_q_cfs = outflow, 
         site_name)%>%
  mutate(source = "Greeley", 
         agency = "Greeley", 
         site_number = NA)
# combining all sources
combined_q <- bind_rows(prepped_dwr, 
                        prepped_larimer_q, 
                        prepped_usgs, 
                        prepped_greeley)

#plotting all sites with Q
ggplot(filter(combined_q, site_number != "3520"), aes(x = Date, y = daily_q_cfs, color = source))+
  geom_line() +
  theme_bw() +
  scale_y_log10()+
  facet_wrap(~site_name)

ggsave("output/test_combined_Q.jpg", width = 30, height = 20)

#export final
write_feather(combined_q, sink = "data/Q_modeling/combined_Q_bigt_clp")
