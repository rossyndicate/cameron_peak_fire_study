library(tidyverse)
library(sf)
library(nhdplusTools)
library(mapview)

huc8 <- get_huc8(id="10190007")

flowlines <- get_nhdplus(AOI = huc8, realization = "flowline", t_srs = 4326)

xy <- read_csv('data/cpf_sites.csv') %>%
  st_as_sf(coords=c('Long','Lat'), crs=4326) %>%
  filter(Campaign%in%c("Chambers Complex") |  Type%in%c('Mainstem'))

 for(i in 1:nrow(xy)){
   xy$comid[i] <- discover_nhdplus_id(xy[i,])
 }
 
xy <- xy %>%
  dplyr::left_join(.,nhd,by='comid')

lowest <- filter(xy, Site_Code=="PBD")

ws_list <- get_UT(flowlines, lowest$comid)
 
flowlines <- flowlines %>%
  filter((comid %in% ws_list) | comid == lowest$comid) 

flowlines <- get_nhdplus(comid=flowlines$comid) %>%
  get_tocomid(., add=TRUE) %>%
  mutate(ID=comid, toID=tocomid, length=lengthkm)

paths <- get_pathlength(flowlines) %>%
  rename(comid=ID) %>%
  right_join(xy, by='comid')