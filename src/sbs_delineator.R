baer_sbs <- function(watersheds){
  
  library(tidyverse)
  
  sf::sf_use_s2(FALSE)
  
  # Download some NHDPlusHR Data
  hr_data <- nhdplusTools::download_nhdplushr("data/metadata/baer_soil_burn_severity/", 1019)
  hr_flowlines <- nhdplusTools::get_nhdplushr(hr_data, layers = c("NHDFlowline"))
  hr_catchments <- nhdplusTools::get_nhdplushr(hr_data, layers = c("NHDPlusCatchment"))

  sites <- sf::st_read('data/metadata/baer_soil_burn_severity/cpf_sites_sbs.shp') %>%
    dplyr::filter(grepl("Reservoir|Inflow", Site_Nm)) %>%
    filter(sit_typ=="Reservoir") %>%
    sf::st_transform(4269)
  
  start_index <- get_flowline_index(hr_flowlines$NHDFlowline,
                                    sites,
                                    search_radius = 10)
  
  watersheds <-  map(start_index$COMID, function(x){
    
    ids <- get_UT(hr_flowlines$NHDFlowline, x)
    
    ws <- hr_catchments[[1]] %>%
      filter(FEATUREID %in% ids) %>%
      filter(FEATUREID != 23001900058842) %>%
      summarize() %>%
      nngeo::st_remove_holes() %>%
      sf::st_transform(26913)
    
    return(ws)
    
  })
  
  mapview(watersheds, alpha.regions = 0.001)
  
  # call <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/baer/cameronpeak_sbs.zip"
  # 
  # temp1 <- tempfile()
  # download.file(paste0(call), destfile = 'data/baer_soil_burn_severity/CPF.zip')
  # temp2 <- tempfile()
  # unzip("data/baer_soil_burn_severity/CPF.zip", exdir = "data/baer_soil_burn_severity/")
  
  sbs <- raster::raster("data/metadata/baer_soil_burn_severity/CameronPeak_SBS_final.tif") 
  legend <- tibble(value = c(0,1,2,3,4,13),
                   legend = c("Unburned", "Very Low/Unburned", "Low", "Moderate", "High", "Unburned"))
  
  # shapefile of watershed(s)
  ws <- watersheds %>%
    bind_rows() %>%
    #mutate(id = c(1,2,3,4,5,6,7))
    cbind(tibble(index=c("Long Draw", "Peterson", "Joe Wright", "Chambers", "Barnes Meadow",
                         "Comanche", "Hourglass")))
  
  # using the exactextractr package, get what is essentially a "zonal histogram" in
  # ArcGIS speak
  ws_sbs <- exactextractr::exact_extract(sbs, ws,
                                         append_cols = T,
                                         summarize_df = T,
                                         fun=function(x) x %>%
                                           group_by(sbs_code = as.character(value)) %>%
                                           summarise(count = sum(coverage_fraction))) %>%
    group_by(index) %>% 
    mutate(total = sum(count)) %>%
    ungroup() %>%
    pivot_wider(names_from = 'sbs_code', values_from = 'count',
                names_glue = "sbs{sbs_code}" ) %>%
    inner_join(ws, .) -> ws_sbs
  
  ws_percents <- ws_sbs %>% 
    mutate(unburned=100*(sbs1/total),
           low=100*(sbs2/total),
           medium=100*(sbs3/total),
           high=100*(sbs4/total),
           outside_perimeter=100*(sbs15/total)) %>%
    select(index, outside_perimeter, unburned, low, medium, high) %>%
    ungroup()

}

baer_sbs(watersheds = )