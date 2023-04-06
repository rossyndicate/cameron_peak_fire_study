baer_sbs <- function(watersheds){
  
  sites <- sf::st_read('data/spatial/cpf_sites_shapefile/cpf_sites.shp') %>%
    dplyr::filter(grepl("Reservoir|Inflow", Site_Nm))
  
  watersheds <- vector("list", length = nrow(sites))
  
  for(i in 1:nrow(sites)){
    
    trace <- nhdplusTools::get_raindrop_trace(sites[i,]) 
    
    # "Snap" our USGS gage to the nearest NHD flowline feature
    snap_point <- sf::st_sfc(sf::st_point(trace$intersection_point[[1]][1:2]),
                             crs = 4326)
    # Clip/split our gage's catchment to only include the portion of the
    # catchment upstream of our gage:
    watersheds[[i]] <- nhdplusTools::get_split_catchment(snap_point, upstream = T)[2,]
    
  }
  
  watersheds <- bind_rows(watersheds) %>%
    sf::st_transform(26913)
  
  mapview(watersheds, alpha.regions = 0.001)
  
  sf::sf_use_s2(FALSE)
  
  call <- "https://edcintl.cr.usgs.gov/downloads/sciweb1/shared/MTBS_Fire/data/baer/cameronpeak_sbs.zip"
  
  temp1 <- tempfile()
  download.file(paste0(call), destfile = 'data/baer_soil_burn_severity/CPF.zip')
  temp2 <- tempfile()
  unzip("data/baer_soil_burn_severity/CPF.zip", exdir = "data/baer_soil_burn_severity/")
  
  sbs <- raster::raster("data/baer_soil_burn_severity/CameronPeak_SBS_final.tif") 
  
  # shapefile of watershed(s)
  ws <- watersheds %>%
    rownames_to_column()
  
  # using the exactextractr package, get what is essentially a "zonal histogram" in
  # ArcGIS speak
  ws_sbs <- exactextractr::exact_extract(sbs, ws,
                                         append_cols = T,
                                         summarize_df = T,
                                         fun=function(x) x %>%
                                           group_by(sbs_code = as.character(value)) %>%
                                           summarise(count = sum(coverage_fraction))) %>%
    group_by(rowname) %>% 
    mutate(total = sum(count)) %>%
    ungroup() %>%
    pivot_wider(names_from = 'sbs_code', values_from = 'count',
                names_glue = "sbs{sbs_code}" ) %>%
    inner_join(ws, .) -> ws_sbs
  
  ws_percents <- ws_sbs %>% 
    mutate(unburned=100*(sbs15/total),
           low=100*(sbs1/total),
           medium=100*(sbs2/total),
           high=100*(sbs3/total),
           outside_perimeter=100*(sbs4/total)) %>%
    select(rowname, outside_perimeter, unburned, low, medium, high) %>%
    ungroup()
  
  # poudre_watershed <- nhdplusTools::get_nldi_basin(nldi_feature = list(featureSource = 'comid', featureID = "2903809"))
  # # Download some NHDPlusHR Data
  # hr_data <- nhdplusTools::download_nhdplushr("data/spatial/baer_soil_burn_severity/", 10190007)
  
  
}
