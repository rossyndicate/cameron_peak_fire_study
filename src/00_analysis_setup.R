#this file contains all the packages,metadata, groupings and color palettes that are used in downstream scripts

### ----- Load packages ----- ###
package_load <- function(package_names){
  for(i in 1:length(package_names)){
    if(!package_names[i] %in% installed.packages()){
      install.packages(package_names[i])
    }
    library(package_names[i],character.only = TRUE)
  }
}

#vector of packages
pack_req <- c( 
  # data wrangling packages
  "tidyverse","lubridate","padr","janitor","padr", "broom","arrow",
  #spatial packages
  "sf","terra","nhdplusTools", "tigris","raster", "leaflet","tmap",
  # plotting
  "ggpubr","ggthemes","scales","corrplot","gghighlight", "geomtextpath", "ggbeeswarm","plotly", "ggpmisc",
  # web scrapping
  "rjson", "rvest", "dataRetrieval", "httr", "jsonlite",
  #extra
  "devtools", "trend")
package_load(pack_req)



remove(pack_req, package_load)
#Simple function to negate %in%
`%nin%` = Negate(`%in%`)

### ----- Meta Data ----- ###

# Distance order and xdistance
order_table <- tibble(
  site_code = c('JOEI', 'JOER', 'CBRI', 'CBRR', 'CHD', 'BRNR', 'BMD', 'LNGR', 'LNGO', 'PTRR', 'PTRO', 'COMI', 'COMR', 'COMO', 'HORI', 'HORR', 'HORO', 'BEAV', 'JWC', 'PJW', 'PFAL', 'SLEP', 'PBR', 'SFM', 'PSF', 'PNF', 'PBD'),
  Distance = c('1 - JOEI', '2 - JOER', '3 - CBRI', '4 - CBRR', '5 - CHD', '6', '7', '8', '9', '99', '999', '11 - COMI', '12 - COMR', '13 - COMO', '14 - HORI', '15 - HORR', '16 - HORO', '17 - BEAV', '18 - JWC', '19 - PJW','20- SLEP', '20 - SLEP', '21 - PBR', '22 - SFM', '23 - PSF', '24 - PNF', '25 - PBD'),
  Xdistance = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 99, 999, 12, 13, 14, 15, 16, 17, 18, 19,20, 20, 21, 22, 23, 24, 25, 26),
  order = c('03', '04', '05', '06', '07', '10', '11', '01', '02', '08', '09', '12', '13', '14', '15', '16', '17', '18', '19','20', '20', '21', '22', '23', '24', '25', '26')
)
# df for sensor metadata
sensor_meta <- tibble(
  # param sonde is the name that the sensor records
  param_sonde = c("% Saturation O₂","Actual Conductivity","Baro","Battery Level","Chl-a Fluorescence", 
                                      "DO","Density","Depth","External Voltage","FDOM Fluorescence","ORP","Partial Pressure O₂",
                                      "Pressure","Resistivity","Salinity","Specific Conductivity", "Temperature", "Total Dissolved Solids",
                                      "Turbidity","pH","pH MV"), 
  # param common is an abbreviated version that is easier to type
                      param_common = c("DO_sat", "actual_cond", "baro", "battery", "chla",
                                       "DO", "density","depth", "voltage", "FDOM", "ORP", "Partial Pressure o2",
                                       "pressure", "Resistivity", "salinity", "spec_cond", "temp", "tds",
                                       "turb", "pH", "pH_mv"), 
  # Param w units is used to label axes
                      param_w_units = c("% Saturation O₂","Actual Conductivity (µS/cm)","Baro","Battery Level","Chl-a Fluorescence (RFU)", 
                                        "DO (mg/L)","Density","Depth (m)","External Voltage","FDOM Fluorescence (RFU)","ORP (mv)","Partial Pressure O₂",
                                        "Pressure","Resistivity","Salinity","Specific Conductivity (µS/cm)", "Temperature (C)", "Total Dissolved Solids (mg/L)",
                                        "Turbidity (NTU)","pH","pH MV (v)"), 
  #key param is logical, derived parameters or parameters that are less important for WQ are F 
                      key_param = c(T, T,F,F,T,
                                    T,F,T,F,T,T,F, 
                                    F,T,T,T,T,F,
                                    T,T,F))

### ----- site and date groupings ----- ###

#Dates for genomic survey
genomic_dates <- as.Date(c("2022-06-22", "2022-08-25", "2022-09-19"))
#JW and Chambers complex
jw_chambers_complex <- c("JOEI", "JOER", "CBRR", "CBRI", "CHD")

#Sonde sites 2023
sonde_sites <- c("JOEI", "CBRI", "CHD", "PBD")

#Weekly reservoir study sites
mainstem_res_set <- c("CBRR", "CHD", "JOEI", "JOER", "CBRI", "PTRR", "PTRO", "BMD", "BRNR", "LNGO", "LNGR")

#^^ but only reservoirs
mainstem_Res_only <-c("CBRR", "JOER", "PTRR",  "BRNR", "LNGR")

#southfork reservoirs
SF_Res <- c("COMI", "COMR", "COMO", "HORI","HORR" , "HORO", "BEAV", "SFM")

#study mainstem sites
mainstem_sites <- c("JWC","PJW","SLEP","PBR","SFM","PSF","PNF","PBD")

#overlap sites between fc gov and our study
fcgov_study <- c("CHD", "BMD","JWC", "PJW", "PBR", "SFM", "PSF", "PNF", "PBD")
#SF and Weekly study reservoirs
all_res_system <- c("COMI", "COMR", "COMO", "HORI","HORR" , "HORO","CBRR", "CHD", "JOEI", "JOER", "CBRI", "PTRR", "PTRO", "BMD", "BRNR", "LNGO", "LNGR")

genomic_sites <- c("LEGC", "TMBR", "TIMB", "ELC", "ARCH", "BOX1", "BOX2", "BOX3", "BOX4")

# Isco rmrs cpf sites
ISCO <- c("FISH_ISCO", "SAWM_ISCO", "BLAK_ISCO", "LBEA_ISCO", "FALL_ISCO")

# Ratio Sites for spatial mapping
ratio_sites_names_all <- c("JOEI_JOER", "JOER_CBRI" , "CBRI_CBRR" , "CBRR_CHD" , "CHD_JWC" , "BRNR_BMD" , "BMD_JWC" , "PTRR_PTRO" , 
                       "PTRO_PJW" , "LNGR_LNGO" , "LNGO_PJW" , "JWC_SLEP" ,"PJW_SLEP" , "SLEP_PBR" , "PBR_PSF" , "COMI_COMR" , 
                       "COMR_COMO" , "COMO_BEAV"  , "HORI_HORR" , "HORR_HORO" , "HORO_BEAV" , "BEAV_SFM", "SFM_PSF", "PSF_PNF" , "PNF_PBD" )
# Ratio upper canyon Sites for spatial mapping
ratio_sites_names_upper <- c("JOEI_JOER", "JOER_CBRI" , "CBRI_CBRR" , "CBRR_CHD" , "CHD_JWC" , "BRNR_BMD" , "BMD_JWC" , "PTRR_PTRO" , 
                           "PTRO_PJW" , "LNGR_LNGO" , "LNGO_PJW" , "JWC_SLEP" ,"PJW_SLEP"  )
# for upper select graph

ratio_sites_names_upper_select <- c("JOEI_JOER", "JOER_CBRI" , "CBRI_CBRR" , "CBRR_CHD" , "CHD_JWC" , "BRNR_BMD" , "BMD_JWC"  , 
                                      "JWC_SLEP" ,"PJW_SLEP"  )

sites_names_upper_select <- c("JOEI", "JOER", "CBRI", "CBRR", "CHD", "BRNR", "BMD", "JWC", "PJW", "SLEP")

ratio_sites_names_SF <- c("PBR_PSF" , "COMI_COMR" , "COMR_COMO" , "COMO_BEAV" ,
                         "HORI_HORR" , "HORR_HORO" , "HORO_BEAV" , "BEAV_SFM", "SFM_PSF")

### ----- Color Sets ----- ###

cbbPalette <- c( "#999999","#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Color for watersheds, matches burn severity
colorsBS <- c("Barnes Meadow Reservoir" = "#D55E00", "Peterson Reservoir" ="#E69F00", "Chambers Reservoir" = "#F0E442", "Joe Wright Reservoir" = "#56B4E9", "Long Draw Reservoir" = "#0072B2")

#Color for reservoirs, matches burn severity by site code

colorsBS_site_code <- c("BRNR" = "#D55E00", "CBRR" = "#F0E442", "JOER" = "#56B4E9", "LNGR" = "#0072B2", "PTRR" ="#E69F00" )

#Color matches burn severity
colorsBS_in_order <-c("#0072B2","#56B4E9","#F0E442","#E69F00","#D55E00")

#Seasonal Color values
season_color_vals =c('#047E82','#397534','#59B851','#DEB907','#FA850E')


