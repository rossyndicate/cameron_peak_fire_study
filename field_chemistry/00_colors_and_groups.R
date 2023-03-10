#this file contains all the metadata, groupings and color palettes that are used in all the other field_chemistry scripts

#functions
`%nin%` = Negate(`%in%`)

## Importing data

#buffer_sbs:Indices based on burn severity directly around the reservoir
buffer_sbs <- read_csv('data/field_chemistry/metadata/sbs_buffer.csv') %>%
  mutate(Buffer_Level=((Unburned*0)+(V_Low*0.1)+(Low*0.4)+(Moderate*0.7)+(High*1))/(Unburned+V_Low+Low+Moderate+High))
# watershed_sbs: within each reservoirs watershed.
watershed_sbs <- read_csv('data/field_chemistry/metadata/sbs_watershed.csv') %>%
  mutate(Watershed_Level=((Unburned*0)+(V_Low*0.1)+(Low*0.4)+(Moderate*0.7)+(High*1))/(Unburned+V_Low+Low+Moderate+High))
# Sites: Locations of each site, includes grouping and type of body of water
Sites <- read.csv('data/field_chemistry/metadata/cpf_sites.csv')
#dist_from_pbd: distance from PBD (mouth of canyon) using NHD flowline
dist_from_pbd <- read.csv('data/field_chemistry/metadata/distance_from_pbd.csv')
#Dates for genomic survey
genomic_dates <- as.Date(c("2022-06-22", "2022-08-25", "2022-09-19"))


## site groupings
#JW and Chambers complex
jw_chambers_complex <- c("JOEI", "JOER", "CBRR", "CBRI", "CHD")

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
## Color sets
cbbPalette <- c( "#999999","#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#Color for watersheds, matches burn severity
colorsBS <- c("Barnes Meadow Reservoir" = "#D55E00", "Peterson Reservoir" ="#E69F00", "Chambers Reservoir" = "#F0E442", "Joe Wright Reservoir" = "#56B4E9", "Long Draw Reservoir" = "#0072B2")

#Color for reservoirs, matches burn severity by site code

colorsBS_site_code <- c("BRNR" = "#D55E00", "CBRR" = "#F0E442", "JOER" = "#56B4E9", "LNGR" = "#0072B2", "PTRR" ="#E69F00" )

#Color matches burn severity
colorsBS_in_order <-c("#0072B2","#56B4E9","#F0E442","#E69F00","#D55E00")

#Seasonal Color values
season_color_vals =c('#047E82','#397534','#59B851','#DEB907','#FA850E')
