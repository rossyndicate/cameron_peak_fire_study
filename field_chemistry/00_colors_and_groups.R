#this file contains all the metadata, groupings and color palettes that are used in all the other field_chemistry scripts



## Importing data

#buffer_sbs:Indices based on burn severity directly around the reservoir
buffer_sbs <- read_csv('data/sbs_buffer.csv') %>%
  mutate(Buffer_Level=((Unburned*0)+(V_Low*0.1)+(Low*0.4)+(Moderate*0.7)+(High*1))/(Unburned+V_Low+Low+Moderate+High))
# watershed_sbs: within each reservoirs watershed.
watershed_sbs <- read_csv('data/sbs_watershed.csv') %>%
  mutate(Watershed_Level=((Unburned*0)+(V_Low*0.1)+(Low*0.4)+(Moderate*0.7)+(High*1))/(Unburned+V_Low+Low+Moderate+High))
# Sites: Locations of each site, includes grouping and type of body of water
Sites <- read.csv('data/CPF_Sites.csv')
#dist_from_pbd: distance from PBD (mouth of canyon) using NHD flowline
dist_from_pbd <- read.csv('data/Distance_from_PBD.csv')


## site groupings

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

## Color sets
cbbPalette <- c( "#999999","#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
