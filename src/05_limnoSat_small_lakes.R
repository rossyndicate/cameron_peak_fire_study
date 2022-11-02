#Created by IAO on 2022-10-04 to preview Colorado data from Michael (pseudo limnosat, lakes < 10 ha)
if (!require('here')) install.packages('here');library('here') 

source("src/00_libraries.R")
source("src/00_helperFuns.R")

CO_data <-
  list.files(path = "~/cameron_peak_fire_study/data/co_lake_reflectance",
             # Identify all CSV files
             pattern = "*.csv",
             full.names = TRUE) %>%
  purrr::map_dfr( ~ .x %>%
                    readr::read_csv(col_types = cols(
                      .default = "?", permanent = "c"
                    )))
  #break system:index into a few columns

#split 'player' column using '_' as the separator
CO_data[c('col1', 'col2','col3','col4','date','col6')] <- str_split_fixed(CO_data$`system:index`, '_', 6)
#make date a date

CO_data <- CO_data %>%
  mutate(date_new= as.Date(paste(date, sep=""), "%Y%m%d")) %>%
  unite("LandsatID", col1:date, sep= "_", 
        remove = TRUE) %>%
  rename(nhdplusv2_comid=permanent,
         mystery_ID=col6,
         date=date_new)  %>%
  mutate(year=year(date)) %>%
  drop_na() %>%
  mutate(dwl=fui.hue(Red, Green, Blue))

names(CO_data)
str(CO_data)

##SKIP TO HERE ONCE IT HAS BEEN PULLED FROM FOLDER##


CO_data <- read.csv("data/compiled_limnosat_110122.csv") %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))
  #mutate(date_new= as.Date(paste(date, sep=""), "%Y%m%d")) %>%
  #unite("LandsatID", col1:date, sep= "_", 
  #      remove = TRUE) %>%
  #rename(nhdplusv2_comid=permanent,
         #mystery_ID=col6,
         #date=date_new)  %>%
  #mutate(year=year(date)) %>%
  #drop_na() %>%
  #mutate(dwl=fui.hue(Red, Green, Blue))

names(CO_data)
str(CO_data)

#How many unique lakes?
length(unique(CO_data$nhdplusv2_comid))
#5324
#Sam: 5213

#How many years?
length(unique(CO_data$year))
#39
# Sam:38

#How many of them have a corresponding lagoslakeid? 
hist(CO_data$dwl)
#Why so green? Benthic periphyton in small, shallow lakes... agricultural ponds

#attempt to join by comid
#res_comid <- read.csv("data/res_comid.csv")%>%
 #  mutate(comid = as.character(comid))

clp_watershed <- get_huc8(id ='10190007')

#CO water bodies

CO <- states()%>%
  filter(NAME == "Colorado")

#all Cache la Poudre waterbodies
clp_waterbodies <- get_waterbodies(AOI = CO)

#%>%
 # mutate(comid = as.character(comid))

#our study sites
cpf_waterbodies <- get_waterbodies(AOI = clp_watershed)%>%
  filter(gnis_name %in% c("Chambers Lake", "Peterson Lake", "Joe Wright Reservoir", "Long Draw Reservoir", "Barnes Meadow Reservoir"))%>%
mutate(comid = as.character(comid), 
gnis_id = as.character(gnis_id))  

co_lakes_data <- clp_waterbodies%>%
  filter(comid %in% CO_data$nhdplusv2_comid)

cpf_lakes_data <- CO_data%>%
#  mutate(nhdplusv2_comid = as.numeric(nhdplusv2_comid))%>%
 filter(nhdplusv2_comid %in% cpf_waterbodies$gnis_id)


write_csv(CO_data, "data/compiled_limnosat_110122.csv")
