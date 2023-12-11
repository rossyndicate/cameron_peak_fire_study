
#Find all files in the api directory
dir <- "data/sensor_data/api/"
data_files <- list.files(dir, full.names = T)
#read in data from csv
grab_data <- function(file_path) {
#grab a single file
  df <- read_csv_arrow(file_path)%>%
    #convert to MST
    with_tz(timestamp, tzone = "MST")%>%
    #remove vulink data
    filter(!grepl("vulink",name, ignore.case = TRUE))%>%
    mutate(id = as.double(id))
  
  return(df)
}
#key params for sensor data
key_params <- filter(sensor_meta, key_param == T)%>%
  select(param_sonde)%>%
  pull()
# SFM data
SFM_data <- read_rds("data/sensor_data/SFM/SFM_sonde_2023data_archive.RDS")%>%
  rename(timestamp = DT_round, 
         value = Value, 
         parameter = Measurement)%>%
  mutate(site = "sfm", 
         id = NA_integer_, 
         name = "SFM AT800", 
         units = NA_character_, 
         parameter = case_when(
           parameter == "Chl-a" ~ "Chl-a Fluorescence", 
           parameter == "Tubidity" ~ "Turbidity",
           parameter == "Actual_Conductivity" ~ "Actual Conductivity",
           parameter == "DO_sat" ~ "% Saturation O₂",
           parameter == "FDOM" ~ "FDOM Fluorescence",
           parameter == "Specific_Conductivity" ~ "Specific Conductivity",
           TRUE ~ parameter
         ))

# map grab data over all data_files
sensor_data <- map(data_files, grab_data)%>%
  bind_rows()%>%
  rbind(SFM_data)%>%
  filter(parameter %in% key_params)

