
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

# map grab data over all data_files
sensor_data <- map(data_files, grab_data)%>%
  bind_rows()%>%
  #rbind(SFM_data)%>%
  filter(parameter %in% key_params)

