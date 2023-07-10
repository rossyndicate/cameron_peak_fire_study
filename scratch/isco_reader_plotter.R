#--------------------#
#ISCO Data Reader 
# Written by: Sam Struthers

# This code is to be run to clean the .dat files that come from Campbell data loggers
# and are used in conjunction with ISCO autosamplers. 

# Where you see ##-----##, you need to modify code to match your file paths

#--------------------#

##-----##
# Set the directory path where the .dat files are located
# Ex: "desktop/CamPeak/ISCO/ISCO_raw_downloads/"
folder_path <- "data/raw_files/ISCO_testing/"
#This is where you want your cleaned or more concise data to live
export_folder <- "data/raw_files/ISCO_testing/concise_data"
#Folder path for raw data to be stored in csv format 
raw_export_folder <- "data/raw_files/ISCO_testing/raw_data"
##-----##


# Install required packages
install.packages("tidyverse")
install.packages("stringr")
# Load necessary libraries
library(tidyverse)
library(stringr)


# List all .dat files in the directory
dat_files <- list.files(folder_path, pattern = "\\.dat$", full.names = TRUE)

# Iterate over each .dat file
for (i in 1:length(dat_files)) {
  # Read the .dat file as a data frame
  data<- arrow::read_delim_arrow(file = dat_files[i], delim = ",", skip = 1)
  
  #clean up gross header and rename columns to have better header names, remove all spaces
  data <- data[-2, ]
  units <- str_replace_all(as.character(data[1, ]), "\\s+", "_")
  column_names <- colnames(data)
  final_names <- paste(column_names, units, sep = "_")
  colnames(data) <-  gsub("_NA", "", final_names)
  raw_data <- data[-1,]
  
  #add a date column and clean the date time to DT
  clean_data <- raw_data%>%
    mutate(DT = as.POSIXct(TIMESTAMP_TS), 
           date = as.Date(DT))
  
  #Find the start and end dates of the dataset
  file_date_start <- format(clean_data$date[1], "%Y%m%d")
  file_date_end  <- format(clean_data$date[nrow(clean_data)], "%Y%m%d")
  
  #Extract site name
  site <- str_extract(string = str_remove(dat_files[i], folder_path), pattern = "[A-Z]+")

  # Construct the new filenames
  new_filename <- paste(site, "ISCO", file_date_start, file_date_end, sep = "_")
  concise_filename <- paste0(new_filename, ".csv")
  raw_filename <- paste0(new_filename,"_RAW", ".csv")
  # Save the data as a CSV file with the new name
  write.csv(raw_data, file.path(raw_export_folder, raw_filename), row.names = FALSE)

  
  # Extract only the desired columns for concise data
  concise_data <- clean_data%>%
    select(DT,"SC_uS_uS/cm",Temp_C_Deg_C, Turb_ave_NTU, Turb_NTU, Turb_stdev_NTU, Turb_med_NTU, date )
  
  # Save the concise data as a CSV file with the new name
  write.csv(concise_data, file.path(export_folder, concise_filename), row.names = FALSE)
  

}

  

  