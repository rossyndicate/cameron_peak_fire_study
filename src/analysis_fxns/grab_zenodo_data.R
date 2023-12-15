
  data_folder <- list.files(path = "data/") %>%
    keep(~ grepl("rossyndicate", .))
  
  dwnload_file <- paste0("https://zenodo.org/records/", DOI, "/files/rossyndicate/CPF_reservoir_study_data-", data_version, ".zip?download=1")
  
  # Check if a folder with the specified name exists in the "data" folder
  if (is_empty(data_folder)) {
    # Download the entire folder from Zenodo DOI as a zipped folder in your working directory
    download.file(url = dwnload_file, destfile = 'data/ross_cpf.zip') 
    
    # Unzip this file
    unzip('data/ross_cpf.zip', exdir = "data/") 
    
    # Grab the name of the download file from the current R project   
    data_folder <- list.files(path = "data/") %>%
      keep(~ grepl("rossyndicate", .))
    
    # Grab the most recent cleaned and collated chemistry dataset
    # For the current release that file is `CPF_reservoir_chemistry_up_to_071023.csv`
    cleaned_chem_file <- list.files(path = paste0("data/", data_folder, "/data/cleaned/"), full.names = TRUE)
    site_meta_file <- list.files(path = paste0("data/", data_folder, "/data/metadata/"), full.names = TRUE) %>%
      keep(~ grepl("cpf_sites", .))
    units_meta_file <- list.files(path = paste0("data/", data_folder, "/data/metadata/"), full.names = TRUE) %>%
      keep(~ grepl("Units", .))
    
    most_recent_chem <- read_csv_arrow(cleaned_chem_file)
    most_recent_meta <- read_csv_arrow(site_meta_file)
    chem_units <- readxl::read_xlsx(units_meta_file)
    rm(cleaned_chem_file, site_meta_file, data_folder,dwnload_file)
  } else {
    #Only grab previously downloaded file
    cleaned_chem_file <- list.files(path = paste0("data/", data_folder, "/data/cleaned/"), full.names = TRUE)
    site_meta_file <- list.files(path = paste0("data/", data_folder, "/data/metadata/"), full.names = TRUE) %>%
      keep(~ grepl("cpf_sites", .))
    units_meta_file <- list.files(path = paste0("data/", data_folder, "/data/metadata/"), full.names = TRUE) %>%
      keep(~ grepl("Units", .))
    
    most_recent_chem <- read_csv_arrow(cleaned_chem_file)
    most_recent_meta <- read_csv_arrow(site_meta_file)
    chem_units <- readxl::read_xlsx(units_meta_file)
    # Folder already exists, you may choose to print a message or take other actions
    cat("Data Version: ",data_version, "\nMake sure this is the most recent version of available data")
    rm(cleaned_chem_file, site_meta_file, data_folder,dwnload_file, units_meta_file )
  }
  
  
  

