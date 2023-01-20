

#Function to install and library all required packages#

package_load <- function(package_names){
  for(i in 1:length(package_names)){
    if(!package_names[i] %in% installed.packages()){
      install.packages(package_names[i])
    }
    library(package_names[i],character.only = TRUE)
  }
}

#list of packages
pack_req <- c("tidyverse",
              "ggplot2", 
              "sf", 
              "terra", 
              "nhdplusTools", 
              "padr", 
              "rjson", 
              "dataRetrieval")

