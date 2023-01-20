#This script will pull in all the downloaded CSVS of Q, precip, etc from:
###Larimer County portal: 
#https://larimerco-ns5.trilynx-novastar.systems/novastar/operator/#/map
##DWR:
#https://dwr.state.co.us/tools/stations

###USGS: Pulled in using dataRetrieval pakage

#Greeley: Hand delivered

##Fort Collins



#Info:
#Workflow to pull all data automatically in progress
#csv data was grabbed manually
# Date range: 2019-01-01 to 2023-01-19

#Setup
source("package_loader.R")
package_load(pack_req)
#https://dwr.state.co.us/tools/stations
