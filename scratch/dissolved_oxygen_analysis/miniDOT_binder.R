library(tidyverse)
library(lubridate)
library(purrr)
library(ggplot2)
library(plotly)

sites <- tibble(sites=c("JWC","PJW","PBR","SFM","PSF","PNF","PBD", "CHD", "JOEI", "CBRI"))
site_list <- c("JWC","PJW","PBR","SFM","PSF","PNF","PBD", "CHD", "JOEI", "CBRI")

dot_puller <- function(site_list){

  tracer <- function(sites){

  txt_files_ls = list.files(path=paste0(site_list), pattern="*.txt", full.names=T, recursive = T)

                            # Read the text files in
                            txt_files <- lapply(txt_files_ls, function(x) {read.delim(file = x, header = F, sep =",", dec=".") %>% slice(-1:-2) %>%
                                janitor::row_to_names(row_number=1) %>%
                                select(seconds=1,
                                       Temp_C=3,
                                       DO_mgL=4) %>%
                                mutate(DT=as_datetime(as.numeric(seconds)),
                                       Site = site_list,
                                       Temp_C=as.numeric(Temp_C),
                                       DO_mgL=as.numeric(DO_mgL))})

                            # Combine them
                            combined_do <- do.call("rbind", lapply(txt_files, as.data.frame)) #%>%
                              #write_csv(paste0(site_list,'_miniDOT_data.csv'))

}
 lister <- map_dfr(sites,tracer)
}

combined_do <- sites %>% mutate(data=map(site_list,dot_puller)) %>%
  unnest(data) %>%
  dplyr::select(Site,
                DT,
                DO_mgL,
                Temp_C)
write_csv(combined_do, "Sensor_DO_CPF.csv")

DO_plot <- ggplot(filter(combined_do, Site == "CBRI" | Site == "CHD" | Site == "JOEI"| Site == "JWC"), aes(x= DT, y= DO_mgL, color= Site))+
  geom_line()+
  geom_smooth()
#  facet_wrap(~Site, scales = "fixed")

ggplotly(DO_plot)

#DO_plot_PJW <- ggplot(filter(combined_do, Site == "PJW" & DT > "09-01-2022"), aes(x= DT, y= Temp_C, color= Site))+

# geom_line()
#facet_wrap(~Site, scales = "fixed")

#ggplotly(DO_plot_PJW)

