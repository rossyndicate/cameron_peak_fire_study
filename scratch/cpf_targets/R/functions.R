#functions for cpf cleaning, graphing and analysis

##function to import all the required files for the reservoir chem dataframe, 
prepare_res_chem <- function(buffer_file,
                             sites_file, 
                             dist_file, 
                             watershed_file, 
                             ross_chem_file, 
                             rmrs_chem_file,  out_path){
  
  buffer_sbs <- buffer_file %>%
    mutate(Buffer_Level=((Unburned*0)+(V_Low*0.1)+(Low*0.4)+(Moderate*0.7)+(High*1))/(Unburned+V_Low+Low+Moderate+High))
  
  watershed_sbs <- watershed_file %>%
    mutate(Watershed_Level=((Unburned*0)+(V_Low*0.1)+(Low*0.4)+(Moderate*0.7)+(High*1))/(Unburned+V_Low+Low+Moderate+High))
  
  Sites <- sites_file
  
  dist_from_pbd <- dist_file
  
  
  
  prepped_reservoir_chemistry <- ross_chem_file%>%
    dplyr::mutate(Date=as.Date(Date,format="%d-%b-%y"))%>%
    dplyr::filter(SampleType == "NORM") %>%
    dplyr::mutate (FCW_Number = as.numeric(gsub('FCW ', '', IDNo)), 
            Year =  year(Date), 
            site_code = SiteLabel)%>%
    dplyr:: mutate(watershed=ifelse(site_code %in% c('LNGR','LNGO'),"Long Draw Reservoir",
                            ifelse(site_code %in% c('PTRO','PTRR'),'Peterson Reservoir',
                                   ifelse(site_code %in% c('SLEP','JWC','PNF','PBD', 'PJW','PBR', 'SFM','PSF' ),'CLP Mainstem Canyon',
                                          ifelse(site_code %in% c('ELC','ARCH', 'LEGC','TIMB' ),'CLP Mainstem Fort Collins',
                                                 ifelse(site_code %in% c('JOEI','JOER'),"Joe Wright Reservoir",
                                                        ifelse(site_code %in% c('CBRI', 'CBRR','CHD'), "Chambers Reservoir",
                                                               ifelse(site_code %in% c('BRNR','BMD'),"Barnes Meadow Reservoir",
                                                                      ifelse(site_code %in% c('COMR','COMO','COMI'),"Comanche Reservoir","Hourglass Reservoir"))))))))) %>%
    left_join(select(buffer_sbs,c(watershed,Buffer_Level)),by="watershed") %>%
    left_join(select(watershed_sbs,c(site_code,Watershed_Level)),by="site_code")%>%
    left_join(select(Sites,c(site_code,Lat,Long)),by="site_code") %>%
    left_join(select(dist_from_pbd,c(site_code,distance_upstream_km)),by="site_code") %>%
    dplyr::mutate(season=ifelse(Date <= "2022-06-09", "SPRING",
                         ifelse(Date <= "2022-09-24", "SUMMER", "FALL"))) %>%
    dplyr:: mutate(status = ifelse(site_code %in% c('LNGR','LNGO','JOEI'),"Unburned",
                           ifelse(site_code %in% c('COMI','COMR','COMO','HORI','HORR','HORO','PTRR','PTRO','CBRR','CHD','BRNR','BMD'), "Burned",                                                                                                            "Partially Burned"))) %>%
    dplyr::mutate(Location = ifelse(site_code %in% 
                               c("LNGR","PTRR","JOER","CBRR","BRNR","COMR","HORR"),"Reservoir",
                             ifelse(site_code %in% c("SLEP","JWC","PNF","PBD", "PJW","PBR", "SFM","PSF" ), "Mainstem",
                                    ifelse(site_code %in% c("LNGO","PTRO","CHD","BMD","HORO","COMO"),"Outflow","Inflow"))),
           Location = factor(Location, levels = c("Inflow","Reservoir", "Outflow", "Mainstem")))%>%
    dplyr::mutate(Distance = case_when(site_code == 'JOEI' ~ '1 - JOEI',
                                site_code == 'JOER' ~ '2 - JOER',
                                site_code == 'CBRI' ~ '3 - CBRI',
                                site_code == 'CBRR' ~ '4 - CBRR',
                                site_code == 'CHD' ~ '5 - CHD',
                                site_code == 'BRNR' ~ '6',
                                site_code == 'BMD' ~ '7',
                                site_code == 'LNGR' ~ '8',
                                site_code == 'LNGO' ~ '9',
                                site_code == 'PTRR' ~ '99',
                                site_code == 'PTRO' ~ '999',
                                site_code == 'COMI' ~ '11 - COMI',
                                site_code == 'COMR' ~ '12 - COMR',
                                site_code == 'COMO' ~ '13 - COMO',
                                site_code == 'HORI' ~ '14 - HORI',
                                site_code == 'HORR' ~ '15 - HORR',
                                site_code == 'HORO' ~ '16 - HORO',
                                site_code == 'BEAV' ~ '17 - BEAV',
                                site_code == 'JWC' ~ '18 - JWC',
                                site_code == 'PJW' ~ '19 - PJW',
                                site_code == 'SLEP' ~ '20 - SLEP',
                                site_code == 'PBR' ~ '21 - PBR',
                                site_code == 'SFM' ~ '22 - SFM',
                                site_code == 'PSF' ~ '23 - PSF',
                                site_code == 'PNF' ~ '24 - PNF',
                                site_code == 'PBD' ~ '25 - PBD'
    )) %>%
    dplyr::mutate(Xdistance = case_when(site_code == 'JOEI' ~ 1,
                                 site_code == 'JOER' ~ 2,
                                 site_code == 'CBRI' ~ 3,
                                 site_code == 'CBRR' ~ 4,
                                 site_code == 'CHD' ~ 5,
                                 site_code == 'LNGR' ~ 6,
                                 site_code == 'BRNR' ~ 7,
                                 site_code == 'COMI'~ 12,
                                 site_code == 'COMR'~ 13,
                                 site_code == 'COMO'~ 14,
                                 site_code == 'HORI'~ 15,
                                 site_code == 'HORR'~ 16,
                                 site_code == 'HORO'~ 17,
                                 site_code == 'BEAV'~ 18,
                                 site_code == 'JWC'~ 19,
                                 site_code == 'PJW'~ 20,
                                 site_code == 'SLEP'~ 21,
                                 site_code == 'PBR'~ 22,
                                 site_code == 'SFM'~ 23,
                                 site_code == 'PSF'~ 24,
                                 site_code == 'PNF'~ 25,
                                 site_code == 'PBD'~ 26)) %>%
    dplyr::arrange(as.factor(status)) %>%
    dplyr::filter(!watershed %in% c('Hourglass','Comanche')) %>%
    dplyr::mutate(order = case_when(site_code =="JOEI" ~ "03",
                             site_code == "LNGR" ~ "01",
                             site_code == "LNGO" ~ "02",
                             site_code == "JOER" ~ "04",
                             site_code == "CBRI" ~ "05",
                             site_code == "CBRR" ~ "06",
                             site_code == "CHD" ~ "07",
                             site_code == "PTRR" ~ "08",
                             site_code == "PTRO" ~ "09",
                             site_code == "BRNR" ~ "10",
                             site_code == "BMD" ~ "11",
                             site_code == "COMI"~ "12",
                             site_code == "COMR" ~ "13",
                             site_code == "COMO"~ "14",
                             site_code == "HORI"~ "15",
                             site_code == "HORR"~ "16",
                             site_code == "HORO"~ "17",
                             site_code == "BEAV"~ "18",
                             site_code == "JWC"~ "19",
                             site_code == "PJW"~ "20",
                             site_code == "SLEP"~ "21",
                             site_code == "PBR"~ "22",
                             site_code == "SFM"~ "23",
                             site_code == "PSF" ~ "24",
                             site_code == "PNF"~ "25",
                             site_code == "PBD"~ "26"))%>%
    dplyr::arrange(as.factor(status))
  
  write_csv(prepped_reservoir_chemistry, 
           file = out_path)
  return(prepped_reservoir_chemistry)
}

##Sample Determination
#this function breaks down all the samples taken in the last two years into 3 sampling campaigns 
sample_determination <- function(rmrs_file){
  #sampling campaign groups
  reservoir_sites <- c("COMI", "COMR", "COMO", "HORI","HORR" , "HORO","CBRR", "CHD", "JOEI", "JOER", "CBRI", "PTRR", "PTRO", "BMD", "BRNR", "LNGO", "LNGR")
  mainstem_sites <- c("JWC","PJW","SLEP","PBR","SFM","PSF","PNF","PBD")
  ISCO <- c("SAWM_ISCO", "FALL_ISCO", "BLAK_ISCO", "FISH_ISCO", "LBEA_ISCO")
  
#Determining number of Samples....

sample_breakdown <- rmrs_file%>%
  dplyr::select(Date, SiteLabel ,ChlA)%>%
  mutate(Date = as.Date.character(Date,format = "%d-%b-%y"),
         yr = lubridate::year(Date),
         campaign = ifelse(SiteLabel %in% reservoir_sites, "Reservoir", 
                           ifelse(SiteLabel %in% ISCO, "ISCO",
                                  ifelse(SiteLabel %in% mainstem_sites, "Mainstem", "Tributary")) ))%>%
  #filter for the years that the ROSS lab and RMRS sampled post fire
  dplyr::filter(yr >= "2021")


return(sample_breakdown)
}



##function to breakdown by number of samples per campaign and per year
campaign_breakdown_fun <- function(sample_breakdown){
 
  
  campaign_yr_breakdown <- sample_breakdown%>%
    group_by(yr, campaign)%>%
    summarise( n = n())
  
  return(campaign_yr_breakdown)
  
}
 
 
## function to determine number of unique chla measurements per campaign and per year
unique_chla_fun <- function(sample_breakdown){
  
  unique_chla <- sample_breakdown%>%
    drop_na()%>%
    group_by(yr, campaign)%>%
    summarise( n = n())
  
  return(unique_chla)
}

##Functions to calculate the number of meso/eutophic days per year per site

eu_meso_calc <- function(res_chem_df){
  #calc the number of days meso
  days_meso <- filter(res_chem_df, ChlA >= 2.6 &ChlA <= 8)%>%
    group_by(site_code, Year)%>%
    summarise(days_meso = n())%>%
    ungroup()
  #calc the number of days eutrophic
  days_eu <- filter(res_chem_df, ChlA >= 8)%>%
    group_by(site_code, Year)%>%
    summarise(days_eu = n())%>%
    ungroup()
  #calc mean chla and then join with days meso/eu
  
  days_meso_eu <- res_chem_df%>%
    select(Date, ChlA, Year, site_code)%>%
    na.omit()%>%
    group_by(site_code, Year)%>%
    summarise( n = n(),
               mean_chlA = mean(ChlA))%>%
    ungroup()%>% 
    left_join(days_meso, by = c("Year", "site_code"))%>%
    left_join(days_eu, by = c("Year", "site_code"))%>%
    #calculate percentages of days either eutropic, mesotrophic, or both for each site over both years
    mutate(perc_eu = (days_eu/n)*100, 
           perc_meso = (days_meso/n)*100,
           perc_meso_eu = ((days_eu+days_meso)/n)*100)
  
  return(days_meso_eu)
}


chla_no3_plot_chambers <- function(res_chem_df, choosen_dates_chambers){
  
  
  #Seasonal Color values
  season_color_vals =c('#047E82','#397534','#59B851','#DEB907','#FA850E')
  
  choosen_dates_chambers <- as.Date(choosen_dates_chambers)
  
  
  
  #chambers dataframe
  chambers_longitudinal <- res_chem_df %>%
    dplyr::filter(!is.na(ChlA)) %>%
    dplyr::filter(site_code %in% c("JOEI", "JOER", "CBRR", "CBRI", "CHD")) %>%
    dplyr::filter(Date %in% choosen_dates_chambers)%>%
    mutate(Date=as.character(Date),
           location_type = ifelse(Location == "Outflow"|Location =="Inflow", "Stream", "Reservoir"))
  
  
  
  
  
  ###PLOTING PART OF FUNCTION
  
  #chla graph
  chambers_chla_long <- chambers_longitudinal%>%
    ggplot(aes(x=Distance, y=ChlA, color=Date)) +
    geom_point( aes(shape = location_type), size=8, alpha = .7) +
    geom_line(aes(x=Xdistance), size=2, alpha = .7) +
    geom_point(aes(shape = location_type), size=8, alpha = .7) +
    scale_color_manual(values = season_color_vals)+
    scale_fill_manual(values = season_color_vals)+
    theme_bw(base_size=30) +
    labs(shape = "", color="") +
    theme(axis.text.x = element_blank(), 
          legend.text = element_text(size = 18)) +
    xlab("") +
    ylab("Chlorophyll a (μg/L)")
  
  #NO3 GRAPH
  chambers_no3_long <- chambers_longitudinal%>%
    ggplot(aes(x=Distance, y=NO3, color=Date)) +
    geom_point( aes(shape = location_type), size=8, alpha = .7) +
    geom_line(aes(x=Xdistance), size=2, alpha = .7) +
    geom_point(aes(shape = location_type), size=8, alpha = .7) +
    scale_color_manual(values = season_color_vals)+
    scale_fill_manual(values = season_color_vals) +
    theme_bw(base_size=30) +
    theme(legend.text = element_text(size =18))+
    labs(shape = "", color="") +
    scale_x_discrete(labels=c("1 - JOEI" = "Above Joe Wright", "2 - JOER" = "", "3 - CBRI" = "Above Chambers",
                              "4 - CBRR" = "", "5 - CHD" = "Below Chambers")) +
    xlab("Longitudinal Profile") +
    ylab("NO3 (mg/L)")
  
  
 chambers_plot <- ggarrange(chambers_chla_long,chambers_no3_long, ncol=1, nrow=2, common.legend=T)
  

 

 
  return(chambers_plot)
}


chla_no3_plot_mainstem <- function(res_chem_df, choosen_dates_mainstem){
  
  #Seasonal Color values
  season_color_vals = c('#047E82','#397534','#59B851','#DEB907','#FA850E')
  
  choosen_dates_mainstem <- as.Date(choosen_dates_mainstem)
#mainstem dataframe
mainstem_longitudinal <- res_chem_df %>%
  dplyr::filter(!is.na(ChlA)) %>%
  dplyr::filter(site_code %in% c("JWC","PJW","SLEP","PBR","SFM","PSF","PNF","PBD"))%>%
  dplyr::filter(Date %in% choosen_dates_mainstem) %>%
  mutate(Date=as.character(Date))

##MAINSTEM PLOT

mainstem_chla_long <- mainstem_longitudinal%>%
  ggplot(aes(x= Distance, y= ChlA, color = factor(Date))) +
  geom_point(aes(shape = Location), size=8) +
  geom_line(aes(group = Date), size=2) +
  geom_point(aes(shape = Location), size=8) +
  scale_color_manual(values = season_color_vals)+
  scale_fill_manual(values = season_color_vals) +
  theme_bw(base_size=24) +
  labs(shape = "", color="") +
  xlab("") +
  ylab("Chlorophyll a (μg/L)")+
  #theme(axis.text.x = element_blank())
  #scale_color_manual(values = cbbPalette)
  scale_x_discrete(labels=c("18 - JWC" = "JWC", "19 - PJW" = "PJW", "20 - SLEP" = "SLEP","21 - PBR" = "PBR", "22 - SFM" = "SFM", "23 - PSF" = "PSF", "24 - PNF" = "PNF", "25 - PBD" = "Canyon Mouth" )) 



#no3
mainstem_no3_long <- mainstem_longitudinal%>%
  ggplot(aes(x=Distance, y=NO3, color=factor(Date))) +
  geom_point(aes(shape = Location), size=5) +
  geom_line(aes(group = Date), size=1) +
  geom_point(aes(shape = Location), size=5) +
  scale_color_manual(values = season_color_vals)+
  scale_fill_manual(values = season_color_vals) +
  theme_bw(base_size=24) +
  labs(shape = "", color="") +
  scale_x_discrete(labels=c("18 - JWC" = "JWC", "19 - PJW" = "PJW", "20 - SLEP" = "SLEP","21 - PBR" = "PBR", "22 - SFM" = "SFM", "23 - PSF" = "PSF", "24 - PNF" = "PNF", "25 - PBD" = "Canyon Mouth" )) +
  #scale_color_manual(values = cbbPalette)+
  #theme(axis.text.x = element_blank()) +
  xlab("Longitudinal Profile") +
  ylab("NO3 (mg/L)")




mainstem_plot <- ggarrange(mainstem_chla_long, mainstem_no3_long, ncol=1, nrow=2, common.legend = TRUE)

return( mainstem_plot )
}

    
chla_no3_plot_southfork <- function(res_chem_df, choosen_dates_southfork){
  
  #Seasonal Color values
  season_color_vals =c('#047E82','#397534','#59B851','#DEB907','#FA850E')
  
  choosen_dates_southfork <- as.Date(choosen_dates_southfork)
  #southfork dataframe
  SF_longitudinal <- res_chem_df %>%
    dplyr::filter(!is.na(ChlA)) %>%
    dplyr::filter(site_code %in% c("COMI", "COMR", "COMO", "HORI", "HORR", "HORO", "BEAV", "SFM")) %>%
    dplyr::filter(Date %in% choosen_dates_southfork) %>%
    mutate(Date=as.character(Date), 
           location_type = ifelse(Location == "Reservoir", "Reservoir", "Stream"))
  
 
  
  ##SOUTH FORK PLOTS
  
  SF_chla_long <- SF_longitudinal%>%
    ggplot(aes( x= Distance, y = ChlA, color = Date)) +
    geom_point(aes(shape = location_type), size=5) +
    geom_line(aes(group = Date), size=1) +
    geom_point(aes(shape = location_type), size=5) +
    scale_color_manual(values=season_color_vals) +
    scale_fill_manual(values=season_color_vals) +
    theme_bw(base_size=24) +
    labs(shape = "", color="") +
    theme(axis.text.x = element_blank(),
          legend.position = "top") +
    xlab("") +
    ylab("Chlorophyll a (μg/L)")
  
  
  
  
  
  #NO3
  
  
  SF_no3_long <- SF_longitudinal%>%
    ggplot(aes(x=Distance, y=NO3, color=Date)) +
    geom_point(aes(shape = location_type), size=5) +
    geom_line(aes(group = Date), size=1) +
    geom_point(aes(shape = location_type), size=5) +
    scale_color_manual( values = season_color_vals)+
    scale_fill_manual( values = season_color_vals) +
    theme_bw(base_size=24) +
    #theme(axis.text.x = element_blank()) +
    labs(shape = "", color="") +
    scale_x_discrete(labels=c("11 - COMI" = "COMI", "12 - COMR" = "COMR", "13 - COMO" = "COMO","14 - HORI" = "HORI", "15 - HORR" = "HORR", "16 - HORO" = "HORO", "17 - BEAV" = "BEAV", "22 - SFM" = "SFM")) +
    xlab("Longitudinal Profile") +
    ylab("NO3 (mg/L)")
  
  
  
  
  southfork_plot <- ggarrange(SF_chla_long,SF_no3_long, ncol=1, nrow=2, common.legend=T)
  
  return(southfork_plot)
}



  

##Function for dotplot and timelines for main study reservoirs 

dotplot_timelines <- function(res_chem_df){
  
  colorsBS <- c("Barnes Meadow Reservoir" = "#D55E00", "Chambers Lake" = "#E69F00", "Joe Wright Reservoir" = "#56B4E9", "Long Draw Reservoir" = "#0072B2")
  
  colorsBS_site_code <- c("BRNR" = "#D55E00", "CBRR" = "#F0E442", "JOER" = "#56B4E9", "LNGR" = "#0072B2", "PTRR" ="#E69F00" )
  
  colorsBS_in_order <-c("#0072B2","#56B4E9","#F0E442","#E69F00","#D55E00")
  
  main_reservoir_chla <-filter(res_chem_df, Year >= "2021", site_code %in% c("LNGR", "BRNR","CBRR","PTRR", "JOER"))%>%
    filter(!is.na(ChlA))
  
  ###CHLA TIMELINE GRAPH ####
  chla_timeline <- main_reservoir_chla %>%
    ggplot() +
    geom_line(aes(x=Date, y=ChlA, group=site_code, color=Watershed_Level, label=watershed), size=1) +
    geom_point( aes(x=Date,y=ChlA, color=Watershed_Level), size=3.5) +
    scale_color_gradient2(name="Contributing Area Burn Intensity",low="#56B4E9", mid="#F0E442", high="#D55E00", guide="colourbar", midpoint=0.225, limits=c(0,0.45576),) +
    geom_abline(aes(slope=0, intercept=2.6), color='black', linetype="dashed") +
    geom_abline(aes(slope=0, intercept = 8), color='black', linetype="dashed") +
    labs(x = "Date", y = " Chlorophyll a (μg/L)") +
    theme_bw(base_size=24) +
    theme(legend.position = "none")+
    coord_cartesian(ylim=c(0,12))+
    facet_wrap(~Year, scales = "free_x")
  
 
  
  
  ###CHLA  dotPLOT GRAPH ###
  chla_boxplot <- main_reservoir_chla %>%
    ggplot(aes(x=reorder(site_code,order,max),y=ChlA )) + 
    #geom_boxplot(aes(x=reorder(site_code,order,max),y=ChlA, middle=mean(ChlA), fill=Watershed_Level))
    
    geom_dotplot(aes( fill = Watershed_Level), stackgroups = FALSE, binaxis = "y", method = "histodot",stackdir = "center", position = "dodge", binwidth = .5, dotsize = 1, alpha = .7)+
    stat_summary(fun.y = median, fun.ymin = median, fun.ymax = median,
                 geom = "crossbar", width = .75)+
    
    scale_fill_gradient2(name="Watershed Burn Intensity",low="#56B4E9", mid="#F0E442", high="#D55E00", 
                         guide="colourbar", midpoint=0.225, limits=c(0,0.45576),)+
    scale_color_grey(start = 0.4, end = 0)+
    geom_abline(aes(slope=0, intercept=2.6), color='black', linetype="dashed") +
    geom_abline(aes(slope=0, intercept=8), color='black', linetype="dashed") +
    theme_bw(base_size=24) +
    theme(legend.position = "none",
          axis.text.x = element_text( color  = colorsBS_in_order),
          strip.background = element_blank(),
          strip.text.x = element_blank()) +
    
    labs( x = "Site", y = "Chlorophyll a (μg/L)")+
    coord_cartesian(ylim=c(0,12))+
    facet_wrap(~Year, scales = "free_x")
  

  
  
  
  
  ###COMBINING AND SAVING GRAPHS###
  
  
  main_study_reservoirs_timeline_dot_and_boxplot <- ggarrange(chla_timeline, chla_boxplot, nrow = 2, ncol = 1)
 # ggsave('output/main_study_reservoirs_timeline_dot_and_boxplot_final.jpg',width=15,height=10, units ="in", dpi = 300)
  
  return(main_study_reservoirs_timeline_dot_and_boxplot)
}

site_map_fun <- function(sites_file, boundary_file_name){
  CPF_sites <- sites_file%>%
    sf::st_as_sf(coords=c("Long","Lat"), crs=4326)%>%
    dplyr::mutate(watershed=ifelse(site_code %in% c('LNGR','LNGO'),"Long Draw Reservoir",
                            ifelse(site_code %in% c('PTRO','PTRR'),'Peterson Reservoir',
                                   ifelse(site_code %in% c('SLEP','JWC','PNF','PBD', 'PJW','PBR', 'SFM','PSF' ),'CLP Mainstem Canyon',
                                          ifelse(site_code %in% c('ELC','ARCH', 'LEGC','TIMB' ),'CLP Mainstem Fort Collins',
                                                 ifelse(site_code %in% c('JOEI','JOER'),"Joe Wright Reservoir",
                                                        ifelse(site_code %in% c('CBRI', 'CBRR','CHD'), "Chambers Reservoir",
                                                               ifelse(site_code %in% c('BRNR','BMD'),"Barnes Meadow Reservoir",
                                                                      ifelse(site_code %in% c('COMR','COMO','COMI'),"Comanche Reservoir",
                                                                             ifelse(site_code %in% c('HORO','HORI','HORR'),"Hourglass Reservoir","CLP Tributary" ))))))))))%>%
    dplyr::mutate(site_type=ifelse(Type %in% c('Outflow','Inflow'),"Stream",
                            ifelse(Type %in% c("Stream"),'Tributary',
                                   ifelse(Type %in% c("Mainstem"),'Mainstem',"Reservoir"))))
  
  
  site_map <- mapview::mapview(CPF_sites,zcol='Campaign')
  return(site_map)
}


