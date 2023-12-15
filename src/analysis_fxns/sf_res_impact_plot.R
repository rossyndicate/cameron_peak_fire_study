plot_sf_impact <- function( param = "ChlA"){

  #filter and org
  sf_chem<- filter(most_recent_chem, site_code %in% SF_Res & Year %in% c(2022, 2023) & !is.na(.data[[param]]))%>%
    select(site_code,Date,all_of(param),location, Buffer_Level, Watershed_Level, watershed, Year )%>%
    mutate(location = case_when(
      location == "Stream" ~ "Beaver Creek", 
      location == "Mainstem" ~ "South Fork", 
      TRUE ~ location), 
      site_group = case_when(
        watershed == "Comanche Reservoir" ~ watershed, 
        watershed == "Hourglass Reservoir"~ watershed, 
        TRUE ~ "Stream"
      ), 
      filled_status = case_when(
        Year == 2023 ~ "Filled (2023)", 
        Year == 2022 ~ "NOT Filled  (2022)"
      ))
    
    
  sf_chem$location <- factor(sf_chem$location, levels = c("Inflow","Reservoir", "Outflow","Beaver Creek","South Fork"))
  
  #get nice units for Y axis
  lab_w_units <-  filter(chem_units, simple %in% param)%>%pull(combined)
  #plot
  ggplot(sf_chem, aes(x=location,y=.data[[param]],  color = filled_status, shape = site_group)) + 
    geom_jitter(size = 8,width = 0.2)+
    theme_bw(base_size = 30)+
    scale_color_manual(name = "Reservoir Status", values = c("#0072B2", "#D55E00"))+
    labs(color = "Reservoir Status",
         shape = "Site Grouping", 
         y = lab_w_units)+
    theme(axis.title.x = element_blank())
  
  
}


sf_impact_df <- function(param = "ChlA"){
  
  sf_chem <- filter(most_recent_chem, site_code %in% SF_Res & Year %in% c(2022, 2023) & !is.na(.data[[param]]))%>%
    select(site_code,Date,all_of(param),location, Buffer_Level, Watershed_Level, watershed, Year )%>%
    mutate(location = case_when(
      location == "Stream" ~ "Beaver Creek", 
      location == "Mainstem" ~ "South Fork", 
      TRUE ~ location), 
      site_group = case_when(
        watershed == "Comanche Reservoir" ~ watershed, 
        watershed == "Hourglass Reservoir"~ watershed, 
        TRUE ~ "Stream"
      ), 
      filled_status = case_when(
        Year == 2023 ~ "Filled (2023)", 
        Year == 2022 ~ "NOT Filled  (2022)"
      ))
  
  return(sf_chem)
}
# plot_sf_impact()
# 
# ggsave("data/figs_output/2023_figs/sf_impact_chla.jpg", width = 20, height = 12, dpi = 300)
    