build_res_dotplot<- function(param = "ChlA"){
  boxplot_res <- filter(tidy_chem, site_code %in% c("LNGR", "PTRR", "BRNR", "JOER", "CBRR") )%>%
    filter(!is.na(.data[[param]]))%>%
    mutate(site_year = paste(site_code, Year, sep = " "),
           dayofyear = yday(Date))
  param_units <- filter(chem_units, simple == param)%>%
    pull(combined)
  
  #Create plot
  ggplot(boxplot_res, aes(x=reorder(site_year,order,max),y=.data[[param]], middle=mean(.data[[param]]), fill=Watershed_Level, colour = factor(Year))) + 
    geom_dotplot(stackgroups = FALSE, binaxis = "y", method = "histodot",stackdir = "center", position = "dodge", binwidth = .3, dotsize = 1)+
    scale_color_grey(start = 0.4, end = 0)+
    
    geom_rect( inherit.aes=FALSE, aes(xmin="LNGR 2021", xmax="LNGR 2023", ymin=-.5,
                                      ymax=0), color="transparent", fill="#0072B2", alpha=0.01)+
    geom_rect( inherit.aes=FALSE, aes(xmin="JOER 2021", xmax="JOER 2023", ymin=-.5,
                                      ymax=0), color="transparent", fill="#56B4E9", alpha=0.01)+
    geom_rect( inherit.aes=FALSE, aes(xmin="CBRR 2021", xmax="CBRR 2023", ymin=-.5,
                                      ymax= 0), color="transparent", fill="#E2DC62", alpha=0.01)+
    geom_rect( inherit.aes=FALSE, aes(xmin="PTRR 2021", xmax="PTRR 2023", ymin=-.5,
                                      ymax=0), color="transparent", fill="#E69F00", alpha=0.01)+
    geom_rect( inherit.aes=FALSE, aes(xmin= 12.5, xmax= 15.5, ymin=-.5,
                                      ymax=0), color="transparent", fill="#D55E00", alpha=0.01)+
    geom_abline(aes(slope=0, intercept=0), color='black', linetype="solid") +
    geom_text(aes(x= 2, y = -.25, label= "Long Draw",  color = "black"), size = 6)+
    geom_text(aes(x= 5, y = -.25, label= "Joe Wright",  color = "black"), size = 6)+
    geom_text(aes(x= 8, y = -.25, label= "Chambers",  color = "black"), size = 6)+
    geom_text(aes(x= 11, y = -.25, label= "Peterson",  color = "black"), size = 6)+
    geom_text(aes(x= 14, y = -.25, label= "Barnes Meadow",  color = "black"), size = 6)+
    
    scale_fill_gradient2(name="Watershed Burn Intensity",low="#56B4E9", mid="#F0E442", high="#D55E00", 
                         guide="colourbar", midpoint=0.225, limits=c(0,0.45576),) +
    theme_bw(base_size=25) +
    
    #theme(axis.text.x = ifelse(study_reservoir_chla$Location == "Reservoir",element_text(face = "bold"),element_text(face = "plain"))) +
    labs( x = "Site", y = param_units)+
    scale_x_discrete(labels = c("2021", "2022", "2023","", "", "","2021", "2022", "2023","", "", "","2021", "2022", "2023"))+
    theme(legend.position = "none",
          axis.title = element_text(face = "bold"),
          axis.text.x = element_text(face = "bold"))
  
}
#test_function
build_res_dotplot(param = "ChlA")
