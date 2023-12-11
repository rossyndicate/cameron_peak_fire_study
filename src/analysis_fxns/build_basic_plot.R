build_basic_chem_plot <-  function(sites, param, title = FALSE, style = "timeline", graph_size = 28){
  
  
  param_label <- filter(chem_units, simple == param)%>%
    pull(combined)
  
  chem_data <- tidy_chem%>%
    filter(site_code %in% sites)%>%
    select(-c(FCW_Number, data_source, Lat, Long, Time, Field_DO_mgL, `Field_Cond_µS/cm`, Field_Temp_C, dt, Campaign, distance_upstream_km, watershed))%>%
    pivot_longer(cols = c(ChlA, Turbidity, TSS, pH,ANC, Na, NH4, K, Mg, Ca, F, Cl, PO4, SO4, SC,NO3, DOC, DTN),names_to = "parameter", values_to = "value")
  
  
  if(style == "timeline"){
    
  if(title == FALSE){
    ## NO TITLE 
    notitle_graph <- chem_data%>%
      filter(parameter == param& !is.na(parameter))%>%
      ggplot(., aes(x=Date,y=value, color=Watershed_Level)) +
      geom_point(size=3) +
      geom_line(aes(group=site_code), linewidth=1) +
      scale_color_gradient2(name="Contributing Area Burn Intensity",low="#56B4E9", mid="#F0E442", high="#D55E00", guide="colourbar", midpoint=0.225, limits=c(0,0.45576),) +
      #geom_abline(aes(slope=0, intercept=2.5), color='black', linetype="dashed") +
      labs(x = "", y = param_label) +
      theme_bw(base_size=28) +
      theme(legend.position = "none",
            strip.background = element_blank(),
            strip.text.x = element_blank())+
      facet_wrap(~Year, scales = "free_x")
    
    return(notitle_graph)
  }
  
  ## Title Plot
  if(title == TRUE){
    title_graph <- chem_data%>%
      filter(parameter == param& !is.na(parameter))%>%
      ggplot(., aes(x=Date,y=value, color=Watershed_Level)) +
      geom_point(size=3) +
      geom_line(aes(group=site_code), linewidth=1) +
      scale_color_gradient2(name="Contributing Area Burn Intensity",low="#56B4E9", mid="#F0E442", high="#D55E00", guide="colourbar", midpoint=0.225, limits=c(0,0.45576),) +
      #geom_abline(aes(slope=0, intercept=2.5), color='black', linetype="dashed") +
      labs(x = "", y = param_label) +
      theme_bw(base_size=28) +
      theme(legend.position = "none")+
      facet_wrap(~Year, scales = "free_x")
    
    return(title_graph)
    }
  }
  ## Create Boxplot
    if(style == "boxplot"){
  
    ## NO TITLE
    if(title == FALSE){
      
      no_title_graph <- chem_data%>%
        filter(parameter == param)%>%
        ggplot() +
        geom_boxplot(aes(x=reorder(site_code,order,max),y=value, middle=mean(value), fill=Watershed_Level)) +
        scale_fill_gradient2(name="Watershed Burn Intensity",low="#56B4E9", mid="#F0E442", high="#D55E00", 
                             guide="colourbar", midpoint=0.225, limits=c(0,0.45576),) +
        theme_bw(base_size=graph_size)+
        theme(legend.position = "none",
              strip.background = element_blank(),
              strip.text.x = element_blank())+
        labs(x = "", y = param_label)+
        facet_wrap(~Year)
      
      return( no_title_graph)
    }
    
    if(title == TRUE){
      
      title_graph <- chem_data%>%
        filter(parameter == param)%>%
        ggplot() +
        geom_boxplot(aes(x=reorder(site_code,order,max),y=value, middle=mean(value), fill=Watershed_Level)) +
        scale_fill_gradient2(name="Watershed Burn Intensity",low="#56B4E9", mid="#F0E442", high="#D55E00", 
                             guide="colourbar", midpoint=0.225, limits=c(0,0.45576),) +
        theme_bw(base_size=graph_size)+
        theme(legend.position = "none")+
        labs(x = "", y = param_label)+
        facet_wrap(~Year)
      
      return( title_graph)
    }
    
  }
  if(style %nin% c("timeline", "boxplot")){
  cat("Style inputted:", style,"\nInputted style does not match current plotting styles: timeline, boxplot")
  }
    
}

#testing
#test<- build_basic_chem_plot(sites = mainstem_Res_only, param = "NO3", title = FALSE, style = "boxplot", graph_size = 20)
#plot(test)
