



build_long_plot <- function(system = "chambers", dates = choosen_dates_chambers ){
  
  
  if(system == "Chambers"){
    
    #chambers dataframe
    chambers_longitudinal <- tidy_chem %>%
      dplyr::filter(!is.na(ChlA)) %>%
      dplyr::filter(site_code %in% c("JOEI", "JOER", "CBRR", "CBRI", "CHD")) %>%
      dplyr::filter(Date %in% dates)%>%
      mutate(Date=as.character(Date),
             location_type = ifelse(location == "Outflow"|location =="Inflow", "Stream", "Reservoir"))
    
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
    
    chambers_plot
    
  }
  else if(system == "CLP"){
    
    
    
    date_df <- tibble( date_col = as.Date(dates), 
                         wk =  as.character(week(date_col)),
                        year =as.character(year(date_col)),
                           wk_year = paste(year, wk), 
                       date_char = as.character(dates))
    
    
    weeks_select2022 <- filter(date_df, year == 2022)%>%
      pull(wk)
    weeks_select2023 <- filter(date_df, year == 2023)%>%
      pull(wk)
    week_all <- date_df%>% pull(wk)
    
    longitudinal_res_mainstem_2022 <- filter(tidy_chem, Year == "2022"& Date != "2022-09-19")%>%
      filter ( site_code %in% c("JOEI","JOER","CBRI","CBRR","CHD", "JWC", "SLEP", "PFAL",
                                "PBR","PSF","PNF","PBD"))%>%
      select(site_code,Date, watershed, Watershed_Level, distance_upstream_km, ChlA, NO3, PO4, K, status, location )%>%
      mutate(wk = as.character(week(Date)))%>%
      filter(wk %in% weeks_select2022 )
    
    longitudinal_res_mainstem_both <- filter(tidy_chem, Year == "2023")%>%
      filter ( site_code %in% c("JOEI","JOER","CBRI","CBRR","CHD", "JWC", "SLEP", "PFAL",
                                "PBR","PSF","PNF","PBD"))%>%
      select(site_code,Date, watershed, Watershed_Level, distance_upstream_km, ChlA, NO3, PO4, K, status, location )%>%
      mutate(wk = as.character(week(Date)))%>%
      filter(wk %in% weeks_select2023 )%>%
        rbind(longitudinal_res_mainstem_2022)%>%
        mutate(dist_pbd = as.character(distance_upstream_km),
               site_code  = case_when(site_code == "PFAL" ~ "SLEP", TRUE ~ site_code),
               type = ifelse(location %in% c("Inflow","Outflow"),"Stream",
                             ifelse(location == "Reservoir", "Reservoir", "Mainstem")))
      
      
    
    
    longitudinal_res_mainstem$wk <- factor(longitudinal_res_mainstem$wk, levels = week_all)
    longitudinal_res_mainstem$type <- factor(longitudinal_res_mainstem$type, levels = c("Stream", "Reservoir", "Mainstem"))
    
    
    
    #graphing
    
    longitudinal_graph_res_mainstem <- longitudinal_res_mainstem_both%>%
      ggplot()+
      geom_point(aes(x = distance_upstream_km, y= ChlA, shape = type, color = wk), alpha = .9, size = 8)+
      geom_line(aes(x= distance_upstream_km, y = ChlA, group = wk, color =  wk), size = 4, alpha = .8)+
      geom_point(aes(x= distance_upstream_km, y= ChlA, shape = type, color = wk), alpha = .9, size = 8)+
      #gghighlight(wk == "30", use_direct_label = FALSE )+
      theme_bw(base_size = 20)+
      scale_x_reverse()+
      scale_color_manual(name = "Week of", labels= c(date_df$date_char), values = c("22" = "#047E82", "28" = "#397534",  "31"= '#59B851', "36" = '#DEB907', "42" = '#FA850E' ))+
      
      scale_shape_manual(values=c(17, 16, 15))+
      scale_fill_manual( values = c("22" = "#047E82", "28" = "#397534",  "31"= '#59B851', "36" = '#DEB907', "42" = '#FA850E'  ))+
      # scale_color_manual(name = "Week of", values = c("6-01" = "#047E82", "7-11" = "#397534", "7-25"='#59B851', "8-08"= '#DEB907' ))+
      labs(shape = "Site Type", color="", fill ="") +
      theme(legend.justification = c(.9,.9) , legend.position = c(.9, .9),
            #legend.box.background  = element_rect(color = "black",size = 1.5),
            axis.title = element_text(face = "bold"),
            axis.text.x = element_text(face = "bold"))+
      #theme(axis.text.x = element_blank()) +
      xlab("Distance from Canyon Mouth (km)") +
      ylab("Chlorophyll a (μg/L)")
    
    #plot(longitudinal_graph_res_mainstem)
    #ggsave("output/longitudinal_res_main_2022.jpg", width = 20, height = 9)
    
    
    #gghighlight graphs for presentation
    
    longitudinal_gghighlight_graph_res_mainstem <- longitudinal_graph_res_mainstem+
      gghighlight(wk == "42", keep_scales = T, use_direct_label = FALSE)
    
    
    plot(longitudinal_gghighlight_graph_res_mainstem)
  }
  else if(system == "SF"){
    
  }
  
  
}





#ggsave('output/2022_joer_cbrr_chla_no3_longitudinal_FINAL.jpg', width=15.5, height=10, dpi = 300)

#highlighted version of graph for presentations

chla_long_highlighted <- chambers_chla_long + gghighlight(Date == "2022-07-26", keep_scales = T)
no3_long_highlighted <-chambers_no3_long + gghighlight(Date == "2022-07-26", keep_scales = T)


ggarrange(chla_long_highlighted,no3_long_highlighted, ncol=1, nrow=2, common.legend=T)


#ggsave('output/2022_joer_cbrr_chla_no3_longitudinal_highlighted.jpg', width=15, height=10)