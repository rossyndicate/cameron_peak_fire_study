library(targets)
library(tarchetypes)

tar_option_set(packages = c("tidyverse", "dplyr"))

source("R/functions.R")


list(
  tar_file_read(buffer_sbs, 
                "data/sbs_buffer.csv", 
                read.csv(!!.x)),
  
  tar_file_read(cpf_sites, 
                "data/cpf_sites.csv", 
                read.csv(!!.x)),
  
  tar_file_read(distance_from_pbd, 
                "data/Distance_from_PBD.csv", 
                read.csv(!!.x)),
  
  tar_file_read(watershed_sbs, 
                "data/sbs_watershed.csv", 
                read.csv(!!.x)),
  
  tar_file_read(all_chemistry_cpf, 
    "data/ReservoirChemistry_010523.csv",
    read.csv(!!.x)),
  tar_file_read(rmrs_chemistry_cpf, 
                "data/cam_peak_total_dec22.csv",
                read.csv(!!.x)),
  
  
  tar_target(name = prepped_reservoir_chem, 
             command = prepare_res_chem(buffer_file = buffer_sbs,
                                        sites_file = cpf_sites, 
                                        dist_file = distance_from_pbd, 
                                        watershed_file = watershed_sbs, 
                                        ross_chem_file = all_chemistry_cpf, 
                                        rmrs_chem_file = rmrs_chemistry_cpf, 
                                        out_path = "data/prepped_reservoir_chem.csv"), 
             packages = c("tidyverse", "dplyr", "lubridate")),
  
   tar_target(campaign_samples,
              sample_determination(rmrs_file = rmrs_chemistry_cpf)),
  
   tar_target(unique_chla,
              unique_chla_fun(campaign_samples)),
  
   tar_target(campaign_yr_breakdown ,
     campaign_breakdown_fun(campaign_samples)),
  
   tar_target(days_percent_meso_eutrophic,
              eu_meso_calc(prepped_reservoir_chem)),

   tar_target(chambers_longitudinal_plot, 
              chla_no3_plot_chambers(prepped_reservoir_chem, 
                                     choosen_dates_chambers = c("2022-06-14", "2022-07-26","2022-08-16","2022-09-27","2022-10-18")),
             packages = c("ggplot2", "ggthemes", "ggpubr", "gghighlight", "plotly")),
  
  tar_target(mainstem_longitudinal_plot, 
             chla_no3_plot_mainstem(prepped_reservoir_chem,
                                    choosen_dates_mainstem = c("2022-05-31", "2022-07-11","2022-09-23","2022-10-21","2022-11-08")),
             packages = c("ggplot2", "ggthemes", "ggpubr", "gghighlight", "plotly")),
  
  tar_target(southfork_longitudinal_plot, 
             chla_no3_plot_southfork(prepped_reservoir_chem, 
                                 choosen_dates_southfork = c("2021-07-09", "2022-06-29","2022-07-18","2022-08-31", "2022-09-28")),
             packages = c("ggplot2", "ggthemes", "ggpubr", "gghighlight", "plotly")),
  
   tar_target(main_study_reservoirs_time_dotplot,
              dotplot_timelines(prepped_reservoir_chem), 
              packages = c("ggplot2", "ggthemes", "ggpubr", "gghighlight", "plotly") ),
  
   tar_target(site_map,
           site_map_fun(sites_file = cpf_sites, boundary_file_name = "data/cpf_sites.csv"),
     packages = c("mapview", "sf")
   ),
  
   tar_render(cpf_report,
              "R/CamPk_Reservoir_report.Rmd")
)

