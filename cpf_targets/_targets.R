library(targets)
library(tarchetypes)


tar_option_set(packages = c("tidyverse", "dplyr"))

source("R/functions.R")


list(
  tar_target(name = reservoir_chem, 
             command = import_res_chem(out_path = "data/prepped_reservoir_chem.csv"), 
             packages = c("tidyverse", "dplyr", "lubridate")),
   tar_target(campaign_samples,
              sample_determination(total_chem_file_path = "data/cam_peak_total_dec22.csv")),
  
   tar_target(unique_chla,
              unique_chla_fun(campaign_samples)),
  
   tar_target(campaign_yr_breakdown ,
     campaign_breakdown_fun(campaign_samples)),
  
   tar_target(days_percent_meso_eutrophic,
              eu_meso_calc(reservoir_chem)),

   tar_target(chla_no3_longitudinal_graphs,
              longitudinal_graphs(reservoir_chem, choosen_dates_chambers = c("2022-06-14", "2022-07-26","2022-08-16","2022-09-27","2022-10-18"),
                      choosen_dates_southfork = c("2021-07-09", "2022-06-29","2022-07-18","2022-08-31", "2022-09-28"),
                        choosen_dates_mainstem = c("2022-05-31", "2022-07-11","2022-09-23","2022-10-21","2022-11-08")),
  
     packages = c("ggplot2", "ggthemes", "ggpubr", "gghighlight", "plotly")
   ),
   tar_target(main_study_reservoirs_time_dotplot,
              dotplot_timelines(reservoir_chem), 
              packages = c("ggplot2", "ggthemes", "ggpubr", "gghighlight", "plotly") ),
  
   tar_target(site_map,
           site_map_fun("data/cpf_sites.csv"),
     packages = c("mapview", "sf")
   ),
  
   tar_render(cpf_report,
              "R/CamPk_Reservoir_report.Rmd")
)


