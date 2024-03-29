#Created by IAO on 2022-10-04 to preview Colorado data from Michael (pseudo limnosat, lakes < 10 ha)
if (!require('here')) install.packages('here');library('here') 

source("src/00_libraries.R")
source("src/00_helperFuns.R")

CO_data <-
  list.files(path = "~/cameron_peak_fire_study/data/co_lake_reflectance",
             # Identify all CSV files
             pattern = "*.csv",
             full.names = TRUE) %>%
  purrr::map_dfr( ~ .x %>%
                    readr::read_csv(col_types = cols(
                      .default = "?", permanent = "c"
                    )))
  #break system:index into a few columns

#split 'player' column using '_' as the separator
CO_data[c('col1', 'col2','col3','col4','date','col6')] <- str_split_fixed(CO_data$`system:index`, '_', 6)
#make date a date

CO_data <- CO_data %>%
  mutate(date_new= as.Date(paste(date, sep=""), "%Y%m%d")) %>%
  unite("LandsatID", col1:date, sep= "_", 
        remove = TRUE) %>%
  rename(lake_nhdid=permanent,
         mystery_ID=col6,
         date=date_new)  %>%
  mutate(year=year(date)) %>%
  drop_na() %>%
  mutate(dwl=fui.hue(Red, Green, Blue))

names(CO_data)
str(CO_data)
names(CO_data)
str(CO_data)

#How many unique lakes?
length(unique(CO_data$nhdplusv2_comid))
#5324
#Sam: 5213

#How many years?
length(unique(CO_data$year))
#39
# Sam:38

#How many of them have a corresponding lagoslakeid? 
hist(CO_data$dwl)
#Why so green? Benthic periphyton in small, shallow lakes... agricultural ponds


##SKIP TO HERE ONCE IT HAS BEEN PULLED FROM FOLDER##


CO_data <- read.csv("data/compiled_limnosat_110122.csv") %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))%>%
  rename(lake_nhdid = nhdplusv2_comid)
 



#join by nhd id from lagos dataset
res_nhd <- read.csv("data/burned_water/lake_information.csv")%>%
  filter(lake_county == "Larimer")%>%
filter(lake_namegnis %in% c("Chambers Lake", "Barnes Meadow Reservoir", "Peterson Lake", "Long Draw Reservoir", "Joe Wright Reservoir", "Horsetooth Reservoir", "Halligan Reservoir", "Seaman Reservoir", "Comanche Reservoir", "Hourglass Reservoir"))

#check that spelling of names worked
unique(res_nhd$lake_namegnis)





#filter dataset to our lakes and add real names
clp_major_lake_data <- CO_data%>%
 filter(lake_nhdid %in% res_nhd$lake_nhdid)%>%
inner_join(., res_nhd, by= "lake_nhdid")
#check to make sure all lakes came through
unique(cpf_lake_data$lake_namegnis)

cpf_lakes_sum <- cpf_lake_data%>%
  group_by(lake_namegnis, year)%>%
  summarise(count = n())
  
cpf_lake_data <- read.csv("data/cpf_lakes_limnosat.csv")%>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))



#graph dominant wavelength
colorsBS <- c("Barnes Meadow Reservoir" = "#E69F00", "Chambers Lake" = "#F0E442", "Peterson Lake" = "#D55E00", "Joe Wright Reservoir" = "#56B4E9", "Long Draw Reservoir" = "#0072B2")

dwl_cpf_graph <- cpf_lake_data%>%
  ggplot()+
  geom_point(aes(x= date, y = dwl, color = site, size = site), alpha=.7)+
  theme_bw()+
  scale_color_manual(name="", values = colorsBS ) +
  scale_size_manual(values = c(2.5,2,1.5,1, .5))+
  guides(size = "none")+
  ylab("Dominant Wavelength")+
  xlab("Date")

plot(dwl_cpf_graph)

dwl_cpf_boxplot <- filter(cpf_lake_data, year>2019) %>%
  ggplot(aes(x=site,y=dwl, middle=mean(dwl), fill=site)) + 
  geom_dotplot(stackgroups = FALSE, binaxis = "y", method = "histodot",stackdir = "center", position = "dodge", binwidth = 5, dotsize = 1)+
  theme_bw()+
  scale_fill_manual(name="", values = colorsBS ) +
  scale_size_manual(values = c(2.5,2,1.5,1, .5))+
  guides(size = "none")+
  ylab("Dominant Wavelength")+
  xlab("Date")
plot(dwl_cpf_boxplot)

write_csv(cpf_lake_data, "data/cpf_lagos_join_limnosat.csv")
