library(tidyverse)
library(lubridate)

setwd("//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/HAB_Shiny_app")
# data <- read.csv("//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/HAB_Shiny_app/Data/data.csv")

dta <- data %>% 
  dplyr::rename(Mean = MEAN_cellsml,
                Maximum = MAX_cellsml,
                Minimum = MIN_cellsml) %>% 
  tidyr::gather(`Summary Statistics`, `Cyanobacteria (cells/mL)`, -GNISIDNAME,-COUNT,-AREA,-Day,-Year,-Date,-wi_DWSA) %>% 
  tidyr::separate(GNISIDNAME,c("GNISNAME","GNISID"), sep="_") %>% 
  dplyr::mutate(GNISIDNAME = paste0(GNISNAME,"-",GNISID)) %>% 
  dplyr::mutate(Date = mdy(Date)) %>% 
  dplyr::mutate(wi_DWSA = ifelse(wi_DWSA == "#N/A", "No",
                                 ifelse(wi_DWSA == "","Unknown",
                                        wi_DWSA))) %>% 
  dplyr::rename(`Within Drinking Water Source Area` = wi_DWSA)


save.image(file = "data.RData")
