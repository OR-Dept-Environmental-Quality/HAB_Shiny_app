library(tidyverse)
library(lubridate)
library(readxl)
library(rgdal) 
library(sf)
library(raster)
library(leaflet)
library(RColorBrewer)

setwd("//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/HAB_Shiny_app")

# (1) Plot and Table ----
dta1 <- readxl::read_xlsx("./data/HAB_resolvablelakes_2016_to_2019.xlsx",
                          sheet = "HAB_resolvablelakes_2016_2020") %>% 
  dplyr::mutate(wi_DWSA = NA)

dta2 <- readxl::read_xlsx("./data/HAB_resolvablelakes_2020.xlsx",
                          sheet = "HAB_resolvable_toAug262020") %>% 
  dplyr::select(-"...13")

dta3 <- readxl::read_xlsx("./data/HAB_resolvablelakes_2020.xlsx",
                          sheet = "NHDWaterbody_resolvable_inDWSA")

GNISNameID <- unique(sort(dta3$GNIS_Name_ID...1))

dta <- rbind(dta1,dta2) %>% 
  dplyr::rename(Mean = MEAN_cellsml,
                Maximum = MAX_cellsml,
                Minimum = MIN_cellsml) %>% 
  tidyr::gather(`Summary Statistics`, `Cyanobacteria (cells/mL)`, -GNISIDNAME,-COUNT,-AREA,-PercentArea_Value, -Day,-Year,-Date,-wi_DWSA) %>% 
  tidyr::separate(GNISIDNAME,c("GNISNAME","GNISID"), sep="_") %>% 
  dplyr::mutate(GNISIDNAME = paste0(GNISNAME,"_",GNISID)) %>% 
  dplyr::mutate(Date = lubridate::ymd(Date)) %>% 
  dplyr::arrange(desc(Date)) %>% 
  dplyr::mutate(wi_DWSA = ifelse(GNISIDNAME %in% GNISNameID, "Yes", "No")) %>% 
  dplyr::filter(!GNISIDNAME == "Goose Lake_01520146")

# (2) Date Lookup Table ----
fulldays <- readxl::read_xlsx("./data/2016-2020.xlsx",
                              sheet = "2016-2020") %>% 
  dplyr::mutate(Date = lubridate::ymd(Date))

lookup.date <- dta %>% 
  dplyr::group_by(Date, Year, Day) %>% 
  dplyr::summarise(n=n()) %>% 
  dplyr::right_join(fulldays, by="Date") %>% 
  dplyr::rename(Year.dta = Year.x,
                Day.dta = Day.x,
                Year.fulldays = Year.y,
                Day.fulldays = Day.y)

missing.dates <- lookup.date %>% 
  dplyr::filter(is.na(Day.dta))

# (3) Map: shapefile ----
lakes.resolvable <- rgdal::readOGR(dsn = "./data/NHDwaterbody_resolvable_lakes_dissolved_oregon.shp",
                                   layer = "NHDwaterbody_resolvable_lakes_dissolved_oregon")

lakes.oregon <- rgdal::readOGR(dsn = "./data/NHDwaterbody_oregon_lakes.shp",
                               layer = "NHDwaterbody_oregon_lakes")

# (4) Map: raster ----
# Raster color 
thevalues <-c(0,6310,18000,43000,61500,84000,100000,130000,1000000)

pal.map <- leaflet::colorBin(palette = RColorBrewer::brewer.pal(7,"YlOrRd"),
                             bins = c(0,6310,18000,43000,61500,84000,100000,130000,1000000),
                             domain = c(0,6310,18000,43000,61500,84000,100000,130000,1000000),
                             na.color = "transparent")
# Legend labels
labels = c("\u2264 6,310 (Not Detected)","6,310 - 18,000","18,000 - 43,000","43,000 - 61,500","61,500 - 84,000","84,000 - 100,000","100,000 - 130,000","130,000 - 1,000,000")
# ----
rm(dta1); rm(dta2); rm(dta3)
save.image(file = "data.RData")
