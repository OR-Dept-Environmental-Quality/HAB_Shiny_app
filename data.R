library(tidyverse)
library(lubridate)
library(readxl)
library(rgdal) 
library(sf)
library(raster)
library(leaflet)
library(RColorBrewer)
library(rasterVis)
library(zoo)

# Get update of NASA data----
# Need to have ArcPro on your machine; modify path in the script to point to the correct version of python
source("Update_NASA_imagery.R")

# (1) Data Table ----
dta1 <- readxl::read_xlsx("./data/Resolvable_Lakes.xlsx", sheet = "cyan_resolvable_lakes")

dta2 <- readxl::read_xlsx("./data/HAB_resolvablelakes_2022.xlsx", sheet = "HAB_resolvable_lake_data") %>% 
  #dplyr::filter(!GNISIDNAME == "Goose Lake_01520146") %>% # located in the WA state
  #dplyr::filter(GNISIDNAME %in% unique(sort(lakes.resolvable$GNISIDNAME))) %>% 
  dplyr::filter(GNISIDNAME %in% dta1$inApp) # filter out saline lakes

dta3 <- readxl::read_xlsx("./data/HAB_resolvablelakes_2016_2021.xlsx",sheet = "HAB_resolvablelakes_2016_2021") %>% 
  #dplyr::filter(!GNISIDNAME == "Goose Lake_01520146") %>% # located in the WA state
  #dplyr::filter(GNISIDNAME %in% unique(sort(lakes.resolvable$GNISIDNAME))) %>% 
  dplyr::filter(GNISIDNAME %in% dta1$inApp) %>% # filter out saline lakes
  dplyr::select(-c(13,14)) # remove two last columns for excel data summary

dta <- rbind(dta2,dta3) %>% 
  dplyr::rename(Mean = MEAN_cellsml,
                Maximum = MAX_cellsml,
                Minimum = MIN_cellsml) %>% 
  tidyr::gather(`Summary Statistics`, `Cyanobacteria (cells/mL)`, -GNISIDNAME,-COUNT,-AREA,-PercentArea_Value, -Day,-Year,-Date) %>% 
  tidyr::separate(GNISIDNAME,c("GNISNAME","GNISID"), sep="_") %>% 
  dplyr::mutate(GNISIDNAME = paste0(GNISNAME,"_",GNISID)) %>% 
  dplyr::mutate(Date = lubridate::ymd(Date)) %>% 
  dplyr::arrange(desc(Date)) %>% 
  dplyr::mutate(wi_DWSA = ifelse(GNISIDNAME %in% dta1$wi_DWSA, "Yes", "No"))
 

#dta_rolling_ave <- dta2 %>% 
#  dplyr::arrange(GNISIDNAME,desc(Date)) %>% 
#  dplyr::group_by(GNISIDNAME) %>% 
#  dplyr::mutate(rollmean_7 = zoo::rollmean(MEAN_cellsml, k = 7, fill = NA, align = "left"),
#                rollmax_7 = zoo::rollmax(MAX_cellsml, k=7, fill =  NA, align = "left")) %>% 
#  dplyr::ungroup() %>% 
#  dplyr::filter(as.Date(Date) == as.Date(max(dta2$Date)))

#mean_top_10 <- dta_rolling_ave %>% 
#  dplyr::arrange(desc(rollmean_7))
#max_top_10 <- dta_rolling_ave %>% 
#  dplyr::arrange(desc(rollmax_7)) %>%
#  dplyr::mutate(rollmax_7 = format(round(rollmax_7,0), big.mark=",", scientific=FALSE)) %>% 
#  dplyr::select(`Waterbody_GNISID` = GNISIDNAME,
#                `7-Day Max Moving Average` = rollmax_7)

# (2) Date Lookup Table ----
fulldays <- readxl::read_xlsx("./data/calendar-dates.xlsx",
                              sheet = "calendar-dates") %>% 
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

# (3) Map: shapefiles ----
lakes.resolvable <- sf::st_read(dsn = "./data/CyAN_Waterbodies.shp",
                                layer = "CyAN_Waterbodies") %>% 
  sf::st_zm() %>% 
  st_transform(crs = 4326) %>% 
  dplyr::filter(GNISIDNAME %in% dta1$inApp) # filter out saline lakes

state.boundary <- sf::st_read("./data/state_boundary_blm.shp") %>% 
  st_transform(crs = 4326)

huc6 <- sf::st_read(dsn = "./data/WBD_HU6.shp",layer = "WBD_HU6")%>% 
  st_transform(crs = 4326)

pal.huc6 <- leaflet::colorFactor(palette = "Paired", domain = unique(sort(huc6$HU_6_NAME)))

# (4) Map: raster ----
# Raster color 
thevalues <- c(0,6310,20000,100000,7000000)
#paletteFunc <- grDevices::colorRampPalette(c('#bdbdbd','#66c2a4','#2ca25f','#006d2c'))
#palette     <- paletteFunc(4)
palette <- c('#bdbdbd','#66c2a4','#2ca25f','#006d2c')

pal.map <- leaflet::colorBin(palette = palette,
                             bins = c(0,6310,20000,100000,7000000),
                             domain = c(0,6310,20000,100000,7000000),
                             na.color = "transparent")

# Legend labels
labels = c("Non-detect","Low: 6,311 - 20,000","Moderate: 20,000 - 100,000","High: >100,000")
# ----
#rm(dta1); rm(dta2); rm(dta3)
save.image(file = "data.RData")
