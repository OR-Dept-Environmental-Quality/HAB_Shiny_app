library(tidyverse)
library(lubridate)
library(readxl)
library(rgdal) 
library(sf)
library(raster)
library(leaflet)
library(RColorBrewer)
library(rasterVis)

# (1) Plot and Table ----
dta1 <- readxl::read_xlsx("./data/HAB_resolvablelakes_2016_2020.xlsx",
                          sheet = "HAB_resolvablelakes_2016_2020")

dta2 <- readxl::read_xlsx("./data/HAB_resolvablelakes_2021.xlsx",
                          sheet = "HAB_resolvable_toMar182021")

dta3 <- readxl::read_xlsx("./data/Resolvable_Lakes.xlsx",
                          sheet = "cyan_resolvable_lakes")

GNISNameID <- unique(sort(dta3$wi_DWSA))

dta <- rbind(dta1,dta2) %>% 
  dplyr::rename(Mean = MEAN_cellsml,
                Maximum = MAX_cellsml,
                Minimum = MIN_cellsml) %>% 
  tidyr::gather(`Summary Statistics`, `Cyanobacteria (cells/mL)`, -GNISIDNAME,-COUNT,-AREA,-PercentArea_Value, -Day,-Year,-Date) %>% 
  tidyr::separate(GNISIDNAME,c("GNISNAME","GNISID"), sep="_") %>% 
  dplyr::mutate(GNISIDNAME = paste0(GNISNAME,"_",GNISID)) %>% 
  dplyr::mutate(Date = lubridate::ymd(Date)) %>% 
  dplyr::arrange(desc(Date)) %>% 
  dplyr::mutate(wi_DWSA = ifelse(GNISIDNAME %in% GNISNameID, "Yes", "No")) %>% 
  dplyr::filter(!GNISIDNAME == "Goose Lake_01520146") # located in the WA state

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

# (3) Map: shapefile ----
lakes.resolvable <- rgdal::readOGR(dsn = "./data/NHDwaterbody_resolvable_lakes_dissolved_oregon_clean.shp",
                                   layer = "NHDwaterbody_resolvable_lakes_dissolved_oregon_clean")

state.boundary <- sf::st_read("./data/state_boundary_blm.shp") %>% 
  st_transform(crs="+init=epsg:4326")

# (4) Map: raster ----
# Raster color 
thevalues <-c(0,6310,13000,25000,50000,100000,200000,400000,800000,1000000,3000000,6000000,7000000)
paletteFunc <- grDevices::colorRampPalette(c('gray','yellow','orange','dark red'))
palette     <- paletteFunc(12)

pal.map <- leaflet::colorBin(palette = palette,
                             bins = c(0,6310,13000,25000,50000,100000,200000,400000,800000,1000000,3000000,6000000,7000000),
                             domain = c(0,6310,13000,25000,50000,100000,200000,400000,800000,1000000,3000000,6000000,7000000),
                             na.color = "transparent")
# Legend labels
labels = c("Non-detect","6,311 - 13,000","13,000 - 25,000","25,000 - 50,000","50,000 - 100,000","100,000 - 200,000","200,000 - 400,000","400,000 - 800,000","800,000 - 1,000,000","1,000,000 - 3,000,000","3,000,000 - 6,000,000","> 6,000,000")
# ----
rm(dta1); rm(dta2); rm(dta3)
save.image(file = "data.RData")
