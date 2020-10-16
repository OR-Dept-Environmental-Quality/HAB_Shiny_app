library(tidyverse)
library(lubridate)
library(readxl)
# library(sp); library(sf)
library(rgdal) 
library(raster)

setwd("//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/HAB_Shiny_app")

# Plot and Table ----
dta1 <- readxl::read_xlsx("//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/GIS/cyan/tables/HAB_resolvablelakes_2016_to_2019.xlsx",
                          sheet = "HAB_resolvablelakes_2016_2020") %>% 
  dplyr::mutate(wi_DWSA = NA)

dta2 <- readxl::read_xlsx("//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/GIS/cyan/tables/HAB_resolvablelakes_toAug262020.xlsx",
                          sheet = "HAB_resolvable_toAug262020") %>% 
  dplyr::select(-"...13")

dta3 <- readxl::read_xlsx("//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/GIS/cyan/tables/HAB_resolvablelakes_toAug262020.xlsx",
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
  dplyr::mutate(wi_DWSA = ifelse(GNISIDNAME %in% GNISNameID, "Yes", "No"))

# Map: shapefile ----
lakes <- rgdal::readOGR(dsn = "//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/GIS/cyan/NHDwaterbody_resolvable_lakes.shp",
                        layer = "NHDwaterbody_resolvable_lakes")

# head(lakes) or head(lakes@data)
# summary(lakes)

# lakes <- sp::spTransform(lakes,CRS("+proj=longlat +datum=WGS84"))

# lakes <- sf::st_read(dsn = "//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/GIS/cyan/NHDwaterbody_resolvable_lakes.shp",
#                      layer = "NHDwaterbody_resolvable_lakes") %>% 
#         sf::st_transform(crs = "+proj=longlat +datum=WGS84")

# Map: raster ----

# check if the r object is loaded into memory, run:
# raster::inMemory(r)
# if FALSE, to force the raster into memory use
## r <- raster::readAll(raster("//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/GIS/cyan/2020/2020190_EPSG3857.tif"))
# r
# raster::crs(r)
## crs(r) <- CRS("+init=epsg:3857")

# Date Lookup Table ----
fulldays <- readxl::read_xlsx("//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/HAB_Shiny_app/2016-2020.xlsx",
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

# ----

save.image(file = "data.RData")

