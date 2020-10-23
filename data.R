library(tidyverse)
library(lubridate)
library(readxl)
# library(sp); library(sf)
library(rgdal) 
library(raster)

setwd("//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/HAB_Shiny_app")

# (1) Plot and Table ----
# Note: Date @ R was used to deploy the app at shinyapps.io; however, this app is too large to use in a free shinyapps.io site.

# _ Data @ GIS ----
dta1 <- readxl::read_xlsx("//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/GIS/cyan/tables/HAB_resolvablelakes_2016_to_2019.xlsx",
                          sheet = "HAB_resolvablelakes_2016_2020") %>% 
  dplyr::mutate(wi_DWSA = NA)

dta2 <- readxl::read_xlsx("//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/GIS/cyan/tables/HAB_resolvablelakes_toAug262020.xlsx",
                          sheet = "HAB_resolvable_toAug262020") %>% 
  dplyr::select(-"...13")

dta3 <- readxl::read_xlsx("//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/GIS/cyan/tables/HAB_resolvablelakes_toAug262020.xlsx",
                          sheet = "NHDWaterbody_resolvable_inDWSA")

# _ Date @ R ----
#dta1 <- readxl::read_xlsx("./data/HAB_resolvablelakes_2016_to_2019.xlsx",
#                          sheet = "HAB_resolvablelakes_2016_2020") %>% 
#  dplyr::mutate(wi_DWSA = NA)

#dta2 <- readxl::read_xlsx("./data/HAB_resolvablelakes_toAug262020.xlsx",
#                         sheet = "HAB_resolvable_toAug262020") %>% 
#  dplyr::select(-"...13")

#dta3 <- readxl::read_xlsx("./data/HAB_resolvablelakes_toAug262020.xlsx",
#                          sheet = "NHDWaterbody_resolvable_inDWSA")

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
# _ Date @ GIS ----
# lakes <- rgdal::readOGR(dsn = "//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/GIS/cyan/NHDwaterbody_resolvable_lakes.shp",
#                        layer = "NHDwaterbody_resolvable_lakes")

# _ Date @ R ----
lakes <- rgdal::readOGR(dsn = "./data/NHDwaterbody_resolvable_lakes.shp",
                        layer = "NHDwaterbody_resolvable_lakes")

# head(lakes) or head(lakes@data)
# summary(lakes)

# lakes <- sp::spTransform(lakes,CRS("+proj=longlat +datum=WGS84"))

# lakes <- sf::st_read(dsn = "//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/GIS/cyan/NHDwaterbody_resolvable_lakes.shp",
#                      layer = "NHDwaterbody_resolvable_lakes") %>% 
#         sf::st_transform(crs = "+proj=longlat +datum=WGS84")

# (4) Map: raster ----
# Codes kept here for note only. The raster file is called at the app.R.
# check if the r object is loaded into memory, run:
# raster::inMemory(r)
# if FALSE, to force the raster into memory use
## r <- raster::readAll(raster("//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/GIS/cyan/2020/2020190_EPSG3857.tif"))
# r
# raster::crs(r)
## crs(r) <- CRS("+init=epsg:3857")

date <- max(dta$Date)

map.day <- lookup.date %>% 
  dplyr::filter(Date == date) %>% 
  dplyr::mutate(map_day = paste0(Year.dta,Day.dta))

tif.dir <- paste0("//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/GIS/cyan/",map.day$Year.dta,"/mosaic/")
# tif.dir <- "//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/HAB_Shiny_app/data/tif/"
# tif.dir <- "./data/tif/"
file.name.1 <- paste0(map.day$map_day,".tif")
#file.name.1 <- paste0(map.day$map_day)

#r <- raster::readAll(raster(paste0(tif.dir,file.name.1)))
r <- raster::raster(paste0(tif.dir,file.name.1))
#crs(r) <- sp::CRS("+init=epsg:9822")

pal.map <- colorNumeric(palette = c("#feb24c","#66c2a4","#67001f"), values(r), na.color = "transparent")

# ----

save.image(file = "data.RData")

