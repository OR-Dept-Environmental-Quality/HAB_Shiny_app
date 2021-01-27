load("data.RData")

df.map.date <- lookup.date %>% 
    dplyr::filter(Date %in% as.Date("2020-09-29")) %>% 
    dplyr::mutate(map_day = paste0("2020","273"))

map.tif.dir <- paste0("data/", df.map.date$Year.dta, "/")
file.name <- paste0(df.map.date$map_day,".tif")

rst <- raster::raster(paste0(map.tif.dir,file.name))

leaflet::leaflet() %>% 
  leaflet::addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
  leaflet::setView(lng = -121, lat = 44, zoom=7) %>%
  leaflet.extras::addResetMapButton() %>% 
  leaflet::addRasterImage(rst, project = FALSE, colors=pal.map, opacity = 1)
