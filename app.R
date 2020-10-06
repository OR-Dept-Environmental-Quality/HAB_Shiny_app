library(tidyverse)
library(lubridate)
library(plotly);library(ggplot2)
library(shiny); library(shinythemes); library(shinyWidgets)
library(leaflet); library(leaflet.extras)
library(DT)
library(viridis)
library(scales)
library(rgdal); library(raster); library(sp)

load("data.RData")

rm(ui); rm(server)

## app.R ----
shinyApp(
  
  ui <- fluidPage(
    
    theme = shinythemes::shinytheme("flatly"),
    shiny::navbarPage("HAB Data Visualizer",
                      
                      # _ Map ----
                      wellPanel(
                        
                        tags$style(type = "text/css", "#map {height: calc(80vh - 80px) !important;}"),
                        leaflet::leafletOutput("map", width="100%",height="100%")
                      ),
                      
                      wellPanel(
                        sidebarLayout(
                          sidebarPanel(
                            
                            # _ Lakes ----
                            shinyWidgets::pickerInput(inputId = "lakes",
                                                      label = "Select a Waterbody:",
                                                      choices = list(
                                                        "Within Drinking Water Source Area" = 
                                                          unique(sort(dta[which(dta$wi_DWSA == c("Yes")),]$GNISIDNAME)),
                                                        "Not-Within Drinking Water Source Area" = 
                                                          unique(sort(dta[which(dta$wi_DWSA == c("No")),]$GNISIDNAME))
                                                      ),
                                                      # selected = "Alkali Lake_01116863",
                                                      multiple = FALSE),
                            
                            # _ Dates ----
                            shiny::dateInput(inputId = "date_map",
                                             label = "Select a date to display cyano status on the Map:",
                                             value = max(dta$Date),
                                             min = min(dta$Date),
                                             max = max(dta$Date),
                                             format = "yyyy-mm-dd",
                                             startview = "year",
                                             weekstart = 0,
                                             datesdisabled = missing.dates$Date),
                            
                            
                            shiny::dateRangeInput(inputId = "date_plot",
                                                  label = "Select a date range to plot time series:",
                                                  start = min(dta$Date),
                                                  end = max(dta$Date),
                                                  min = min(dta$Date),
                                                  max = max(dta$Date),
                                                  separator = "to",
                                                  format = "yyyy-mm-dd",
                                                  startview = "year",
                                                  weekstart = 0),
                            
                            # _ Summary Statistics ----
                            checkboxGroupInput(inputId = "matrix",
                                               label = "Summary Statistics:",
                                               choices = c("Maximum" = "Maximum",
                                                           "Mean" = "Mean",
                                                           "Minimum" = "Minimum"),
                                               selected = "Mean"),
                            
                            # _ Plot types ----
                            checkboxGroupInput(inputId = "plot_log",
                                               label = "See Log Scale:",
                                               choices = c("Log Scale" = "log"))
                          ),
                          
                          mainPanel(
                            
                            plotlyOutput("plot")
                            
                          )
                        )
                      ),
                      
                      wellPanel(
                        
                        DT::dataTableOutput("table")
                      )
    )
    
  ),
  
  server <- function(input, output, session) {
    
    # Map ----
    
    # _map 1st ----
    dta.test <- dta %>% 
      dplyr::filter(Date %in%  c(as.Date("2020-09-20"),as.Date("2020-09-19"),as.Date("2020-07-08")))
    date <- max(dta.test$Date)
    
    map.day <- lookup.date %>% 
      dplyr::filter(Date %in%  date) %>% 
      dplyr::mutate(map_day = paste0(Year.dta,Day.dta))
    
    tif.dir <- "//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/GIS/cyan/2020/"
    file.name.1 <- paste0(map.day$map_day,"_EPSG3857.tif")
    
    #r <- raster::readAll(raster(paste0(tif.dir,file.name.1)))
    r <- raster::raster(paste0(tif.dir,file.name.1))
    # crs(r) <- sp::CRS("+init=epsg:3857")
    
    pal.map <- colorNumeric(palette = c("#feb24c","#66c2a4","#67001f"), values(r), na.color = "transparent")
    
    #m <- leaflet() %>%
    #  addTiles() %>%
    #  addRasterImage(r, colors=pal, opacity = 0.9, maxBytes = 123123123) %>%
    #  addLegend(pal = pal, values = values(r), title = "Test")
    
    
    output$map <- leaflet::renderLeaflet({
      
      leaflet::leaflet() %>% 
        leaflet::addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
        #leaflet::addTiles() %>% 
        leaflet::setView(lng = -122.1739, lat = 44.0582, zoom=7) %>%
        leaflet::addRasterImage(r, colors=pal.map, opacity = 1) %>% 
        leaflet::addLegend(pal = pal.map, values = values(r),title = "Cyanobacteria (cells/mL)") %>% 
        leaflet::addPolygons(data = study_lakes,
                             color = "blue",
                             weight = 2,
                             layer = ~study_lakes$Name,
                             smoothFactor = 0.5,
                             opacity = 0.5,
                             fillColor = "transparent",
                             fillOpacity = 1.0,
                             #highlightOptions = highlightOptions(color = "",
                             #                                   weight = 2,
                             #                                   bringToFront = FALSE),
                             label = ~study_lakes$Name) %>% 
        leaflet.extras::addResetMapButton() %>% 
        leaflet::addMiniMap(position = "bottomleft",
                            width = 300,
                            height = 300,
                            zoomLevelFixed = 5)
      
    })
    
    # _map reactive ----
    
    df.map.date <- reactive({
      
      lookup.date %>% 
        dplyr::filter(Date == input$date_map) %>% 
        dplyr::mutate(map_day = paste0(Year.dta,Day.dta))
      
    })
    
    # tif.dir <- "//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/GIS/cyan/2020/"
    
    file.name.2 <- reactive(paste0(df.map.date()$map_day,"_EPSG3857.tif"))
    
    rst <- reactive({
      
      raster::readAll(raster(paste0(tif.dir,file.name.2())))
      
    })
    
    # crs(rst) <- CRS("+init=epsg:3857")
    
    observe({
      
      leafletProxy(mapId = "map", 
                   data = rst())
      
      
    })                                
    
    # _observe click on the map ----
    observe({
      
      click <- input$map_click
      
      # coords <- input$map_bounds
      
      #if(is.null(click))
      #  return()
      #else
      #  leafletProxy("map") %>% 
      # fitBounds(coords$west,
      #          coords$south,
      #         coords$east,
      #        coords$north)
      
      if(is.null(click))
        return()
      else
        leafletProxy("map") %>% 
        setView(
          lng = click$lng,
          lat = click$lat,
          zoom = 12)
    })
    
    # Chart ----
    pal.plot <- c("orange","blue","green","white","white","white")
    pal.plot <- setNames(pal.plot,unique(sort(dta$`Summary Statistics`)))
    
    df <- reactive({
      
      dta %>% 
        dplyr::filter(GNISIDNAME %in% input$lakes) %>% 
        dplyr::filter(`Summary Statistics` %in% input$matrix) %>% 
        dplyr::filter(Date >= input$date_plot[1],Date <= input$date_plot[2])
      
    })
    
    type <- reactive({
      
      input$plot_log
      
    })
    
    yaxis <- reactive({
      
      if_else(length(input$plot_log)>0,
              "Cyanobacteria (cells/mL)",
              "Cyanobacteria (cells/mL)")
      
    })
    
    
    output$plot <- renderPlotly({
      
      plotly::plot_ly(
        data = df(),
        x = ~as.Date(Date),
        y = ~`Cyanobacteria (cells/mL)`,
        split = ~`Summary Statistics`,
        type = "scatter",
        mode = "lines",
        color = ~`Summary Statistics`,
        colors = pal.plot) %>% 
        plotly::layout(xaxis = list(title = "Date", range = c(min(df()$Date),max(df()$Date))),
                       # yaxis = list(title = "Cyanobacteria (cells/mL)"),
                       title = as.character(unique(df()$GNISIDNAME))
        ) %>% 
        plotly::layout(yaxis = list(type = type(),
                                    title = yaxis()))
      
    })
    
    
    # Data table ----
    
    df_tbl <- reactive({
      
      df() %>% 
        dplyr::select(GNISIDNAME,Date,`Cyanobacteria (cells/mL)`,`Summary Statistics`) %>% 
        dplyr::mutate(`Cyanobacteria (cells/mL)` = ifelse(as.character(`Cyanobacteria (cells/mL)`) == "6310", "Not Detected",
                                                          scales::comma(`Cyanobacteria (cells/mL)`))) %>% 
        dplyr::mutate(`Summary Statistics` = ifelse(as.character(`Cyanobacteria (cells/mL)`) == "Not Detected", "NA",
                                                    `Summary Statistics`))
    })
    
    output$table <- DT::renderDataTable({
      
      DT::datatable(
        data = df_tbl(),
        style = 'bootstrap',
        extensions = 'Buttons',
        options = list(dom = 'Bfrtilp',
                       pageLength = 10,
                       compact = TRUE,
                       nowrap = TRUE,
                       scorllX = TRUE,
                       buttons = list('print',
                                      list(extend = 'collection',
                                           buttons = c('csv','excel','pdf'),
                                           text = 'Download')
                       )),
        rownames = FALSE,
        filter = 'bottom'
      ) #%>% 
      #DT::formatDate("Date","toLocaleString")
    }, server = FALSE)
    
  }
)
