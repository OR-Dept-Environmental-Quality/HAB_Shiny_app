library(tidyverse)
library(lubridate)
library(plotly);library(ggplot2)
library(shiny); library(shinythemes); library(shinyWidgets)
library(leaflet); library(leaflet.extras)
library(DT)
library(viridis)
library(scales)
library(rgdal); library(raster); 
library(sp);library(sf)

load("data.RData")

rm(ui); rm(server)

## app.R ----
shinyApp(
  
  ui <- fluidPage(
    
    theme = shinythemes::shinytheme("flatly"),
    shiny::navbarPage("Brian is Awesome! :-)",
                      
                      wellPanel(
                        # Waterbody ----
                        shinyWidgets::pickerInput(inputId = "waterbody",
                                                  label = "Select a Waterbody:",
                                                  choices = list(
                                                    "Within Drinking Water Source Area" = 
                                                      unique(sort(dta[which(dta$wi_DWSA == c("Yes")),]$GNISIDNAME)),
                                                    "Not-Within Drinking Water Source Area" = 
                                                      unique(sort(dta[which(dta$wi_DWSA == c("No")),]$GNISIDNAME))
                                                  ),
                                                  # selected = "Alkali Lake_01116863",
                                                  multiple = FALSE)
                        
                      ),
                      
                      # Map ----
                      wellPanel(
                        
                        tags$style(type = "text/css", "#map {height: calc(80vh - 80px) !important;}"),
                        leaflet::leafletOutput("map", width="100%",height="100%"),
                        
                        
                        # _ Dates ----
                        shiny::sliderInput(inputId = "date_map",
                                           label = "Select a Day to Display on the Map:",
                                           min = min(dta$Date),
                                           max = max(dta$Date),
                                           value = max(dta$Date))
                        
                      ),
                      
                      # Plot ----
                      wellPanel(
                        
                        sidebarLayout(
                          sidebarPanel(
                            
                            # _ Date range ----
                            shiny::dateRangeInput(inputId = "date_plot",
                                                  label = "Select Date Range to Plot:",
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
                      
                      # Table ----
                      wellPanel(
                        
                        DT::dataTableOutput("table")
                        
                      )           
    )
  ),
  
  server <- function(input, output, session) {
    
    # Map ----
    lake.GNISID <- reactive({
      
      unique(dta[which(dta$GNISIDNAME %in% input$waterbody),]$GNISID)
      
    })
    
    one.lake <- reactive({
      
      lakes[which(lakes@data$GNIS_ID == lake.GNISID()),]
      
    })
    
    bounds <- reactive({
      
      data.frame(bbox(one.lake()))
      
    })
    
    # _ map reactive @ waterbody picker ----
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
    
    output$map <- leaflet::renderLeaflet({
      
      leaflet::leaflet() %>% 
        leaflet::addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
        leaflet::fitBounds(lng1=bounds()$min[1], lat1=bounds()$min[2], lng2=bounds()$max[1], lat2=bounds()$max[2]) %>%
        leaflet::addRasterImage(r, colors=pal.map, opacity = 1) %>% 
        leaflet::addLegend(pal = pal.map, values = values(r),title = "Cyanobacteria (cells/mL)") %>% 
        leaflet.extras::addResetMapButton() %>% 
        leaflet::addMiniMap(position = "bottomleft",
                            width = 300,
                            height = 300,
                            zoomLevelFixed = 5) %>% 
        leaflet::addPolygons(data = lakes, 
                             color = "blue",
                             weight = 2,
                             layer = ~lakes$GNIS_Name,
                             smoothFactor = 0.5,
                             opacity = 0.5,
                             fillColor = "transparent",
                             fillOpacity = 1.0,
                             label = ~lakes$GNIS_Name)
      
    })
    
    # _ map reactive @ date slider ----
    observe({
      
      if(missing.dates$Date <- input$date_map) {
        
        return()
        
      } else {
        
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
        
        leafletProxy(mapId = "map", 
                     data = rst())
        
      }
      
    })
    
    
    # _ click on the map ---- 
    observe({
      
      click <- input$map_click
      
      if(is.null(click)){
        
        return()
        
      } else {
        
        leafletProxy("map") %>% 
          setView(
            lng = click$lng,
            lat = click$lat,
            zoom = 12)
        
      }
      
    })
    
    
    # Plot ----
    pal.plot <- c("orange","blue","green","white","white","white")
    pal.plot <- setNames(pal.plot,unique(sort(dta$`Summary Statistics`)))
    
    df <- reactive({
      
      dta %>% 
        dplyr::filter(GNISIDNAME %in% input$waterbody) %>% 
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
