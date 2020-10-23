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
                        
                        sidebarLayout(
                          
                          sidebarPanel(width = 3,
                                       
                                       # (1) Waterbody ----
                                       shinyWidgets::pickerInput(inputId = "waterbody",
                                                                 label = "Select a Waterbody:",
                                                                 choices = list(
                                                                   "Oregon",
                                                                   "Within Drinking Water Source Area" = 
                                                                     unique(sort(dta[which(dta$wi_DWSA == c("Yes")),]$GNISIDNAME)),
                                                                   "Not-Within Drinking Water Source Area" = 
                                                                     unique(sort(dta[which(dta$wi_DWSA == c("No")),]$GNISIDNAME))
                                                                 ),
                                                                 # selected = "Alkali Lake_01116863",
                                                                 multiple = FALSE)
                                       
                                       
                          ),
                          
                          
                          mainPanel(width = 9,
                                    
                                    # (2) Map ----
                                    wellPanel(
                                      tags$style(type = "text/css", "#map {height: calc(80vh - 80px) !important;}"),
                                      leaflet::leafletOutput("map", width="100%",height="100%")),
                                    
                                    
                                    # _ Dates ----
                                    wellPanel(
                                      shiny::sliderInput(inputId = "date_map",
                                                         label = "Select a Day to Display on the Map:",
                                                         min = min(dta$Date),
                                                         max = max(dta$Date),
                                                         value = max(dta$Date)),
                                      
                                      tableOutput("values"))
                                    
                          )
                          
                        )
                      ),
                      
                      # (3) Plot ----
                      wellPanel(
                        
                        sidebarLayout(
                          sidebarPanel(width = 3,
                                       
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
                          
                          mainPanel(width = 9,
                                    
                                    plotlyOutput("plot")
                                    
                          )
                        )
                      ),
                      
                      # (4) Table ----
                      wellPanel(
                        
                        DT::dataTableOutput("table")
                        
                      )           
    )
  ),
  
  server <- function(input, output, session) {
    
    # (1) Map ----
    # _ initial map ----
    output$map <- leaflet::renderLeaflet({
      
      leaflet::leaflet() %>% 
        leaflet::addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
        #leaflet::fitBounds(lng1=-125, lat1=47, lng2=116, lat2=42) %>%
        leaflet::setView(lng = -121, lat = 44, zoom=7) %>%
        leaflet::addRasterImage(r, colors=pal.map, opacity = 1) %>% 
        leaflet::addLegend(pal = pal.map, values = values(r),title = "Cyanobacteria (cells/mL)") %>% 
        leaflet.extras::addResetMapButton() %>% 
        leaflet::addMiniMap(position = "bottomleft",
                            width = 200,
                            height = 200,
                            zoomLevelFixed = 5) %>% 
        leaflet::addPolygons(data = lakes, 
                             color = "red",
                             weight = 2,
                             layer = ~lakes$GNIS_Name,
                             smoothFactor = 0.5,
                             opacity = 0.5,
                             fillColor = "transparent",
                             fillOpacity = 1.0,
                             label = ~lakes$GNIS_Name)
      
    })
    
    
    
    # _ map reactive @ waterbody picker ----
    
    observeEvent(input$waterbody,{
      
      if(input$waterbody == c("Oregon")) {
        
        leafletProxy("map") %>% 
          leaflet::setView(lng = -121, lat = 44, zoom=7)
        
      } else {
        
        one.lake <- reactive({
          
          lakes[which(lakes@data$GNISIDNAME == input$waterbody),]
          
        })
        
        bounds <- reactive({
          
          data.frame(bbox(one.lake()))
          
        })
        
        leafletProxy("map") %>% 
          leaflet::fitBounds(lng1=bounds()$min[1], lat1=bounds()$min[2], lng2=bounds()$max[1], lat2=bounds()$max[2])
      }
      
    })
    
    
    # _ map reactive @ date slider ----
    
    observeEvent(input$date_map,{
      
      #if(missing.dates$Date %in% input$date_map) {
      
      #  return()
      
      #} else {
      
      df.map.date <- reactive({
        
        lookup.date %>% 
          dplyr::filter(Date %in% input$date_map) %>% 
          dplyr::mutate(map_day = paste0(Year.dta,Day.dta))
        
      })
      
      map.tif.dir <- reactive(paste0("//deqhq1/WQ-Share/Harmful Algal Blooms Coordination Team/GIS/cyan/",df.map.date()$Year.dta,"/mosaic/"))
      
      file.name.2 <- reactive(paste0(df.map.date()$map_day,".tif"))
      
      rst <- reactive({
        
        raster::raster(paste0(map.tif.dir(),file.name.2()))
        
      })
      
      # crs(rst) <- CRS("+init=epsg:3857")
      
      leafletProxy("map") %>% 
        clearImages() %>% 
        leaflet::addRasterImage(rst(), colors=pal.map, opacity = 1)
      
      
    })
    
    #}
    
    sliderValues <- reactive({
      
      data.frame(
        Name = c("input$date_map"),
        Value = as.character(c(input$date_map)),
        stringsAsFactors = FALSE)
      
    })
    
    output$values <- renderTable({
      sliderValues()
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
    
    
    # (2) Plot ----
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
    
    # (3) Data table ----
    
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
