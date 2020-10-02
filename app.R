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
                        
                        tags$style(type = "text/css", "#map_shape {height: calc(80vh - 80px) !important;}"),
                        leaflet::leafletOutput("map_shape", width="100%",height="100%")
                      ),
                      
                      wellPanel(
                        
                        shiny::sliderInput(inputId = "day",
                                           label = "Select a Day to Display on the Map:",
                                           min = min(dta$Date),
                                           max = max(dta$Date),
                                           value = max(dta$Date))
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
                            
                            # _ Date ----
                            
                            # shiny::sliderInput(inputId = "day_1",
                            #                   label = "Select a Day to Display on the Map:",
                            #                   min = min(dta$Date),
                            #                   max = max(dta$Date),
                            #                   value = max(dta$Date)),
                            
                            shiny::dateRangeInput(inputId = "date_plot",
                                                  label = "Select Date Range to plot:",
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
    
    # map ----
    output$map_shape <- leaflet::renderLeaflet({
      
      leaflet::leaflet() %>% 
        leaflet::addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
        leaflet::setView(lng = -122.1739, lat = 44.0582, zoom=7) %>%
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
        leaflet::addRasterImage(r) %>%
        leaflet.extras::addResetMapButton() %>% 
        leaflet::addMiniMap(position = "bottomleft",
                            width = 300,
                            height = 300,
                            zoomLevelFixed = 5)
    })
    
    # Observe click on shapes
    observe({
      
      click <- input$map_shape_click
      
      # coords <- input$map_shape_bounds
      
      #if(is.null(click))
      #  return()
      #else
      #  leafletProxy("map_shape") %>% 
       # fitBounds(coords$west,
        #          coords$south,
         #         coords$east,
          #        coords$north)
      
      if(is.null(click))
        return()
      else
        leafletProxy("map_shape") %>% 
        setView(
          lng = click$lng,
          lat = click$lat,
          zoom = 12)
    })
    
    # chart ----
    pal <- c("orange","blue","green","white","white","white")
    pal <- setNames(pal,unique(sort(dta$`Summary Statistics`)))
    
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
        colors = pal) %>% 
        plotly::layout(xaxis = list(title = "Date", range = c(min(df()$Date),max(df()$Date))),
                       # yaxis = list(title = "Cyanobacteria (cells/mL)"),
                       title = as.character(unique(df()$GNISIDNAME))
        ) %>% 
        plotly::layout(yaxis = list(type = type(),
                                    title = yaxis()))
      
    })
    
    
    
    # data table ----
    
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
