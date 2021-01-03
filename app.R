library(tidyverse)
library(shiny);library(shinyWidgets);library(shinythemes)
library(shinydashboard);library(shinydashboardPlus);library(shinyjs)
library(raster);library(sp)
library(leaflet);library(leaflet.extras)
library(scales)
library(plotly)
library(DT)

load("data.RData")

# Shiny App ----

shinyApp(
  
  ui = dashboardPage(
    
    dashboardHeader(disable = TRUE),
    
    # _ Siderbar ----
    dashboardSidebar(collapsed = TRUE,
                     width = 500),
    
    # _ Body ----
    dashboardBody(
      
      tags$head(tags$style(HTML('
                                /* logo */
                                .skin-blue .main-header .logo {
                                background-color: #23769a;
                                }

                                /* logo when hovered */
                                .skin-blue .main-header .logo:hover {
                                background-color: #23769a;
                                }

                                /* navbar (rest of the header) */
                                .skin-blue .main-header .navbar {
                                background-color: #23769a;
                                }

                                /* main sidebar */
                                .skin-blue .main-sidebar {
                                background-color: #23769a;
                                }
                                
                                .sidebar {
                                color: #23769a;
                                position: fixed;
                                width: 1px;
                                white-space: nowrap;
                                overflow: visible;
                                background-color: #23769a;
                                }

                                /* active selected tab in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                                background-color: white;
                                }

                                /* other links in the sidebarmenu */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                                background-color: white;
                                color: #000000;
                                }

                                /* other links in the sidebarmenu when hovered */
                                .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                                background-color: white;
                                }
                                /* toggle button when hovered  */
                                .skin-blue .main-header .navbar .sidebar-toggle:hover{
                                background-color: white;
                                }

                                /* body */
                                .content-wrapper, .right-side {
                                background-color: white;
                                }
                                '))),
      
      fluidRow(
        
        tags$head(tags$style(HTML('.box{-webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;}'))),
        
        # __ 1. Header ----
        shinydashboard::box(
          width = 12,
          solidHeader = TRUE,
          
          #tags$script(HTML("$('.box').eq(0).css('border', '0px solid white');")),
          #shinyjs::useShinyjs(),
          #div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
          tags$img(src = "DEQ-logo-color-horizontal370x73.png"),
          tags$div(span("Oregon CyAN Image of Cyanobacteria Abundance",
                        #style = "color: black; font-size: 50px; margin-left: 20px")),
                        style = "color: black; font-size: 50px")),
          tags$br(),
          shinyjs::useShinyjs(),
          #div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
          div(style="display: inline-block;vertical-align:top; height:20px; width: 300px;",h4("Show Introduction and User Guide:")),
          div(style="display: inline-block;vertical-align:top; height:50px; width: 150px;",shinyWidgets::switchInput(inputId = "sidebarSwitch",size = "small"))
        ),
        
        # __ 2. Map ----
        shinydashboardPlus::box(
          title = "Map",
          status = "primary",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          dropdownMenu = boxDropdown(),
          
          shinydashboard::box(width = 2,
                              height = "700px",
                              solidHeader = TRUE,
                              
                              # ___ (.1) Waterbody ----
                              shinyWidgets::pickerInput(inputId = "waterbody",
                                                        label = tags$h4("Select a Waterbody:"),
                                                        choices = list(
                                                          "Oregon",
                                                          "Within Drinking Water Source Area" = 
                                                            unique(sort(dta[which(dta$wi_DWSA == c("Yes")),]$GNISIDNAME)),
                                                          "Not-Within Drinking Water Source Area" = 
                                                            unique(sort(dta[which(dta$wi_DWSA == c("No")),]$GNISIDNAME))
                                                        ),
                                                        # selected = "Alkali Lake_01116863",
                                                        multiple = FALSE),
                              
                              # ___ (.2) Dates ----
                              tags$style(HTML(".datepicker {z-index:99999 !important;}")),
                              
                              shiny::dateInput(inputId = "date_map",
                                               label = tags$h4("Select a Date:"),
                                               value = max(dta$Date),
                                               min = min(dta$Date),
                                               max = max(dta$Date),
                                               format = "yyyy-mm-dd",
                                               startview = "month",
                                               weekstart = 0,
                                               datesdisabled = missing.dates$Date),
                              
                              # tableOutput("values"),
                              
                              # ___ (.3) Boxplot ----
                              
                              HTML(paste(
                                #tags$br(),
                                tags$h4("Cyanobacteria abundance of the selected waterbody on the selected date\n(cell/mL):")
                              )),
                              
                              plotlyOutput("boxplot")
                              
          ),
          
          # ___ (.4) Map ----
          shinydashboard::box(width = 10,
                              height = "700px",
                              solidHeader = TRUE,
                              
                              # tags$style(type = "text/css", "#map {height: calc(80vh - 80px) !important;}"),
                              leaflet::leafletOutput("map", height = "680px"))
          
        ),
        
        # __ 3. Plot ----
        shinydashboardPlus::box(
          title = "Plot",
          status = "warning",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          dropdownMenu = boxDropdown(),
          
          shinydashboard::box(
            width = 2,
            height = "400px",
            solidHeader = TRUE,
            
            # ___ (.1) Date range ----
            shiny::dateRangeInput(inputId = "date_plot",
                                  label = tags$h4("Select Date Range to Plot:"),
                                  start = min(dta$Date),
                                  end = max(dta$Date),
                                  min = min(dta$Date),
                                  max = max(dta$Date),
                                  separator = "to",
                                  format = "yyyy-mm-dd",
                                  startview = "year",
                                  weekstart = 0),
            
            # ___ (.2) Summary Statistics ----
            checkboxGroupInput(inputId = "matrix",
                               label = tags$h4("Summary Statistics:"),
                               choices = c("Maximum" = "Maximum",
                                           "Mean" = "Mean",
                                           "Minimum" = "Minimum"),
                               selected = "Mean"),
            
            # ___ (.3) Plot types ----
            checkboxGroupInput(inputId = "plot_log",
                               label = tags$h4("See Log Scale:"),
                               choices = c("Log Scale" = "log"))
            
          ),
          
          # ___ (.4) Plot ----
          shinydashboard::box(
            width = 10,
            height = "400px",
            solidHeader = TRUE,
            
            plotlyOutput("plot")
            
          )
          
        ),
        
        # __ 4. Table ----
        shinydashboardPlus::box(
          title = "Table",
          status = "success",
          solidHeader = TRUE,
          width = 12,
          collapsible = TRUE,
          collapsed = FALSE,
          dropdownMenu = boxDropdown(),
          
          DT::dataTableOutput("table")
          
        )
      )
    )
  ),
  
  server = function(input, output, session) {
    
    # Sidebar switch ----
    observeEvent(input$sidebarSwitch,{
      
      if(input$sidebarSwitch==TRUE) {
        shinyjs::removeClass(selector = "body", class = "sidebar-collapse")
      } else {
        shinyjs::addClass(selector = "body", class = "sidebar-collapse")
      }
      
    })
    
    
    
    # (1) Map ----
    # _ initial map ----
    output$map <- leaflet::renderLeaflet({
      
      leaflet::leaflet() %>% 
        leaflet::addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
        leaflet::setView(lng = -121, lat = 44, zoom=7) %>%
        leaflet.extras::addResetMapButton() %>% 
        leaflet::addMiniMap(position = "bottomleft",
                            width = 200,
                            height = 200,
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
    
    
    # _ map reactive @ date selector ----
    
    observeEvent(input$date_map,{
      
      df.map.date <- reactive({
        
        lookup.date %>% 
          dplyr::filter(Date %in% as.Date(input$date_map)) %>% 
          dplyr::mutate(map_day = paste0(Year.dta,Day.dta))
        
      })
      
      if(is.na(df.map.date()$Year.dta)){
        
        return(NULL)
        
      } else {
        
        map.tif.dir <- reactive(paste0("data/", df.map.date()$Year.dta, "/"))
        
        file.name <- reactive(paste0(df.map.date()$map_day,".tif"))
        
        rst <- reactive({
          
          raster::raster(paste0(map.tif.dir(),file.name()))
          
        })
        
        leafletProxy("map") %>% 
          leaflet::clearImages() %>% 
          leaflet::clearControls() %>% 
          leaflet::addRasterImage(rst(), project = FALSE, colors=pal.map, opacity = 1) %>% 
          leaflet::addLegend(pal = pal.map, values = thevalues, title = "Cyanobacteria (cells/mL)")
        
      } 
      
    })
    
    sliderValues <- reactive({
      
      df.map.date <- reactive({
        
        lookup.date %>% 
          dplyr::filter(Date %in% as.Date(input$date_map)) %>% 
          dplyr::mutate(map_day = paste0(Year.dta,Day.dta))
        
      })
      
      data.frame(`Note:` = ifelse(is.na(df.map.date()$Year.dta),
                                  "There is no raster data on the selected date:",
                                  "Raster image is updated for"),
                 Date = as.character(input$date_map),
                 stringsAsFactors = FALSE)
      
    })
    
    output$values <- renderTable({
      sliderValues()
    })  
    
    
    
    # _ click on the map ---- 
    observe({
      
      click <- input$map_click
      
      if(is.null(click)){
        
        return(NULL)
        
      } else {
        
        leafletProxy("map") %>% 
          setView(
            lng = click$lng,
            lat = click$lat,
            zoom = 12)
        
      }
      
    })
    
    
    # (2) Plots ----
    
    # _ Boxplot ----
    df.box <- reactive({
      
      dta %>% 
        dplyr::filter(GNISIDNAME %in% input$waterbody) %>%
        dplyr::filter(Date %in% input$date_map)
      
    })
    
    output$boxplot <- renderPlotly({
      
      plotly::plot_ly(data = df.box(),
                      x = ~as.Date(Date, format= "%Y-%m-%d"),
                      y = ~`Cyanobacteria (cells/mL)`,
                      type = "box",
                      name = unique(df.box()$GNISIDNAME)) %>% 
        add_trace(x = input$date_map,
                  y = 100000) %>% 
        plotly::layout(xaxis = list(title = "",
                                    zeroline = FALSE,
                                    showline = FALSE,
                                    showticklabels = FALSE,
                                    showgrid = FALSE),
                       yaxis = list(type = "log",
                         title = "",
                         zeroline = TRUE,
                         showline = TRUE,
                         showticklabels = TRUE,
                         showgrid = FALSE),
                       showlegend = FALSE,
                       annotations = list(x = input$date_map,
                                          y = log(100000)/log(10),
                                          #y = 100000,
                                          text = "WHO Thresholds\n(100,000)",
                                          xref = "x",
                                          yref = "y",
                                          showarrow = TRUE,
                                          arrowhead = 3,
                                          arrowsize = 1,
                                          ax = 40,
                                          ay = 50))
      
    })
    
    # _ Time series plot ----
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
