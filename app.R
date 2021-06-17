library(tidyverse)
library(shiny)
library(shinyBS)
library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyEffects)
library(raster)
library(leaflet)
library(leaflet.extras)
library(leafem)
library(mapview)
library(plainview)
library(scales)
library(plotly)
library(DT)
library(lubridate)

load("data.RData")

# Shiny App ----
shinyApp(
  ui = shinydashboardPlus::dashboardPage(
    options = list(sidebarExpandOnHover = TRUE),
    header = shinydashboardPlus::dashboardHeader(titleWidth = 400),
    
    # Sidebar ----
    sidebar = shinydashboardPlus::dashboardSidebar(
      minified = TRUE, collapsed = TRUE, width = 400,
      
      sidebarMenu(
        menuItem("About", icon = icon("info-circle"),
                 menuSubItem(h4(HTML("
                 This web application provides an interactive<br/>
                 map to view satellite derived data on<br/>
                 cyanobacteria harmful algal blooms in<br/>
                 freshwater ecosystems of Oregon. Satellite<br/>
                 data come from the US EPA CyAN project<br/>
                 and are updated on a regular basis.")))),
        menuItem("User Guide",  icon = icon("cog"), href="userGuide.html"),
        menuItem("Contact", icon = icon("envelope"),
                 menuSubItem(h5(HTML("
                 For more information on the Oregon HABs Map<br/>
                 Application Project, please contact<br/>
                 <br/>
                 Dan Sobota, Water Quality Analyst (Lead) <br/>
                 Daniel.Sobota@deq.state.or.us<br/>
                 <br/>
                 Erin Costello, Water Quality Analyst<br/>
                 Erin.Costello@deq.state.or.us<br/>
                 <br/>
                 Yuan Grund, Water Quality Analyst<br/>
                 Yuan.Grund@deq.state.or.us"))))
      ) # sidebarMenu END
    ), # dashboardSidebar END
    
    # Body ----
    body = shinydashboard::dashboardBody(
      
      tags$div(
        tags$style(HTML('/* logo */
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
                         
                         .main-sidebar {
                         font-size: 20px;
                         }
                                
                         /* active selected tab in the sidebarmenu */
                         .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                         background-color: #23769a;
                         }

                         /* other links in the sidebarmenu */
                         .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                         background-color: #23769a;
                         color: white;
                         }

                         /* other links in the sidebarmenu when hovered */
                         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                         background-color: #23769a;
                         }
                                
                         /* toggle button when hovered  */
                         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                         background-color: #23769a;
                         }

                         /* body */
                         .content-wrapper, .right-side {
                         background-color: white;
                         }
                                
                         /* box */
                         .box{
                         -webkit-box-shadow: none; -moz-box-shadow: none;box-shadow: none;
                         }
                         
                         .box-body {
                         padding-left: 0px;
                         padding-right: 0px;
                         }
                         
                         /* sidebar */
                         .sidebar {
                         padding-top: 100px;
                         }
                         
                         /*pickerinput_waterbody*/
                         .selectpicker {
                         z-index: 999999999 !important;
                         }
                         
                         /*datepicker*/
                         .datepicker {
                         z-index:99999 !important;
                         }
                         '))),
      
      # _ Header ----
      shinydashboard::box(
        width = 12,
        solidHeader = TRUE,
        
        tags$img(src = "DEQ-logo-color-horizontal370x73.png"),
        tags$div(span("Satellite Imagery of Cyanobacteria in Oregon Lakes and Reservoirs",
                      style = "color: black; font-size: 40px"))
        ,
        tags$div(span(HTML(paste0("Last sourced from the ",
                                  a("U.S. EPA CyAN Project", 
                                    href="https://www.epa.gov/water-research/cyanobacteria-assessment-network-cyan"),
                                  " on: ",
                                  max(dta$Date))),
                      style = "color: black; font-size: 20px"))
        
        #tags$br(),
        #,
        #tags$div(span(HTML(paste0("A Map Application for cyanobacteria blooms from the ",
        #                          a("U.S. EPA CyAN Project", 
        #                            href="https://www.epa.gov/water-research/cyanobacteria-assessment-network-cyan"))),
        #              style = "color: black; font-size: 20px")),
        #tags$br(),
      ), # Header box END 
      
      # _ Part 1: Mapping data ----
      shinydashboardPlus::box(
        width = 12,
        #title = "Mapping Data",
        status = "primary",
        solidHeader = FALSE,
        collapsible = FALSE,
        collapsed = FALSE,
        #dropdownMenu = boxDropdown(),
        
        shinydashboard::box(
          width = 3,
          #title = "date_and_waterbody",
          solidHeader = TRUE,
          
          # __ Select a Date ----
          #tags$style(HTML(".datepicker {z-index:99999 !important;}")),
          
          shiny::dateInput(inputId = "date_map",
                           label = tags$h4("Select a Date:"),
                           value = max(dta$Date),
                           min = min(dta$Date),
                           max = max(dta$Date),
                           format = "yyyy-mm-dd",
                           startview = "month",
                           weekstart = 0,
                           datesdisabled = missing.dates$Date),
          
          # __ 7DADM ----
          uiOutput("tbl7dadm"),
          
          #tags$hr(),
          
          # __ Select a Basin ----
          shinyWidgets::pickerInput(inputId = "basin",
                                    label = tags$h4("Select a Basin:"),
                                    choices = list(
                                      "Oregon",
                                      "HUC6 Basin" = unique(sort(huc6@data$HU_6_NAME))
                                    ),
                                    multiple = FALSE),
          
          #tags$hr(),
          
          # __ Select a Waterbody ----
          shinyWidgets::pickerInput(inputId = "waterbody",
                                    label = tags$h4("Select a Waterbody:"),
                                    choices = list(
                                      "Oregon",
                                      "Waterbody Name_GNISID" = unique(sort(lakes.resolvable$GNISIDNAME))
                                    ),
                                    multiple = FALSE),
          
          shiny::textOutput("dw"),
          
          tags$hr(),
          
          # __ Boxplot ----
          HTML(paste(
            #tags$br(),
            tags$h5("Boxplot of cyanobacteria estimates (cells/mL)\n
                    in waterbody on selected date:")
          )),
          
          plotlyOutput("boxplot")
          
        ), # Date box END
        
        # __ Map ----
        shinydashboard::box(
          width = 9,
          #title = "map",
          solidHeader = TRUE,
          
          leaflet::leafletOutput("map", height = "750px")
          
        ) # Map box END
        
      ), # Part 1 box END
      
      # _ Part 2: Plot ----
      shinydashboardPlus::box(
        width = 12,
        #title = "Time Series Data of Selected Lake",
        status = "primary",
        solidHeader = FALSE,
        collapsible = FALSE,
        collapsed = FALSE,
        #dropdownMenu = boxDropdown(),
        
        # __ Date Range ----
        shinydashboard::box(
          width = 3,
          #title = "options",
          solidHeader = TRUE,
          
          shiny::dateRangeInput(inputId = "date_plot",
                                label = tags$h4("Date Range:"),
                                start = min(dta$Date),
                                end = max(dta$Date),
                                min = min(dta$Date),
                                max = max(dta$Date),
                                separator = "to",
                                format = "yyyy-mm-dd",
                                startview = "year",
                                weekstart = 0),
          
          # __ Summary Statistics ----
          checkboxGroupInput(
            inputId = "matrix",
            label = tags$h4("Summary Statistics:"),
            choices = c("Maximum" = "Maximum",
                        "Mean" = "Mean",
                        "Minimum" = "Minimum"),
            selected = "Mean"),
          
          # __ Plot types ----
          checkboxGroupInput(
            inputId = "plot_log",
            label = tags$h4("y-axis:"),
            choices = c("Log Scale" = "log"))
          
        ),
        
        # __ Cell count ----
        shinydashboard::box(
          width = 9,
          #title = "time_series_plot",
          solidHeader = TRUE,
          
          plotlyOutput("plot_cell")
        )
      ), # Part 2 END
      
      # _ Part 3: Table ----
      shinydashboard::box(
        width = 12,
        #title = "data_table",
        
        DT::dataTableOutput("table")
      ) # Part 3 END
      
    ) # dashboardBody END
  ), # dashboardPage END
  
  server = function(input, output, session) {
    
    # (1) Map ----
    # _ initial map ----
    output$map <- leaflet::renderLeaflet({
      
      leaflet::leaflet() %>% 
        leaflet::addMapPane("OpenStreetMap", zIndex = -40) %>% 
        leaflet::addMapPane("National Geographic World Map", zIndex = -40) %>%
        #leaflet::addMapPane("Tiles", zIndex = -40) %>%
        leaflet::addMapPane("state.boundary", zIndex = -30) %>%
        leaflet::addMapPane("HUC6",zIndex = -20) %>% 
        leaflet::addMapPane("lakes.resolvable", zIndex = -10) %>%
        #leaflet::addMapPane("raster", zIndex = 450) %>%
        leaflet::addProviderTiles("OpenStreetMap",group = "OpenStreetMap",
                                  options = pathOptions(pane = "OpenStreetMap")) %>% 
        leaflet::addProviderTiles(providers$Esri.NatGeoWorldMap,group = "National Geographic World Map",
                                  options = pathOptions(pane = "National Geographic World Map")) %>% 
        #leaflet::addWMSTiles(baseUrl = 'https://www.mrlc.gov/geoserver/mrlc_display/NLCD_2016_Land_Cover_L48/wms?',
        #                     group = "Land Cover (NLCD 2016)",
        #                     layers = "NLCD_2016_Land_Cover_L48",
        #                     options = leaflet::WMSTileOptions(version = '1.3.0',
        #                                                       format = 'image/png',
        #                                                       transparent = TRUE,
        #                                                       pane = "Tiles")) %>%
        leaflet.extras::addResetMapButton() %>% 
        leaflet::addScaleBar(position = c("bottomright"),
                             options = scaleBarOptions()) %>% 
        leaflet::setView(lng = -120, lat = 44, zoom=7) %>%
        leaflet::addMiniMap(position = "bottomright",
                            width = 180,
                            height = 200,
                            zoomLevelFixed = 5) %>% 
        leaflet::addPolygons(data = lakes.resolvable, 
                             color = "blue",
                             weight = 2,
                             layer = ~lakes.resolvable$GNISIDNAME,
                             smoothFactor = 0.5,
                             opacity = 0.5,
                             fillColor = "transparent",
                             fillOpacity = 1.0,
                             label = ~lakes.resolvable$GNIS_Name,
                             labelOptions = labelOptions(style = list("font-size" = "18px",
                                                                      "color" = "blue")),
                             options = pathOptions(pane = "lakes.resolvable"))%>% 
        leaflet::addPolygons(data = huc6, 
                             group = "Basins (HUC6)",
                             color = "grey",
                             weight = 2,
                             smoothFactor = 0.5,
                             opacity = 0.5,
                             fillColor = ~pal.huc6(HU_6_NAME),
                             fillOpacity = 0.2,
                             label = ~huc6$HU_6_NAME,
                             labelOptions = labelOptions(noHide = TRUE,
                                                         textOnly = TRUE,
                                                         style = list("font-size" = "12px",
                                                                      "color" = "black")),
                                                         #             "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                         #             "border-color" = "rgba(0,0,0,0.5)")),
                             options = pathOptions(pane = "HUC6"))%>% 
        leaflet::addPolygons(data = state.boundary, 
                             color = "black",
                             weight = 2,
                             fillColor = "transparent",
                             fillOpacity = 1.0,
                             options = pathOptions(pane = "state.boundary")) %>% 
        leaflet::addLayersControl(baseGroups = c("OpenStreetMap","National Geographic World Map"),
                                  #overlayGroups = c("Hydrologic Unit 8 (HU8)","Land Cover (NLCD 2016)"),
                                  overlayGroups = c("Basins (HUC6)"),
                                  position = "topleft",
                                  options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE)) %>% 
      #leaflet::hideGroup(c("HUC8","Land Cover (NLCD 2016)"))
        leaflet::hideGroup(c("Basins (HUC6)"))
      
    })
    
    # _ map reactive @ waterbody picker ----
    
    observeEvent(input$waterbody,{
      
      if(input$waterbody == c("Oregon")) {
        
        leafletProxy("map") %>% 
          leaflet::setView(lng = -120, lat = 44, zoom=7)
        
      } else {
        
        one.lake <- reactive({
          
          lakes.resolvable[which(lakes.resolvable@data$GNISIDNAME == input$waterbody),]
          
        })
        
        bounds <- reactive({
          
          data.frame(bbox(one.lake()))
          
        })
        
        leafletProxy("map") %>% 
          leaflet::fitBounds(lng1=bounds()$min[1], lat1=bounds()$min[2], lng2=bounds()$max[1], lat2=bounds()$max[2])
      }
      
    })
    
    # _ map reactive @ basin picker ----
    
    observeEvent(input$basin,{
      
      if(input$basin == c("Oregon")) {
        
        leafletProxy("map") %>% 
          leaflet::setView(lng = -120, lat = 44, zoom=7)
        
      } else {
        
        one.basin <- reactive({
          
          huc6[which(huc6@data$HU_6_NAME == input$basin),]
          
        })
        
        bounds.basin <- reactive({
          
          data.frame(bbox(one.basin()))
          
        })
        
        leafletProxy("map") %>% 
          leaflet::fitBounds(lng1=bounds.basin()$min[1], lat1=bounds.basin()$min[2], lng2=bounds.basin()$max[1], lat2=bounds.basin()$max[2])
        
      }
      
    })
    
    # _ map reactive @ date selector ----
    
    observeEvent(input$date_map,{
      
      df.map.date <- reactive({
        
        lookup.date %>% 
          dplyr::filter(Date %in% as.Date(input$date_map)) %>% 
          dplyr::mutate(Day.dta = ifelse(Day.dta < 100, paste0("0",as.character(Day.dta)),Day.dta)) %>% 
          dplyr::mutate(map_day = paste0(Year.dta,Day.dta))
        
      })
      
      if(is.na(df.map.date()$Year.dta)){
        
        return(NULL)
        
      } else {
        
        map.tif.dir <- reactive(paste0("./data/", df.map.date()$Year.dta, "/"))
        
        file.name <- reactive(paste0(df.map.date()$map_day,".tif"))
        
        rst <- reactive({
          
          raster::raster(paste0(map.tif.dir(),file.name()))
          
        })
        
        leafletProxy("map") %>% 
          leaflet::clearImages() %>% 
          leaflet::clearControls() %>% 
          leaflet::addRasterImage(rst(), layerId = "Value", project = FALSE, colors=pal.map, opacity = 1) %>% 
          #leafem::addMouseCoordinates() %>% 
          #leafem::addImageQuery(rst(), layerId = "Value", digits = 0, project = TRUE, type = "mousemove",
          #                      position="topright",prefix = "") %>% 
          leaflet::addLegend(pal = pal.map, values = thevalues, title = "Cyanobacteria (cells/mL)", position = "topright",
                             labFormat = function(type,cuts,p){paste0(labels)}) %>% 
          leaflet::addLayersControl(#overlayGroups = c("Hydrologic Unit 8 (HU8)","Land Cover (NLCD 2016)","Value"),
            #overlayGroups = c("Value"),
            baseGroups = c("OpenStreetMap","National Geographic World Map"),
            overlayGroups = c("Basins (HUC6)"),
            position = "topleft",
            options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE)) %>% 
        #leaflet::hideGroup(c("Hydrologic Unit 8 (HU8)","Land Cover (NLCD 2016)"))
          leaflet::hideGroup(c("Basins (HUC6)"))
        
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
                  y = 100000,
                  line = list(color = "red")) %>% 
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
                                          text = "WHO Threshold",
                                          font = list(size = 8),
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
    
    # __ (.1) Data count ----
    df_data <- reactive({
      
      dta %>% 
        dplyr::filter(GNISIDNAME %in% input$waterbody) %>% 
        #dplyr::filter(`Summary Statistics` %in% input$matrix) %>% 
        dplyr::filter(`Summary Statistics` == "Mean") %>% 
        dplyr::filter(Date >= input$date_plot[1],Date <= input$date_plot[2]) %>% 
        dplyr::group_by(`Summary Statistics`,
                        mth = floor_date(Date,"month"),
                        yr = floor_date(Date,"year")) %>% 
        dplyr::summarise(`Data Count`= n())
      
    })
    
    output$plot_data <- renderPlotly({
      
      plotly::plot_ly(
        data = df_data(),
        x = ~ df_data()[[input$mthyr]],
        y = ~`Data Count`,
        type = "bar",
        marker = list(color = "light-blue")) %>% 
        plotly::layout(yaxis = list(title = "Sample Counts"),
                       xaxis = list(title = "Month",
                                    range = c(min(df()$Date),max(df()$Date))))
    })
    
    # __ (.2) Cell count ----
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
    
    output$plot_cell <- renderPlotly({
      
      plotly::plot_ly(data = df(), x = ~as.Date(Date)) %>% 
        plotly::add_trace(y = ~`Cyanobacteria (cells/mL)`,
                          split = ~`Summary Statistics`,
                          type = "scatter",
                          mode = "lines+markers",
                          #connectgaps = TRUE,
                          color = ~`Summary Statistics`,
                          colors = pal.plot,
                          marker = list(size = 8),
                          legendgroup = "sta") %>% 
        plotly::layout(xaxis = list(title = "Date", range = c(min(df()$Date),max(df()$Date))),
                       # yaxis = list(title = "Cyanobacteria (cells/mL)"),
                       title = as.character(unique(df()$GNISIDNAME))) %>% 
        plotly::layout(yaxis = list(type = type(),
                                    title = yaxis())) %>% 
        plotly::add_trace(y = 100000, mode = "lines",
                          line = list(shape = 'spline', color = 'red', width = 3),
                          name = "WHO Threshold",
                          legendgroup = "who",
                          showlegend = FALSE) %>% 
        plotly::layout(annotations = list(x = max(df()$Date),
                                          y = 100000,
                                          text = "WHO Threshold",
                                          font = list(size = 12),
                                          xref = "x",
                                          yref = "y",
                                          showarrow = TRUE,
                                          arrowhead = 3,
                                          arrowsize = 1,
                                          ax = -60,
                                          ay = -20))
      
    })
    
    # (3) Tables ----
    
    # _ Data table ----
    df_tbl <- reactive({
      
      df() %>% 
        dplyr::select(GNISIDNAME,Date,`Cyanobacteria (cells/mL)`,`Summary Statistics`) %>% 
        dplyr::mutate(`Cyanobacteria (cells/mL)` = ifelse(`Cyanobacteria (cells/mL)` <= 6310, "Non-detect",
                                                          scales::comma(`Cyanobacteria (cells/mL)`))) %>%
        dplyr::rename(Waterbody_GNISID = GNISIDNAME)
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
                       buttons = list(#'print',
                         list(extend = 'collection',
                              buttons = c('csv','excel','pdf'),
                              text = 'Download')
                       )),
        rownames = FALSE,
        filter = 'bottom'
      ) #%>% 
      #DT::formatDate("Date","toLocaleString")
    }, server = FALSE)
    
    # _ 7DADM ----
    title <- reactive({
      
      paste0("Waterbodies ranked by the 7-Day Average Daily Maximum (7DADM) of cyanobacteria abundance (cells/mL) during the 7 days from ", 
             as.Date(input$date_map)-7, " to ",input$date_map, 
             ".")
      
    })
    
    tbl.data.7days <- reactive({
      
      dta2 %>% 
        dplyr::arrange(GNISIDNAME, desc(Date)) %>% 
        dplyr::filter((as.Date(Date) <= as.Date(input$date_map)) & (as.Date(Date) >= as.Date(input$date_map)-7))
      
    })
    
    tbl.data <- reactive({
      
      tbl.data.7days() %>% 
        dplyr::group_by(GNISIDNAME) %>% 
        dplyr::summarise(mean_7DayMax = mean(MAX_cellsml)) %>% 
        dplyr::ungroup() %>% 
        #dplyr::left_join(tbl.data.7days(),by="GNISIDNAME") %>% 
        #dplyr::filter(mean_7DayMax == MAX_cellsml) %>% 
        dplyr::arrange(desc(mean_7DayMax)) %>% 
        dplyr::left_join(lakes.resolvable@data, by = "GNISIDNAME") %>% 
        dplyr::mutate(Basin = ifelse(Name_1 == "Willamette",Name,Name_1)) %>% 
        dplyr::select(GNISIDNAME,Basin,mean_7DayMax) %>% 
        dplyr::distinct(GNISIDNAME, .keep_all = TRUE) %>% 
        dplyr::mutate(mean_7DayMax = ifelse(mean_7DayMax<= 6310, "Non-detect",
                                            format(round(mean_7DayMax,0),big.mark=",",scientific = FALSE))) %>% 
        #dplyr::mutate(Date = as.Date(Date,format="%Y-%b-%d")) %>% 
        dplyr::rename(Waterbody_GNISID = GNISIDNAME,
                      `7-Day Average Daily Maximum (cells/mL)` = mean_7DayMax)
    })
    
    output$tbl7dadm <- renderUI({
      tagList(
        bsModal("modal", title(), trigger = "a", DT::renderDataTable(tbl.data()), size = "large"),
        actionButton("a", "Table of Waterbodies Ranked by 7DADM")
      )
    })
    
    # _ Bloom table ----
    # (didn't use)
    df.blooms <- reactive({
      
      dta %>% 
        dplyr::filter(Date %in% input$date_map) %>% 
        dplyr::filter(`Summary Statistics` == "Mean") %>%
        dplyr::filter(`Cyanobacteria (cells/mL)` > 100000) %>% 
        dplyr::select(GNISIDNAME,"Cyanobacteria (cells/mL)") %>% 
        dplyr::distinct() %>% 
        dplyr::rename(Waterbody_GNISID = GNISIDNAME) %>% 
        dplyr::mutate(`Cyanobacteria (cells/mL)` = scales::comma(`Cyanobacteria (cells/mL)`))
      
    })
    
    output$bloom_lakes <- renderTable(df.blooms())
    
    # (4) Text: Drinking Water Area ----
    dw <- reactive({
      
      dta %>% 
        dplyr::filter(GNISIDNAME %in% input$waterbody) %>% 
        dplyr::mutate(dwsa = ifelse(wi_DWSA == "Yes", "Public Drinking Water Source", "Recreational Waterbody")) %>% 
        pull(dwsa)
      
    })
    
    output$dw <- renderText({ 
      
      if(input$waterbody == c("Oregon")) {}
      else {
        unique(dw())
      }
    })
    
  }
  
) # shinyApp END