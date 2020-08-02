library(shiny)
library(plotly)

rm(ui); rm(server)

ui <- fluidPage(
    
    theme = shinythemes::shinytheme("flatly"),
    shiny::navbarPage("HAB Data Visualizer",
                      
                      wellPanel(
                        leaflet::leafletOutput("map")
                      ),
                      
                      wellPanel(
                        sidebarLayout(
                          sidebarPanel(
                            
                            # _ Lakes ----
                            shinyWidgets::pickerInput(inputId = "lakes",
                                                      label = "Select a Waterbody:",
                                                      choices = unique(sort(dta$GNISIDNAME)),
                                                      selected = "Alkali Lake_01116863",
                                                      multiple = FALSE),
                            
                            # _ Date ----
                            shiny::dateRangeInput(inputId = "date",
                                                  label = "Select Date Range:",
                                                  start = min(dta$Date),
                                                  end = max(dta$Date),
                                                  min = min(dta$Date),
                                                  max = max(dta$Date),
                                                  separator = "to",
                                                  format = "yyyy-mm-dd",
                                                  startview = "year",
                                                  weekstart = 0),
                            
                            # _ Variables ----
                            checkboxGroupInput(inputId = "matrix",
                                               label = "Statistical Base:",
                                               choices = c("Mean" = "MEAN_cellsml",
                                                           "Maximum" = "MAX_cellsml",
                                                           "Minimum" = "MIN_cellsml"),
                                               selected = "MEAN_cellsml")
                            
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
    
  ) # ui END
  
  server <- function(input, output, session) {
    
    # map ----
    output$map <- leaflet::renderLeaflet({
      
      leaflet::leaflet() %>% 
        leaflet::addProviderTiles("Esri.WorldImagery",group = "Esir Satellite Map")
      
    })
    
    # chart ----
    df <- reactive({
    
    dta %>% 
      dplyr::filter(GNISIDNAME %in% input$lakes) %>% 
      dplyr::filter(variable %in% input$matrix)
      
    })
    
    output$plot <- renderPlotly({
      
      plotly::plot_ly(
        data = df(),
        x = ~Date,
        y = ~value,
        split = ~variable,
        mode = "lines") %>% 
        plotly::layout(xaxis = list(title = "Date", range = c(min(df()$Date),max(df()$Date))),
                       yaxis = list(title = "Cell (mg/L)"))
      
    })
    
    
  } # server END
    
    
    
  
    
    
    shinyApp(ui, server)  

