library(tidyverse)
library(lubridate)
library(plotly);library(ggplot2)
library(shiny); library(shinythemes); library(shinyWidgets)
library(leaflet)
library(DT)
library(viridis)

load("data.RData")

rm(ui); rm(server)

## app.R ----
shinyApp(

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
                            
                            # _ Statistical Base ----
                            checkboxGroupInput(inputId = "matrix",
                                               label = "Statistical Base:",
                                               choices = c("Mean" = "Mean",
                                                           "Maximum" = "Maximum",
                                                           "Minimum" = "Minimum"),
                                               selected = "Mean")
                            
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
    output$map <- leaflet::renderLeaflet({
      
      leaflet::leaflet() %>% 
        leaflet::addProviderTiles("Esri.WorldImagery",group = "Esir Satellite Map")
      
    })
    
    # chart ----
    pal <- c("orange","blue","green","white","white","white")
    pal <- setNames(pal,unique(sort(dta$`Statistical Base`)))
    
    df <- reactive({
      
      dta %>% 
        dplyr::filter(GNISIDNAME %in% input$lakes) %>% 
        dplyr::filter(`Statistical Base` %in% input$matrix) %>% 
        dplyr::filter(Date >= input$date[1],Date <= input$date[2])
      
    })
    
    output$plot <- renderPlotly({
      
      plotly::plot_ly(
        data = df(),
        x = ~Date,
        y = ~`Value (cells/mL)`,
        split = ~`Statistical Base`,
        type = "scatter",
        mode = "lines",
        color = ~`Statistical Base`,
        colors = pal) %>% 
        plotly::layout(xaxis = list(title = "Date", range = c(min(df()$Date),max(df()$Date))),
                       yaxis = list(title = "Count (cells/mL)"))
      
    })
    
    
    
    # data table ----
    
    output$table <- DT::renderDataTable({
      
      DT::datatable(
        data = df(),
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
      ) %>% 
        DT::formatDate("Date","toLocaleString")
    }, server = FALSE)
    
    
    
    
    
  }
)
