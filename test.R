shinyApp(
  ui = fluidPage(
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
                                                      label = "Select Lakes:",
                                                      choices = unique(sort(dta$GNISIDNAME)),
                                                      selected = "Alkali Lake_01116863",
                                                      multiple = TRUE),
                            
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
                                                  weekstart = 0)
                            # _ Variables ----
                            # shinyWidgets::pickerInput(inputId = "variables",
                            #                          label = "Select Variables:",
                            #                          choices = unique(sort(dta$variable)),
                            #                          selected = "MAX_cellsml",
                            #                          multiple = TRUE)
                            
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
  server = function(input, output) { 
    
    # map ----
    output$map <- leaflet::renderLeaflet({
      
      leaflet::leaflet() %>% 
        leaflet::addProviderTiles("Esri.WorldImagery",group = "Esir Satellite Map")
      
    })
    
    # chart ----
    
    # df <- reactive({
    # dta %>% 
    #  dplyr::filter(GNISNAME == input$lakes) %>% 
    #  dplyr::filter(Date >= input$date[1],Date <= input$date[2])
    
    #})
    
    # __ temp.df
    df <- dta %>% 
      dplyr::filter(GNISIDNAME == "Odell Lake_01147159")
    
    #output$plot <- renderPlotly({
      
     # p <- ggplot2::ggplot(data = df, # reactive df()
      #                    aes(x = as.Date(Date), y = MEAN_cellsml)) + 
      #  geom_line(color = "dark red") +
      #  geom_line(aes(y = MAX_cellsml), color = "light blue", linetype = "dotted") +
      #  geom_line(aes(y = MIN_cellsml), color = "light green", linetype = "dotted")
      
      # p <- p + 
      #  labs(title = "Odell Lake") +
      #  scale_x_date(date_breaks = "1 day") +
      #  theme(axis.text.x = element_text(angle = 30, hjust = 1))
      
      # fig <- ggplotly(p)
      
      # fig
      
    # })
    
    output$plot <- renderPlotly({
      
      plotly::plot_ly(
        data = df,
        x = df$Date,
        y = df$MEAN_cellsml,
        name = "Mean",
        type = "scatter",
        mode = "lines",
        line = list(width = 4, color = "blue"),
        showlegend = TRUE) %>% 
        plotly::add_trace(y = df$MAX_cellsml,
                          name = "Maximum",
                          line = list(width = 2, color = "red", dash = "dot")) %>% 
        plotly::add_trace(y = df$MIN_cellsml,
                          name = "Minimum",
                          line = list(width = 2, color = "green", dash = "dot")) %>% 
        plotly::layout(xaxis = list(title = "Date", range = c(min(df$Date),max(df$Date))),
                       yaxis = list(title = "Cell (mg/L)")
        )
      
    })
    
    
    # data table ----
    
    output$table <- DT::renderDataTable({
      
      DT::datatable(
        data = df,  # reactive df()
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
