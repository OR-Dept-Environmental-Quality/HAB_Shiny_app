library(tidyverse)
library(shiny);library(shinyWidgets);library(shinythemes)
library(shinydashboard);library(shinydashboardPlus);library(shinyjs)
library(raster);library(sp)
library(leaflet);library(leaflet.extras)
library(scales)
library(plotly)
library(DT)
library(lubridate)

# load("data.RData")

# Shiny App ----
shinyApp(
  ui = dashboardPage(
    options = list(sidebarExpandOnHover = TRUE),
    header = dashboardHeader(titleWidth = 400),
    
    # Sidebar ----
    sidebar = dashboardSidebar(
      minified = TRUE, collapsed = TRUE, width = 400,
      
      sidebarMenu(
        menuItem("About", icon = icon("info-circle"),
                 menuSubItem(h5(HTML("12345678911121314151617181912021222324252628<br/>
                                     12345678911121314151617181912021222324252628<br/>
                                     12345678911121314151617181912021222324252628")))),
        menuItem("User Guide", icon = icon("cogs"),
                 menuSubItem(h5(HTML("12345678911121314151617181912021222324252628<br/>
                                     12345678911121314151617181912021222324252628<br/>
                                     12345678911121314151617181912021222324252628")))),
        menuItem("Contact", icon = icon("envelope"),
                 menuSubItem(h5(HTML("12345678911121314151617181912021222324252628<br/>
                                     12345678911121314151617181912021222324252628<br/>
                                     12345678911121314151617181912021222324252628"))))
      )),
    # Body ----
    body = dashboardBody(
      
      tags$head(
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
                         padding-top: 200px;
                         }
                         '))),
      
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
        #shinyjs::useShinyjs(),
        #div(style="display: inline-block;vertical-align:top; width: 20px;",HTML("<br>")),
        #div(style="display: inline-block;vertical-align:top; height:20px; width: 300px;",h4("Show Introduction and User Guide:")),
        #div(style="display: inline-block;vertical-align:top; height:50px; width: 150px;",shinyWidgets::switchInput(inputId = "sidebarSwitch",size = "small"))
      ) 
      
    )#,
    #controlbar = dashboardControlbar(),
    #title = "DashboardPage"
  ),
  server = function(input, output) { }
)