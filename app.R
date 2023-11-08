

library(shiny)
library(shinydashboard)
library(ggplot2)
devtools::install_github("shgroves/NOAA.Explore.QAQC")
#library(NOAA.Explore.QAQC)

dat <- NOAA.Explore.QAQC::ASPIRE_SHIP_CTD_data 
#load(here::here("data/ASPIRE_SHIP_CTD_data.rda"))
#dat <- ASPIRE_SHIP_CTD_data

ui <- tagList( 
  #dashboard page-----
  dashboardPage(
    
    #dashboard header-----
    dashboardHeader(
      title="NOAA Ocean Exploration Demo Data Dashboard", titleWidth = 500,
      tags$li(a(href = 'https://oceanexplorer.noaa.gov/',
                target = "_blank",
                h4("",
                   style="display:inline; vertical-align: middle;"),
                img(src='ocean-exploration-logo-360.png',
                    title = "NOAA Ocean Exploration", height="40px"),
                style = "padding-top:5px; padding-bottom:5px;"),
              class = "dropdown"),
      
      tags$li(a(href="https://www.noaa.gov/",
                target = "_blank",
                img(src = 'noaaLogo.png',
                    title = "National Oceanic and Atmospheric Administration", height = "40px"),
                style = "padding-top:5px; padding-bottom:5px;"),
              class = "dropdown")
      
    ), # END DASHBOARD HEADER
    #dashboard sidebar-----
    dashboardSidebar(
      actionLink("goToHomePage", 
                 img(src='EX.png', 
                     width='50%',
                     alt='EX cartoon',
                     style="padding-left:20px;")
      ),
      sidebarMenu(
        id = "tabs",
        menuItem("Home", tabName = "home_tab", icon = icon("home", lib = "glyphicon")),
        menuItem("CTD Rosette", tabName = "CTDR_tab", icon = icon("plus", lib = "glyphicon")),
        menuItem("ROV CTD", tabName = "ROVCTD_tab", icon = icon("plus", lib = "glyphicon"))
        
      ) # END DASHBOARD SIDEBAR 
    ), #END DASHBOARD MENU
    
    
    
    #dashboard Body ----
    dashboardBody(
      tabItems(
        #Home page-----
        tabItem(tabName = "home_tab", fluidPage(
          fluidRow(h2("Home page explanatory text"),
                   p("Explanatory text and maybe some photos..."))
        )), 
        #CTDR page-----
        tabItem(tabName = "CTDR_tab", fluidPage(
          fluidRow(h2("CTD Rosette"),
                   p("Explanatory text...")
          ), # END FLUID ROW
          
          fluidRow(
            column(5, selectInput(inputId = "expedition",
                                  label = "Expedition:",
                                  choices = c("EX1805",
                                              "EX1810",
                                              "EX1812",
                                              "EX1903L1" = "EX1903l1",
                                              "EX1905L1" = "EX1905l1",
                                              "EX1906",
                                              "EX2101", "EX2102", "EX2107", "EX2202", "EX2203"),
                                  selected = "EX1805"),
                   
                   selectInput(inputId = "sensor",
                               label = "Sensor:",
                               choices = c("Temperature (C)" = "temperature",
                                           "Conductivity (S/m)" = "conductivity",
                                           "Density (kg/m3)" = "density",
                                           "Salinity (PSU)" = "salinity",
                                           "Dissolved Oxygen (%)" = "oxygen",
                                           "Dissolved Oxygen (mg/L)" = "oxygen2",
                                           "Oxygen reduction potential" = "upoly",
                                           "Sound speed (m/s)" = "soundSpeed")),
                   checkboxInput("WOA", "Show NOAA World Ocean Atlas values", value = FALSE),
                   
                   # Horizontal line for visual separation
                   hr(),
                   helpText("Data from the CTD Rosette collected during the ASPIRE campaign")
            ),
            column(7, plotOutput("ctdPlot"))
          ) # END FLUID ROW
          
        ) # END FLUID PAGE
        
        ) # END CTDR_tab TAB ITEM
        
      ) # END TAB ITEMS
      
    ), # END DASHBOARD BODY
    
  ), # END DASHBOARD PAGE
  
  #dashboard Footer ----
  tags$footer(tags$a(href="https://www.noaa.gov/protecting-your-privacy","Place | ", target="_blank"),
              tags$a(href="https://www.noaa.gov/foia-freedom-of-information-act","to put | ", target="_blank"),
              tags$a(href="https://www.cio.noaa.gov/services_programs/info_quality.html","links | ", target="_blank"),
              tags$a(href="https://www.noaa.gov/disclaimer","and disclaimers | ", target="_blank"),
              #tags$a(href="https://www.usa.gov/","USA.gov | ", target="_blank"),
              #tags$a(href="https://www.ready.gov/","Ready.gov | ", target="_blank"),
              #tags$a(href="mailto:webmaster@coral.aoml.noaa.gov?subject=AcDC%20Website","Contact Webmaster", target="_blank"),
              
              tags$a(href = 'https://www.noaa.gov/', target = "_blank",
                     img(src = 'noaaLogo.png',
                         title = "NOAA", height = "35px",
                         style="padding-bottom: 5px;")),
              
              align = "center", 
              style = "
              position:fixed;
              bottom:0;
              width:100%;
              height:50px;   /* Height of the footer */
              color: white;
              padding: 10px;
              background-color: black;
              z-index: 1500;")
) # END TAG LIST


# Define server logic 
server <- function(input, output) {
  
  # Create a subset of data filtering for expedition ------
  # This gets cached
  subset_by_cruise <- reactive({
    req(input$expedition) # ensure availability of value before proceeding
    dat |>
      dplyr::filter(expedition %in% input$expedition)
  })
  
  
  # Fill in the spot we created for a plot
  output$ctdPlot <- renderPlot({ # you could potentially source a function here.
    
    if (input$WOA) { 
      
      subset_by_cruise() |>
        ggplot(aes(x=.data[[input$sensor]], y=depth)) + # update this plot to show WOA values
        geom_line() +
        ggtitle(paste(input$expedition, input$sensor, sep=" ")) +
        labs(x = input$sensor, y = "Depth (m)") +
        scale_y_reverse() +
        scale_x_continuous(position = "top") +
        theme_bw() 
      
    } else {
      subset_by_cruise() |>
        ggplot(aes(x=.data[[input$sensor]], y=depth)) +
        geom_line() +
        ggtitle(paste(input$expedition, input$sensor, sep=" ")) +
        labs(x = input$sensor, y = "Depth (m)") +
        scale_y_reverse() +
        scale_x_continuous(position = "top") +
        theme_bw()   
      
    }
    
  }) 
  
  
  showModal(modalDialog(
    title = 'Welcome! You have reached the Okeanos Explorer demo data dashboard. Explore physical and biological data collect during the ASPIRE campaign. This product is for demostration purposes and internal use only.',
    p("This application is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration (NOAA), or the United States Department of Commerce. All NOAA data are provided on an \'as is\' basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this app will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a Department of Commerce bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by the Department of Commerce or the United States Government."),
    footer =  modalButton("Confirm")
  )
  )
  
}


# Run the application 
shinyApp(ui = ui, server = server)

