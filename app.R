

library(shiny)
library(shinydashboard)
library(ggplot2)
#devtools::install_github("shgroves/NOAA.Explore.QAQC")
#library(NOAA.Explore.QAQC)

load(here::here("data/ASPIRE_SHIP_CTD_data.rda"))
dat <- ASPIRE_SHIP_CTD_data

group <- c("Cnidaria", "Porifera", "Echinodermata", "Chordata")

load(here::here("data/ASPIRE_ROV_BIOPHYS_data.rda"))

dat2 <- ASPIRE_ROV_BIOPHYS_data |> 
  dplyr::filter(Phylum %in% group) |>
  dplyr::select(cruise, dive_number, Phylum, Class, Order, Genus, Species, ROV1_tempC_SBE, ROV1_O2_mgL, ROV1_lat_dd, ROV1_lon_dd, ROV1_depth_m, ROV1_depth_m_time) |>
  dplyr::filter(!grepl("^\\s*$", ROV1_depth_m)) |>
  dplyr::mutate(present = 1,
                ROV1_depth_m = round(as.numeric(ROV1_depth_m, 3))) |>
  dplyr::mutate(dplyr::across(c("Class","Order", "Genus", "Species"), ~ifelse(.=="", NA, as.character(.))))



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
        #menuItem("ROV CTD", tabName = "ROVCTD_tab", icon = icon("plus", lib = "glyphicon")),
        menuItem("Benthic Observations", tabName = "Bio_tab", icon = icon("plus", lib = "glyphicon"))
        
      ) # END DASHBOARD SIDEBAR 
    ), #END DASHBOARD MENU
    
    
    
    #dashboard Body ----
    dashboardBody(
      tabItems(
        #Home page-----
        tabItem(tabName = "home_tab", fluidPage(
          fluidRow(h2("Welcome to the NOAA Ocean Exploration data visualization tool for the ASPIRE Campaign."),
                   p("This tool is currently in development for demonstration purposes."),
                   column(4, img(src = "ctd-380.jpg"),
                          hr(),
                   img(src = "ex2206-dive08-goosefish-380.jpg")),
                   column(4, img(src = "ex2206-dive03-medusa-380.jpg"),
                          hr(),
                          img(src = "ex2206-dive08-unknown-380.jpg")),
                   column(4, img(src = "dive10-bigfin-squid-380.jpg"),
                          hr(),
                          img(src = "dive05-volcanic-pillow-mound-380.jpg")))
        )), 
        #CTDR page-----
        tabItem(tabName = "CTDR_tab", fluidPage(
          fluidRow(h2("CTD Rosette"),
                   p("Use the drop down menus below to select the expedition, dive, and sensor of interest.")
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
                   
                   selectInput(inputId = "cast",
                               label = "Cast:",
                               choices = c()),
                   
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
                   checkboxInput("WOA", "Plot NOAA World Ocean Atlas values (NOT OPERATIONAL)", value = FALSE),
                   downloadButton("download1"),
                   # Horizontal line for visual separation
                   p("Use the download buttom to export the data in .csv format."),
                   helpText("Data from the CTD Rosette collected during the ASPIRE campaign. Downloads will inlcude the entire sensor suite for the expedition and cast selected.")
            ),
            column(7, plotOutput("ctdPlot"))
          ) # END FLUID ROW
          
        ) # END FLUID PAGE
        
        ), # END CTDR_tab TAB ITEM
        
        #"Bio_tab" page-----
        tabItem(tabName = "Bio_tab",
                fluidPage(
                  tabsetPanel(
                    tabPanel("Overall",
                             fluidRow(h2("Biological observations"),
                                      p("Use the drop down menus below to select the phyla and subtaxa of interest.")
                             ), # END Overall FLUID ROW 1
                             
                             fluidRow(
                               column(4, selectInput(inputId = "taxon1",
                                                     label = "Phylum:",
                                                     choices = c("Cnidaria", "Porifera", "Echinodermata", "Chordata")),
                                      
                                      selectInput(inputId = "taxon2",
                                                  label = "Subtaxa:",
                                                  choices = c("Class", "Order", "Genus", "Species")),
                                      
                                      checkboxInput("ROVCTD_sensor", "A future feature could overlay a ROV sensor profile (temp., O2) on the plot when you check this box. What would you like to see?", value = FALSE),
                                      downloadButton("download2"),
                                      
                                      # Horizontal line for visual separation
                                      p("Use the download buttom to export the data in .csv format."),
                                      helpText("Biological observations collected during the ASPIRE campaign for the phyla: Cnidaria, Porifera, Chordata, and Echinodermata.")
                               ),
                               column(8, plotOutput("bioPlot1"))
                             ) # END Overall FLUID ROW2
                    ), # END Overall tabPanel
                    
                    tabPanel("By expedition",
                             fluidRow(h2("Biological observations"),
                                      p("Use the drop down menus below to select the expedition, phyla, and subtaxa of interest.")
                             ), # END By expedition FLUID ROW 1
                             
                             fluidRow(
                               column(4, selectInput(inputId = "expedition1",
                                                     label = "Expedition:",
                                                     choices = sort(unique(dat2$cruise))),
                                      
                                      selectInput(inputId = "taxon3",
                                                  label = "Phylum:",
                                                  choices = c("Cnidaria", "Porifera", "Echinodermata", "Chordata")),
                                      
                                      selectInput(inputId = "taxon4",
                                                  label = "Subtaxa:",
                                                  choices = c("Class", "Order", "Genus", "Species")),
                                      
                                      checkboxInput("ROVCTD_sensor", "A future feature could overlay a ROV sensor profile (temp., O2) on the plot. What would you like to see?", value = FALSE),
                                      downloadButton("download3"),
                                      
                                      # Horizontal line for visual separation
                                      p("Use the download buttom to export the data in .csv format."),
                                      helpText("Biological observations collected during the ASPIRE campaign for the phyla: Cnidaria, Porifera, Chordata, and Echinodermata.")
                               ),
                               column(8, plotOutput("bioPlot2"))
                             ) # END By expedition FLUID ROW2
                    ), # END By expedition tabPanel
                    
                    tabPanel("By dive",
                             fluidRow(h2("Biological observations"),
                                      p("Use the drop down menus below to select the expedition, dive, phylum, and subtaxa of interest.")
                             ), # END By dive FLUID ROW 1
                             
                             fluidRow(
                               column(5, selectInput(inputId = "expedition2",
                                                     label = "Expedition:",
                                                     choices = sort(unique(dat2$cruise))),
                                      
                                      selectInput(inputId = "dive",
                                                  label = "Dive:",
                                                  choices = c()),
                                      
                                      selectInput(inputId = "taxon5",
                                                  label = "Phylum:",
                                                  choices = c("Cnidaria", "Porifera", "Echinodermata", "Chordata")),
                                      
                                      selectInput(inputId = "taxon6",
                                                  label = "Subtaxa:",
                                                  choices = c("Class", "Order", "Genus", "Species")),
                                      
                                      #checkboxInput("ROVCTD_sensor", "A future feature could overlay a ROV sensor profile (temp., O2) on the plot. What would you like to see?", value = FALSE),
                                      downloadButton("download4"),
                                      # Horizontal line for visual separation
                                      p("Use the download buttom to export the data in .csv format."),
                                      helpText("Biological observations collected during the ASPIRE campaign for the phyla: Cnidaria, Porifera, Chordata, and Echinodermata.")
                               ),
                               column(7, plotOutput("bioPlot3"))
                             ) # END By dive FLUID ROW 2
                    ) # END By dive tabPanel
                    
                  ) # END Bio_tab tabsetPanel
                ) # END Bio_tab FLUID PAGE
        ) # END Bio_tab TAB ITEM
        
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
  
  # Begin CTDR section
  
  # Downloads 
  output$download1 = downloadHandler(
    filename = function() {
      paste0(
        paste(input$expedition,input$cast,collapse="-") |>
          stringr::str_replace(" ", "_") |>
          tolower(), 
        "CTD_OER_ASPIRE.csv"
      )
    },
    content = function(file) {
      readr::write_csv(subset_by_cast(), file)
    }
  )
  
  # Create a subset of data filtering for expedition ------
  # This gets cached
  subset_by_exped <- reactive({
    req(input$expedition) # ensure availability of value before proceeding
    dat |>
      dplyr::filter(expedition %in% input$expedition)
  })
  
  subset_by_cast <- reactive({
    req(input$cast) # ensure availability of value before proceeding
    subset_by_exped() |>
      dplyr::filter(cast %in% input$cast)
  })
  
  observe({
    cast = dat |>
      dplyr::filter(expedition == input$expedition) |>
      dplyr::pull(cast) |>
      unique() |>
      sort()
    
    updateSelectInput(
      inputId = "cast", 
      choices = cast
    )
  })
  
  
  # CTDR Plot
  output$ctdPlot <- renderPlot({ # you could potentially source a function here.
    
    if (input$WOA) { 
      
      subset_by_cast() |>
        ggplot(aes(x=.data[[input$sensor]], y=depth)) + # update this plot to show WOA values
        geom_line() +
        ggtitle(paste(input$expedition, input$cast, input$sensor,  sep=" ")) +
        labs(x = input$sensor, y = "Depth (m)") +
        scale_y_reverse() +
        scale_x_continuous(position = "top") +
        theme_bw() 
      
    } else {
      subset_by_cast() |>
        ggplot(aes(x=.data[[input$sensor]], y=depth)) +
        geom_line() +
        ggtitle(paste(input$expedition, input$cast, input$sensor, sep=" ")) +
        labs(x = input$sensor, y = "Depth (m)") +
        scale_y_reverse() +
        scale_x_continuous(position = "top") +
        theme_bw()   
      
    }
  }) # End CTDR section
  
  # Begin Bio tab section
  
  # Downloads for each tab
  ## Overall
  output$download2 = downloadHandler(
    filename = function() {
      paste0(
        paste(input$taxon1,collapse="-") |>
          stringr::str_replace(" ", "_") |>
          tolower(), 
        "OER_ASPIRE.csv"
      )
    },
    content = function(file) {
      readr::write_csv(subset_by_phylum(), file)
    }
  )
  
  ## By expedition
  output$download3 = downloadHandler(
    filename = function() {
      paste0(
        paste(input$taxon1,input$expedition1,collapse="-") |>
          stringr::str_replace(" ", "_") |>
          tolower(), 
        "OER_ASPIRE.csv"
      )
    },
    content = function(file) {
      readr::write_csv(subset_by_ph_ex(), file)
    }
  )
  
  ## By expedition
  output$download4 = downloadHandler(
    filename = function() {
      paste0(
        paste(input$taxon1,input$expedition1,input$dive, collapse="-") |>
          stringr::str_replace(" ", "_") |>
          tolower(), 
        "OER_ASPIRE.csv"
      )
    },
    content = function(file) {
      readr::write_csv(subset_by_ph_ex_d(), file)
    }
  )
  
  # Create subset for overall tab
  subset_by_phylum <- reactive({
    req(input$taxon1) # ensure availability of value before proceeding
    dat2 |>
      dplyr::filter(Phylum %in% input$taxon1) 
  })
  
  # Create subset for expedition tab
  subset_by_ph_ex <- reactive({
    req(input$expedition1) # ensure availability of value before proceeding
    dat2 |>
      dplyr::filter(cruise %in% input$expedition1) |>
      dplyr::filter(Phylum %in% input$taxon3) 
  })
  
  # Create subset for expedition/dive tab
  subset_by_ph_ex_d <- reactive({
    req(input$expedition2) # ensure availability of value before proceeding
    req(input$dive)
    dat2 |>
      dplyr::filter(cruise %in% input$expedition2) |>
      dplyr::filter(dive_number %in% input$dive) |>
      dplyr::filter(Phylum %in% input$taxon5) 
  })
  

  observe({
    dives = dat2 |>
      dplyr::filter(cruise == input$expedition2) |>
      dplyr::pull(dive_number) |>
      unique() |>
      sort()
    
    updateSelectInput(
      inputId = "dive", 
      choices = dives)
  })
  
  # Overall Bio/depth Plot
  output$bioPlot1 <- renderPlot({ 
    
    if (input$ROVCTD_sensor) {
      
      subset_by_phylum() |>
        ggplot(aes(x=reorder(.data[[input$taxon2]], ROV1_depth_m), y=ROV1_depth_m)) + 
        geom_boxplot() +
        theme(axis.text.x = element_text(angle = -90, vjust = 0, hjust=0),
              legend.position="none") + 
        ggtitle(input$taxon1) +
        labs(x = paste(input$taxon1, "observations by", input$taxon2), y = "Depth (m)") +
        scale_y_reverse()
      
    } else {
      subset_by_phylum() |>
        ggplot(aes(x=reorder(.data[[input$taxon2]], ROV1_depth_m), y=ROV1_depth_m)) + 
        geom_boxplot() +
        theme(axis.text.x = element_text(angle = -90, vjust = 0, hjust=0),
              legend.position="none") + 
        ggtitle(input$taxon1) +
        labs(x = paste(input$taxon1, "observations by", input$taxon2), y = "Depth (m)") +
        scale_y_reverse()
    }
  }) # End Overall Bio/depth Plot
  
  # By expedition Bio/depth Plot
  output$bioPlot2 <- renderPlot({ # you could potentially source a function here.
    
    if (input$ROVCTD_sensor) {
      
      subset_by_ph_ex() |>
        ggplot(aes(x=reorder(.data[[input$taxon4]], ROV1_depth_m), y=ROV1_depth_m)) + 
        geom_boxplot() +
        theme(axis.text.x = element_text(angle = -90, vjust = 0, hjust=0),
              legend.position="none") + 
        ggtitle(paste(input$expedition2, input$taxon3,  sep=" ")) +
        labs(x = paste(input$taxon3, "observations by", input$taxon4), y = "Depth (m)") +
        scale_y_reverse()
      
    } else {
      subset_by_ph_ex() |>
        ggplot(aes(x=reorder(.data[[input$taxon4]], ROV1_depth_m), y=ROV1_depth_m)) + 
        geom_boxplot() +
        theme(axis.text.x = element_text(angle = -90, vjust = 0, hjust=0),
              legend.position="none") + 
        ggtitle(paste(input$expedition2, input$taxon3,  sep=" ")) +
        labs(x = paste(input$taxon3, "observations by", input$taxon4), y = "Depth (m)") +
        scale_y_reverse()
    }
  }) # End By expedition Bio/depth Plot
  
  # By dive Bio/depth Plot
  output$bioPlot3 <- renderPlot({ # you could potentially source a function here.
    
    # if (input$ROVCTD_sensor) {
    #   
    #   subset_by_ph_ex_d() |>
    #     ggplot(aes(x=reorder(.data[[input$taxon6]], ROV1_depth_m), y=ROV1_depth_m)) + 
    #     geom_point() +
    #     theme(axis.text.x = element_text(angle = -90, vjust = 0, hjust=0),
    #           legend.position="none") +
    #     ggtitle(paste(input$expedition2, "dive", input$dive, input$taxon5,  sep=" ")) +
    #     labs(x = paste(input$taxon5, "observations by", input$taxon6), y = "Depth (m)") +
    #     scale_y_reverse()
    #   
    # } else {
      subset_by_ph_ex_d() |>
        ggplot(aes(x=reorder(.data[[input$taxon6]], ROV1_depth_m), y=ROV1_depth_m)) + 
        geom_point() +
        theme(axis.text.x = element_text(angle = -90, vjust = 0, hjust=0),
              legend.position="none") +
        ggtitle(paste(input$expedition2, "dive", input$dive, input$taxon5,  sep=" ")) +
        labs(x = paste(input$taxon5, "observations by", input$taxon6), y = "Depth (m)") +
        scale_y_reverse()
    #}
  }) # End dive Bio/depth Plot
  
  showModal(modalDialog(
    title = 'Welcome! You have reached the Okeanos Explorer demo data dashboard. Explore physical and biological data collect during the ASPIRE campaign. This product is for demostration purposes and internal use only.',
    p("This application is a scientific product and is not official communication of the National Oceanic and Atmospheric Administration (NOAA), or the United States Department of Commerce. All NOAA data are provided on an \'as is\' basis and the user assumes responsibility for its use. Any claims against the Department of Commerce or Department of Commerce bureaus stemming from the use of this app will be governed by all applicable Federal law. Any reference to specific commercial products, processes, or services by service mark, trademark, manufacturer, or otherwise, does not constitute or imply their endorsement, recommendation or favoring by the Department of Commerce. The Department of Commerce seal and logo, or the seal and logo of a Department of Commerce bureau, shall not be used in any manner to imply endorsement of any commercial product or activity by the Department of Commerce or the United States Government."),
    footer =  modalButton("Confirm")
  )
  )
  
}


# Run the application 
shinyApp(ui = ui, server = server)

