###############################################################################
# COVID-19 VACCINE LOGISTICS                                                  #
# Created by: Thomas D. Pellegrin                                             #
#             contact@theaviationdoctor.com                                   #
#             https://theaviationdoctor.com                                   #
#             November 2020                                                   #
# Font Awesome icons taken from https://fontawesome.com/v4.7.0/icons/         #
###############################################################################
###############################################################################
# ROADMAP FOR FUTURE FEATURES                                                 #
#                                                                             #
###############################################################################
# Check out https://rstudio.github.io/leaflet/markers.html for reference
###############################################################################
# HOUSEKEEPING                                                                #
###############################################################################
# Clear the console
#cat("\014")
# Load libraries
library(DT)                 # To better display data tables
library(geosphere)          # To display great circle lines
library(leaflet)            # To better display data tables
library(pins)               # To locally cache downloaded data for performance
library(shiny)              # to build and display the app in a browser
library(shinycssloaders)    # To style the app and spinners in particular
library(tidyverse)          # To wrangle the data
###############################################################################
# VARIABLE DECLARATION AND DATA IMPORT/WRANGLING                              #
###############################################################################
# Header labels
AppHeader               <- "COVID-19 vaccine logistics"
#URLs
URLNodes      <- "https://raw.githubusercontent.com/TheAviationDoctor/CoViD19VaccineLogistics/main/data/nodes.csv"
# Import and wrangle the manufacturing data
DataNodes <- pin(URLNodes) %>%
    read_csv(na = "", col_names = TRUE, col_types = list(col_integer(), col_factor(), col_factor(), col_factor(), col_factor(), col_factor(), col_factor(), col_factor(), col_double(), col_double(), col_factor(), col_factor(), col_factor(), col_character())) %>%
    filter(show == TRUE)
###############################################################################
# USER INTERFACE LOGIC                                                        #
###############################################################################
ui <- fluidPage(
  # Globally style the app
  tags$head(
    tags$style("* { font-family: 'Aktiv Grotesk', Arial, sans-serif; !important }"),
    tags$style("hr { border: 1px solid #000000 }")
  ),
  # App header
  titlePanel(AppHeader),
  hr(),
  sidebarLayout(
    #######################################################################
    # SIDEBAR PANEL FOR INPUTS                                            #
    #######################################################################
    sidebarPanel(
      #Title
      h3("Vaccine logistics"),
      # Vaccine selection
      selectInput(
        "vaccine",
        "Select one or more vaccine",
        unique(DataNodes$vaccine),
        selected = unique(DataNodes$vaccine),
        multiple = TRUE,
        selectize = TRUE,
        width = "100%"
      ),
      # Site selection
      selectInput(
        "site",
        "Select one or more site",
        unique(DataNodes$site),
        selected = unique(DataNodes$site),
        multiple = TRUE,
        selectize = TRUE,
        width = "100%"
      )
    ),
    #######################################################################
    # MAIN PANEL FOR OUTPUTS                                              #
    #######################################################################
    mainPanel(
      leafletOutput("mymap"),
    )
  )
)
###############################################################################
# SERVER LOGIC                                                                #
###############################################################################
server <- function(input, output) {
  ###########################################################################
  # IMPORT AND WRANGLE DATA                                                 #
  ###########################################################################
  # Import and wrangle the node data
  DataNodesFiltered <- reactive({
    DataNodes %>%
      filter(vaccine %in% input$vaccine) %>% 
      filter(site %in% input$site)
  })
  output$mymap <- renderLeaflet({
      gcIntermediate(c(-90.556371,38.658831), c(-71.1694422,42.6147393), n = 100, addStartEnd = TRUE, sp = TRUE) %>%
      leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE)) %>%
      addAwesomeMarkers(
        data = DataNodesFiltered() %>% cbind("longitude", "latitude"),
        lng = ~longitude,
        lat = ~latitude,
        label = DataNodesFiltered()$vaccine,
        popup = paste("<strong>", DataNodesFiltered()$vaccine," | </strong>", DataNodesFiltered()$comments),
        icon = awesomeIcons(
          icon = "truck",
          iconColor = DataNodesFiltered()$iconcolor,
          library = 'fa',
          markerColor = DataNodesFiltered()$markercolor,
          text = substr(DataNodesFiltered()$site, 1, 1)
       )
      ) %>%
      addPolylines()
  })
}
###############################################################################
# RUN THE APP                                                                 #
###############################################################################
shinyApp(ui, server)