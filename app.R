###############################################################################
# COVID-19 VACCINE LOGISTICS                                                  #
# Created by: Thomas D. Pellegrin                                             #
#             contact@theaviationdoctor.com                                   #
#             https://theaviationdoctor.com                                   #
#             November 2020                                                   #
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
URLManufacturing      <- "https://raw.githubusercontent.com/TheAviationDoctor/CoViD19VaccineLogistics/main/data/manufacturing.csv"
# Import and wrangle the manufacturing data
DataManufacturing <- pin(URLManufacturing) %>%
    read_csv(na = "", col_names = TRUE, col_types = list(col_factor(), col_factor(), col_factor(), col_factor(), col_factor(), col_factor(), col_double(), col_double(), col_factor(), col_factor(), col_factor(), col_character()))
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
        unique(DataManufacturing$vaccine),
        selected = unique(DataManufacturing$vaccine),
        multiple = TRUE,
        selectize = TRUE,
        width = "100%"
      ),
      # Site selection
      selectInput(
        "site",
        "Select one or more site",
        unique(DataManufacturing$site),
        selected = unique(DataManufacturing$site),
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
  # Import and wrangle the manufacturing data
  DataManufacturingFiltered <- reactive({
    DataManufacturing %>%
      filter(vaccine %in% input$vaccine) %>% 
      filter(site %in% input$site)
  })
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE)) %>%
      addAwesomeMarkers(
        data = DataManufacturingFiltered() %>% cbind("longitude", "latitude"),
        lng = ~longitude,
        lat = ~latitude,
        label = paste(DataManufacturingFiltered()$vaccine),
        popup = paste("<strong>", DataManufacturingFiltered()$vaccine," | </strong>", DataManufacturingFiltered()$comments),
        # label = ~city,
        icon = awesomeIcons(
          icon = DataManufacturingFiltered()$icon,
          iconColor = DataManufacturingFiltered()$iconcolor,
          library = 'fa',
          markerColor = DataManufacturingFiltered()$markercolor
        )
      )
  })
}
###############################################################################
# RUN THE APP                                                                 #
###############################################################################
shinyApp(ui, server)