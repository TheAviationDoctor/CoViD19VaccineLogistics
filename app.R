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
# VARIABLE DECLARATION                                                        #
###############################################################################
# Header labels
AppHeader               <- "COVID-19 vaccine logistics"

#URLs
URLManufacturing      <- "https://raw.githubusercontent.com/TheAviationDoctor/CoViD19VaccineLogistics/main/data/manufacturing.csv"
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
      h3("Vaccines"),
      # Vaccine selection
      hr(),
      selectInput(
        "vaccine",
        "Select one or more vaccine",
        c("Moderna", "Pfizer"),
        selected = "Pfizer",
        multiple = TRUE,
        selectize = TRUE,
        width = "100%"
      ),
      # Site selection
      hr(),
      selectInput(
        "site",
        "Select one or more vaccine",
        c("1. Raw material", "2. Drug substance", "3. Formulation, Fill & Finish", "4. Distribution"),
        selected = "1. Raw material",
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
  Manufacturing <- reactive({
    pin(URLManufacturing) %>%
      read_csv(na = "", col_names = TRUE, col_types = list(col_factor(), col_factor(), col_factor(), col_factor(), col_factor(), col_factor(), col_double(), col_double(), col_factor(), col_character())) %>%
      filter(vaccine %in% input$vaccine) %>%
      filter(site %in% input$site)
  })

  # output$MyTable <- DT::renderDataTable({
  #   datatable(Manufacturing(), rownames = NULL, options = list(dom = "t", ordering = FALSE, paging = FALSE))
  # })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE)) %>%
      addAwesomeMarkers(
        data = Manufacturing() %>% cbind("longitude", "latitude"),
        lng = ~longitude,
        lat = ~latitude,
        # label = ~city,
        icon = awesomeIcons(
          # icon = "industry",
          icon = ifelse(
            test = Manufacturing()$purpose == "1. Raw material",
            yes = "industry",
            no = "clinic-medical"  # up arrow for secondary
          ),
          iconColor = Manufacturing()$color,
          library = 'fa',
          markerColor = "lightblue"
        )
      )
  })
}
###############################################################################
# RUN THE APP                                                                 #
###############################################################################
shinyApp(ui, server)