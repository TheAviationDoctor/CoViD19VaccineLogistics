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
r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

Moderna <- cbind(-70.8036334, 43.082936)

oceanIcons <- iconList(
  ship = makeIcon("ferry-18.png", "ferry-18@2x.png", 18, 18),
  pirate = makeIcon("danger-24.png", "danger-24@2x.png", 24, 24)
)

icons <- awesomeIcons(
  icon = 'industry',
  iconColor = 'black',
  library = 'fa',
  markerColor = "red"
)

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
      h3("Manufacturers"),
      # Manufacturer selection
      hr(),
      selectInput(
        "Manufacturer",
        "Select one or more manufacturers",
        c("Moderna", "Pfizer"),
        selected = NULL,
        multiple = FALSE,
        selectize = TRUE,
        width = "100%"
      )
    ),
    #######################################################################
    # MAIN PANEL FOR OUTPUTS                                              #
    #######################################################################
    mainPanel(
      leafletOutput("mymap"),
      DT::dataTableOutput("MyTable")
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
      read_csv(na = "", col_names = TRUE, col_types = list(col_character(), col_character(), col_double(), col_double())) %>%
      filter(Manufacturer == input$Manufacturer)
    # %>%
    #   cbind("Longitude", "Latitude")
  })

  output$MyTable <- DT::renderDataTable({
    datatable(Manufacturing(), rownames = NULL, options = list(dom = "t", ordering = FALSE, paging = FALSE))
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE)) %>%
      addAwesomeMarkers(data = Manufacturing() %>% cbind("Longitude", "Latitude"), icon=icons)
  })
}
###############################################################################
# RUN THE APP                                                                 #
###############################################################################
shinyApp(ui, server)