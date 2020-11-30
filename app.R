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
AppHeader     <- "COVID-19 vaccine logistics"
#URLs
URLNodes      <- pin("https://raw.githubusercontent.com/TheAviationDoctor/CoViD19VaccineLogistics/main/data/nodes.csv")
URLLinks      <- pin("https://raw.githubusercontent.com/TheAviationDoctor/CoViD19VaccineLogistics/main/data/links.csv")
# Import and wrangle the supply chain data
DataNodes <- pin(URLNodes) %>%
  read_csv(na = "", col_names = TRUE, col_types = list(col_integer(), col_factor(), col_factor(), col_factor(), col_factor(), col_factor(), col_factor(), col_factor(), col_double(), col_double(), col_character(), col_factor(), col_factor(), col_factor(), col_character())) %>%
  filter(show == TRUE)
DataLinks <- pin(URLLinks) %>%
  read_csv(na = "", col_names = TRUE, col_types = list(col_integer(), col_integer(), col_integer(), col_factor(), col_character())) %>%
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
        selected = unique(DataNodes$vaccine), # Select all values by default
        multiple = TRUE,
        selectize = TRUE,
        width = "100%"
      ),
      # Site selection
      selectInput(
        "site",
        "Select one or more site",
        unique(DataNodes$site),
        selected = unique(DataNodes$site), # Select all values by default
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
      textOutput("Test"),
      dataTableOutput("DataNodesFiltered"),
      dataTableOutput("DataLinksFiltered"),
      dataTableOutput("arc")
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
  # Filter the nodes data based on user selection
  DataNodesFiltered <- reactive({ DataNodes %>% filter(vaccine %in% input$vaccine) %>% filter(site %in% input$site) %>% select(id, vaccine, operator, site, latitude, longitude) })
  # Build the nodes data from filtered nodes data
  DataLinksFiltered <- reactive({DataLinks %>%
      mutate(
        sourceLatitude = DataNodesFiltered() %>% filter(id %in% DataLinks$source) %>% pull(latitude),
        sourceLongitude = DataNodesFiltered() %>% filter(id %in% DataLinks$source) %>% pull(longitude),
        targetLatitude = DataNodesFiltered() %>% filter(id %in% DataLinks$target) %>% pull(latitude),
        targetLongitude = DataNodesFiltered() %>% filter(id %in% DataLinks$target) %>% pull(longitude)
      )
  })
  # Display the nodes and links data as tables
  # output$DataNodesFiltered <- DT::renderDataTable({datatable(DataNodesFiltered(), rownames = NULL, options = list(dom = "t", ordering = FALSE, paging = FALSE))})
  output$DataLinksFiltered <- DT::renderDataTable({datatable(DataLinksFiltered(), rownames = NULL, options = list(dom = "t", ordering = FALSE, paging = FALSE))})
  ############ EXPERIMENTAL
  # col.1 <- adjustcolor("orange red", alpha=0.4)
  # col.2 <- adjustcolor("orange", alpha=0.4)
  # edge.pal <- colorRampPalette(c(col.1, col.2), alpha = TRUE)
  # edge.col <- edge.pal(100)
  #     for(i in 1:nrow(DataLinks)) {
  #       node1 <- DataNodes[DataNodes$id == DataLinks[i,]$source,]
  #       node2 <- DataNodes[DataNodes$id == DataLinks[i,]$target,]
  #       arc <- gcIntermediate( c(node1[1,]$longitude, node1[1,]$latitude), c(node2[1,]$longitude, node2[1,]$latitude), n=1000, addStartEnd=TRUE )
  #       edge.ind <- round(100 / max(DataLinks$volume))
  #     }
  # output$arc <- DT::renderDataTable({datatable(arc, rownames = NULL, options = list(dom = "t", ordering = FALSE, paging = FALSE))})
  ############ END EXPERIMENTAL
  output$mymap <- renderLeaflet({
      gcIntermediate(c(-90.556371,38.658831), c(-71.1694422,42.6147393), n = 100, addStartEnd = TRUE, sp = TRUE) %>%
      leaflet() %>%
      setView(15, 8, zoom = 2) %>%
      addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE)) %>%
      addAwesomeMarkers(
        data = DataNodesFiltered() %>% cbind("longitude", "latitude"),
        lng = ~longitude,
        lat = ~latitude,
        label = DataNodesFiltered()$vaccine,
       #  popup = paste("<strong>", DataNodesFiltered()$vaccine," | </strong>", DataNodesFiltered()$comments),
       #  icon = awesomeIcons(
       #    icon = "truck",
       #    iconColor = DataNodesFiltered()$iconcolor,
       #    library = 'fa',
       #    markerColor = DataNodesFiltered()$markercolor,
       #    text = DataNodesFiltered()$id
       # )
      ) %>%
        addPolylines()
        # %>%
        # lines(arc, col=edge.col[edge.ind], lwd=edge.ind/30)
  })
}
###############################################################################
# RUN THE APP                                                                 #
###############################################################################
shinyApp(ui, server)