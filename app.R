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
URLNodes      <- "https://raw.githubusercontent.com/TheAviationDoctor/CoViD19VaccineLogistics/main/data/nodes.csv"
URLLinks      <- "https://raw.githubusercontent.com/TheAviationDoctor/CoViD19VaccineLogistics/main/data/links.csv"
URLColor      <- "https://raw.githubusercontent.com/TheAviationDoctor/CoViD19VaccineLogistics/main/data/color.csv"
# URLNodes      <- "./data/nodes.csv"
# URLLinks      <- "./data/links.csv"
# URLColor      <- "./data/color.csv"
# Import and wrangle the supply chain data
DataNodes <- URLNodes %>% read_csv(na = "", col_names = TRUE, col_types = list(col_integer(), col_factor(), col_factor(), col_factor(), col_factor(), col_factor(), col_factor(), col_double(), col_double(), col_character(), col_factor()))
DataLinks <- URLLinks %>% read_csv(na = "", col_names = TRUE, col_types = list(col_integer(), col_integer(), col_integer(), col_factor(), col_character()))
DataColor <- URLColor %>% read_csv(na = "", col_names = TRUE, col_types = list(col_factor(), col_character()))
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
      # Display the map
      leafletOutput("mymap"),
      # Display the links table
      dataTableOutput("DataNodesDisplay"),
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
  DataNodesWithColors <- DataNodes %>% inner_join(DataColor, by = c("site" = "site"))
  DataNodesFiltered <- reactive({ DataNodesWithColors %>% filter(vaccine %in% input$vaccine) %>% filter(site %in% input$site) })
  # Build the links coordinates from filtered nodes data
  DataLinksFiltered <- reactive({ DataLinks %>%
      # Join the nodes table to the links table so we get information about the source and the target nodes
      inner_join(DataNodesFiltered(), by = c("source" = "id")) %>%
      inner_join(DataNodesFiltered(), by = c("target" = "id")) %>%
      # Rename the columns to add "source" and "target" and remove ".x" and ".y" wherever they were added after the joins
      rename_with(~paste0("source", str_to_title(substr(., 1, nchar(.) - 2))), (ncol(DataLinks) + 1):(ncol(DataLinks) + ncol(DataNodesFiltered()) - 1)) %>%
      rename_with(~paste0("target", str_to_title(substr(., 1, nchar(.) - 2))), (ncol(DataLinks) + ncol(DataNodesFiltered())):(ncol(DataLinks) + ncol(DataNodesFiltered()) + ncol(DataNodesFiltered()) - 2))
  })
  # Display the nodes data as tables
  output$DataNodesDisplay <- DT::renderDataTable({
    datatable(
      DataNodesFiltered() %>% select(id, vaccine, cmo, site, city, country),
      rownames = NULL, options = list(dom = "t", ordering = TRUE, paging = FALSE)
    )
  })
  # output$DataNodesFiltered <- DT::renderDataTable({datatable(DataNodesFiltered(), rownames = NULL, options = list(dom = "t", ordering = FALSE, paging = FALSE))})
  ###########################################################################
  # BUILD AND RENDER THE MAP                                                #
  ###########################################################################
  icons()
  output$mymap <- renderLeaflet({
      # Build the great circles to represent links
      gcIntermediate(DataLinksFiltered() %>% select(sourceLongitude, sourceLatitude), DataLinksFiltered() %>% select(targetLongitude, targetLatitude), n = 100, addStartEnd = TRUE, sp = TRUE, breakAtDateLine = TRUE) %>%
      leaflet() %>%
#      setView(15, 8, zoom = 2) %>%
      addTiles(group = "color", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Stamen.Toner, group = "black & white", options = providerTileOptions(noWrap = TRUE)) %>%
      addProviderTiles(providers$Stamen.TonerLite, group = "grayscale", options = providerTileOptions(noWrap = TRUE)) %>%
      addMapPane("links", zIndex = 410) %>%
      addMapPane("nodes", zIndex = 420) %>%
      ###########################################################################
      # BUILD AND RENDER THE LINKS LAYER                                        #
      ###########################################################################
      addPolylines(
        # Great circle line thickness is made proportional to the volume shipped (max volume is set to a baseline)
        color = DataNodesFiltered()$color,
        group = "links",
        label = paste("From", DataLinksFiltered()$sourceCmo, "in", DataLinksFiltered()$sourceCity, "to", DataLinksFiltered()$targetCmo, "in", DataLinksFiltered()$targetCity),
        labelOptions = labelOptions(permanent = FALSE, direction = "bottom", textOnly = FALSE, style = list("color" = DataLinksFiltered()$sourceMarkercolor, "font-weight" = "normal")),
        opacity = 1,
        options = pathOptions(pane = "links"),
        popup = DataLinksFiltered()$popup,
        weight = DataLinksFiltered()$volume * 2 / max(DataLinksFiltered()$volume)
      ) %>%
      ###########################################################################
      # BUILD AND RENDER THE NODES LAYER                                        #
      ###########################################################################
      addCircleMarkers(
        color = "white",
        data = DataNodesFiltered() %>% cbind("longitude", "latitude"),
        fill = TRUE,
        fillColor = DataNodesFiltered()$color,
        fillOpacity = 1,
        group = "nodes",
        label = paste(DataNodesFiltered()$cmo, "in", DataNodesFiltered()$city),
        labelOptions = labelOptions(permanent = FALSE, direction = "bottom", textOnly = FALSE, style = list("color" = "#1e32fa", "font-weight" = "normal")),
        lat = ~latitude,
        lng = ~longitude,
        opacity = x,
        options = pathOptions(pane = "nodes"),
        popup = paste("<strong>", DataNodesFiltered()$vaccine," | </strong>", DataNodesFiltered()$tooltip),
        radius = 10,
        stroke = TRUE,
        weight = 1
      ) %>%
      addLayersControl(
        baseGroups = c("color", "black & white", "grayscale"),
        overlayGroups = c("links", "nodes"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
}
###############################################################################
# RUN THE APP                                                                 #
###############################################################################
shinyApp(ui, server)