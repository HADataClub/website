## Info ####


## Contents ####
## 1 Setup
## 2 GBIF search function
## 3 Shiny ui
## 4 Shiny server

## 1 Setup ####

# Install packages
# install.packages(c("shiny", "rgbif", "leaflet", dependencies=T))
# Load packages
require(shiny)
require(rgbif)
require(leaflet)

## 2 GBIF search function ####

# Define function to search for occurrences of 
# specified clades within a polygon (i.e, bounding box=bbox)
search_occurrences <- function(bbox, clade) {
  occ_search_result <- occ_search(
    geometry = paste("POLYGON((", bbox["min_longitude"], " ", bbox["min_latitude"], ",", 
                     bbox["min_longitude"], " ", bbox["max_latitude"], ",", 
                     bbox["max_longitude"], " ", bbox["max_latitude"], ",", 
                     bbox["max_longitude"], " ", bbox["min_latitude"], ",", 
                     bbox["min_longitude"], " ", bbox["min_latitude"], "))"),
    month = c(1:12), # define months of the year
    scientificName = clade,
    hasCoordinate = TRUE
  )
  return(occ_search_result)
}

## 3 Shiny ui ####

# Define Shiny user interface
ui <- fluidPage(
  titlePanel("Species Occurrence"),
  sidebarLayout(
    sidebarPanel(
      selectInput("clade", "Choose a clade:",
                  # You can change the default choices according to your taste in biodiversity
                  choices = c("Aves", "Coleoptera", "Amphibia", "Plantae", "Mammalia", "Insecta"),
                  # First clade to be shown in the drop down box
                  selected = "Aves"), 
      # By default you will have the approximate borders of Shropshire
      numericInput("min_longitude", "Minimum Longitude:", value = -3.1),
      numericInput("max_longitude", "Maximum Longitude:", value = -1.9),
      numericInput("min_latitude", "Minimum Latitude:", value = 52.3),
      numericInput("max_latitude", "Maximum Latitude:", value = 53.1)
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)

## 4 Shiny server ####

# Define Shiny server
server <- function(input, output) {
  # Render the leaflet map based on user's clade selection and polygon coordinates
  output$map <- renderLeaflet({
    clade <- input$clade
    bbox <- c(
      min_longitude = input$min_longitude,
      min_latitude = input$min_latitude,
      max_longitude = input$max_longitude,
      max_latitude = input$max_latitude
    )
    
    occ_search_result <- search_occurrences(bbox, clade)
    
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        data = occ_search_result$data,
        lng = ~decimalLongitude,
        lat = ~decimalLatitude,
        popup = ~species,
        radius = 5,
        color = "blue",
        fillOpacity = 0.7
      ) %>%
      setView(
        lng = mean(bbox[c("min_longitude", "max_longitude")]),
        lat = mean(bbox[c("min_latitude", "max_latitude")]),
        zoom = 8
      )
  })
}
#et voilà! You can run the application
shinyApp(ui = ui, server = server)