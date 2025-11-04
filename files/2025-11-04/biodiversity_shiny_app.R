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
search_occurrences <- function(county, clade) {
  # Shropshire GADM code
  gadm_code <- "GBR.1.84.1_1" # Shropshire GADM code
  
  occ_search_result <- occ_search(
    gadmGid = gadm_code,
    scientificName = clade,
    hasCoordinate = TRUE,
    limit = 500
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
      textInput("county", "Enter UK County:", value = "Shropshire")
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
    county <- input$county
    occ_search_result <- search_occurrences(county, clade)
    
    map <- leaflet() %>% addTiles()
    
    if (!is.null(occ_search_result$data) && nrow(occ_search_result$data) > 0) {
      map <- map %>%
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
          lng = mean(occ_search_result$data$decimalLongitude, na.rm = TRUE),
          lat = mean(occ_search_result$data$decimalLatitude, na.rm = TRUE),
          zoom = 9
        )
    } else {
      map <- map %>% setView(lng = -2.75, lat = 52.7, zoom = 9)
    }
    
    map
  })
}
#et voilà! You can run the application
shinyApp(ui = ui, server = server)