## Info ####




# Install packages
# install.packages(c("shiny", "rgbif", "leaflet", dependencies=T))
# Load packages
require(shiny)
require(rgbif)
require(leaflet)
# Define function to search for occurrences of  specified clades within a polygon (i.e, bounding box=bbox)
search_occurrences <- function(bbox, clade) {
  occ_search_result <- occ_search(
    geometry = paste("POLYGON((", bbox["min_longitude"], " ", bbox["min_latitude"], ",", 
                     bbox["min_longitude"], " ", bbox["max_latitude"], ",", 
                     bbox["max_longitude"], " ", bbox["max_latitude"], ",", 
                     bbox["max_longitude"], " ", bbox["min_latitude"], ",", 
                     bbox["min_longitude"], " ", bbox["min_latitude"], "))"),
    month = 1, 12,###define months of the year
    scientificName = clade,
    hasCoordinate = TRUE
  )
  return(occ_search_result)
}
# Define user interface
ui <- fluidPage(
  titlePanel("Species Occurrence"),
  sidebarLayout(
    sidebarPanel(
      selectInput("clade", "Choose a clade:",
                  choices = c("Aves", "Coleoptera", "Amphibia", "Plantae", "Mammalia", "Actinopterygii", "Insecta"),#you can change the default clades according to your taste in biodiversity
                  selected = "Aves"), #first clade to be shown in the drop down box
      numericInput("min_longitude", "Minimum Longitude:", value = -9),##by default you will have the approximate borders of portugal, but this can be changed in the user interface or directly here
      numericInput("max_longitude", "Maximum Longitude:", value = -6),
      numericInput("min_latitude", "Minimum Latitude:", value = 36),
      numericInput("max_latitude", "Maximum Latitude:", value = 42)
    ),
    mainPanel(
      leafletOutput("map")
    )
  )
)
# Define server logic
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
        zoom = 14
      )
  })
}
#et voilà! You can run the application
shinyApp(ui = ui, server = server)