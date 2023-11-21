library(shiny)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)

# UI Definition
ui <- fluidPage(
  titlePanel("Interactive World Map"),
  sidebarLayout(
    sidebarPanel(
      p("Click on a country to view its details."),
      textOutput("selectedCountryDetails")
    ),
    mainPanel(
      leafletOutput("worldMap")
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  # Load world data
  worldData <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Define a color palette for population-based coloring
  pal <- colorNumeric(palette = "viridis", domain = worldData$pop_est)
  
  # Render the map
  output$worldMap <- renderLeaflet({
    leaflet(data = worldData) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(pop_est),  # Color based on population
        weight = 1,                 # Border weight
        color = "#000080",          # Navy blue borders
        fillOpacity = 0.7,
        popup = ~paste(name_long, "<br/>Population: ", formatC(pop_est, format = "f", big.mark = ","))
      )
  })
  
  # Observe clicks on the map and display country details in the sidebar
  observeEvent(input$worldMap_shape_click, {
    clickData <- input$worldMap_shape_click
    if (!is.null(clickData)) {
      selectedCountry <- worldData[worldData$id == clickData$id, ]
      if (nrow(selectedCountry) > 0) {
        countryDetails <- paste(
          "Country: ", selectedCountry$name_long,
          "<br>Continent: ", selectedCountry$continent,
          "<br>Population: ", formatC(selectedCountry$pop_est, format = "f", big.mark = ","),
          "<br>Region: ", selectedCountry$subregion,
          sep = ""
        )
        output$selectedCountryDetails <- renderUI({
          HTML(countryDetails)
        })
      }
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
