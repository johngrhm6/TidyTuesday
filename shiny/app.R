library(shiny)
library(tidyverse)
library(rnaturalearth)
library(sf)
getwd()

ufo_sightings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/ufo_sightings.csv')
places <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-06-20/places.csv')

world <- ne_countries(scale = "small", type = "countries") %>% 
  st_as_sf() %>% 
  st_transform(crs = 'EPSG:4087')

places_sf <- places %>% 
  count(longitude, latitude, sort = TRUE) %>% 
  st_as_sf(coords = c("longitude", "latitude"), crs = "EPSG:4326") %>% 
  st_transform(crs = st_crs(world))

ui <- fluidPage(
  titlePanel("UFO Sightings on World Map"),
  mainPanel(
    plotOutput("worldMap")
  )
)

server <- function(input, output, session) {
  output$worldMap <- renderPlot({
    ggplot() +
      geom_sf(data = world, mapping = aes(geometry = geometry)) +
      geom_sf(data = places_sf, mapping = aes(geometry = geometry, size = n)) +
      scale_size(range = c(1, 2), limits = c(1, 2), trans = "log10")
  })
}

shinyApp(ui = ui, server = server)
