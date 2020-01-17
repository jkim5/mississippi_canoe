library(shiny)
library(leaflet)
library(RColorBrewer)
library(readr)
library(sf)
library(rnaturalearth)
library(viridis)
library(dplyr)
library(glue)
library(htmltools)


(list.files(recursive = TRUE))

miss_basin <- st_read("clean_data/miss_basin/Miss_RiverBasin.shp") %>% 
  st_transform(crs = 4326)
miss_basin_simple <- rmapshaper::ms_simplify(miss_basin)



all_legs_med <- read_csv("clean_data/clean_data_non_geom.csv")
clean_legs <- st_as_sf(all_legs_med, coords = c("longitude", "latitude"), crs = 4326)
tiny_legs = clean_legs[seq(1, nrow(clean_legs), 50), ]




ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
    selectInput("cont_var", "Select Variable",
        colnames(select_if(tiny_legs, is.numeric))
    )
  )
)

server <- function(input, output, session) {
    
  my_pal <- reactive({
      colorNumeric("plasma", tiny_legs[[input$cont_var]])
  })
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(miss_basin_simple) %>%
        addProviderTiles(providers$OpenStreetMap.HOT) %>% 
        addPolygons(fillOpacity = 0, opacity = 0.5)
  })

  observe({
    leafletProxy("map", data = tiny_legs) %>% 
      clearMarkers() %>% 
      addCircleMarkers(radius = 4, 
                       color = my_pal()(tiny_legs[[input$cont_var]]),
                       fill = TRUE,
                       fillColor = my_pal()(tiny_legs[[input$cont_var]]),
                       popup = glue('{tiny_legs$date_time}<br> Value: {tiny_legs[[input$cont_var]]}'),
                       label = glue('Value: {tiny_legs[[input$cont_var]]}'),
                       group = "sparse_markers"
      ) %>% 
      clearControls() %>% 
      addLegend(position = "bottomright",
        pal = my_pal(), 
        values = ~(eval(parse(text = input$cont_var))),
        title = input$cont_var
      )
  })
  

}

shinyApp(ui, server)
