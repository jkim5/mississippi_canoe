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


miss_basin_simple <- st_read("clean_data/miss_basin_simple/Miss_RiverBasin.shp")

clean_legs <- st_read("clean_data/clean_legs/clean_data_geom.shp")
tiny_legs = clean_legs[seq(1, nrow(clean_legs), 10), ]




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
