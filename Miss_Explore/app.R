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
library(purrr)

miss_basin_simple <- st_read("clean_data/miss_basin_simple/Miss_RiverBasin.shp")



clean_legs <- st_read("clean_data/clean_legs/clean_data_geom.shp")
tiny_legs = clean_legs[seq(1, nrow(clean_legs), 50), ]

section_1_poly <- st_polygon(x = list(rbind(c(-95.66165, 47.00948),
                                  c(-95.66165, 47.7275),
                                  c(-94.68412, 47.7275), 
                                  c(-94.68412, 47.00948),
                                  c(-95.66165, 47.00948))))

section_2_poly <- st_polygon(x = list(rbind(c(-90.70569, 43.13306),
                                  c(-90.70569, 43.89789),
                                  c(-91.73814, 43.89789), 
                                  c(-91.73814, 43.13306),
                                  c(-90.70569, 43.13306))))

section_3_poly <- st_polygon(x = list(rbind(c(-88.49971, 35.90685),
                                  c(-88.49971, 39.43619),
                                  c(-91.04787, 39.43619), 
                                  c(-91.04787, 35.90685),
                                  c(-88.49971, 35.90685))))

section_4_poly <- st_polygon(x = list(rbind(c(-89.12831, 29.17231),
                                  c(-89.12831, 30.58212),
                                  c(-91.37375, 30.58212), 
                                  c(-91.37375, 29.17231),
                                  c(-89.12831, 29.17231))))


section_data <- map(1:4, ~filter(clean_legs, clean_legs$section == .x))
section_1_data <- section_data[[1]]

map_section_k <- function(k, cont_var) {
  my_pal <-  colorNumeric("plasma", clean_legs[[cont_var]])
  
  leafletProxy("map", data = section_data[[k]]) %>%
          clearMarkers() %>%
          addCircleMarkers(radius = 4,
                           color = my_pal(section_data[[k]][[cont_var]]),
                           fill = TRUE,
                           fillColor = my_pal(section_data[[k]][[cont_var]]),
                           popup = glue('{section_data[[k]]$date_time}<br> Value: {tiny_legs[[cont_var]]}'),
                           label = glue('Value: {section_data[[k]][[cont_var]]}'),
                           group = "sparse_markers"
          ) %>%
          clearControls() %>%
          addLegend(position = "bottomright",
            pal = my_pal,
            values = clean_legs[[cont_var]],
            title = cont_var
          )
}


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
  current_shown_section <- -1
  force_update = TRUE
  
  
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

  # observe({
  #   leafletProxy("map", data = tiny_legs) %>%
  #     clearMarkers() %>%
  #     addCircleMarkers(radius = 4,
  #                      color = my_pal()(tiny_legs[[input$cont_var]]),
  #                      fill = TRUE,
  #                      fillColor = my_pal()(tiny_legs[[input$cont_var]]),
  #                      popup = glue('{tiny_legs$date_time}<br> Value: {tiny_legs[[input$cont_var]]}'),
  #                      label = glue('Value: {tiny_legs[[input$cont_var]]}'),
  #                      group = "sparse_markers"
  #     ) %>%
  #     clearControls() %>%
  #     addLegend(position = "bottomright",
  #       pal = my_pal(),
  #       values = ~(eval(parse(text = input$cont_var))),
  #       title = input$cont_var
  #     )
  # })

  observe({
    input$cont_var
    force_update <<- TRUE
  })
  
  observeEvent({input$map_bounds},{
    map_bounds <- input$map_bounds
    cont_var <- input$cont_var
    
    if(!is.null(map_bounds)){
      map_bounds_poly <- st_polygon(x = list(rbind(c(map_bounds$east, map_bounds$north),
                                    c(map_bounds$east, map_bounds$south),
                                    c(map_bounds$west, map_bounds$south), 
                                    c(map_bounds$west, map_bounds$north),
                                    c(map_bounds$east, map_bounds$north))))
      shown_sections <- as.logical(st_intersects(map_bounds_poly,
                                  st_as_sfc(list(section_1_poly,
                                            section_2_poly, 
                                            section_3_poly,
                                            section_4_poly)),
                                  sparse = FALSE))

      print(current_shown_section)
      
      if(shown_sections[1] &&
         sum(shown_sections) == 1 &&
         (current_shown_section != 1||
          force_update)) {
        
        map_section_k(k = 1, cont_var = cont_var)
        current_shown_section <<- 1

      } else if (shown_sections[2] &&
         sum(shown_sections) == 1 &&
         (current_shown_section != 2||
          force_update)) {
        
        map_section_k(k = 2, cont_var = cont_var)
        current_shown_section <<- 2
        
      } else if (shown_sections[3] &&
         sum(shown_sections) == 1 &&
         (current_shown_section != 3||
          force_update)) {
        
        map_section_k(k = 3, cont_var = cont_var)
        current_shown_section <<- 3
        
      } else if (shown_sections[4] &&
         sum(shown_sections) == 1 &&
         (current_shown_section != 4 ||
          force_update)) {
        
        map_section_k(k = 4, cont_var = cont_var)
        current_shown_section <<- 4
        
      } else if (sum(shown_sections) > 1 &&
                 (current_shown_section!= 0 ||
                  force_update)) {
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
        current_shown_section <<- 0
      }
      
      force_update <<- FALSE
    }
  })
}

shinyApp(ui, server)
