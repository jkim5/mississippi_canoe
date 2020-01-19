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

date_time_data <- readr::read_csv("clean_data/date_time_data.csv")
clean_legs <- st_read("clean_data/clean_legs/clean_data_geom.shp") %>% 
  mutate(date_time = date_time_data$date_time)

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

colorNumericScaled <- function (palette, domain, na.color = "#808080", alpha = FALSE, 
    reverse = FALSE, scale_function = asinh) 
{
    rng <- NULL
    domain <- scale_function(domain)
    if (length(domain) > 0) {
        rng <- range(domain, na.rm = TRUE)
        if (!all(is.finite(rng))) {
            stop("Wasn't able to determine range of domain")
        }
    }
    pf <- leaflet:::safePaletteFunc(palette, na.color, alpha)
    leaflet:::withColorAttr("numeric", list(na.color = na.color), 
        function(x) {
            x <- scale_function(x)
          
            if (length(x) == 0 || all(is.na(x))) {
                return(pf(x))
            }
            if (is.null(rng)) 
                rng <- range(x, na.rm = TRUE)
            rescaled <- scales::rescale(x, from = rng)
            if (any(rescaled < 0 | rescaled > 1, na.rm = TRUE)) 
                warning("Some values were outside the color scale and will be treated as NA")
            if (reverse) {
                rescaled <- 1 - rescaled
            }
            pf(rescaled)
        })
}


server <- function(input, output, session) {
    
  my_pal <- reactive({
    
      if(input$cont_var == "air_gas" ||
         input$cont_var == "gyro_x" ||
         input$cont_var == "gyro_y" ||
         input$cont_var == "gyro_z") {
        colorNumericScaled("plasma", 
                           tiny_legs[[input$cont_var]], 
                           scale_function = asinh)
      } else {
        colorNumeric("plasma", 
                     tiny_legs[[input$cont_var]])
      }
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
