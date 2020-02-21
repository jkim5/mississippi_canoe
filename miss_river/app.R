library(ggplot2)
library(dplyr)
library(sf)
library(readr)
library(viridis)
library(ggmap)
library(rnaturalearth)
library(lubridate)
library(purrr)
library(tidyr)
library(shiny)
library(glue)


date_time_data <- readr::read_csv("date_time_data.csv")
clean_legs <- st_read("clean_legs/clean_data_geom.shp") %>% 
  mutate(date_time = date_time_data$date_time)

tiny_legs = clean_legs[seq(1, nrow(clean_legs), 10), ]

coords <- st_coordinates(clean_legs)

chunk_1_interval <- interval(ymd_hms("2019-08-29 00:00:00"),
                             ymd_hms("2019-09-03 23:59:59"))

chunk_2_interval <- interval(ymd_hms("2019-09-24 00:00:00"),
                             ymd_hms("2019-09-26 23:59:59"))

chunk_3_interval <- interval(ymd_hms("2019-10-08 00:00:00"),
                             ymd_hms("2019-10-12 23:59:59"))

chunk_4_interval <- interval(ymd_hms("2019-10-13 00:00:00"),
                             ymd_hms("2019-10-18 23:59:59"))

chunk_5_interval <- interval(ymd_hms("2019-10-31 00:00:00"),
                             ymd_hms("2019-11-22 23:59:59"))

chunk_intervals <- list(chunk_1_interval,
                        chunk_2_interval,
                        chunk_3_interval,
                        chunk_4_interval,
                        chunk_5_interval)

chunk_num <- map(chunk_intervals, function(b, a){a %within% b}, clean_legs$date_time) %>%
  pmap_int(function(a, b, c, d, e) {
      chunk_num <- which(c(a, b, c, d, e))

      ifelse(length(chunk_num) == 0, NA, chunk_num)
    })
clean_legs_non_geom <- st_drop_geometry(clean_legs) %>% 
  select(-date_tm) %>% 
  bind_cols(as.data.frame(coords)) %>% 
  rename(lat = "Y", lon = "X")  %>% 
  mutate(chunk = chunk_num) %>%
  filter(!is.na(chunk)) %>%
  mutate(index = index + ((chunk-1)*50))




map_left_edge <- -98

gradient_right_edge <- -96
gradient_top_edge <- 48.16291
gradient_bottom_edge <- 28.92833


variable_list <-list(Altitude = "alt",
                     Speed = "speed",
                     Heading = "heading",
                     Climb = "climb",
                     `X Acceleration` = "accel_x",
                     `Y Acceleration` = "accel_y",
                     `Z Acceleration` = "accel_z",
                     `X Gyroscope` = "gyro_x",
                     `Y Gyroscope` = "gyro_y",
                     `Z Gyroscope` = "gyro_z",
                     `Air Temperature` = "air_tmp",
                     `Air Gas` = "air_gas",
                     `Air Humidity` = "air_hmd",
                     `Air Pressure` = "ar_prss",
                     `Water Temperature` = "wtr_tmp")

names_df <- as.data.frame(as.matrix(variable_list)) %>% 
  mutate(formatted = names(variable_list)) %>% 
  rename(unformatted = "V1")


us_states <- read_sf("state_data.shp")
line_data <- read_sf("line_data.shp")
miss_cities <- read_sf("city_data.shp")
miss_basin_rivers_10 <- read_sf("miss_basin_rivers_10.shp")


# Define UI for application that draws a histogram
ui <- fluidPage(
        # Show a plot of the generated distribution
  fluidRow(
    column(width = 12,
           align = "center",
           selectInput("layer", "Choose Variable", variable_list),
           plotOutput("miss_map", width = "100%", height = "600px")

    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$miss_map <- renderPlot({
      layer = input$layer
      layer_formatted = names_df %>% 
        filter(unformatted == layer) %>% 
        pull(formatted)
      map <- ggplot() +
        geom_sf(data = us_states, fill= "#F1F2F4", color = "white", size = 0.25) +
        geom_sf(data = miss_basin_rivers_10, color = "#b1d3de", size = 0.3) +
        geom_sf(data = filter(miss_basin_rivers_10, name %in% c("Mississippi")), size = 0.4, color = "#85c9de") +
        geom_sf(data = miss_cities, size = 0.5) +
        geom_text(data = miss_cities, 
                  aes(x = lon, y = lat, label = name),
                  hjust = 0, 
                  nudge_x = 0.3, nudge_y = 0.3,
                  fontface = "italic",
                  color = "grey22",
                  size = 2.3) +
        geom_sf(data = line_data, color = "gray22", show.legend = FALSE, size = 0.5) +
        geom_sf(data = clean_legs[seq(1, nrow(clean_legs), 30), ], aes(color = !!sym(layer)), alpha = 0.2, size = 0.5, show.legend = TRUE) +
        scale_color_viridis(option = "D", trans = "identity") +
        coord_sf(datum = NA, 
                 xlim = c(map_left_edge, -83.5),
                 ylim = c(28.85423, 48.7391)) +
        theme_void() +
        theme(legend.position = "right", 
              legend.title = element_blank(),
              legend.text = element_text(size = 6))
      
      gradient <- clean_legs_non_geom[seq(1, nrow(clean_legs), 30), ] %>% 
        mutate(index = 1:n() + 50*(chunk-1)) %>% 
        ggplot() +
        geom_tile(aes(x = 0, y = -index, fill = !!sym(layer)), width = 1, height = 11, alpha = 0.3) +
        scale_fill_viridis(option = "D", na.value = "gray85") +
        theme_void() +
        theme(legend.position = "none") +
        coord_cartesian(ylim = -c(1, 1042),
                        xlim = c(-0.5, 0.5),
                        expand = FALSE)
      
      map +
        annotation_custom(ggplotGrob(gradient), 
                          xmin = map_left_edge +1,
                          xmax = gradient_right_edge,
                          ymin = gradient_bottom_edge,
                          ymax = gradient_top_edge) +
        labs(caption = glue("Canoe {layer_formatted}")) +
        theme(plot.caption = element_text(size=10, hjust = 0, color = "grey22"))
    },
    res = 144, type = "cairo")
}

# Run the application 
shinyApp(ui = ui, server = server)
