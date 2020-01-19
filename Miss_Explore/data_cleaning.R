library(tidyverse)
library(readr)
library(sf)
library(glue)
library(lubridate)
library(rnaturalearthdata)
library(rnaturalearth)

##########################################
# load in data
##########################################
leg_0_col_names <- c("index", 
               "crew", 
               "date", 
               "time_GMT", 
               "latitude", 
               "longitude",
               "alt",
               "speed", 
               "heading",
               "climb",
               "accel_x",
               "accel_y", 
               "accel_z",
               "gyro_x",
               "gyro_y",
               "gyro_z", 
               "air_temp",
               "air_gas",
               "air_humid",
               "air_pressure",
               "water_temp", 
               "ysi_ph",
               "ysi_do",
               "ysi_no3",
               "ysi_sal",
               "ysi_tur",
               "flow")

leg_0_col_types <- list(col_number(),
                  col_character(),
                  col_date(format = "%Y-%m-%d"),
                  col_time(format = "%H:%M:%S"),
                  col_number(), 
                  col_number(), 
                  col_number(),
                  col_number(),
                  col_number(),
                  col_number(),
                  col_number(),
                  col_number(),
                  col_number(),
                  col_number(),
                  col_number(),
                  col_number(),
                  col_number(),
                  col_number(),
                  col_number(),
                  col_number(),
                  col_number(),
                  col_number(),
                  col_number(),
                  col_number(),
                  col_number(),
                  col_number(),
                  col_number())

leg_2_col_names <- c(leg_0_col_names, "comp_time")
leg_2_col_types <- rlist::list.append(leg_0_col_types, col_integer())

leg_0_file_names <- list("raw_data/2019830-datalog.txt",
                         "raw_data/2019831-datalog.txt",
                         "raw_data/20190902-datalog.txt")

leg_2_file_names <- setdiff(list.files(path = "raw_data", 
                               pattern = ".txt",
                               full.names = TRUE),
                            leg_0_file_names)

leg_0_data <- map(leg_0_file_names, read_csv,
                  col_names = leg_0_col_names,
                  col_types = leg_0_col_types,
                  na = c("None")) %>% 
  bind_rows()

leg_2_data <- map(leg_2_file_names, read_csv,
                  col_names = leg_2_col_names,
                  col_types = leg_2_col_types,
                  na = c("None", ""),
                  skip_empty_rows = TRUE) %>% 
  bind_rows()

##########################################
# preparing data
##########################################
leg_0_data <- leg_0_data %>% 
  select(-ysi_ph, 
         -ysi_do, 
         -ysi_no3, 
         -ysi_sal, 
         -ysi_tur, 
         -flow, 
         -index) %>% 
  mutate(leg = 0)

leg_2_data <- leg_2_data %>% 
  select(-ysi_ph, 
         -ysi_do, 
         -ysi_no3, 
         -ysi_sal, 
         -ysi_tur, 
         -flow,
         -index,
         -comp_time) %>% 
  mutate(leg = 2) %>% 
  mutate(crew = NA, water_temp = NA)




all_legs <- bind_rows(leg_0_data, leg_2_data) %>% 
  mutate(date_time = parse_datetime(paste(date, time_GMT), 
                                    format = "%Y-%m-%d %H:%M:%S")) %>% 
  group_by(leg) %>% 
  mutate(index = 1:n()) %>% 
  ungroup() %>% 
  select(index, crew, date, time_GMT, date_time, everything())
#warnings expected, come from NAs in date and time


##########################################
# manipulating data
##########################################

# removing NAs that make anaylsis impossible

all_legs_small <- all_legs %>% 
  filter(!is.na(date)) %>% 
  arrange(leg,date_time) %>% 
  filter(!is.na(air_pressure))

# taking median value to smooth extreme/absurd values

all_legs_median <- all_legs_small %>% 
  mutate(air_temp = ifelse(air_temp == 0, NA, air_temp),
         air_gas = ifelse(air_gas == 0, NA, air_gas),
         air_humid = ifelse(air_humid == 0, NA, air_humid),
         air_pressure = ifelse(air_pressure == 0, NA, air_pressure)) %>% 
  mutate(time_in_sec = as.numeric(date_time)) %>% 
  mutate(fifteen_sec_chunk = (time_in_sec%/%15)*15) %>% 
  group_by(fifteen_sec_chunk) %>% 
  summarise_at(6:23, median, na.rm = TRUE) %>% 
  mutate(date_time = as_datetime(fifteen_sec_chunk)) %>% 
  mutate(index = 1:n())

# partitioning data into 4 distinct sections to ease mapping

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

st_as_sf(all_legs_med, coords = c("longitude", "latitude"), crs = 4326) %>% 
  mutate(section = ifelse(as.logical(st_within(geometry, section_1_poly, sparse = FALSE)), 1, 0),
         section = ifelse(as.logical(st_within(geometry, section_2_poly, sparse = FALSE)), 2, section),
         section = ifelse(as.logical(st_within(geometry, section_3_poly, sparse = FALSE)), 3, section),
         section = ifelse(as.logical(st_within(geometry, section_4_poly, sparse = FALSE)), 4, section)) %>% 
  st_write("clean_data/clean_legs/clean_data_geom.shp")

miss_basin <- st_read("clean_data/miss_basin/Miss_RiverBasin.shp") %>% 
  st_transform(crs = 4326) %>% 
  rmapshaper::ms_simplify() %>% 
  st_write("clean_data/miss_basin_simple/Miss_RiverBasin.shp")
