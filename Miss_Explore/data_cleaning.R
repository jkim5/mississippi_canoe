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

# days while walking / time out of river
nola_leg <- interval(ymd_hms("2019-11-09 00:00:00"),
                 ymd_hms("2019-11-17 23:59:59"))

st_louis_leg <- interval(ymd_hms("2019-09-27 00:00:00"),
                 ymd_hms("2019-10-07 23:59:59"))

highway_leg <- interval(ymd_hms("2019-10-11 14:33:30"),
                 ymd_hms("2019-10-11 19:37:00"))

all_legs_small <- all_legs %>% 
  filter(!is.na(date)) %>% 
  arrange(leg,date_time) %>% 
  filter(!is.na(air_pressure)) %>% 
  filter(!(date_time %within% nola_leg),
         !(date_time %within% st_louis_leg),
         !(date_time %within% highway_leg))

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

st_as_sf(all_legs_median, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_write("clean_data/clean_legs/clean_data_geom.shp", delete_layer = TRUE)

miss_basin <- st_read("clean_data/miss_basin/Miss_RiverBasin.shp") %>% 
  st_transform(crs = 4326) %>% 
  rmapshaper::ms_simplify() %>% 
  st_write("clean_data/miss_basin_simple/Miss_RiverBasin.shp", delete_layer = TRUE)

date_time_data <- all_legs_median %>% 
  select(date_time) %>% 
  readr::write_csv("clean_data/date_time_data.csv")
