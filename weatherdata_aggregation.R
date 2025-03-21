
library(dplyr)
library(readr)


weather_folder <- "/Users/joinz/Desktop/weather_data"  
header_path <- "/Users/joinz/Desktop/weather_data/header.csv"  


header <- read.csv(header_path, header = FALSE, sep = ";", stringsAsFactors = FALSE)
colnames(header) <- trimws(header[1, ])  # Extract column names
header <- unlist(strsplit(header[1, 1], ","))  # Extract column names from header


weather_files <- list.files(path = weather_folder, pattern = "*.txt", full.names = TRUE)

combined_weather_data <- lapply(weather_files, function(file) {
  weather_data <- read_delim(file, delim = ";", col_names = FALSE, trim_ws = TRUE)
  colnames(weather_data) <- header  
  weather_data
}) %>%
  bind_rows()  


combined_weather_data <- combined_weather_data %>%
  mutate(
    Datum = as.Date(substr(date_time, 1, 10), format = "%d.%m.%y"), 
    U = wind_speed * sin(wind_direction * pi / 180),  
    V = wind_speed * cos(wind_direction * pi / 180)   
  )


aggregated_data <- combined_weather_data %>%
  group_by(Datum) %>%
  summarise(
   
    air_temperature_avg = mean(air_temperature, na.rm = TRUE),
    relative_humidity_avg = mean(relative_humidity, na.rm = TRUE),
    wind_speed_avg = mean(wind_speed, na.rm = TRUE),
    air_pressure_avg = mean(absolute_air_pressure, na.rm = TRUE),
   
    
   
    U_avg = mean(U, na.rm = TRUE),
    V_avg = mean(V, na.rm = TRUE),
    wind_direction_mean = (atan2(U_avg, V_avg) * 180 / pi) %% 360,  
    
  
    brightness_N_avg = mean(brightness_N, na.rm = TRUE),
    brightness_E_avg = mean(brightness_E, na.rm = TRUE),
    brightness_S_avg = mean(brightness_S, na.rm = TRUE),
    brightness_W_avg = mean(brightness_W, na.rm = TRUE),
    

    precipitation_intensity_avg = mean(precipitation_instantaneous_intensity_radar, na.rm = TRUE),
    precipitation_sum_total = sum(precipitation_sum_radar, na.rm = TRUE),
    precipitation_intensity_max = max(precipitation_instantaneous_intensity_radar, na.rm = TRUE),
    
   
    brightness_max = max(brightness_max, na.rm = TRUE),
    air_temperature_max = max(air_temperature, na.rm = TRUE),
    wind_speed_max = max(wind_speed, na.rm = TRUE),
    relative_humidity_max = max(relative_humidity, na.rm = TRUE),
    
    
    air_temperature_avg_precip = mean(air_temperature[precipitation_sum_radar > 0], na.rm = TRUE)
  )


write.csv(aggregated_data, "aggregated_daily_data.csv", row.names = FALSE)


print(head(aggregated_data))