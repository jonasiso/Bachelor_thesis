

library(dplyr)
library(readr)
library(tidyr)
setwd("/Users/joinz/Desktop")

aggregated_weather_path <- "/Users/joinz/Desktop/aggregated_daily_data.csv"  
isotope_data_path <- "/Users/joinz/Desktop/BA/Gefilterte_Isotopiedaten_ohne_bestimmte_Eintr_ge.csv"  

aggregated_weather <- read.csv(aggregated_weather_path)
isotope_data <- read.csv(isotope_data_path, stringsAsFactors = FALSE, sep = ";")


aggregated_weather <- aggregated_weather %>%
  mutate(Datum = as.Date(Datum))


isotope_data <- isotope_data %>%
  mutate(
    Date = as.Date(substr(Name, 4, 9), format = "%y%m%d"), 
    Type = substr(Name, 1, 1)  
  )


daily_isotopes <- isotope_data %>%
  filter(Type == "D") %>%
  left_join(aggregated_weather, by = c("Date" = "Datum"))


weekly_isotopes <- isotope_data %>%
  filter(Type == "W")

weekly_weather <- weekly_isotopes %>%
  rowwise() %>%
  mutate(
    WeeklyWeather = list(
      aggregated_weather %>%
        filter(Datum >= Date & Datum <= Date + 6) %>%
        summarise(
          air_temperature_avg = mean(air_temperature_avg, na.rm = TRUE),
          relative_humidity_avg = mean(relative_humidity_avg, na.rm = TRUE),
          wind_speed_avg = mean(wind_speed_avg, na.rm = TRUE),
          air_pressure_avg = mean(air_pressure_avg, na.rm = TRUE),
          brightness_max = max(brightness_max, na.rm = TRUE),
          precipitation_sum_total = sum(precipitation_sum_total, na.rm = TRUE),
          precipitation_intensity_avg = mean(precipitation_intensity_avg, na.rm = TRUE),
          air_temperature_avg_precip = mean(air_temperature_avg_precip, na.rm = TRUE),
          
          # Corrected Weekly Wind Direction Calculation
          U_avg = mean(wind_speed_avg * cos(wind_direction_mean * pi / 180), na.rm = TRUE),
          V_avg = mean(wind_speed_avg * sin(wind_direction_mean * pi / 180), na.rm = TRUE),
          wind_direction_mean = (atan2(U_avg, V_avg) * 180 / pi + 360) %% 360  
        )
    )
  ) %>%
  unnest(cols = c(WeeklyWeather))  


merged_data <- bind_rows(daily_isotopes, weekly_weather)


write.csv(merged_data, "merged_isotope_weather_data.csv", row.names = FALSE)


print(head(merged_data))