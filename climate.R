library(ggplot2)
library(dplyr)
library(lubridate)
setwd("/Users/joinz/Desktop")
data <- read.csv("aggregated_daily_data.csv")


data$Datum <- as.Date(data$Datum, format="%Y-%m-%d")


data_filtered <- data %>% 
  filter(year(Datum) %in% c(2023, 2024))


data_filtered <- data_filtered %>% 
  group_by(year(Datum), month(Datum)) %>% 
  mutate(
    air_temperature_avg = ifelse(is.na(air_temperature_avg),
                                 mean(air_temperature_avg, na.rm = TRUE),
                                 air_temperature_avg),
    precipitation_sum_total = ifelse(is.na(precipitation_sum_total),
                                     mean(precipitation_sum_total, na.rm = TRUE),
                                     precipitation_sum_total)
  ) %>% 
  ungroup()


climate_data <- data_filtered %>% 
  group_by(Year = year(Datum), Month = month(Datum, label = TRUE)) %>% 
  summarize(
    avg_temp = mean(air_temperature_avg, na.rm = TRUE),
    total_precip = sum(precipitation_sum_total, na.rm = TRUE)
  )


total_precip <- data_filtered %>% group_by(Year = year(Datum)) %>% 
  summarize(total_precip = sum(precipitation_sum_total, na.rm = TRUE),
            mean_temp = mean(air_temperature_avg, na.rm = TRUE))


limits_2024 <- climate_data %>% filter(Year == 2024)
y_temp_max <- max(limits_2024$avg_temp) + 40
y_temp_min <- min(limits_2024$avg_temp) - 2
y_precip_max <- max(limits_2024$total_precip) / 3


plot_list <- lapply(unique(climate_data$Year), function(year) {
  df_year <- climate_data %>% filter(Year == year)
  total_precip_year <- total_precip %>% filter(Year == year)
  
  ggplot(df_year, aes(x = Month)) +
    geom_bar(aes(y = total_precip / 3), stat = "identity", fill = "blue", alpha = 0.7) +
    geom_line(aes(y = avg_temp, group = Year), color = "red", size = 1) +
    scale_y_continuous(name = "Temperature (°C)", sec.axis = sec_axis(~.*3, name = "Precipitation (mm)")) +
    coord_cartesian(ylim = c(y_temp_min, y_temp_max)) +
    labs( x = year) +
    theme_minimal() +
    annotate("text", x = 11.5, y = y_temp_max - 1, label = paste0("Total Precipitation: ", round(total_precip_year$total_precip, 1), " mm"), hjust = 1, vjust = 1) +
    annotate("text", x = 11.5, y = y_temp_max - 5, label = paste0("Mean Temperature: ", round(total_precip_year$mean_temp, 1), " °C"), hjust = 1, vjust = 1)
})


print(plot_list[[1]])
print(plot_list[[2]])
