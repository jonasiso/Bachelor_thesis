
library(dplyr)
library(lubridate)
library(ggplot2)


df_main <- read.csv("merged_isotope_weather_data.csv")
df_agg <- read.csv("aggregated_daily_data.csv")


df_main$Date <- as.Date(df_main$Date)
df_agg$Datum <- as.Date(df_agg$Datum)


df_filtered <- df_main %>%
  filter(precipitation_sum_total > 0, Type == "D")  


df_filtered <- df_filtered %>%
  mutate(week = isoweek(Date), year = year(Date))


df_weighted <- df_filtered %>%
  group_by(year, week) %>%
  summarise(
    count = n(),  
    weighted_delta018 = sum(delta.018 * precipitation_sum_total) / sum(precipitation_sum_total),
    .groups = 'drop'
  ) %>%
  filter(count >= 2)  


df_temp <- df_agg %>%
  mutate(week = isoweek(Datum), year = year(Datum)) %>%
  group_by(year, week) %>%
  summarise(avg_air_temp = mean(air_temperature_avg, na.rm = TRUE), .groups = 'drop')


df_final <- merge(df_weighted, df_temp, by = c("year", "week"))


model <- lm(weighted_delta018 ~ avg_air_temp, data = df_final)
summary_model <- summary(model)


slope <- round(coef(model)[2], 3)  
intercept <- round(coef(model)[1], 3)  
r2 <- round(summary_model$r.squared, 3)  
p_value <- round(summary_model$coefficients[2, 4], 3)  
n <- nrow(df_final)  


regression_text <- paste0("δ18O =", slope, "T", intercept,
", R² = ", r2, ", P = 0.00009", ", n = ", n)


print("Used calendar weeks (year-week):")
print(df_final %>% select(year, week) %>% arrange(year, week))


ggplot(df_final, aes(x = avg_air_temp, y = weighted_delta018)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  annotate("text", x = max(df_final$avg_air_temp) - 2, 
           y = min(df_final$weighted_delta018) + 1, 
           label = regression_text, color = "black", hjust = 0.89, size = 5) +
  labs(
       x = "Average Air Temperature (°C)",
       y = expression(delta^{18}*O ~ " (‰)")) +
  theme_minimal()
print(summary_model)
