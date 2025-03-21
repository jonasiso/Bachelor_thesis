library(ggplot2)
library(dplyr)
library(tidyr)


data <- read.csv("merged_isotope_weather_data.csv")


data <- data %>%
  mutate(
    Date = as.Date(Date, format="%Y-%m-%d"),
    delta.018 = as.numeric(delta.018),
    relative_humidity_avg = as.numeric(relative_humidity_avg),
    absolute_humidity = as.numeric(absolute_humidity)
  )


model_rh <- lm(delta.018 ~ relative_humidity_avg, data = data)
model_ah <- lm(delta.018 ~ absolute_humidity, data = data)


slope_rh <- round(coef(model_rh)[2], 2)
intercept_rh <- round(coef(model_rh)[1], 2)
r2_rh <- round(summary(model_rh)$r.squared, 3)
n_rh <- nrow(data)

slope_ah <- round(coef(model_ah)[2], 2)
intercept_ah <- round(coef(model_ah)[1], 2)
r2_ah <- round(summary(model_ah)$r.squared, 3)
n_ah <- nrow(data)


eq_rh <- paste0("Relative Humidity: Slope = ", slope_rh, ", R² = ", r2_rh, ", n = ", n_rh)
eq_ah <- paste0("Absolute Humidity: Slope = ", slope_ah, ", R² = ", r2_ah, ", n = ", n_ah)


scale_factor <- (max(data$relative_humidity_avg) - min(data$relative_humidity_avg)) / 
  (max(data$absolute_humidity) - min(data$absolute_humidity))


plot <- ggplot(data) +
  
  geom_point(aes(x = relative_humidity_avg, y = delta.018), color = "blue", alpha = 0.7, size = 2) +
  geom_smooth(aes(x = relative_humidity_avg, y = delta.018), method = "lm", color = "blue", se = TRUE, linewidth = 1) +
  
 
  geom_point(aes(x = absolute_humidity * scale_factor + min(relative_humidity_avg), y = delta.018), 
             color = "red", alpha = 0.7, size = 2) +
  geom_smooth(aes(x = absolute_humidity * scale_factor + min(relative_humidity_avg), y = delta.018), 
              method = "lm", color = "red", se = TRUE, linewidth = 1) +
  
  
  annotate("text", x = min(data$relative_humidity_avg) + 5, 
           y = max(data$delta.018) - 2, 
           label = eq_ah, color = "red", hjust = 0.1, size = 5) +
  

  annotate("text", x = max(data$relative_humidity_avg) - 5, 
           y = min(data$delta.018) + 2, 
           label = eq_rh, color = "blue", hjust = 0.75, size = 5) +
  

  scale_x_continuous(
    name = "Relative Humidity (%)",
    sec.axis = sec_axis(~ (. - min(data$relative_humidity_avg)) / scale_factor, 
                        name = "Absolute Humidity (g/m³)")
  ) +
  
  labs(
    y = expression(delta^{18}*O ~ " (‰)")) +
  
  theme_minimal()
summary_ah<- summary(model_ah)
summary_rh<- summary(model_rh)

print(plot)
print(summary_ah)
print(summary_rh)