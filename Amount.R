
library(ggplot2)
library(dplyr)
library(tidyr)  
library(patchwork)  


data <- read.csv("merged_isotope_weather_data.csv")


data$Date <- as.Date(data$Date, format="%Y-%m-%d")


data$delta.018 <- as.numeric(data$delta.018)  
data$air_temperature_avg <- as.numeric(data$air_temperature_avg)  
data$precipitation_sum_total <- as.numeric(data$precipitation_sum_total)  


data <- data %>% drop_na(delta.018, air_temperature_avg, precipitation_sum_total)


data_daily <- data %>% filter(Type == "D")


model_precip <- lm(delta.018 ~ precipitation_sum_total, data = data)
model_precip_daily <- lm(delta.018 ~ precipitation_sum_total, data = data_daily)

summary_precip <- summary(model_precip)
summary_precip_daily <- summary(model_precip_daily)


slope_precip <- round(coef(model_precip)[2], 3)
intercept_precip <- round(coef(model_precip)[1], 3)
r2_precip <- round(summary_precip$r.squared, 3)
n_precip <- nrow(data)

slope_precip_daily <- round(coef(model_precip_daily)[2], 3)
intercept_precip_daily <- round(coef(model_precip_daily)[1], 3)
r2_precip_daily <- round(summary_precip_daily$r.squared, 3)
n_precip_daily <- nrow(data_daily)


eq_precip <- substitute(δ^18*O == a * P ~ b ~~ "," ~~ R^2 == r2 ~~ ", n =" ~ n,
                        list(a = paste(slope_precip),
                             b = paste(intercept_precip),
                             r2 = paste(r2_precip),
                             n = paste(n_precip)))

eq_precip_daily <- substitute(δ^18*O[D] == a * P ~ b ~~ "," ~~ R^2 == r2 ~~ ", n =" ~ n,
                              list(a = paste(slope_precip_daily),
                                   b = paste(intercept_precip_daily),
                                   r2 = paste(r2_precip_daily),
                                   n = paste(n_precip_daily)))


plot_precip <- ggplot(data, aes(x = precipitation_sum_total, y = delta.018)) +
  geom_point(color = "blue", size = 2) +  
  geom_smooth(method = "lm", se = TRUE, color = "black") +  
  labs(
    x = "Precipitation Amount (mm)",
    y = expression(delta^18*"O (‰)")
  ) +
  annotate("text", x = min(data$precipitation_sum_total, na.rm = TRUE), 
           y = max(data$delta.018, na.rm = TRUE), label = as.character(as.expression(eq_precip)), 
           parse = TRUE, hjust = 0, vjust = 1, size = 5) +
  theme_minimal()


plot_precip_daily <- ggplot(data_daily, aes(x = precipitation_sum_total, y = delta.018)) +
  geom_point(color = "red", size = 2) +  
  geom_smooth(method = "lm", se = TRUE, color = "black") +  
  labs(
    x = "Precipitation Amount (mm)",
    y = expression(delta^18*"O (‰)")
  ) +
  annotate("text", x = min(data_daily$precipitation_sum_total, na.rm = TRUE), 
           y = max(data_daily$delta.018, na.rm = TRUE), label = as.character(as.expression(eq_precip_daily)), 
           parse = TRUE, hjust = 0, vjust = 1, size = 5) +
  theme_minimal()


print(plot_precip)
print(plot_precip_daily)