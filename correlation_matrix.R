
library(ggplot2)
library(ggcorrplot)
library(psych)
library(dplyr)


data <- read.csv("merged_isotope_weather_data.csv")


data$brightness_avg <- rowSums(data[, c("brightness_N_avg", "brightness_E_avg", "brightness_S_avg", "brightness_W_avg")], na.rm = TRUE)


calculate_absolute_humidity <- function(temp_C, rh_percent) {
  rh <- rh_percent / 100  
  es <- 6.112 * exp((17.67 * temp_C) / (temp_C + 243.5))  
  ah <- (217 * rh * es) / (temp_C + 273.15)  
  return(ah)
}


data$absolute_humidity <- mapply(calculate_absolute_humidity, data$air_temperature_avg, data$relative_humidity_avg)


correlation_vars <- c("delta.DH", "delta.018", "air_temperature_avg", 
                      "relative_humidity_avg", "wind_direction_mean", 
                      "precipitation_sum_total", "air_pressure_avg",
                      "absolute_humidity")  


correlation_data <- data[, correlation_vars]


cor_matrix <- cor(correlation_data, use = "pairwise.complete.obs", method = "pearson")


cor_test <- corr.test(correlation_data, use = "pairwise.complete.obs", method = "pearson")
p_values <- cor_test$p


custom_names <- c(expression(delta^18*"O"), expression(delta^2*"H"), "Temperature", 
                  "Relative Humidity", "Wind Direction", "Precipitation amount", 
                  "Air Pressure", "Absolute Humidity")  

names(custom_names) <- correlation_vars  


ggcorrplot(cor_matrix, method = "circle", type = "lower", 
           lab = TRUE, p.mat = p_values, sig.level = 0.05, insig = "blank",
           colors = c("blue", "white", "red")) +
  scale_x_discrete(labels = custom_names) +  
  scale_y_discrete(labels = custom_names) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.title.x = element_blank(),  
        axis.title.y = element_blank())  