# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(mgcv)
library(lubridate)
library(gridExtra)
library(nlme)  


data <- read.csv("merged_isotope_weather_data.csv")


data$Date <- as.Date(data$Date, format="%Y-%m-%d")


data <- data %>% distinct(Date, .keep_all = TRUE)


data$delta.018 <- as.numeric(data$delta.018)
data$air_temperature_avg <- as.numeric(data$air_temperature_avg)


data <- data %>% drop_na(delta.018, air_temperature_avg)


data$DOY <- yday(data$Date)


gam_temp <- gam(air_temperature_avg ~ s(DOY, bs="cc", k=20), data=data, method="REML")
gam_delta_temp <- gam(delta.018 ~ s(air_temperature_avg, bs="cs", k=10), data=data, method="REML")
gam_delta_season <- gam(delta.018 ~ s(DOY, bs="cc", k=20), data=data, method="REML")


data$delta.018_resid <- residuals(gam_delta_temp)


gam_delta_resid_season <- gamm(delta.018_resid ~ s(DOY, bs="cc", k=20),
                               data = data,
                               correlation = corAR1(form = ~ as.numeric(Date)))


r2_temp <- round(summary(gam_temp)$r.sq, 3)
r2_delta_temp <- round(summary(gam_delta_temp)$r.sq, 3)
r2_delta_season <- round(summary(gam_delta_season)$r.sq, 3)
r2_delta_resid_season <- round(summary(gam_delta_resid_season$gam)$r.sq, 3)


predicted_delta_resid_season <- data.frame(DOY = 1:365)
delta_resid_season_pred <- predict(gam_delta_resid_season$gam, newdata=predicted_delta_resid_season, type="response", se.fit=TRUE)
predicted_delta_resid_season$Delta018_resid <- delta_resid_season_pred$fit
predicted_delta_resid_season$Lower <- delta_resid_season_pred$fit - 1.96 * delta_resid_season_pred$se.fit
predicted_delta_resid_season$Upper <- delta_resid_season_pred$fit + 1.96 * delta_resid_season_pred$se.fit


plot_temp_seasonality <- ggplot() +
  geom_point(data=data, aes(x=DOY, y=air_temperature_avg), color="darkblue", size=2, alpha=0.5) +
  geom_smooth(data=data, aes(x=DOY, y=air_temperature_avg), method="gam", formula = y ~ s(x, bs="cc", k=20), color="red") +
  labs(x="Day of Year", y="Temperature (°C)") +
  annotate("text", x=max(data$DOY) * 0.85, y=max(data$air_temperature_avg) * 0.85, 
           label=paste("R² =", r2_temp), size=5, fontface="bold") +
  theme_minimal()

plot_delta_temp <- ggplot() +
  geom_point(data=data, aes(x=air_temperature_avg, y=delta.018), color="darkblue", size=2, alpha=0.5) +
  geom_smooth(data=data, aes(x=air_temperature_avg, y=delta.018), method="gam", formula = y ~ s(x, bs="cs", k=10), color="red") +
  labs(x="Average Air Temperature (°C)", y=expression(delta^18*"O (‰)")) +
  annotate("text", x=max(data$air_temperature_avg) * 0.85, y=max(data$delta.018) * 0.85, 
           label=paste("R² =", r2_delta_temp), size=5, fontface="bold") +
  theme_minimal()

plot_delta_seasonality <- ggplot() +
  geom_point(data=data, aes(x=DOY, y=delta.018), color="darkblue", size=2, alpha=0.5) +
  geom_smooth(data=data, aes(x=DOY, y=delta.018), method="gam", formula = y ~ s(x, bs="cc", k=20), color="red") +
  labs(x="Day of Year", y=expression(delta^18*"O (‰)")) +
  annotate("text", x=max(data$DOY) * 0.85, y=max(data$delta.018) * 0.85, 
           label=paste("R² =", r2_delta_season), size=5, fontface="bold") +
  theme_minimal()

plot_delta_resid_seasonality <- ggplot() +
  geom_point(data=data, aes(x=DOY, y=delta.018_resid), color="darkblue", size=2, alpha=0.5) +
  geom_smooth(data=data, aes(x=DOY, y=delta.018_resid), method="gam", formula = y ~ s(x, bs="cc", k=20), color="red") +
  labs(x="Day of Year", y=expression("Residual " ~ delta^18*"O (‰)")) +
  annotate("text", x=max(data$DOY) * 0.85, y=max(data$delta.018_resid) * 0.85, 
           label=paste("R² =", r2_delta_resid_season), size=5, fontface="bold") +
  theme_minimal()


grid.arrange(plot_temp_seasonality, plot_delta_temp, plot_delta_seasonality, plot_delta_resid_seasonality, ncol=2, nrow=2)