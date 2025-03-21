
if (!require("openair")) install.packages("openair", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("viridis")) install.packages("viridis", dependencies = TRUE)

library(openair)
library(ggplot2)
library(dplyr)
library(viridis)

data <- read.csv("/Users/joinz/Desktop/aggregated_daily_data.csv", stringsAsFactors = FALSE)


data$Datum <- as.Date(data$Datum, format = "%Y-%m-%d")

data_2023 <- data %>%
  filter(format(Datum, "%Y") == "2023" & wind_speed_avg > 0)

data_2024 <- data %>%
  filter(format(Datum, "%Y") == "2024")


if (!all(c("wind_speed_avg", "wind_direction_mean") %in% colnames(data))) {
  stop("Error: Columns 'wind_speed_avg' and 'wind_direction_mean' are missing in the dataset.")
}

plot_wind_rose <- function(df, year) {
  if (nrow(df) > 0) {
    windRose(df, 
             ws = "wind_speed_avg", 
             wd = "wind_direction_mean",  
             
             col = plasma(5),  
             grid.line = list(col = "gray80", lwd = 1.2),  
             border = "black",  
             key.position = "right",  
             breaks = c(0, 1, 2, 3, 4),  
             annotate = TRUE,  
             normalise = FALSE,  
             paddle = FALSE,  
             width = 1.5, 
             calm = NULL,  
             par.settings = list(
               fontsize = list(text = 14, points = 12),  
               axis.line = list(col = "gray40", lwd = 1.5)  
             )
    )
  } else {
    message(paste("No data available for", year))
  }
}

plot_wind_rose(data_2023, "2023")
plot_wind_rose(data_2024, "2024")
