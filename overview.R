library(ggplot2)
library(dplyr)
library(readr)

setwd("/Users/joinz/Desktop")
df <- read_csv("merged_isotope_weather_data.csv")


df$Collector <- substr(df$Name, 2, 3)


collector_labels <- c("AL" = "Automated Sampler UFZ", "AJ" = "Automated Sampler GUZ", "MC" = "Metal collector", "PV" = "Plastic collector 1", "PJ" = "Plastic collector 2")


type_labels <- c("D" = "Daily sample", "W" = "Weekly sample")


p <- ggplot(df, aes(x = as.Date(Date), y = delta.018, color = Collector, shape = Type)) +
  geom_point(size = 3) +
  scale_shape_manual(values = c("D" = 16, "W" = 4), labels = type_labels) + 
  scale_color_manual(values = c("AL" = "orange", "AJ" = "red", "MC" = "green", "PV" = "purple", "PJ" = "blue"),
                     labels = collector_labels) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  labs(y = expression(delta^{18}*O ~ "(‰)"), color = "", shape = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        legend.position = "right",
        plot.margin = margin(10, 10, 10, 10))


print(p)
