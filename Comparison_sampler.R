
library(ggplot2)
library(dplyr)
library(readr)


setwd("/Users/joinz/Desktop")

df <- read_csv("merged_isotope_weather_data.csv")


df$Date <- as.Date(df$Date)


df$Collector <- substr(df$Name, 2, 3)


df$Collector <- recode(df$Collector, "AL" = "UFZ-Sampler", "AJ" = "GUZ-Sampler")


df <- df %>% filter(Type == "D")


date_collector_counts <- df %>% 
  group_by(Date) %>% 
  summarise(UniqueCollectors = n_distinct(Collector))


dates_with_two_samplers <- date_collector_counts %>% 
  filter(UniqueCollectors == 2) %>% 
  pull(Date)


df_filtered <- df %>% 
  filter(Date %in% dates_with_two_samplers) %>% 
  mutate(Date = factor(Date, levels = unique(Date)))  


differences <- df_filtered %>%
  group_by(Date) %>%
  summarise(Diff = delta.018[Collector == "GUZ-Sampler"] - delta.018[Collector == "UFZ-Sampler"])  # GUZ - UFZ


mean_diff <- round(mean(differences$Diff, na.rm = TRUE), 2)


p <- ggplot() +

  geom_point(data = df_filtered, aes(x = Date, y = delta.018, color = Collector), size = 3) +
  

  geom_bar(data = differences, aes(x = Date, y = Diff), stat = "identity", fill = "gray", alpha = 0.5) +
  
  
  geom_text(data = differences, aes(x = Date, y = Diff - 0.1, label = round(Diff, 2)), 
            color = "black", size = 4, vjust = ifelse(differences$Diff >= 0, 1.5, -1)) +
  

  
  

  labs(y = expression(delta^{18}*O ~ " (‰)"), color = "Sampler") +

  theme_minimal() +
  theme(legend.position = "right",
        legend.margin = margin(5, 5, 5, 5),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))  


print(differences)


print(p)