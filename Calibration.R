
library(tibble)
library(dplyr)
library(readr)
setwd("//Users/joinz/Desktop")

data <- read_delim("/Users/joinz/Desktop/remeasure_251222.csv", delim = ";") 

colnames(data) <- trimws(colnames(data)) 
print(data)


data <- data %>%
  select(`Inj Nr`, `d(18_16)Mean`, `d(D_H)Mean`, `Identifier 1`) %>%
  rename(
    Inj_Nr = `Inj Nr`,
    d18O_Mean = `d(18_16)Mean`,
    dD_Mean = `d(D_H)Mean`,
    Identifier = `Identifier 1`
  )

filtered_data <- data %>%
  filter(Inj_Nr >= 4 & Inj_Nr <= 7)
print(filtered_data)

grouped_data <- filtered_data %>%
  group_by(Identifier) %>%
  summarise(
    d18O_Mean = mean(d18O_Mean, na.rm = TRUE),
    dD_Mean = mean(dD_Mean, na.rm = TRUE)
  )


reference_values <- data.frame(
  Identifier = c("USGS47", "USGS48"),
  d18O_Ref = c(-19.8, -2.224),
  dD_Ref = c(-150.2, -2.0)
)


standards <- grouped_data %>%
  filter(Identifier %in% c("USGS47", "USGS48")) %>%
  inner_join(reference_values, by = "Identifier")


lm_d18O <- lm(d18O_Ref ~ d18O_Mean, data = standards)
m_d18O <- coef(lm_d18O)[2]  
b_d18O <- coef(lm_d18O)[1]


lm_dD <- lm(dD_Ref ~ dD_Mean, data = standards)
m_dD <- coef(lm_dD)[2]  
b_dD <- coef(lm_dD)[1]  



calibrated_data <- grouped_data %>%
  mutate(
    Calibrated_d18O = round(m_d18O * d18O_Mean + b_d18O, 2),
    Calibrated_dD = round(m_dD * dD_Mean + b_dD, 2)
  ) %>%
  select(Identifier, Calibrated_d18O, Calibrated_dD)


write_csv(calibrated_data, "calibrated_data_241211.csv")


print(calibrated_data)


