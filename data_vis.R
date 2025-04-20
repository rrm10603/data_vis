library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(leaflet)
library(readxl)
library(ggstream)



# read files 

url <- "https://public.opendatasoft.com/explore/dataset/us-state-boundaries/download/?format=geojson"
states_sf <- sf::st_read(url)

#data files 

#linux path
#wonder <-read_excel("/home/robmcneil/Documents/data_vis/vis_project/Provisional Mortality Statistics, 2018 through Last Week (1).xlsx")
#windows path
wonder <-read_excel("C:/Users/rrm10/OneDrive/Desktop/vis_project/Provisional Mortality Statistics^J 2018 through Last Week (1).xlsx")
wonder <- wonder |>
  select(-Notes, -`Year Code`) |>
  filter(Year != "2024 (provisional") |>
  mutate(`Crude Rate` = as.numeric(ifelse(`Crude Rate` == "Unreliable", NA, `Crude Rate`)))
#linux path
#dose <- read.delim("/home/robmcneil/Documents/data_vis/vis_project/cdc_dose.txt")
#windows path 
dose <- read.delim("C:/Users/rrm10/OneDrive/Desktop/vis_project/cdc_dose.txt")
dose <- dose |>
  mutate(across(starts_with("X"), ~ as.numeric(.))) |>
  `colnames<-`(gsub("^X", "", colnames(dose))) |>
  pivot_longer(cols = -State,               
               names_to = "Year",            
               values_to = "Dose Rate")     
#linux  path
#bp<-read.csv("/home/robmcneil/Documents/data_vis/vis_project/State Buprenorphine Dispensing Rates.csv")
#windows path
bp<-read.csv("C:/Users/rrm10/OneDrive/Desktop/vis_project/State Buprenorphine Dispensing Rates.csv")
#linux path
#naloxone <-read.csv("/home/robmcneil/Documents/data_vis/vis_project/State Naloxone Dispensing Rates (1).csv")
#windows path
naloxone <-read.csv("C:/Users/rrm10/OneDrive/Desktop/vis_project/State Naloxone Dispensing Rates (1).csv")


dose$Year <- as.integer(dose$Year)
wonder$Year <- as.integer(wonder$Year)
bp$YEAR <- as.integer(bp$YEAR)
naloxone$YEAR <- as.integer(naloxone$YEAR)


combined_data <- wonder |>
  left_join(dose, by = c("Residence State" = "State", "Year" = "Year"))|>
  left_join(bp, by = c("Residence State" = "STATE_NAME", "Year" = "YEAR")) |>
  left_join(naloxone, by = c("Residence State" = "STATE_NAME", "Year" = "YEAR"))

#create national summary data frame 

national <- combined_data|>
  filter(Year %in% 2019:2023) |>
  group_by(Year, `Multiple Cause of death`) |>
  summarize(
    total_deaths = sum(Deaths, na.rm = TRUE),
    total_population = sum(Population, na.rm = TRUE),
    crude_rate = (total_deaths / total_population) * 100000,
    bupe_disp_rate_100 = first(buprenorphine_dispensing_rate) ,
    nalox_disp_rate_100 = first(naloxone_dispensing_rate) ,
    .groups = "drop"
  )

#for chloropleth: 
#want crude rate dose rate bp.dispensing rate..per.100, nx dispensing per 100 persons 

unique(combined_data$`Multiple Cause of death`)


#line plot 

ggplot(combined_data %>%
         filter(!is.na(`Crude Rate`),
                `Multiple Cause of death` %in% c("Heroin", "Methadone",
                                                 "Other synthetic narcotics", "Cocaine",
                                                 "Psychostimulants with abuse potential"),
                Year >= 2018 & Year <= 2023),
       aes(x = Year, y = `Crude Rate`, color = `Multiple Cause of death`)) +
  geom_line(aes(group = interaction(`Residence State`, `Multiple Cause of death`)), alpha = 0.5) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Crude Rate by Cause of Death (2018–2023)",
    x = "Year",
    y = "Crude Rate",
    color = "Cause of Death"
  ) +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = 2018:2023) +
  theme(legend.position = "bottom")


#ggplot with national data 

ggplot(national, aes(x = Year, y = crude_rate, group = `Multiple Cause of death`, color = `Multiple Cause of death`)) +
  geom_line(size = 1) +  # Plot the line for each cause of death over time
  geom_point(size = 2) +  # Add points at each data point
  labs(
    x = "Year",
    y = "Crude Overdose Rate (per 100,000)",
    title = "Crude Overdose Rates by Cause of Death (2019–2023)"
  ) +
  theme_minimal() +  # Clean theme
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    legend.title = element_blank()  # Remove legend title
  )






