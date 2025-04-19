library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(leaflet)
library(readxl)



# read files 

url <- "https://public.opendatasoft.com/explore/dataset/us-state-boundaries/download/?format=geojson"
states_sf <- sf::st_read(url)

#data files 

wonder <-read_excel("/home/robmcneil/Documents/data_vis/vis_project/Provisional Mortality Statistics, 2018 through Last Week (1).xlsx")
wonder <- wonder |>
  select(-Notes,'Year Code')|>
  filter(Year != "2024 (provisional")|>
  mutate('Crude Rate' = as.numeric('Crude Rate'))
dose <- read.delim("/home/robmcneil/Documents/data_vis/vis_project/cdc_dose.txt")
dose <- dose |>
  mutate(across(starts_with("X"), ~ as.numeric(.))) |>
  `colnames<-`(gsub("^X", "", colnames(dose))) |>
  pivot_longer(cols = -State,               
               names_to = "Year",            
               values_to = "Dose Rate")     

bp<-read.csv("/home/robmcneil/Documents/data_vis/vis_project/State Buprenorphine Dispensing Rates.csv")
naloxone <-read.csv("/home/robmcneil/Documents/data_vis/vis_project/State Naloxone Dispensing Rates (1).csv")


dose$Year <- as.integer(dose$Year)
wonder$Year <- as.integer(wonder$Year)
bp$YEAR <- as.integer(bp$YEAR)
naloxone$YEAR <- as.integer(naloxone$YEAR)


combined_data <- wonder |>
  left_join(dose, by = c("Residence State" = "State", "Year" = "Year"))|>
  left_join(bp, by = c("Residence State" = "STATE_NAME", "Year" = "YEAR")) |>
  left_join(naloxone, by = c("Residence State" = "STATE_NAME", "Year" = "YEAR"))











