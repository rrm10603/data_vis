library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(leaflet)
library(readxl)
library(RColorBrewer)
library(shiny)
library(htmltools)
library(usmap)


# read files 


url <- "https://public.opendatasoft.com/explore/dataset/us-state-boundaries/download/?format=geojson"
states_sf <- sf::st_read(url)


#data files 

#linux path
wonder <-read_excel("/home/robmcneil/Documents/data_vis/vis_project/Provisional Mortality Statistics, 2018 through Last Week (1).xlsx")
#windows path
#wonder <-read_excel("C:/Users/rrm10/OneDrive/Desktop/vis_project/Provisional Mortality Statistics^J 2018 through Last Week (1).xlsx")
wonder <- wonder |>
  select(-Notes, -`Year Code`) |>
  filter(Year != "2024 (provisional") |>
  mutate(`Crude Rate` = as.numeric(ifelse(`Crude Rate` == "Unreliable", NA, `Crude Rate`)))
#linux path
dose <- read.delim("/home/robmcneil/Documents/data_vis/vis_project/cdc_dose.txt")
#windows path 
#dose <- read.delim("C:/Users/rrm10/OneDrive/Desktop/vis_project/cdc_dose.txt")
dose <- dose |>
  mutate(across(starts_with("X"), ~ as.numeric(.))) |>
  `colnames<-`(gsub("^X", "", colnames(dose))) |>
  pivot_longer(cols = -State,               
               names_to = "Year",            
               values_to = "Dose Rate")     
#linux  path
bp<-read.csv("/home/robmcneil/Documents/data_vis/vis_project/State Buprenorphine Dispensing Rates.csv")
#windows path
#bp<-read.csv("C:/Users/rrm10/OneDrive/Desktop/vis_project/State Buprenorphine Dispensing Rates.csv")
#linux path
naloxone <-read.csv("/home/robmcneil/Documents/data_vis/vis_project/State Naloxone Dispensing Rates (1).csv")
#windows path
#naloxone <-read.csv("C:/Users/rrm10/OneDrive/Desktop/vis_project/State Naloxone Dispensing Rates (1).csv")


dose$Year <- as.integer(dose$Year)
wonder$Year <- as.integer(wonder$Year)
bp$YEAR <- as.integer(bp$YEAR)
naloxone$YEAR <- as.integer(naloxone$YEAR)


combined_data <- wonder |>
  left_join(dose, by = c("Residence State" = "State", "Year" = "Year"))|>
  left_join(bp, by = c("Residence State" = "STATE_NAME", "Year" = "YEAR")) |>
  left_join(naloxone, by = c("Residence State" = "STATE_NAME", "Year" = "YEAR"))

#create interention map data

map_data <- combined_data |>
  filter(Year %in% 2019:2023) |>
  select(State = `Residence State`, Year, 
         naloxone_disp = `Naloxone.Dispensing.Rate..per.100.persons.`,
         buprenorphine_disp = `Buprenorphine.Dispensing.Rate..per.100.persons.`,
         cause = `Multiple Cause of death`,
         crude_rate = `Crude Rate`) |>
  filter(!is.na(naloxone_disp) | !is.na(buprenorphine_disp))  |>
  mutate(naloxone_disp = factor(naloxone_disp, 
                                levels = c("<0.2", "0.2 - 0.4", "0.5 - 0.6", ">0.6"),
                                ordered = TRUE),
          buprenorphine_disp = factor(buprenorphine_disp,
                                      levels = c(">7.0","4.8 - 7.0", "<2.8" , "2.8 - 4.7"),
                                      ordered=TRUE))
         


#regionalized facets 

# Make a state -> region lookup
state_region_lookup <- tibble(
  `Residence State` = state.name,
  region = state.region
)


# Join it in
combined_data <- combined_data |>
  left_join(state_region_lookup, by = "Residence State")








#shiny 

ui <- navbarPage("Overdose Visualization Dashboard",
                 
                 tabPanel("State Trends (Faceted)",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("region_select", "Select Region:",
                                          
                              choices = unique(na.omit(combined_data$region)), selected = "South"),
                            checkboxGroupInput("cause_select", "Select Causes of Death:",
                                               choices = unique(combined_data$`Multiple Cause of death`),
                                               selected = c("Heroin", "Methadone",
                                                            "Other synthetic narcotics", "Cocaine",
                                                            "Psychostimulants with abuse potential"))
                          ),
                          mainPanel(
                            plotOutput("overdosePlot", height = "800px")
                          )
                 )
),

tabPanel("Choropleth Map",
         sidebarLayout(
           sidebarPanel(
             sliderInput("year", "Select Year:", min = 2019, max = 2023, value = 2019, sep = ""),
             selectInput("disp_var", "Select Dispensing Rate:", 
                         choices = c("Naloxone" = "naloxone_disp", 
                                     "Buprenorphine" = "buprenorphine_disp"),
                         selected = "naloxone_disp")
           ),
           mainPanel(
             leafletOutput("map", height = 700)
           )
         )
))


server <- function(input, output, session) {
  
  
  ### Plot: State Trends by Region (faceted)
  output$overdosePlot <- renderPlot({
    filtered_data <- combined_data |>
      filter(!is.na(`Crude Rate`),
             region == input$region_select,
             `Multiple Cause of death` %in% input$cause_select,
             Year >= 2018 & Year <= 2023)
    
    ggplot(filtered_data,
           aes(x = Year, y = `Crude Rate`, color = `Multiple Cause of death`)) +
      geom_line(aes(group = interaction(`Residence State`, `Multiple Cause of death`)), alpha = 0.6) +
      geom_point(alpha = 0.7) +
      facet_wrap(~ `Residence State`, ncol = 3) +
      
      scale_x_continuous(breaks = 2018:2023) +
      labs(
        title = paste("Overdose Rates by Cause of Death -", input$region_select),
        x = "Year",
        y = "Crude Rate",
        color = "Cause of Death"
      ) +
      guides(color = guide_legend(nrow = 2)) +
      theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.line = element_line(color = "black", size = 0.5),    # Solid axis lines
      panel.grid.major = element_line(color = "grey90"),         # Light gridlines
      panel.grid.minor = element_blank(),                        # Optional: remove minor gridlines
      axis.ticks = element_line(color = "black") ,                # Solid ticks
      plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
    )
  })
  
  
  ### Reactive: Wide-format map data
  map_data_wide <- reactive({
    map_data |>
      filter(Year == input$year) |>
      select(State, naloxone_disp, buprenorphine_disp, cause, crude_rate) |>
      pivot_wider(names_from = cause, values_from = crude_rate) |>
      distinct()
  })
  
  ### Reactive: Spatial join
  states_map <- reactive({
    states_sf |>
      left_join(map_data_wide(), by = c("name" = "State"))
  })
  
  ### Reactive: Palette
  color_pal <- reactive({
    palette <- if (input$disp_var == "naloxone_disp") "Blues" else "Reds"
    colorFactor(palette = brewer.pal(4, palette), 
                domain = states_map()[[input$disp_var]], ordered = TRUE)
  })
  
  ### Map Output
  output$map <- renderLeaflet({
    leaflet(states_map()) |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = -98.5795, lat = 39.8283, zoom = 3.5) |>
      addPolygons(
        fillColor = ~color_pal()(get(input$disp_var)),
        color = "white",
        weight = 1,
        fillOpacity = 0.7,
        label = ~paste0(
          name, "<br>",
          if (input$disp_var == "naloxone_disp") {
            paste0("Naloxone Rate: ", naloxone_disp)
          } else {
            paste0("Buprenorphine Rate: ", buprenorphine_disp)
          }, "<br><br>",
          "Heroin: ", round(Heroin, 1), "<br>",
          "Methadone: ", round(Methadone, 1), "<br>",
          "Cocaine: ", round(Cocaine, 1), "<br>",
          "Other synthetics: ", round(`Other synthetic narcotics`, 1), "<br>",
          "Psychostimulants: ", round(`Psychostimulants with abuse potential`, 1)
        ) |> lapply(htmltools::HTML),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) |>
      addLegend(
        pal = color_pal(),
        values = ~get(input$disp_var),
        title = if (input$disp_var == "naloxone_disp") {
          "Naloxone Dispensing Rate"
        } else {
          "Buprenorphine Dispensing Rate"
        },
        position = "bottomright"
      )
  })
}


shinyApp(ui = ui, server = server)