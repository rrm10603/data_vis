library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(leaflet)
library(readxl)
library(RColorBrewer)
library(shiny)
library(htmltools)


#outputs the map but no hover 



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

ggplot(combined_data |>
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


#regionalized facets 

# Make a state -> region lookup
state_region_lookup <- tibble(
  `Residence State` = state.name,
  region = state.region
)


# Join it in
combined_data <- combined_data |>
  left_join(state_region_lookup, by = "Residence State")



ggplot(combined_data |>
         filter(!is.na(`Crude Rate`),
                region == "West",
                `Multiple Cause of death` %in% c("Heroin", "Methadone",
                                                 "Other synthetic narcotics", "Cocaine",
                                                 "Psychostimulants with abuse potential"),
                Year >= 2018 & Year <= 2023),
       aes(x = Year, y = `Crude Rate`, color = `Multiple Cause of death`)) +
  geom_line(aes(group = interaction(`Residence State`, `Multiple Cause of death`)), alpha = 0.6) +
  geom_point(alpha = 0.7) +
  facet_wrap(~ `Residence State`) +
  labs(
    title = "Crude Overdose Rate by Cause of Death (Faceted by State - West Region)",
    x = "Year",
    y = "Crude Rate",
    color = "Cause of Death"
  ) +
  theme_minimal(base_size = 12) +
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


## chloropleth 

# Prepare data for plotting
map_data <- combined_data %>%
  filter(Year == 2019) %>%
  select(State = `Residence State`, naloxone_disp = `Naloxone.Dispensing.Rate..per.100.persons.`,
         bupe_disp = `Buprenorphine.Dispensing.Rate..per.100.persons.`,
         cause = `Multiple Cause of death`, crude_rate = `Crude Rate`,Year) |>
  filter(!is.na(naloxone_disp) | !is.na(bupe_disp))



#may need wide format for this to work 


# Join spatial data
states_map <- states_sf |>
  left_join(map_data, by = c("name" = "State"))



pal <- colorFactor(
  palette = brewer.pal(4, "Blues"),  # 4 categories
  domain = states_map$naloxone_disp,
  ordered = TRUE
)

leaflet(states_map) |>
  addProviderTiles("CartoDB.Positron") |>
  addPolygons(
    fillColor = ~pal(naloxone_disp),
    color = "white",
    weight = 1,
    fillOpacity = 0.7,
    label = ~paste0(
      name, "<br>",
      "Naloxone Rate: ", naloxone_disp, "<br>",
      "Cause: ", cause, "<br>",
      "Crude Rate: ", crude_rate
    ),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  ) |>
  addLegend(
    pal = pal,
    values = ~naloxone_disp,
    title = "Naloxone Dispensing Rate",
    position = "bottomright"
  )





##############


map_data_wide <- map_data |>
  filter(Year == 2019) |>
  select(State, naloxone_disp, cause, crude_rate) |>
  pivot_wider(
    names_from = cause,
    values_from = crude_rate
  ) |>
  distinct()

# Now join to spatial
states_map <- states_sf |>
  left_join(map_data_wide, by = c("name" = "State"))



leaflet(states_map) |>
  addProviderTiles("CartoDB.Positron") |>
  addPolygons(
    fillColor = ~pal(naloxone_disp),
    color = "white",
    weight = 1,
    fillOpacity = 0.7,
    label = ~paste0(
      name, "<br>",
      "Naloxone Rate: ", naloxone_disp, "<br><br>",
      "Heroin: ", round(Heroin, 1), "<br>",
      "Methadone: ", round(Methadone, 1), "<br>",
      "Cocaine: ", round(Cocaine, 1), "<br>",
      "Other synthetics: ", round(`Other synthetic narcotics`, 1), "<br>",
      "Psychostimulants: ", round(`Psychostimulants with abuse potential`, 1)
    ) |>
      lapply(htmltools::HTML),
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#666",
      fillOpacity = 0.9,
      bringToFront = TRUE
    )
  ) |>
  addLegend(
    pal = pal,
    values = ~naloxone_disp,
    title = "Naloxone Dispensing Rate",
    position = "bottomright"
  )







#shiny 


# Shiny UI
ui <- navbarPage("Overdose Visualization Dashboard",
                 
                 tabPanel("State Trends (Faceted)",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("region_select", "Select Region:",
                                          choices = unique(combined_data$region), selected = "West"),
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
                                                      "Buprenorphine" = "bupe_disp"),
                                          selected = "naloxone_disp")
                            ),
                            mainPanel(
                              plotlyOutput("usMap", height = 700)
                            )
                          )
                 )
)

# Server function
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
      facet_wrap(~ `Residence State`) +
      labs(
        title = paste("Overdose Rates by Cause of Death -", input$region_select),
        x = "Year",
        y = "Crude Rate",
        color = "Cause of Death"
      ) +
      theme_minimal(base_size = 12) +
      scale_x_continuous(breaks = 2018:2023) +
      theme(legend.position = "bottom")
  })
  
  ### Reactive: Wide-format map data (for year and dispensing variable)
  map_data_wide <- reactive({
    map_data |>
      filter(Year == input$year, cause %in% input$disp_var) |>
      select(State, naloxone_disp, bupe_disp, cause, crude_rate) |>
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
  
  ### Map Output (Plotly Choropleth)
  output$usMap <- renderPlotly({
    # Dynamically select data based on the selected dispensing rate
    map_data_to_use <- map_data %>%
      mutate(disp_rate = ifelse(input$disp_var == "naloxone_disp", naloxone_disp, bupe_disp)) %>%
      mutate(disp_rate = as.numeric(disp_rate)) %>%
      filter(!is.na(disp_rate))  # Remove rows where dispensing rate is NA
    
    # Create Plotly Choropleth map
    plot_ly(
      data = map_data_to_use,
      type = "choropleth",
      locationmode = "USA-states",
      locations = ~State,
      z = ~disp_rate,  # Color intensity based on dispensing rate
      text = ~paste("State:", State, "<br>",
                    "Dispensing Rate: ", round(disp_rate, 2), "<br>",
                    "Heroin: ", round(crude_rate[cause == "Heroin"], 2), "<br>",
                    "Methadone: ", round(crude_rate[cause == "Methadone"], 2), "<br>",
                    "Cocaine: ", round(crude_rate[cause == "Cocaine"], 2), "<br>",
                    "Other synthetic narcotics: ", round(crude_rate[cause == "Other synthetic narcotics"], 2), "<br>",
                    "Psychostimulants with abuse potential: ", round(crude_rate[cause == "Psychostimulants with abuse potential"], 2)),
      colorbar = list(title = ifelse(input$disp_var == "naloxone_disp", "Naloxone Rate", "Buprenorphine Rate")),
      colorscale = "Viridis"
    ) %>%
      layout(
        geo = list(
          scope = "usa",
          projection = list(type = "albers usa"),
          showlakes = TRUE,
          lakecolor = toRGB("white"),
          subunitwidth = 1,
          countrywidth = 2
        )
      )
  })
  
  ### Hover Data Output
  output$hoverData <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) {
      "Hover over a state"
    } else {
      d
    }
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)