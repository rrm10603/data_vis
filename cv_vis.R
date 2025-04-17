library(ggplot2)
library(dplyr)
library(sf)
library(leaflet)
library(vistime)


url <- "https://public.opendatasoft.com/explore/dataset/us-state-boundaries/download/?format=geojson"
states_sf <- sf::st_read(url)

focus_states <- c("District of Columbia", "Maryland", "California", "Colorado")
highlight_states <- states_sf[states_sf$name %in% focus_states, ]

# Simulated data for education levels
edu_work_data <- data.frame(
  name = focus_states,
  education = c(85, 88, 90, 87)  # example: % high school or higher
)

# Join data with spatial info
highlight_states <- states_sf %>%
  filter(name %in% focus_states) %>%
  left_join(edu_work_data, by = "name")




# Plot without grid lines or axes
ggplot() +
  geom_sf(data = states_sf, fill = "gray90", color = "white") +
  geom_sf(data = highlight_states, aes(fill = education), color = "black", size = 1.2) +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "% Education") +
  coord_sf(
    xlim = c(-125, -66.5),
    ylim = c(24, 50),
    expand = FALSE,
    datum = NA  # removes graticule (grid lines)
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),        # remove axis text
    axis.ticks = element_blank(),       # remove axis ticks
    panel.grid = element_blank()        # remove grid lines
  ) +
  labs(title = "Time Series Visualization of Education and Work History Across US")


#idea 2: make interactive timeline in r, using gg_vistime



timeline_data <- data.frame(event = c("BA, History (Salisbury University, Maryland",
                                      "Project Engineer (Donohoe Construction, Washington, DC",
                                      "Assistant Superintendent (Shea Homes, Denver, Colorado",
                                      "Assistant Superintendent (Shea Homes, San Diego, California",
                                      "Math Tutor, San Diego City College",
                                      "MPH Biostatistics (Colorado School of Public Health, Aurora, Colorado"),
                            start = c("2007-08-01", "2011-06-01", "2014-01-01", 
                                      "2016-01-01", "2023-05-01", "2023-08-01"),
                            end   = c("2011-05-15", "2014-01-01", "2016-01-01", 
                                      "2023-06-01", "2023-08-31", "2025-05-01"),
                            group = "My Events")
gg_vistime(timeline_data)



#try 3 from AI 

timeline <- tibble(
  event = c("BA, History\n(Salisbury Univ., MD)",
            "Project Engineer\n(Donohoe, DC)",
            "Asst. Superintendent\n(Shea Homes, Denver)",
            "Asst. Superintendent\n(Shea Homes, San Diego)",
            "Math Tutor\n(SDCC)",
            "MPH Biostat\n(CO School of Public Health)"),
  
  year = c(2011, 2014, 2016, 2023, 2023, 2025),
  
  type = c("Education", "Work", "Work", "Work", "Work", "Education"),
  
  # Note: Swapped position for Math Tutor and Shea Homes (SD)
  position = c(0.07, -0.07, 0.07, 0.07, -0.07, -0.07)
)

colors <- c("Education" = "#4A90E2", "Work" = "#F5A623")

ggplot(timeline) +
  geom_hline(yintercept = 0, color = "gray30", size = 1) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = position), color = "gray60") +
  geom_point(aes(x = year, y = position, color = type), size = 5) +
  geom_text(aes(x = year, y = position, label = event),
            vjust = ifelse(timeline$position > 0, -0.2, 1.2),
            size = 3.5, lineheight = 1) +
  scale_color_manual(values = colors) +
  scale_x_continuous(breaks = seq(2010, 2026, 1), expand = expansion(add = 0.5)) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = margin(t = 30, r = 30, b = 30, l = 60),  # added more left margin
    legend.position = "bottom",
    plot.title = element_blank()
  ) +
  labs(color = "Type")