library(shiny)
library(plotly)
library(dplyr)

# Sample data: state names and dummy values
state_data <- data.frame(
  state = state.abb,
  value = runif(50, 100, 500)
)

ui <- fluidPage(
  plotlyOutput("usMap"),
  verbatimTextOutput("hoverData")
)

server <- function(input, output, session) {
  output$usMap <- renderPlotly({
    plot_ly(
      data = state_data,
      type = "choropleth",
      locationmode = "USA-states",
      locations = ~state,
      z = ~value,
      text = ~paste("State:", state, "<br>Value:", round(value, 1)),
      colorbar = list(title = "Value"),
      colorscale = "Viridis"
    ) %>%
      layout(
        geo = list(
          scope = "usa",
          projection = list(type = "albers usa"),
          showlakes = TRUE,
          lakecolor = toRGB("white")
        )
      )
  })
  
  output$hoverData <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) {
      "Hover over a state"
    } else {
      d
    }
  })
}

shinyApp(ui, server)