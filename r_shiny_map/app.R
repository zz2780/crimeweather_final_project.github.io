library(shiny)
library(tidyverse)
library(leaflet) #interactive app
library(htmlwidgets) #labels on interactive app
library(sf)#for analyzing spatial data

crime_month = readRDS("crime_month.RDS")

#define UI
ui = fluidPage(
  titlePanel("NYC crime visualization by 28 days interval and modified zip code regions"),
  sidebarLayout(
    sidebarPanel(
      h5("Data metrics are aggregated by 4 weeks and categorized by the first day in the 4 weeks interval. Data metrics are grouped into modified zip code regions instead of standard zip code regions to account for the discrepant population size in different standard zip code regions. Crime counts for a modified zip code region is the aggregated number of crime incidents occurred in the modified zip code region during the 4 weeks period starting from the specified date. Crime rate is the corresponding crime counts divided by the population size in the corresponding modified zip code region."),
      selectInput(
        "date",
        "Select a date (a 4 weeks period starting from):",
        choices = unique(crime_month$month_following)
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Crime counts", leafletOutput("crime_counts")),
        tabPanel("Crime rate", leafletOutput("crime_rate"))
      )
    )
  )
)

#define server logic
server <- function(input, output){
  month_selected <- reactive({
    w <- crime_month %>%
      filter(month_following == input$date)
    return(w)
  })
  output$crime_counts <- renderLeaflet({
    pal <- colorBin(palette = "YlGn", 9, domain = crime_month$crime_counts)
    labels = sprintf(
      "<strong>%s</strong><br/>%g incidents in the month",
      month_selected()$modzcta, month_selected()$crime_counts) %>%
      lapply(htmltools::HTML)
    month_selected() %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(-73.9, 40.7, zoom = 10) %>%
      addPolygons(label = labels,
                  stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~ pal(month_selected()$crime_counts),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "black",
                                                      opacity = 1,
                                                      bringToFront = TRUE)) %>%
      addLegend("bottomright",
                pal = pal,
                values = ~ crime_counts,
                title = "Crime incident counts",
                opacity = 0.7)
  })
  output$crime_rate <- renderLeaflet({
    pal <- colorBin(palette = "OrRd", 9, domain = crime_month$crime_rate)
    labels = sprintf(
      "<strong>%s</strong><br/>%g incidents in the month",
      month_selected()$modzcta, month_selected()$crime_rate) %>%
      lapply(htmltools::HTML)
    month_selected() %>%
      st_transform(crs = "+init=epsg:4326") %>%
      leaflet() %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(-73.9, 40.7, zoom = 10) %>%
      addPolygons(label = labels,
                  stroke = FALSE,
                  smoothFactor = 0.5,
                  opacity = 1,
                  fillOpacity = 0.7,
                  fillColor = ~ pal(month_selected()$crime_rate),
                  highlightOptions = highlightOptions(weight = 5,
                                                      fillOpacity = 1,
                                                      color = "black",
                                                      opacity = 1,
                                                      bringToFront = TRUE)) %>%
      addLegend("bottomright",
                pal = pal,
                values = ~ crime_rate,
                title = "Crime incident rate",
                opacity = 0.7)
  })
}

shinyApp(ui = ui, server = server)