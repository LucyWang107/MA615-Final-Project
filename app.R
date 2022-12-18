library(data.table)
library(ggplot2)
library(plotly)
library(shiny)
library(scales)
library(dplyr)
library(fontawesome)
library(leaflet)
library(readr)
library(DT)
library(magrittr)

LR_stop <- read.csv("LR_stop.csv")

basemap <- leaflet() %>%
  addTiles() %>% 
  setView(lng = -71.17, lat = 42.35, zoom = 12)


ui <- fluidPage(
  titlePanel("MBTA"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Date", "Travel Date",unique(LR_stop$service_date)
                  ),
      br(),
      radioButtons("Transport", "Route",unique(LR_stop$route_id)),
      br(),
      radioButtons("Station","Select From Station",unique(LR_stop$from_stop)),
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Map",leafletOutput("map")),
                  tabPanel("Table",tableOutput("table"))
      ),
    )
  )
)

server <- function(input,output){
  
  newdf <- reactive({
    LR_stop %>% filter(route_id%in%input$Transport, 
                      service_date%in%input$Date,
                      from_stop%in%input$Station)
  })
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -71.17, lat = 42.35, zoom = 12) %>%
      addMarkers(basemap, lat = 42.339581, lng = -71.157499, popup= "Travel Time B 169.6 sec") %>%
      addMarkers(basemap, lat = 42.34024, lng = -71.166849, popup= "Travel Time B 169.6 sec")%>%
      
      addMarkers(basemap, lat = 42.340053, lng = -71.128869, popup= "Travel Time C 224.4 sec")%>%
      addMarkers(basemap, lat = 42.33781, lng = -71.141753, popup= "Travel Time C 224.4 sec")%>%
      
      addMarkers(basemap, lat = 42.31921, lng = -71.216949, popup= "Travel Time D 1495.1 sec")%>%
      addMarkers(basemap, lat = 42.337348, lng = -71.252236, popup= "Travel Time D 1495.1 sec")%>%
      
      addMarkers(basemap, lat = 42.333740, lng = -71.105721, popup= "Travel Time E 274.05 sec")%>%
      addMarkers(basemap, lat = 42.32937, lng = -71.11105, popup= "Travel Time E 274.05 sec")
  })
  
  output$table <- renderTable({newdf()})
  
}

shinyApp(ui = ui, server = server)