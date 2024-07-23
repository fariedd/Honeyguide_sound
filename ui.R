if (!require("shiny")) { install.packages("shiny", dependencies = TRUE) ; library(shiny)}
if (!require("leaflet")) { install.packages("leaflet", dependencies = TRUE) ; library(leaflet)}


# Define UI
ui <- fluidPage(
  titlePanel("Clickable Map with Leaflet"),
  sidebarLayout(
    sidebarPanel(
      h3("Clicked Information"),
      uiOutput("info_ui")
    ),
    mainPanel(
      leafletOutput("map", width = "100%", height = 640),
      tags$style(type = "text/css", "html, body {width:100%;height:100%}")
    )
  )
)
