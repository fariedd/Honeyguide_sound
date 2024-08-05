library(shiny)
library(leaflet)

#use update in server and observerEvent for click to create homebutton

# Define UI
ui <- fluidPage(
  fluidRow(width = 12,style = "background-color: #3d3d3d;color: white;",
           headerPanel(h2(strong( "Honey Trails: A journey through Africa's Honey-Hunting Traditions"), align='center' )),
           h4("Discover the Richness of Africa’s Honey-Hunting Cultures with the Honey-Hunting Research Network",
              align='center'),
           br()
           
  ),
  fluidRow(width =12,
           sidebarLayout(
             sidebarPanel(
               style = "height:85vh; overflow-y: auto;",
               h3(textOutput("title_panel")),
               uiOutput("info_ui")
             ),
             mainPanel(
               leafletOutput("map", width = "100%", height = "85vh"),
               tags$style(type = "text/css", "html, body {width:100%;height:100%}")
             )
           )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Data frame with points and associated text information
  points <- data.frame(
    Country = c("Benin", "Botswana", "Kingdom of Eswatini", "Ghana", 
                "Malawi", "Mozambique", "Nigeria", "Tanzania North", "Tanzania South", 
                "Congo"),
    
    lat = c(9.541169523, -24.54612347, -26.83657033, 7.855764149, 
            -13.1217448, -19.20639098, 8.626644654, -3.692144742, -9.885523001, 
            -2.677489607),
    lng = c(2.250000536, 24.30175781, 31.49780273, 
            -1.072265357, 33.88183594, 34.27734375, 9.052734375, 32.46679768, 
            36.90527424, 19.63476777),
    img = c("space_x-min.jpg", "jonatan-pie_compressed_300.jpg", "swati_resized.JPG", "sidebar-blueswirl.jpg", "space_x-min.jpg", "jonatan-pie_compressed_300.jpg",  "space_x-min.jpg",
            "jonatan-pie_compressed_300.jpg", "tempopic-min.jpg", "space_x-min.jpg"),
    sound = c("Honeyguide3.mp3", "Honeyguide1.mp3", "Honeyguide2.mp3","Honeyguide3.mp3","Honeyguide2.mp3","Honeyguide1.mp3","Honeyguide3.mp3",
              "Honeyguide2.mp3","Honeyguide1.mp3","Honeyguide3.mp3"),
    paragraph = c("benin_page_1.html",
                  "botswana_page_1.html",
                  "eswatini_page_1.html",
                  "ghana_page_1.html",
                  "malawi_page_1.html",
                  "mozambique_page_1.html",
                  "nigeria_page_1.html",
                  "tanzania_north_page_1.html",
                  "tanzania_south_page_1.html",
                  "congo_page_1.html")
  )
  
  # Render leaflet map
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
      setView(lng =  25.736678919, lat = -6.402645740, zoom = 4) %>%
      addMarkers(
        data = points, 
        ~lng, ~lat, 
        layerId = ~lat, # Use lat as the unique ID for simplicity
        label = ~Country,
        labelOptions = labelOptions(
          style = list(
            "color" = "black",
            "font-family" = "serif",
            "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
            "font-size" = "15px",
            "border-color" = "rgba(0,0,0,0.5)"
          ))
      )
  })
  
  # Initialize a reactiveValues object to store clicked information
  click_info <- reactiveValues(info = NULL)
  
  # Update clicked information when a marker is clicked
  observeEvent(input$map_marker_click, {
    click_info$info <- input$map_marker_click$id
  })
  
  # Dynamically render UI for sidebar panel
  output$info_ui <- renderUI({
    click <- click_info$info
    if (!is.null(click)) {
      point_data <- points[points$lat == as.numeric(click), ]
      tagList(
        tags$img(src =paste0("image/" , point_data$img), width = "100%"),
        tags$audio(controls = F,type = "audio/mp3", src =paste0("sound/",point_data$sound) ),
        includeHTML(paste0("html files_pages/" ,point_data$paragraph) )
      )
    } else {
      
      p( style="text-align: justify;",h5("This interactive map showcases information on Africa’s honey-hunting cultures which the Honey-Hunting Research Network have been conducting research with."),
        h5(em( "Click on any marker for more information on each region."), align = "left"))
    }
  })
  
  ###change title of side bar
  
  output$title_panel <- renderText({
    click = click_info$info
    if(!is.null(click)){
      point_data <- points[points$lat == as.numeric(click), ] 
      point_data$Country
    } else {
      "Welcome!"
    }
   
  })
}

# Run the application
shinyApp(ui = ui, server = server)
