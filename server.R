
if (!require("shiny")) { install.packages("shiny", dependencies = TRUE) ; library(shiny)}
if (!require("leaflet")) { install.packages("leaflet", dependencies = TRUE) ; library(leaflet)}

# Define server logic
server <- function(input, output, session) {
  
  # Data frame with points and associated text information
  points <- data.frame(
    Country = c("Benin", "Botswana", "Eswatini", "Ghana", 
                "Malawi", "Mozambique", "Nigeria", "Tanzania_north", "Tanzania_south", 
                "Congo"),
    lat = c(9.541169523, -24.54612347, -26.83657033, 7.855764149, 
            -13.1217448, -19.20639098, 8.626644654, -3.692144742, -9.885523001, 
            -2.677489607),
    lng = c(2.250000536, 24.30175781, 31.49780273, 
            -1.072265357, 33.88183594, 34.27734375, 9.052734375, 32.46679768, 
            36.90527424, 19.63476777),
    img = c("space_x-min.jpg", "jonatan-pie_compressed_300.jpg", "image3.jpg", "image4.jpg", "image5.jpg", "image6.jpg",  "image7.jpg",
            "image8.jpg", "image9.jpg", "image10.jpg"),
    sound = c("liberation.mp3", "sjavldo.mp3", "sound3.mp3","sound4.mp3","sound5.mp3","sound6.mp3","sound7.mp3",
              "sound8.mp3","sound9.mp3","sound10.mp3"),
    paragraph = c("Point 1: Some what! interesting information here.",
                  "Point 2: Another piece of information.",
                  "Point 3: More details about this point.",
                  "Point 4: More details about this point.",
                  "Point 5: More details about this point.",
                  "Point 6: More details about this point.",
                  "Point 7: More details about this point.",
                  "Point 8: More details about this point.",
                  "Point 9: More details about this point.",
                  "Point 10: More details about this point.")
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
        popup = ~Country
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
        tags$audio(controls = NA,type = "audio/mp3", autoplay = TRUE, src =paste0("sound/",point_data$sound) ),
        p(point_data$paragraph)
      )
    } else {
      "Click on a marker to see the information."
    }
  })
}
