library(shiny)
library(dplyr)
library(markdown)
library(leaflet)

#complete_data <- readRDS("beer data/beer_complete_data.rds")
complete_data$UT_sub_style <- as.character(complete_data$UT_sub_style)
complete_data$large_style <- as.character(complete_data$large_style)

pal <- colorNumeric(palette = "YlOrRd", domain = c(0:5))


ui <- navbarPage(strong("TravelBeeR"),
           tabPanel("Legal Drinking Age",
                    sidebarLayout(
                      sidebarPanel(
                        dateInput("birthdate", "Enter your date of birth (yyyy-mm-dd): ")
                      ),
                      mainPanel(
                        plotOutput("plot"),
                        tableOutput("table")
                      )
                    )
           ),
           tabPanel("Beer Map",
                    sidebarLayout(
                      sidebarPanel(
                        uiOutput("styleOutput"),
                        radioButtons("pointDisplay", "Show results as: ", 
                                     c("Points", "Clusters"), selected = "Clusters")
                        ),
                        mainPanel(
                          leafletOutput("selected_map", height=625),
                          dataTableOutput("beer_locations")
                      )
                    )
           )
)


server <- function(input, output) {
  filtered <- reactive({
    if (input$styleInput == "-- ALL STYLES --") {
      return(complete_data)
    } 
    else if (input$styleInput == "ALL: Brown Ale") {
      return (complete_data %>%
                filter(large_style == "Brown Ale"))
      
    } 
    else if (input$styleInput == "ALL: IPA") {
      return (complete_data %>%
                filter(large_style == "IPA"))
    } 
    else if (input$styleInput == "ALL: Red Ale") {
      return (complete_data %>%
                filter(large_style == "Red Ale"))
    }
    else if (input$styleInput == "ALL: Lager") {
      return(complete_data %>%
               filter(large_style == "Lager"))
    } 
    else if (input$styleInput == "ALL: Pale Ale") {
      return(complete_data %>%
               filter(large_style == "Pale Ale"))
    } 
    else if (input$styleInput == "ALL: Pilsner") {
      return(complete_data %>%
               filter(large_style == "Pilsner"))
    } 
    else if (input$styleInput == "ALL: Porter") {
      return(complete_data %>%
               filter(large_style == "Porter"))
    }
    else if (input$styleInput == "ALL: Sour") {
      return(complete_data %>%
               filter(large_style == "Sour"))
    }
    else if (input$styleInput == "ALL: Stout") {
      return(complete_data %>%
               filter(large_style == "Stout"))
    }
    else {
      return(complete_data %>%
               filter(UT_sub_style == input$styleInput))
    }
  })
  
  mapClusters <- reactive({
    filtered() %>%
      leaflet(options = leafletOptions(minZoom = 1, dragging = TRUE)) %>%
      addProviderTiles("CartoDB") %>%
      addCircleMarkers(lng = ~lon, 
                       lat = ~lat,
                       popup = ~paste0("<b>", UT_beer_name, "</b>", 
                                       "<br/>", UT_sub_style,
                                       "<br/>", UT_brewery,
                                       "<br/>","Rating: ", UT_rating),
                       radius = 3,
                       color = ~pal(UT_rating),
                       clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE))%>%
      addLegend(pal = pal, 
                opacity = 0.5,
                values = c(0:5),
                title = "Untappd Rating",
                position = "bottomleft",
                bins = 5)
  })
  
  mapPoints <- reactive({
    filtered() %>%
      leaflet(options = leafletOptions(minZoom = 1, dragging = TRUE)) %>%
      addProviderTiles("CartoDB") %>%
      addCircleMarkers(lng = ~lon, 
                       lat = ~lat,
                       popup = ~paste0("<b>", UT_beer_name, "</b>", 
                                       "<br/>", UT_sub_style,
                                       "<br/>", UT_brewery,
                                       "<br/>", "Rating: ", UT_rating),
                       radius = 3,
                       color = ~pal(UT_rating)) %>%
      addLegend(pal = pal, 
                opacity = 0.5,
                values = c(0:5),
                title = "Untappd Rating",
                position = "bottomleft",
                bins = 5)
  })
  
  # Return the requested graph
  mapInput <- reactive({
    switch(input$pointDisplay,
           "Points" = mapPoints(),
           "Clusters" = mapClusters()
    )
  })
  
  output$selected_map <- renderLeaflet({ 
    mapInput()
  })
    
  output$styleOutput <- renderUI({
      selectInput("styleInput", "Beer Style",
                  c("-- ALL STYLES --", "ALL: Brown Ale", "ALL: IPA", "ALL: Lager", 
                    "ALL: Pale Ale", "ALL: Pilsner", "ALL: Porter", "ALL: Sour",
                    "ALL: Stout", "ALL: Red Ale", sort(unique(complete_data$UT_sub_style))),
                  selected = "-- ALL STYLES --")
    })
  
  output$beer_locations <- renderDataTable({
    table <- filtered() %>%
      group_by(large_style, city, country) %>%
      count() %>%
      arrange(desc(n))
    
    colnames(table) <- c("Beer Style", "City", "Country", "Number of Unique Beers")
    
    table[1:250,]
  })
}

shinyApp(ui = ui, server = server)