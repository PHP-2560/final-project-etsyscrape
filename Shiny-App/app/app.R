library(shiny)
library(dplyr)
library(markdown)
library(leaflet)

complete_data <- readRDS("beer data/beer_complete_data.rds")
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
                        leafletOutput("relevant_countries")
                      )
                    )
           ),
           tabPanel("Beer Map",
                    sidebarLayout(
                      sidebarPanel(
                        uiOutput("styleOutput_1"),
                        radioButtons("pointDisplay", "Show results as: ", 
                                     c("Points", "Clusters"), selected = "Clusters")
                      ),
                      mainPanel(
                          leafletOutput("selected_map"))
                        )),
           tabPanel("Top Destinations",
                    sidebarLayout(
                      sidebarPanel(
                        uiOutput("styleOutput_2")
                      ),
                      mainPanel(
                        tabsetPanel(
                          tabPanel(
                            title = "Unique Beers by City", 
                            dataTableOutput("beer_unique_city")
                          ),
                          tabPanel(
                            title = "Unique Beers by Country",
                            dataTableOutput("beer_unique_country")
                          ),
                          tabPanel(
                            title = "Average Rating by City",
                            dataTableOutput("beer_avgrating_city")
                          ),
                          tabPanel(
                            title = "Average Rating by Country",
                            dataTableOutput("beer_avgrating_country")
                          )
                        )
                      )
                    )))


server <- function(input, output) {
  filtered_map <- reactive({
    if (input$styleInput_1 == "-- ALL STYLES --") {
      return(complete_data)
    } 
    else if (input$styleInput_1 == "ALL: Brown Ale") {
      return (complete_data %>%
                filter(large_style == "Brown Ale"))
      
    } 
    else if (input$styleInput_1 == "ALL: IPA") {
      return (complete_data %>%
                filter(large_style == "IPA"))
    } 
    else if (input$styleInput_1 == "ALL: Red Ale") {
      return (complete_data %>%
                filter(large_style == "Red Ale"))
    }
    else if (input$styleInput_1 == "ALL: Lager") {
      return(complete_data %>%
               filter(large_style == "Lager"))
    } 
    else if (input$styleInput_1 == "ALL: Pale Ale") {
      return(complete_data %>%
               filter(large_style == "Pale Ale"))
    } 
    else if (input$styleInput_1 == "ALL: Pilsner") {
      return(complete_data %>%
               filter(large_style == "Pilsner"))
    } 
    else if (input$styleInput_1 == "ALL: Porter") {
      return(complete_data %>%
               filter(large_style == "Porter"))
    }
    else if (input$styleInput_1 == "ALL: Sour") {
      return(complete_data %>%
               filter(large_style == "Sour"))
    }
    else if (input$styleInput_1 == "ALL: Stout") {
      return(complete_data %>%
               filter(large_style == "Stout"))
    }
    else {
      return(complete_data %>%
               filter(UT_sub_style == input$styleInput_1))
    }
  })
  
  filtered_dest <- reactive({
    if (input$styleInput_2 == "-- ALL STYLES --") {
      return(complete_data)
    } 
    else if (input$styleInput_2 == "ALL: Brown Ale") {
      return (complete_data %>%
                filter(large_style == "Brown Ale"))
      
    } 
    else if (input$styleInput_2 == "ALL: IPA") {
      return (complete_data %>%
                filter(large_style == "IPA"))
    } 
    else if (input$styleInput_2 == "ALL: Red Ale") {
      return (complete_data %>%
                filter(large_style == "Red Ale"))
    }
    else if (input$styleInput_2 == "ALL: Lager") {
      return(complete_data %>%
               filter(large_style == "Lager"))
    } 
    else if (input$styleInput_2 == "ALL: Pale Ale") {
      return(complete_data %>%
               filter(large_style == "Pale Ale"))
    } 
    else if (input$styleInput_2 == "ALL: Pilsner") {
      return(complete_data %>%
               filter(large_style == "Pilsner"))
    } 
    else if (input$styleInput_2 == "ALL: Porter") {
      return(complete_data %>%
               filter(large_style == "Porter"))
    }
    else if (input$styleInput_2 == "ALL: Sour") {
      return(complete_data %>%
               filter(large_style == "Sour"))
    }
    else if (input$styleInput_2 == "ALL: Stout") {
      return(complete_data %>%
               filter(large_style == "Stout"))
    }
    else {
      return(complete_data %>%
               filter(UT_sub_style == input$styleInput_2))
    }
  })
  
  mapClusters <- reactive({
    filtered_map() %>%
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
    filtered_map() %>%
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
    
  output$styleOutput_1 <- renderUI({
      selectInput("styleInput_1", "Beer style:",
                  c("-- ALL STYLES --", "ALL: Brown Ale", "ALL: IPA", "ALL: Lager", 
                    "ALL: Pale Ale", "ALL: Pilsner", "ALL: Porter", "ALL: Sour",
                    "ALL: Stout", "ALL: Red Ale", sort(unique(complete_data$UT_sub_style))),
                  selected = "-- ALL STYLES --")
    })
  
  output$styleOutput_2 <- renderUI({
    selectInput("styleInput_2", "Beer style:",
                c("-- ALL STYLES --", "ALL: Brown Ale", "ALL: IPA", "ALL: Lager", 
                  "ALL: Pale Ale", "ALL: Pilsner", "ALL: Porter", "ALL: Sour",
                  "ALL: Stout", "ALL: Red Ale", sort(unique(complete_data$UT_sub_style))),
                selected = "-- ALL STYLES --")
  })
  
  output$beer_unique_city <- renderDataTable({
    if (input$styleInput_2 == "-- ALL STYLES --") {
      table <- filtered_dest() %>%
        group_by(city, country) %>%
        count() %>%
        arrange(desc(n))
      
      colnames(table) <- c("City", "Country", "Number of Unique Beers")
    } else if (input$styleInput_2 == "ALL: Brown Ale" | input$styleInput_2 == "ALL: IPA"
               | input$styleInput_2 =="ALL: Lager" | input$styleInput_2 == "ALL: Pale Ale"
               | input$styleInput_2 =="ALL: Pilsner" | input$styleInput_2 =="ALL: Porter"
               | input$styleInput_2 =="ALL: Sour" | input$styleInput_2 == "ALL: Stout"
               | input$styleInput_2 =="ALL: Red Ale") {
      table <- filtered_dest() %>%
        group_by(large_style, city, country) %>%
        count() %>%
        arrange(desc(n))
      
      colnames(table) <- c("Beer Style", "City", "Country", "Number of Unique Beers")
    } else {
      table <- filtered_dest() %>%
        group_by(UT_sub_style, city, country) %>%
        count() %>%
        arrange(desc(n))
      
      colnames(table) <- c("Beer Style", "City", "Country", "Number of Unique Beers")
    }
    table
  })
  
  output$beer_unique_country <- renderDataTable({
    if (input$styleInput_2 == "-- ALL STYLES --") {
      table <- filtered_dest() %>%
        group_by(country) %>%
        count() %>%
        arrange(desc(n))
      
      colnames(table) <- c("Country", "Number of Unique Beers")
      
    } else if (input$styleInput_2 == "ALL: Brown Ale" | input$styleInput_2 == "ALL: IPA"
               | input$styleInput_2 =="ALL: Lager" | input$styleInput_2 == "ALL: Pale Ale"
               | input$styleInput_2 =="ALL: Pilsner" | input$styleInput_2 =="ALL: Porter"
               | input$styleInput_2 =="ALL: Sour" | input$styleInput_2 == "ALL: Stout"
               | input$styleInput_2 =="ALL: Red Ale") {
      table <- filtered_dest() %>%
        group_by(large_style, country) %>%
        count() %>%
        arrange(desc(n))
      
      colnames(table) <- c("Beer Style", "Country", "Number of Unique Beers")
    } else {
      table <- filtered_dest() %>%
        group_by(UT_sub_style, country) %>%
        count() %>%
        arrange(desc(n))
      
      colnames(table) <- c("Beer Style", "Country", "Number of Unique Beers")
    }
    table
  })
  
  output$beer_avgrating_city <- renderDataTable({
    if (input$styleInput_2 == "-- ALL STYLES --") {
      table2 <- filtered_dest() %>%
        group_by(city, country) %>%
        summarize(avg_rating = mean(UT_rating)) %>%
        arrange(desc(avg_rating))
      
      colnames(table2) <- c("City", "Country", "Average Beer Rating")
      
    } else if (input$styleInput_2 == "ALL: Brown Ale" | input$styleInput_2 == "ALL: IPA"
              | input$styleInput_2 =="ALL: Lager" | input$styleInput_2 == "ALL: Pale Ale"
              | input$styleInput_2 =="ALL: Pilsner" | input$styleInput_2 =="ALL: Porter"
              | input$styleInput_2 =="ALL: Sour" | input$styleInput_2 == "ALL: Stout"
              | input$styleInput_2 =="ALL: Red Ale") {
      
      table2 <- filtered_dest() %>%
        group_by(large_style, city, country) %>%
        summarize(avg_rating = mean(UT_rating)) %>%
        arrange(desc(avg_rating))
      
      colnames(table2) <- c("Beer Style", "City", "Country", "Average Beer Rating")
      
    } else {
      table2 <- filtered_dest() %>%
        group_by(UT_sub_style, city, country) %>%
        summarize(avg_rating = mean(UT_rating)) %>%
        arrange(desc(avg_rating))
      
      colnames(table2) <- c("Beer Style", "City", "Country", "Number of Unique Beers")
    }
    table2
  })
  
  output$beer_avgrating_country <- renderDataTable({
    if (input$styleInput_2 == "-- ALL STYLES --") {
      table2 <- filtered_dest() %>%
        group_by(country) %>%
        summarize(avg_rating = mean(UT_rating)) %>%
        arrange(desc(avg_rating))
      
      colnames(table2) <- c("Country", "Average Beer Rating")
      
    } else if (input$styleInput_2 == "ALL: Brown Ale" | input$styleInput_2 == "ALL: IPA"
               | input$styleInput_2 =="ALL: Lager" | input$styleInput_2 == "ALL: Pale Ale"
               | input$styleInput_2 =="ALL: Pilsner" | input$styleInput_2 =="ALL: Porter"
               | input$styleInput_2 =="ALL: Sour" | input$styleInput_2 == "ALL: Stout"
               | input$styleInput_2 =="ALL: Red Ale") {
      
      table2 <- filtered_dest() %>%
        group_by(large_style, country) %>%
        summarize(avg_rating = mean(UT_rating)) %>%
        arrange(desc(avg_rating))
      
      colnames(table2) <- c("Beer Style","Country", "Average Beer Rating")
      
    } else {
      table2 <- filtered_dest() %>%
        group_by(UT_sub_style, country) %>%
        summarize(avg_rating = mean(UT_rating)) %>%
        arrange(desc(avg_rating))
      
      colnames(table2) <- c("Beer Style","Country", "Number of Unique Beers")
    }
    table2
  })
}

shinyApp(ui = ui, server = server)