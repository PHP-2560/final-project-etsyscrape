library(shiny)
library(dplyr)
library(markdown)
library(leaflet)
library(RColorBrewer)
library(rworldmap)

# load applicable age data table 
clean_age <- readRDS("age_data/clean_age_app_tables.rds")

complete_data <- readRDS("beer_data/beer_complete_data.rds")
complete_data$UT_sub_style <- as.character(complete_data$UT_sub_style)
complete_data$large_style <- as.character(complete_data$large_style)

pal <- colorNumeric(palette = "YlOrRd", domain = c(0:5))

colourPalette <- brewer.pal(6,"PuRd")

# Making the map using the age_tbl data, read in
age_tbl <- readRDS("age_data/age_tbl.rds")

ui <- navbarPage(strong("TravelBeeR"),
                 tabPanel("Beer Map",
                          sidebarLayout(
                            sidebarPanel(
                              htmlOutput("beer_directions"),
                              uiOutput("styleOutput_1"),
                              radioButtons("pointDisplay", "Show results as: ", 
                                           c("Points", "Clusters"), selected = "Clusters"),
                              htmlOutput("beer_directions_2")
                              ),
                            mainPanel(
                              leafletOutput("selected_map"))
                          )),
                 tabPanel("Top Destinations",
                          sidebarLayout(
                            sidebarPanel(
                              htmlOutput("destination_description"),
                              uiOutput("styleOutput_2"),
                              htmlOutput("destination_description_2")
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
                          )
                          ),
                 tabPanel("Legal Drinking Age",
                          sidebarLayout(
                            sidebarPanel(
                              htmlOutput("premise_definition")
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                tabPanel(
                                  title = "All Countries",
                                  htmlOutput("age_directions"), 
                                  dataTableOutput("clean_age")
                                ),
                                tabPanel(
                                  title = "No Minimum Age",
                                  htmlOutput("none"),
                                  dataTableOutput("no_age")
                                ),
                                tabPanel(
                                  title = "Restrictions",
                                  htmlOutput("restricted"),
                                  dataTableOutput("age_restricted")
                                ),
                                tabPanel(
                                  title = "Age Map",
                                  htmlOutput("age_map_descr"),
                                  imageOutput("age_map")
                                )
                              ))))
                 )



server <- function(input, output) {
  # description for legal drinking age tab
  output$premise_definition <- renderUI({
    HTML(paste("", "Use the tables provided to explore the legal drinking age in 180 countries worldwide!",
"", "On-premise age refers to the drinking age required to purchase alcohol and
consume it on-site at the establishment of purchase (e.g. restaurants and pubs)",
               "", "Off-premise age refers to the age required to purchase alcohol at an establishment selling
               for off-site consumption (e.g. liquor stores).",
               "", "", sep = "<br/>"))
  })
  # direction for all country tab
  output$age_directions <- renderUI({
    HTML(paste("", strong("Use the search box to the right to search by age or country."), 
               "", "", sep = "<br/>"))
  })
    # explanation of no age tab
  output$none <- renderUI({
    HTML(paste("", strong("There is no minimum drinking age in these countries!"), 
               "", "", sep = "<br/>"))
  })
    # explanation of restriction tab
  output$restricted <- renderUI({
    HTML(paste("", strong("Consumption of alcohol is restricted or varies geographically in the following countries."), 
               "The drinking laws in these places may be dependent on province, religion, jurisdiction or type of beverage.",
               "", "Helpful key search terms include: 'prohibited', 'restricted', 'religion', 'jurisdiction', 'beverage'", 
               "", "", sep = "<br/>"))
  })
  
  output$age_map_descr <- renderUI({
    HTML(paste("", strong("Countries on the map are coloured by on-premise legal drinking age."), sep = "<br/>"))
  })
    # data table for all country tab
  output$clean_age <- renderDataTable(clean_age)
    # data table for restriction tab
  output$age_restricted <- renderDataTable(clean_age %>%
                                             filter(clean_age[,2] %in% c("[all sale is prohibited]", "[varies by beverage and jurisdiction]", 
                                                                         "[varies by beverage]", "[varies by jurisdiction and by beverage]", 
                                                                         "[varies by jurisdiction]", "[varies by province]", 
                                                                         "[varies by religion and jurisdiction]", "[varies by religion]")))
    # data table for no age tab
  output$no_age <- renderDataTable(clean_age %>%
                                     select("Country") %>%                                     
                                     filter(clean_age[,2] %in% c("[none]")))
  # table for Beer map tab and selected input from drop down menu
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
  # table for Top Destinations tab and selected input from drop down menu
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
    # beer map when clusters selected
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
    # beer map when points selected
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
  # directions for Beer Map tab
  output$beer_directions <- renderUI({
    HTML(paste("", "Select your favourite style of beer to see all beers of that type displayed on the map! 
               The results can be displayed by geographical clusters or individual points.",
               "", "", sep = "<br/>"))
  })
  
  output$beer_directions_2 <- renderUI({
    HTML(paste("", "Click on clusters to zoom in, and click on individual points to 
               see the name of the beer, its style, its brewery, and its Untappd rating!",
               "", sep = "<br/>"))
  })
  # output map
  output$selected_map <- renderLeaflet({ 
    mapInput()
  })
  # dropdown list for Beer Map tab
  output$styleOutput_1 <- renderUI({
    selectInput("styleInput_1", "Beer style:",
                c("-- ALL STYLES --", "ALL: Brown Ale", "ALL: IPA", "ALL: Lager", 
                  "ALL: Pale Ale", "ALL: Pilsner", "ALL: Porter", "ALL: Sour",
                  "ALL: Stout", "ALL: Red Ale", sort(unique(complete_data$UT_sub_style))),
                selected = "-- ALL STYLES --")
  })
  # dropdown list for Top Destinations tab
  output$styleOutput_2 <- renderUI({
    selectInput("styleInput_2", "Beer style:",
                c("-- ALL STYLES --", "ALL: Brown Ale", "ALL: IPA", "ALL: Lager", 
                  "ALL: Pale Ale", "ALL: Pilsner", "ALL: Porter", "ALL: Sour",
                  "ALL: Stout", "ALL: Red Ale", sort(unique(complete_data$UT_sub_style))),
                selected = "-- ALL STYLES --")
  })
  # description for Top Destinations tab
  output$destination_description <- renderUI({
    HTML(paste("", "The tabs and tables to the right filter based on the selected beer style.", 
               "", "", sep = "<br/>"))
  })
  
  output$destination_description_2 <- renderUI({
    HTML(paste("", "The 'Unique Beers' tabs display the number of unique beers in each specific city
               or country, and the 'Average Rating' tabs display the average Untappd rating of beers from
               each location.",
               "", "", sep = "<br/>"))
  })
  # table of unique beers by city
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
  
  output$age_map <- renderImage({
    # Initiating process to write map to file
    png(filename="age_map.png")
    
    # Map object is created from the age_tbl, recognizes the countries by the code column,
    # because they are identified as "ISO3".
    map <- joinCountryData2Map(age_tbl, joinCode = "ISO3", nameJoinColumn =
                                 "code", nameCountryColumn = "country", 
                               suggestForFailedCodes = FALSE, mapResolution = "coarse", 
                               projection = NA, verbose = FALSE)
    
    # Specify the parameters for creating the map - using the age column as the plotting 
    # variable, the entire world as the region, with NA age countries coloured dark grey
    mapParams <- mapCountryData(map, nameColumnToPlot = "age", addLegend = FALSE, 
                                mapRegion = "world",
                                catMethod = c(16:21), missingCountryCol="dark grey", 
                                numCats = length(unique(map$age)), colourPalette = colourPalette, 
                                mapTitle = "", borderCol = "black")
    
    # Add a custom legend to the map
    do.call(addMapLegend, c(mapParams, legendLabels= "all", legendWidth=0.5,
                            legendMar = 2))
    
    # Writing plot output to file
    dev.off()
      
      # Return a list containing the filename
    list(src = "age_map.png",
           contentType = 'image/png',
           width = 700,
           height = 700,
           alt = "This is alternate text")
  })
  
  # table of unique beers by country
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
  # table of average beer rating by city
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
  # table of average beer rating by country
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