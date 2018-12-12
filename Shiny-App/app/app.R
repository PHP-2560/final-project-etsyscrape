library(shiny)
library(dplyr)
library(markdown)
library(leaflet)

#complete_data <- readRDS("beer_data_loc_all.rds")

complete_data$UT_sub_style <- as.character(complete_data$UT_sub_style)

pal <- colorNumeric(
  palette = "YlOrRd",
  domain = c(0:5))


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
                filter(UT_sub_style == "Brown Ale - American" | 
                         UT_sub_style == "Brown Ale - English" |
                         UT_sub_style == "Brown Ale - Imperial / Double" | 
                         UT_sub_style == "Brown Ale - Belgian" | 
                         UT_sub_style =="Brown  Ale - Other" ))
      
    } 
    else if (input$styleInput == "ALL: IPA") {
      return (complete_data %>%
                filter(UT_sub_style == "IPA - American" |
                         UT_sub_style == "IPA - Imperial / Double" |
                         UT_sub_style == "IPA - Black / Cascadian Dark Ale" |
                         UT_sub_style == "IPA - Session / India Session Ale" | 
                         UT_sub_style == "IPA - English" |
                         UT_sub_style == "IPA - Belgian" | 
                         UT_sub_style == "IPA - Triple" |
                         UT_sub_style == "Rye IPA" |
                         UT_sub_style == "IPA - White"  | 
                         UT_sub_style == "IPA - Imperial / Double Black" |
                         UT_sub_style == "IPA - Red" | 
                         UT_sub_style == "IPA - International" |
                         UT_sub_style == "Sour - Farmhouse IPA"))
    } 
    else if (input$styleInput == "ALL: Lager") {
      return(complete_data %>%
               filter(UT_sub_style == "Lager Pale" |
                        UT_sub_style == "Lager - North American Adjunct" |
                        UT_sub_style == "Lager - American Light" |  
                        UT_sub_style == "Lager - Helles"  |              
                        UT_sub_style == "Lager - Vienna" |                
                        UT_sub_style == "Lager - Dark" | 
                        UT_sub_style ==  "Lager - Dortmunder / Export" |   
                        UT_sub_style == "Lager - IPL (India Pale Lager)" |
                        UT_sub_style == "Lager - American Amber / Red" |  
                        UT_sub_style == "Lager - Dunkel Munich" |         
                        UT_sub_style == "Lager - Japanese Rice" |         
                        UT_sub_style == "Lager - Euro" |                  
                        UT_sub_style == "Lager - Euro Dark" |             
                        UT_sub_style == "Lager - Winter" |                
                        UT_sub_style == "Lager - Euro Strong" |           
                        UT_sub_style == "Lager - Amber" |                
                        UT_sub_style == "Lager - Red"   
               ))
    } 
    else if (input$styleInput == "ALL: Pale Ale") {
      return(complete_data %>%
               filter(UT_sub_style == "Pale Ale - American" |
                        UT_sub_style == "Pale Ale - English" |
                        UT_sub_style == "Pale Ale - Belgian" |
                        UT_sub_style == "Pale Ale - Australian" |
                        UT_sub_style == "Pale Ale - New Zealand" |
                        UT_sub_style ==  "Pale Ale - International"
                        ))
    } 
    else if (input$styleInput == "ALL: Pilsner") {
      return(complete_data %>%
               filter(UT_sub_style == "Pilsner - Other" |
                        UT_sub_style == "Pilsner - German" |           
                        UT_sub_style == "Pilsner - Czech" |       
                        UT_sub_style == "Pilsner - Imperial / Double"))
    } 
    else if (input$styleInput == "ALL: Porter") {
      return(complete_data %>%
               filter(UT_sub_style == "Porter - American" |
                        UT_sub_style == "Porter - Other" |
                        UT_sub_style == "Porter - Imperial / Double" |
                        UT_sub_style == "Porter - Baltic" |
                        UT_sub_style == "Porter - English"))
    }
    else if (input$styleInput == "ALL: Sour") {
      return(complete_data %>%
               filter(UT_sub_style == "Sour - American Wild Ale" |
                        UT_sub_style == "Sour - Ale" |
                        UT_sub_style == "Sour - Gose" |
                        UT_sub_style == "Sour - Berliner Weisse" |
                        UT_sub_style == "Sour - Gueuze" |
                        UT_sub_style == "Sour - Flanders Red Ale" | 
                        UT_sub_style == "Sour - Flanders Oud Bruin" |
                        UT_sub_style == "Sour - Farmhouse IPA"))
    }
    else if (input$styleInput == "ALL: Stout") {
      return(complete_data %>%
               filter(UT_sub_style == "Stout - American Imperial / Double" |
                        UT_sub_style == "Stout - Russian Imperial" |
                        UT_sub_style == "Stout - Milk / Sweet" |
                        UT_sub_style == "Stout - Other" | 
                        UT_sub_style == "Stout - American" |
                        UT_sub_style == "Stout - Imperial / Double" | 
                        UT_sub_style == "Stout - Oatmeal" |
                        UT_sub_style == "Stout - Imperial Oatmeal" |
                        UT_sub_style == "Stout - Irish Dry" |
                        UT_sub_style == "Stout - Imperial Milk / Sweet" | 
                        UT_sub_style == "Stout - Foreign / Export" |
                        UT_sub_style == "Stout - Oyster"))
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
                    "ALL: Stout", sort(unique(complete_data$UT_sub_style))),
                  selected = "-- ALL STYLES --")
    })
  
  output$beer_locations <- renderDataTable({
    table <- filtered() %>%
      group_by(UT_sub_style, city, country) %>%
      count() %>%
      arrange(desc(n))
    
    colnames(table) <- c("Beer Style", "City", "Country", "Number of Unique Beers")
    
    table[1:250,]
  })
}

shinyApp(ui = ui, server = server)