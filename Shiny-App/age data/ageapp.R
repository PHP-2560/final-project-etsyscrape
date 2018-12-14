library(shiny)
library(dplyr)
library(markdown)
library(leaflet)

# calculate age and create table of countries where one meets minimum drinking age

ui <- navbarPage(strong("TravelBeeR"),
                 tabPanel("Legal Drinking Age",
                          sidebarLayout(
                            sidebarPanel(
                              actionButton("Calculate", "Click to calculate age"),
                              uiOutput("CalculatedAge")
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel(
                                  title = "All Countries",
                              dataTableOutput("clean_age")
                                  ),
                                tabPanel(
                                  title = "Filter by Age",
                                  dataTableOutput("age_to_filter")
                                  ),
                                tabPanel(
                                  title = "No Minimum Age",
                                  dataTableOutput("no_age")
                                  ),
                                tabPanel(
                                  title = "Restrictions",
                                  dataTableOutput("age_restricted")
                                )
                              ))))
)
           
server <- shinyServer(function(input,output,session){
  
  observeEvent(input$Calculate, 
                output$CalculatedAge <- renderUI({isolate({
                  
                  fluidRow(
                    column(10,dateInput("dob", label="Date of Birth (yyyy-mm-dd)",
                                        min = "1900-01-01",
                                        max = Sys.Date(), format = "yyyy-mm-dd", 
                                        startview = "year",
                                        weekstart = 0, language = "en")),
                    
                    column(10, textInput("age", label = "Age (in years)"))
                  )
                  
                })
              }))  
  
  observe({dob <- input$dob
  if(!is.null(dob)) {
    days <- as.integer((Sys.Date() - as.Date(dob)))
    age <- as.integer(days / 365)
    
    updateTextInput(session, "age", value = age)
  }
  
  filteredAge <- reactive({
    if (textInput("age") >= clean_age$`on premise`){
      filter(clean_age$`on premise` >= textInput("age"))
    }
    })
      
  # display table
  output$ageTable <- renderDataTable({
    table <- filteredAge
  })
})
})

shinyApp(ui = ui, server = server)