
# calculate age

ui <- shinyUI(
  fluidPage({
    column(2, actionButton("Calculate", "Click to calculate age"),
           fluidRow(uiOutput("CalculatedAge")))
  })
)

server <- shinyServer(function(input,output,session){
  
  observeEvent(input$Calculate, 
                output$CalculatedAge <- renderUI({isolate({
                  
                  fluidRow(
                    column(10,dateInput("dob", label="Date of Birth (yyyy-mm-dd):",
                                        min = "1900-01-01",
                                        max = Sys.Date(), format = "yyyy-mm-dd", 
                                        startview = "year",
                                        weekstart = 0, language = "en")),
                    
                    column(10, textInput("age",label = "Age (in years):"))
                  )
                  
                })}))  
  
  observe({   dob <- input$dob
  if(!is.null(dob)) {
    days <- as.integer((Sys.Date() - as.Date(dob)))
    age <- as.integer(days / 365)
    
    updateTextInput(session, "age", value = age)
  }
  
  })
  
  relevant_countries <- reactive({min_age_table_clean %>%
    filter("'On Premise" | "'Off Premise" >= as.numeric(age))

  })
})

shinyApp(ui = ui, server = server)