
library(shiny)
library(dplyr)
library(markdown)
library(leaflet)

# create table of countries where one meets minimum drinking age

# first clean age data
age <- readRDS("C:/Users/akosu/Desktop/final-project-etsyscrape/Shiny-App/age data/min_age_table_unclean_latlong.rds")

# delete first row of data as it is a duplicate of column names
# also delete lat long data
age <- age[-1,-c(4,5)]

# rename variables
colnames(age) <- c("Country", "On Premise", "Off Premise")

# within each column, remove *
age$`On Premise` <- gsub("[*]", "", age$`On Premise`)
age$`Off Premise` <- gsub("[*]", "", age$`Off Premise`)

clean_age <- as.data.frame(age)

# APP

ui <- navbarPage(strong("TravelBeeR"),
                 tabPanel("Legal Drinking Age",
                            mainPanel(
                              tabsetPanel(
                                tabPanel(
                                  title = "All Countries",
                                  htmlOutput("directions"), 
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
                                )
                              )))
)

server <- function(input,output){
  output$directions <- renderUI({
                        HTML(paste("", "Search by country or age", sep = "<br/>"))
  })
  output$none <- renderUI({
                    HTML(paste("", "There is no minimum drinking age in the below countries!", sep = "<br/>"))
  })
  output$restricted <- renderUI({
                        HTML(paste("", "Consumption of alcohol is prohibited or restricted in the below countries,", " dependent on province, religion, jurisdiction or type of beverage.",
                                   "", "Search by key words: 'prohibited', 'restricted', 'religion', 'jurisdiction', 'beverage'", sep = "<br/>"))
  })
  
  output$clean_age <- renderDataTable(clean_age)
 
  output$age_restricted <- renderDataTable(clean_age %>%
    filter(clean_age[,2] %in% c("[all sale is prohibited]", "[varies by beverage and jurisdiction]", 
                                "[varies by beverage]", "[varies by jurisdiction and by beverage]", 
                                "[varies by jurisdiction]", "[varies by province]", 
                                "[varies by religion and jurisdiction]", "[varies by religion]")))
  
  output$no_age <- renderDataTable(clean_age %>%
    select("Country") %>%                                     
    filter(clean_age[,2] %in% c("[none]")))
}

shinyApp(ui = ui, server = server)