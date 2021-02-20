


# libraries 

library(shiny)
library(dplyr) 
library(ggplot2) 
library(readr) 
library(shinydashboard) 

# load student quiz categories and midterm test results 

midterm <- read_csv("midterm-results.csv") 
quiz_cat <- read_csv("quiz-categories.csv") 




# UI
ui <- dashboardPage( 
    dashboardHeader(), 
    dashboardSidebar(), 
    dashboardBody()
    
    )





# Server 
server <- function(input, output) {} 






# Run the application 
shinyApp(ui = ui, server = server)
