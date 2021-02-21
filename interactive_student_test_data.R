


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
    dashboardHeader(
        title = "Visualizing Student Test Data"
    ), 
    dashboardSidebar(), 
    dashboardBody(
        
        box(
            selectInput(inputId = "group", label = "Group By: ", 
                        choices = c("googleable", "non.googleable"), 
                        selected = "Googleable"), 
            plotOutput("frequencyplot")
        )
    )
    
    )


# Server 
server <- function(input, output) {
        
        output$frequencyplot <- renderPlot({ 
            
            GROUP <- input$group 
            fPlot <- plot(quiz_cat[ , GROUP], main = "Quiz Type") 
            paste(fPlot) 
            
            })
}
        




# Run the application 
shinyApp(ui = ui, server = server)

