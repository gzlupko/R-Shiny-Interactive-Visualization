


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
            plotOutput('quiz_help')
        )
    )
    
    )


# Server 

server <- function(input, output) {
    
    output$quiz_help <- renderPlot({ 
        quiz_data <- quiz_cat %>%
            group_by(googleable, non.googleable) 
        
        ggplot(data = quiz_data, aes(x = input$group)) + geom_bar(stat = "identity")
        
        })
        
}

        

# Run the application 
shinyApp(ui = ui, server = server)




# second app 


# UI

ui <- dashboardPage( 
    dashboardHeader(
        title = "Visualizing Student Test Data"
    ), 
    dashboardSidebar(), 
    dashboardBody(
        
        box(
            selectInput(inputId = "Question", label = "Question Number: ", 
                        choices = c("Q1", "Q2", "Q3", "Q4", "Q5", 
                                    "Q6", "Q7"), 
                        selected = "Choose a topic"), 
            plotOutput('question_time')
        )
    )
    
)


# Server 

server <- function(input, output) {
    
    output$question_time <- renderPlot({ 
        
        students <- as.factor(midterm$id) 
        question_number <- paste(input$Question, "_c", sep = "") 
        question_duration <- paste(input$Question, "_time", sep = "")
        
        midterm %>%
            group_by(question_number) %>%
        ggplot(aes(x = students, y = question_duration)) + 
            geom_bar(stat = "identity")
        
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)

