


# libraries 

library(shiny)
library(dplyr) 
library(ggplot2) 
library(readr) 
library(shinydashboard) 

# load student quiz categories and midterm test results 

midterm <- read_csv("midterm-results.csv") 
quiz_cat <- read_csv("quiz-categories.csv") 

midterm_filter <- midterm %>%
    rename(Q1 = Q1_c, Q2 = Q2_c, Q3 = Q3_c, Q4 = Q4_c)



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
                                    "Q6", "Q7")), 
            plotOutput('question_time')
        )
    )
    
)


# Server 

server <- function(input, output) {
    
    question_number <- reactive({ 
        question_number <- as.numeric(paste(input$Question, "_c", sep = ""))
        })
    
    question_duration <- reactive({ 
        question_duration <- as.numeric(paste(input$Question, "_time", sep = ""))
    })
    
    output$question_time <- renderPlot({ 
        
        students <- as.factor(midterm$id)
        
        midterm_filter %>%
            group_by(.data[[input$Question]]) %>%
        ggplot(aes(x = students, y = .env$question_duration) + 
                   geom_bar(stat = "identity")) 
    }) }


# Run the application 
shinyApp(ui = ui, server = server)







# office hours test 


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
                                    "Q6", "Q7")), 
            plotOutput('question_time')
        )
    )
    
)


# Server 

server <- function(input, output) {
    

    output$question_time <- renderPlot({ 
        
    question_duration <- as.numeric(paste(input$Question, "_time", sep = "")) 
     question_number <- as.numeric(paste(input$Question, "_c", sep = ""))
                                        
        output$question_time <- renderPlot({ 
            
            students <- as.factor(midterm$id)
            
            midterm_filter %>%
                group_by(question_number) %>%
                ggplot(aes_string(x = students, y = question_duration) + 
                           geom_bar(stat = "identity"))
      
    })
    
}) 
}


# Run the application 
shinyApp(ui = ui, server = server)





# 4th attempt 

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
                                    "Q6", "Q7")), 
            plotOutput('question_time')
        )
    )
    
)


# Server 

server <- function(input, output) {
    
    observeEvent(input$Question, { 
        question_number <- as.numeric(paste(input$Question, "_c", sep = ""))
        midterm_question <- midterm[[ , question_number]]
        })
    
    
    observeEvent(input$Question, { 
        question_duration <- as.numeric(paste(input$Question, "_time", sep = ""))
        midterm_question_duration <- midterm[[ , question_duration]]
    })

    
    output$question_time <- renderPlot({ 
        
        students <- as.factor(midterm$id)
        
        midterm_filter %>%
            group_by(midterm_question) %>%
            ggplot(aes(x = students, y = .env$midterm_question_duration) + 
                       geom_bar(stat = "identity")) 
    }) }


# Run the application 
shinyApp(ui = ui, server = server)



