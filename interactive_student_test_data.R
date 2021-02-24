


# libraries 

library(shiny)
library(dplyr) 
library(ggplot2) 
library(readr) 
library(shinydashboard) 

# load student quiz categories and midterm test results 

midterm <- read_csv("midterm-results.csv") 
quiz_cat <- read_csv("quiz-categories.csv") 

#midterm_filter <- midterm %>%
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
        
      search_count <- quiz_cat %>%
         count(input$group) 
      
      quiz_data <- quiz_cat %>%
            group_by(googleable, non.googleable) 
        
        ggplot(data = quiz_data, aes(x = input$group, y = search_count) + 
                 geom_bar(stat = "identity"))
        
        })
        
}

        

# Run the application 
shinyApp(ui = ui, server = server)




# second app using reactive 


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





midterm1 <- midterm %>%
  select(id, Q1_time, Q2_time, Q3_time, Q4_time, Q5_time, Q6_time, 
         Q7_time) 

midterm1
colnames(midterm1) 
choices <- colnames(midterm1) 

ui <- dashboardPage( 
    dashboardHeader(
        title = "Visualizing Student Test Data"
    ), 
    dashboardSidebar(), 
    dashboardBody(
        
        box(
            varSelectInput(inputId = "question", 
                           label = "Question Duration: ", midterm1),  
            plotOutput('question_time')
        )
    )
    
) 


# Server 

server <- function(input, output) { 
    
  
  
        output$question_time <- renderPlot({
          
                ggplot(midterm1, aes(x = id, y = !!input$question)) + 
                           geom_bar(stat = "identity") + 
            xlab("Students") + ylab("Question Duration (seconds)") 
        })
    
} 


# Run the application 
shinyApp(ui = ui, server = server)







# old version with concatenate and question_number and question_duration


# UI

ui <- dashboardPage( 
  dashboardHeader(
    title = "Visualizing Student Test Data"
  ), 
  dashboardSidebar(), 
  dashboardBody(
    
    box(
      selectInput(inputId = "Question", label = "Question Number: ", 
                  choices = c("Q1_time", "Q2_time", "Q3_time", 
                              "Q4_time", "Q5_time", 
                              "Q6_time", "Q7_time")), 
      plotOutput('question_time')
    )
  )
  
)


# Server 

server <- function(input, output) {
  
  
  
  output$question_time <- renderPlot({ 
    
    midterm <- read_csv("midterm-results.csv")
    
    
    question_duration <- paste(input$Question, "_time", sep = "")
    question_number <- paste(input$Question, "_c", sep = "")
    students <- as.factor(midterm$id)
    
    midterm %>%
      ggplot(aes(x = students, y = question_duration)) + 
      geom_bar(stat = "identity")
    
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)





# test new by using function to define new objects 


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
  
  
  output$mytable <- renderDataTable({
    
    midterm <- read_csv("midterm-results.csv") }) 
  
  
  
  output$question_time <- renderPlot({ 
  
    
    duration <- paste(input$Question, "_time", sep = "")  
    
    students <- (midterm$id) 
    
    question_duration <- midterm[ , duration]
    
    new_table$students <- students
    new_table$question_number <- input$Question
    new_table$question_duration <- question_duration
    new_table <- as.data.frame(new_table) 
    
    
    new_table %>%
      ggplot(aes(x = students, y = question_duration)) + 
      geom_bar(stat = "identity")
}) 
    
} 



# Run the application 
shinyApp(ui = ui, server = server)









# 3rd attempt with observeEvent 

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



