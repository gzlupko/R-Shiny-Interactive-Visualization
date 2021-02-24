
library(shiny) 
library(dplyr)
library(ggplot2)
library(readr)
library(shinydashboard)

midterm <- read_csv("midterm-results_copy.csv")



midterm1 <- midterm %>%
    select(id, Q1_time, Q2_time, Q3_time, Q4_time, Q5_time, Q6_time, 
           Q7_time) 

midterm1$id <- 1:26


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
            xlab("Students") + ylab("Question Duration (seconds)") + 
            ggtitle("Midterm Question Duration by Student") 
    })
    
} 

# Run the application 
shinyApp(ui = ui, server = server)
