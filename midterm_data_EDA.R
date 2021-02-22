# data analysis of midterm test data to add to shiny app server 


midterm <- read_csv("midterm-results.csv") 
quiz_cat <- read_csv("quiz-categories.csv") 

# Question Topics - number of questions asked by topic 



View(quiz_cat) 






# Question 'difficulty' 
# create custom function to calculate question difficulty 
# then bring that over to categories data and use regression to 
# predict topic difficulty for students

question_difficulty <- midterm %>%
  summarize(prop_agreed = mean(Q1_c)) %>%
  mutate(Q1_difficulty = mean(Q1_time) * prop_agreed)


question_difficulty <- function(x) { 
  
  question_number <- as.character(paste("Q", x, "_c", sep =""))  
  question_time <- as.character(paste("Q", x, "_time", sep = "")) 
  
  midterm %>%
    summarize(pase = mean(question_number) * mean(question_time)) 
  }



question_difficulty(1) 



midterm_filtered <- midterm %>%
  select(Q1_c, Q1_time, Q2_c, Q2_time, Q3_c, Q3_time, Q4_c, Q4_time, 
         Q5_c, Q5_time, Q6_c, Q6_time, Q7_c, )



# brute force approach ad
difficulty_column <- midterm %>%
  summarize(Q1_difficulty = mean(Q1_c) * mean(Q1_time), 
            Q2_difficulty = mean(Q2_c) * mean(Q2_time), 
            Q3_difficulty = mean(Q3_c) * mean(Q3_time), 
            Q4_difficulty = mean(Q4_c) * mean(Q4_time), 
            Q5_difficulty = mean(Q5_c) * mean(Q5_time), 
            Q6_difficulty = mean(Q6_c) * mean(Q6_time), 
            Q7_difficulty = mean(Q7_c) * mean(Q7_time), 
            Q8_difficulty = mean(Q8_c) * mean(Q8.9_time), 
            Q10_difficulty = mean(Q10_c) * mean(Q10_time), 
            Q11_difficulty = mean(Q11_c) * mean(Q11_time), 
            Q12_ifficulty = mean(Q12_c) * mean(Q12_time), 
            Q13_difficulty = mean(Q13_c) * mean(Q13_time), 
            Q14_difficulty = mean(Q14_c) * mean(Q14_time), 
            Q15_difficulty = mean(Q15_c) * mean(Q15_time), 
            Q16_difficulty = mean(Q16_c) * mean(Q16_time), 
            Q17_difficulty = mean(Q17_c) * mean(Q17_time))  


topics_and_difficulty <- cbind(quiz_cat, difficulty_column) 


difficulty_calculation <- function(x) { 
  
  question_number <- paste("Q", x, "_c", sep ="") 
  question_time <- paste("Q", x, "_time") 
  
  midterm %>%
summarize(Qx_difficulty = mean(question_number)*mean(question_time)) } 




test_output <- function(x) { 
  
  question_number <- as.character(paste("Q", x, "_c", sep ="")) 
  paste(question_number)
} 




question_difficulty <- function(x) { 
  
  question_number <- paste("Q", x, "_c", sep ="", collapse = "")
  mean(midterm$question_number) 
  
} 



question_difficulty(3) 



paste("Q", 3, "_c", sep ="", collapse = "")



as.character(paste("Q", 3, "_c", sep = "")) 

class(midterm$Q10_c) 




test_calculation <- difficulty_calculation(3)          
View(test_calculation) 


View(difficulty_calculation(Q3))           



      






  

quiz_transpose <- as.data.frame(t(as.matrix(quiz_cat)))

View(quiz_transpose) 

quiz_transpose %>%
  mutate(total)
  

View(quiz_cat) 
View(quiz_transpose) 

ggplot(data = quiz_transpose, aes(x = Question, y = wrangling)) + geom_bar(stat = "identity") 

