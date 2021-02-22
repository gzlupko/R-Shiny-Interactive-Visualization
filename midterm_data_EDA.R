# data analysis of midterm test data to add to shiny app server 


midterm <- read_csv("midterm-results.csv") 
quiz_cat <- read_csv("quiz-categories.csv") 

# Question Topics - number of questions asked by topic 









# Question 'difficulty' 
# create custom function to calculate question difficulty 
# then bring that over to categories data and use regression to 
# predict topic difficulty for students

question_difficulty <- midterm %>%
  summarize(prop_agreed = mean(Q1_c)) %>%
  mutate(Q1_difficulty = mean(Q1_time) * prop_agreed)


question_difficulty <- function(x) { 
  
  x <- paste(x, "_c", sep = "") 
  y <- paste(x, "_time", sep = "") 
  
  midterm %>%
    select(x, y) %>%
    mutate(question_difficulty = mean(x) * mean(y)) 
  }



question_difficulty(midterm$Q1) 



quiz_cat %>%
  group_by(wrangling) 


  


  

quiz_transpose <- as.data.frame(t(as.matrix(quiz_cat)))

View(quiz_transpose) 

quiz_transpose %>%
  mutate(total)
  

View(quiz_cat) 
View(quiz_transpose) 

ggplot(data = quiz_transpose, aes(x = Question, y = wrangling)) + geom_bar(stat = "identity") 

