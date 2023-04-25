library(shiny)

# Define the MCQ data
questions <- list(
  question1 = list(
    question = "What is the capital of France?",
    options = c("Paris", "London", "Berlin", "Madrid"),
    answer = "Paris"
  ),
  question2 = list(
    question = "What is the largest continent in the world?",
    options = c("Africa", "North America", "Europe", "Asia"),
    answer = "Asia"
  ),
  question3 = list(
    question = "What is the largest planet in the solar system?",
    options = c("Mars", "Venus", "Jupiter", "Neptune"),
    answer = "Jupiter"
  ),
  question4 = list(
    question = "What is the largest planet in the solar system?",
    options = c("Mars", "Venus", "Jupiter", "Neptune"),
    answer = "Jupiter"
  )
)

# Define the UI
ui <- fluidPage(
  
  # Define the navigation pane
  tabsetPanel(
    tabPanel("Question 1", 
             radioButtons("q1", label = questions$question1$question, 
                          choices = questions$question1$options),
             verbatimTextOutput("ans1")
    ),
    tabPanel("Question 2",
             radioButtons("q2", label = questions$question2$question, 
                          choices = questions$question2$options),
             verbatimTextOutput("ans2")
    ),
    tabPanel("Question 3",
             radioButtons("q3", label = questions$question3$question, 
                          choices = questions$question3$options),
             verbatimTextOutput("ans3")
    ),
    tabPanel("Score",
             verbatimTextOutput("score")
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Define the answer output for question 1
  output$ans1 <- renderText({
    if(input$q1 == questions$question1$answer) {
      "Correct!"
    } else {
      paste("Incorrect. The correct answer is", questions$question1$answer)
    }
  })
  
  # Define the answer output for question 2
  output$ans2 <- renderText({
    if(input$q2 == questions$question2$answer) {
      "Correct!"
    } else {
      paste("Incorrect. The correct answer is", questions$question2$answer)
    }
  })
  
  # Define the answer output for question 3
  output$ans3 <- renderText({
    if(input$q3 == questions$question3$answer) {
      "Correct!"
    } else {
      paste("Incorrect. The correct answer is", questions$question3$answer)
    }
  })
  
  # Define the score output
  output$score <- renderText({
    correct <- 0
    if(input$q1 == questions$question1$answer) correct <- correct + 1
    if(input$q2 == questions$question2$answer) correct <- correct + 1
    if(input$q3 == questions$question3$answer) correct <- correct + 1
    paste("You scored", correct, "out of 3")
  })
}

# Run the app
shinyApp(ui = ui, server = server)