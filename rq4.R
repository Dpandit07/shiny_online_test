library(shiny)

# Define the questions and answers
questions <- c("What is the capital of France?",
               "Who was the first president of the United States?",
               "What is the largest organ in the human body?")
answers <- list("Paris", "George Washington", "Skin")

# Define the ShinyApp UI
ui <- fluidPage(
  titlePanel("Multiple Choice Quiz"),
  sidebarLayout(
    sidebarPanel(
      h4("Select your answer for each question:"),
      fluidRow(
        column(width = 4, h5(questions[1])),
        column(width = 8, radioButtons("answer1", label = NULL, choices = answers[[1]]))
      ),
      fluidRow(
        column(width = 4, h5(questions[2])),
        column(width = 8, radioButtons("answer2", label = NULL, choices = answers[[2]]))
      ),
      fluidRow(
        column(width = 4, h5(questions[3])),
        column(width = 8, radioButtons("answer3", label = NULL, choices = answers[[3]]))
      ),
      actionButton("submit", "Submit Answers")
    ),
    mainPanel(
      h4("Results:"),
      verbatimTextOutput("results")
    )
  )
)

# Define the ShinyApp server
server <- function(input, output) {
  # Define the reactive function for the quiz results
  quiz_results <- reactive({
    # Check if all questions have been answered
    if (is.null(input$answer1) || is.null(input$answer2) || is.null(input$answer3)) {
      return("Please answer all questions.")
    }
    
    # Check the answers and calculate the score
    score <- 0
    if (input$answer1 == answers[[1]][1]) {
      score <- score + 1
    }
    if (input$answer2 == answers[[2]][1]) {
      score <- score + 1
    }
    if (input$answer3 == answers[[3]][1]) {
      score <- score + 1
    }
    
    # Return the results
    return(paste("You got", score, "out of 3 questions correct."))
  })
  
  # Define the output for the quiz results
  output$results <- renderPrint({
    quiz_results()
  })
}

# Run the ShinyApp
shinyApp(ui = ui, server = server)
