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
server <- function(input, output, session) {
  
  # Define reactive values to store the current question number and the total score
  current_question <- reactiveVal(1)
  score <- reactiveVal(0)
  
  # Define the answer output for the current question
  output$ans <- renderText({
    if(input[[paste0("q", current_question())]] == questions[[paste0("question", current_question())]]$answer) {
      score(score() + 1) # increase the score if the answer is correct
      "Correct!"
    } else {
      paste("Incorrect. The correct answer is", questions[[paste0("question", current_question())]]$answer)
    }
  })
  
  # Define a reactive value to check if all questions have been answered
  all_answered <- reactive({
    !is.null(input$q1) & !is.null(input$q2) & !is.null(input$q3)
  })
  
  # Define the output for the final score
  output$final_score <- renderText({
    if(all_answered()) {
      paste("You scored", score(), "out of 3")
    } else {
      ""
    }
  })
  
  # Define the next question button
  observeEvent(input$next_question, {
    if(current_question() < 3) {
      current_question(current_question() + 1)
    }
  })
  
  # Define the previous question button
  observeEvent(input$prev_question, {
    if(current_question() > 1) {
      current_question(current_question() - 1)
    }
  })
  
  # Define the reset button
  observeEvent(input$reset, {
    current_question(1)
    score(0)
    updateRadioButtons(session, "q1", selected = NULL)
    updateRadioButtons(session, "q2", selected = NULL)
    updateRadioButtons(session, "q3", selected = NULL)
  })
  
  # Define the UI for the current question
  output$question <- renderUI({
    question <- questions[[paste0("question", current_question())]]
    tagList(
      h3(question$question),
      radioButtons(paste0("q", current_question()), label = "", choices = question$options),
      br(),
      actionButton("prev_question", "Previous Question", icon("caret-left")),
      actionButton("next_question", "Next Question", icon("caret-right"))
    )
  })
  
  # Define the UI for the final score
  output$score_ui <- renderUI({
    tagList(
      h3("Final Score"),
      verbatimTextOutput("final_score"),
      br(),
      actionButton("reset", "Reset Quiz")
    )
  })
  
  # Define the UI for the app
  output$ui <- renderUI({
    tagList(
      navbarPage(
        "Quiz App",
        tabPanel("Questions", uiOutput("question")),
        tabPanel("Score", uiOutput("score_ui"))
      )
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)