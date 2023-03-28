library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Sample Questions"),
  sidebarLayout(
    sidebarPanel(
      h3("Instructions"),
      p("Please answer the following questions."),
      hr(),
      h3("Questions"),
      uiOutput("questions"),
      actionButton("submit", "Submit")
    ),
    mainPanel(
      h3("Results"),
      verbatimTextOutput("results")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Define questions and correct answers
  questions <- data.frame(
    Question = c("What is the capital of France?", "What is 2 + 2?", "What is the tallest mountain in the world?"),
    Answer = c("Paris", "4", "Mount Everest")
  )
  
  # Render questions
  output$questions <- renderUI({
    question_list <- lapply(seq_len(nrow(questions)), function(i) {
      question <- questions[i, "Question"]
      choices <- c(questions[i, "Answer"], paste0("Wrong answer ", i, "."))
      radioButtons(paste0("q", i), question, choices)
    })
    do.call(tagList, question_list)
  })
  
  # Calculate results
  results <- reactive({
    scores <- lapply(seq_len(nrow(questions)), function(i) {
      answer <- input[[paste0("q", i)]]
      correct_answer <- questions[i, "Answer"]
      score <- ifelse(answer == correct_answer, 1, 0)
      score
    })
    correct <- lapply(seq_len(nrow(questions)), function(i) {
      correct_answer <- questions[i, "Answer"]
      paste0("Question ", i, ": ", correct_answer)
    })
    list(scores = sum(unlist(scores)), correct = correct)
  })
  
  # Render results
  # Render results
  output$results <- renderText({
    if (input$submit > 0) {
      correct <- results()$correct
      total <- nrow(questions)
      paste("You answered", results()$scores, "out of", total, "questions correctly.","\n\n",
            "Here are the correct answers:\n\n", paste(correct, collapse = "\n"))
    }
  })
  
}

# Run the app
shinyApp(ui, server)
