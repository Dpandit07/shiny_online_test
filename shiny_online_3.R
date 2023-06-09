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
  
  questions <- data.frame(
    Question = c("What is the capital of France?",
                 "What is 2 + 2?", 
                 "What is the tallest mountain in the world?"),
    Option1 = c("Paris", "3", "Mount Everest"),
    Option2 = c("London", "4", "K2"),
    Option3 = c("Berlin", "5", "Makalu"),
    Option4 = c("Madrid", "2", "Cho Oyu"),
    stringsAsFactors = FALSE
  )
  
  # Randomize the position of the correct answer for each question
  set.seed(123)
  for (i in 1:nrow(questions)) {
    correct_answer <- questions[i, paste0("Option", sample(1:4))]
    questions[i, "Answer"] <- correct_answer
  }
  
  # Render questions
  # Render questions
  output$questions <- renderUI({
    question_list <- lapply(seq_len(nrow(questions)), function(i) {
      question <- questions[i, "Question"]
      choices <- questions[i, c("Option1", "Option2", "Option3", "Option4")]
      radioButtons(paste0("q", i), question, choices = choices)
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
