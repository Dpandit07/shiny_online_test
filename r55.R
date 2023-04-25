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
  
  # Rename the column names to be valid variable names
  names <- make.names(c("Option1", "Option2", "Option3", "Option4"), unique=TRUE)
  questions[names] <- questions[c("Option1", "Option2", "Option3", "Option4")]
  
  # Randomize the position of the correct answer for each question
  set.seed(123)
  for (i in 1:nrow(questions)) {
    correct_answer <- questions[i, sample(names(questions)[-1])]
    questions[i, "Answer"] <- correct_answer
  }
  
  # Render questions
  output$questions <- renderUI({
    question_list <- lapply(seq_len(nrow(questions)), function(i) {
      question <- questions[i, "Question"]
      choices <- questions[i, names(questions)[-1]]
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
