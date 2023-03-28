library(shiny)

# Define the exam questions and correct answers
questions <- list(
  list(
    "What is the capital of France?",
    c("Paris", "London", "Berlin", "Madrid"),
    "Paris"
  ),
  list(
    "What is the largest planet in the solar system?",
    c("Jupiter", "Saturn", "Uranus", "Neptune"),
    "Jupiter"
  )
)

# Define the UI elements
ui <- fluidPage(
  # Exam questions
  lapply(seq_along(questions), function(i) {
    q <- questions[[i]]
    fluidRow(
      column(width = 12, h2(paste0("Question ", i))),
      column(width = 12, p(q[[1]])),
      column(width = 12, radioButtons(paste0("q", i, "_answer"), "Select your answer:",
                                      q[[2]]))
    )
  }),
  
  # Submit button
  fluidRow(
    column(width = 12, br()),
    column(width = 12, actionButton("submit_button", "Submit"))
  )
)

# Define the server logic
server <- function(input, output) {
  # Calculate the score and display the correct answers
  observeEvent(input$submit_button, {
    # Calculate the score
    score <- sum(
      sapply(seq_along(questions), function(i) input[[paste0("q", i, "_answer")]] == questions[[i]][[3]])
    )
    
    # Display the correct answers
    correct_answers <- lapply(seq_along(questions), function(i) {
      q <- questions[[i]]
      paste0("Question ", i, ": ", q[[1]], "\nCorrect answer: ", q[[3]], "\n")
    })
    
    showModal(modalDialog(
      title = "Your score",
      paste("You scored", score, "out of", length(questions), "."),
      "Here are the correct answers:",
      paste0(correct_answers, collapse = "\n"),
      footer = tagList(
        actionButton("ok_button", "OK")
      )
    ))
  })
}

# Run the app
shinyApp(ui, server)
