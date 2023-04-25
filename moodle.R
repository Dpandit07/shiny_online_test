library(shiny)
library(shinyWidgets)

# Define quiz questions
questions <- list(
  list(text = "What is the capital of France?", 
       choices = list("Paris", "Berlin", "London"), 
       correct = "Paris"),
  list(text = "What is the largest planet in our solar system?", 
       choices = list("Jupiter", "Mars", "Venus"), 
       correct = "Jupiter"),
  list(text = "What is the highest mountain in the world?", 
       choices = list("Mount Kilimanjaro", "Mount Everest", "Mount Fuji"), 
       correct = "Mount Everest")
)

# Define helper function to create radio buttons for answer choices
radio_choices <- function(choices) {
  choices_list <- lapply(choices, function(choice) {
    radioButtons(selected = "", choice)
  })
  tagList(choices_list)
}

# Define UI
ui <- fluidPage(
  titlePanel("Quiz"),
  sidebarLayout(
    sidebarPanel(
      awesomeCheckbox(id = "quiz_wizard",
             lapply(seq_along(questions), function(i) {
               question <- questions[[i]]
               fluidRow(
                 column(12, h4(question$text)),
                 column(12, radio_choices(question$choices))
               )
             }),
             steps = lapply(seq_along(questions), function(i) {
               question <- questions[[i]]
               wizard_step(
                 title = paste0("Question ", i),
                 subtitle = question$text,
                 description = "",
                 icon = icon("question")
               )
             })
      )
    ),
    mainPanel(
      verbatimTextOutput("results")
    )
  )
)

# Define server
server <- function(input, output, session) {
  # Calculate quiz results
  results <- reactive({
    correct <- 0
    for (i in seq_along(questions)) {
      if (input[[paste0("question", i)]] == questions[[i]]$correct) {
        correct <- correct + 1
      }
    }
    list(
      score = paste0(correct, " out of ", length(questions)),
      feedback = ifelse(correct == length(questions), "Congratulations, you passed the quiz!", "Sorry, you did not pass the quiz.")
    )
  })
  
  # Display quiz results
  output$results <- renderPrint({
    results()$score
    results()$feedback
  })
}

# Run app
shinyApp(ui, server)
