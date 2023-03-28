library(shiny)

# Define the UI elements
ui <- fluidPage(
  # Exam questions
  fluidRow(
    column(width = 12, h2("Question 1")),
    column(width = 12, p("What is the capital of France?")),
    column(width = 12, radioButtons("q1_answer", "Select your answer:",
                                    c("Paris", "London", "Berlin", "Madrid")))
  ),
  
  fluidRow(
    column(width = 12, h2("Question 2")),
    column(width = 12, p("What is the largest planet in the solar system?")),
    column(width = 12, radioButtons("q2_answer", "Select your answer:",
                                    c("Jupiter", "Saturn", "Uranus", "Neptune")))
  ),
  
  # Submit button
  fluidRow(
    column(width = 12, br()),
    column(width = 12, actionButton("submit_button", "Submit"))
  )
)

# Define the server logic
server <- function(input, output) {
  # Calculate the score
  observeEvent(input$submit_button, {
    score <- sum(input$q1_answer == "Paris", input$q2_answer == "Jupiter")
    showModal(modalDialog(
      title = "Your score",
      paste("You scored", score, "out of 2."),
      footer = tagList(
        actionButton("ok_button", "OK")
      )
    ))
  })
}

# Run the app
shinyApp(ui, server)
