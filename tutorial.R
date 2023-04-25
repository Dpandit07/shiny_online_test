library(shiny)
library(ggplot2)

# Define the UI
ui <- fluidPage(
  
  # Sidebar panel with inputs
  sidebarLayout(
    sidebarPanel(
      # action button
      actionButton("reset_btn", "Reset"),
      
      # checkbox input
      checkboxInput("log_scale", "Use log scale", value = FALSE),
      
      # date input
      dateInput("date_input", "Select a date", value = Sys.Date()),
      
      # file input
      fileInput("file_input", "Upload a file"),
      
      # numeric input
      numericInput("bins", "Number of bins", value = 30, min = 1),
      
      # radio buttons
      radioButtons("dataset", "Select a dataset:",
                   choices = c("mtcars", "iris", "ChickWeight"),
                   selected = "mtcars"),
      
      # select input
      selectInput("var", "Select a variable:",
                  choices = c("mpg", "cyl", "disp", "hp"),
                  selected = "mpg"),
      
      # slider input
      sliderInput("range", "Range:", min = 0, max = 10, value = c(2, 8)),
      
      # text input
      textInput("title", "Title:", value = "Histogram"),
      
      # password input
      passwordInput("password", "Enter password:"),
      
      # verbatim text output
      verbatimTextOutput("summary")
      
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      plotOutput("hist")
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Render the histogram plot
  output$hist <- renderPlot({
    # Get the selected dataset
    data <- switch(input$dataset,
                   "mtcars" = mtcars,
                   "iris" = iris,
                   "ChickWeight" = ChickWeight)
    
    # Get the selected variable
    var <- input$var
    
    # Create the histogram plot
    ggplot(data, aes_string(x = var)) +
      geom_histogram(binwidth = diff(range(data[[var]])) / input$bins,
                     fill = "blue", color = "white") +
      xlim(input$range) +
      labs(title = input$title, x = var, y = "Frequency") +
      theme_bw() +
      if (input$log_scale) scale_y_log10()
  })
  
  # Reset the inputs when the reset button is clicked
  observeEvent(input$reset_btn, {
    updateCheckboxInput(session, "log_scale", value = FALSE)
    updateDateInput(session, "date_input", value = Sys.Date())
    updateNumericInput(session, "bins", value = 30)
    updateRadioButtons(session, "dataset", selected = "mtcars")
    updateSelectInput(session, "var", selected = "mpg")
    updateSliderInput(session, "range", value = c(2, 8))
    updateTextInput(session, "title", value = "Histogram")
    updatePasswordInput(session, "password", value = "")
  })
  
  # Display a summary of the inputs
  output$summary <- renderPrint({
    paste("Date input:", input$date_input,
          "\nFile input:", input$file_input$name,
          "\nNumeric input:", input$bins,
          "\nRadio buttons:", input$dataset,
          "\nSelect input:", input$var,
          "\nSlider input:", input$range,
          "\nText input:", input$title,
          "\nPassword input:", input$password,
          "\n")
  })
  
}

# Run the app


shinyApp(ui, server)
          