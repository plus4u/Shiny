library(shiny)
    shinyApp(
        ui = basicPage(
            textInput("txt", "Enter the text to display below:"),
            verbatimTextOutput("default"),
            verbatimTextOutput("placeholder", placeholder = TRUE)
        ),
        server = function(input, output) {
            output$default <- renderText({ input$txt })
            output$placeholder <- renderText({ input$txt })
        }
    )
