### 

ui <- fluidPage(
    headerPanel("Example"),
    
    sidebarPanel(
        checkboxGroupInput("variable", "Variable:", choices = names(iris))
    ),
    mainPanel(
        tableOutput("content")
    )
)

server <- function(input, output, session) {
    
    output$content <- renderTable({
        
        if(is.null(input$variable))
            return()
        
        iris[input$variable]
    })
}

## shinyApp

shinyApp(ui, server)
