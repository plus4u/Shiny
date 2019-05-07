
ui <- fluidPage(
    
    titlePanel("Sum of two integers"),
    
    #number input form
    sidebarLayout(
        sidebarPanel(
        textInput("one", "First Integer"),
        textInput("two", "Second Integer"),
        actionButton("add", "Add")
    ),
    
    # Show result
    mainPanel(
        
        textOutput("sum")
        
    )))
    
    
    
    ##
    
    server <- function(input,output,session) {
        #observe the add click and perform a reactive expression
        observeEvent( input$add,{
            x <- as.numeric(input$one)
            y <- as.numeric(input$two)
            
            #reactive expression
            n <- x+y
            
            output$sum <- renderPrint(n)
        }
        
        )}
    
    ## shinyApp
    
    shinyApp(ui, server) 
        
        
        