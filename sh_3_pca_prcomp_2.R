## linear regression

library(shiny)

ui <- fluidPage("PCA Model by ghs", 
                tabPanel("Data Import",
                         
                         sidebarLayout(
                             
                             sidebarPanel(
                                 
                                 fileInput("file","Upload your CSV",multiple = FALSE),
                                 tags$hr(),
                                 h5(helpText("Select the read.table parameters below")),
                                 
                                 checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
                                 
                                 checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                     
                     radioButtons(inputId = 'sep', label = 'Separator', 
                                  choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
                 ),
                 
                 mainPanel(
                     tabPanel("Data", DT::dataTableOutput('tbl'))
                     
                 ))),
    
    
    
    tabPanel("Analysis",
             
             sidebarLayout(
                 sidebarPanel(
                     uiOutput("var_select")
                     ),
                 
                 mainPanel(
                     tabsetPanel(
                         
                         tabPanel("Data Summary", verbatimTextOutput("data_sum")),
                         
                         
                         tabPanel("Scree Plot", plotOutput("scrplot")
                                  
                         )            )
                 )))
                      )


server <- function(input, output) {
    
    data <- reactive({
        file1 <- input$file
        if(is.null(file1)){return()} 
        
        read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
        
    })  
    
    # Data output
    output$tbl = DT::renderDataTable({
        DT::datatable(data(), options = list(lengthChange = FALSE))
    })   
    
    
    output$var_select <- renderUI({
        checkboxGroupInput("var_selected","Select Variables", choices =as.list(names(data())))
    })
    
    
    # Regression output
    output$data_sum <- renderPrint({
        
        x <- data()[ , which(names(data())) %in% c(input$var_selected)]
        fit <- prcomp(x, center = T, scale=T)
        summary(fit)
    })
    

    # Scree plot
    output$scrplot <- renderPlot({
        screeplot(fit) 
    })
}


shinyApp(ui = ui, server = server)

 