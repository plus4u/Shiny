## linear regression

library(shiny)

ui <- navbarPage("PCA Model by ghs",
    titlePanel("PCA Model"),
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
#                     uiOutput("tb1"))
                     tabPanel("Data", DT::dataTableOutput('tbl'))
                 
             ) ),
    
    ##
    tabPanel("Analysis",

             sidebarLayout(sidebarPanel(
                 uiOutput("var_select")),
                 
                 mainPanel(
                     tabsetPanel(type = "tabs",
#                                 tabPanel("Data", DT::dataTableOutput('tbl')), # Data as datatable                  
                                 
                                 tabPanel("Data Summary", verbatimTextOutput("data_sum")), 
                                 
                                 tabPanel("Model Summary", verbatimTextOutput("summary")),
                                 tabPanel("Scree Plot", plotOutput("scrplot")
                                                   
                    )            )
        )))))

 
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
    
    
    output$var_sel <- renderUI({
        checkboxGroupInput("var_selected","Select Variables", choices =as.list(names(data())))
    })
    
  
    # Regression output
    output$summary <- renderPrint({
        
        x <- data()[ , which(names(data())) %in% c(input$var_selected)]
        fit <- prcomp(x, center = T, scale=T)
#        fit <- prcomp(data(),[,input$var_select] ~ data()[,input$var_x_select])
        summary(fit)
    })

    
    # Scatterplot output
    output$scatterplot <- renderPlot({
        plot(data()[,input$var_x_select], data()[,input$var_y_select], main="Scatterplot",
             xlab=input$var_x_select, ylab=input$var_y_select, pch=19)
        abline(lm(data()[,input$var_x_select] ~ data()[,input$var_y_select]), col="red")
        lines(lowess(data()[,input$var_y_select],data()[,input$var_x_select]), col="blue")
    }, height=400)
    
    
    # Scree plot
    output$scrplot <- renderPlot({
        screeplot(fit) 
    })
}


shinyApp(ui = ui, server = server)

 