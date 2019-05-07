## linear regression

library(shiny)

ui <- fluidPage(
    titlePanel("Regression Model"),
    sidebarLayout(
        
        sidebarPanel(
            
            fileInput("file","Upload your CSV",multiple = FALSE),
            tags$hr(),
            h5(helpText("Select the read.table parameters below")),
            
            checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
            
            checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
            
            radioButtons(inputId = 'sep', label = 'Separator', 
                         choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
            uiOutput("var_y"),
            uiOutput("var_x")
            
#            selectInput("var_x_select", label = h3("var_x_select"),
#                        choices = list("Fertility" = "Fertility",
#                                       "Agriculture" = "Agriculture",
#                                       "Examination" = "Examination",
#                                       "Education" = "Education",
#                                       "Catholic" = "Catholic",
#                                       "Infant.Mortality" = "Infant.Mortality"), selected = 1),

#            selectInput("var_y_select", label = h3("Explanatory variable"),
#                        choices = list("Fertility" = "Fertility",
#                                    "Catholic" = "Catholic",
#                                       "Infant.Mortality" = "Infant.Mortality"), selected = 1)
       
        ),
    
    mainPanel(
        
        tabsetPanel(type = "tabs",
                    
                    tabPanel("Data", DT::dataTableOutput('tbl')), # Data as datatable                  
                    
                    tabPanel("Scatterplot", plotOutput("scatterplot")), # Plot
                    tabPanel("Model Summary", verbatimTextOutput("summary")), # Regression output
                    tabPanel("Distribution", # Plots of distributions
                             fluidRow(
                                 column(6, plotOutput("distribution1")),
                                 column(6, plotOutput("distribution2")))
                    )
                    
                    
            )
        )))



# SERVER
server <- function(input, output) {
 
    data <- reactive({
        file1 <- input$file
        if(is.null(file1)){return()} 
        
        read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
        
    })  
    
    output$var_y <- renderUI({
        checkboxGroupInput("var_y_select","Select respond Var", choices =as.list(names(data())))
    })
    
    output$var_x <- renderUI({
        checkboxGroupInput("var_x_select","Select independant Var", choices =as.list(names(data())))
    })  
     
    # Regression output
    output$summary <- renderPrint({
        fit <- lm(data()[,input$var_y_select] ~ data()[,input$var_x_select])
        names(fit$coefficients) <- c("Intercept", input$var2)
        summary(fit)
    })
    
    # Data output
    output$tbl = DT::renderDataTable({
        DT::datatable(data(), options = list(lengthChange = FALSE))
    })
    
    
    # Scatterplot output
    output$scatterplot <- renderPlot({
        plot(data()[,input$var_x_select], data()[,input$var_y_select], main="Scatterplot",
             xlab=input$var_x_select, ylab=input$var_y_select, pch=19)
        abline(lm(data()[,input$var_x_select] ~ data()[,input$var_y_select]), col="red")
        lines(lowess(data()[,input$var_y_select],data()[,input$var_x_select]), col="blue")
    }, height=400)
    
    
    # Histogram output var 1
    output$distribution1 <- renderPlot({
        hist(data()[,input$var_x_select], main="", xlab=input$var_x_select)
    }, height=300, width=300)
    
    # Histogram output var 2
    output$distribution2 <- renderPlot({
        hist(data()[,input$var_y_select], main="", xlab=input$var_y_select)
    }, height=300, width=300)
}

shinyApp(ui = ui, server = server)