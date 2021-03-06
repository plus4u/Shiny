### covariance 

library(shiny)
library(shinythemes)

# shinyUI(fluidPage(

ui <- fluidPage(
    
    theme = shinytheme("united"),
    titlePanel("Covariance & Correlation"),
    br(),
    br(),
    br(),
    
    sidebarPanel(

        # SlidebarPanel for file upload tab
        conditionalPanel(
            condition = "$('li.active a').first().html()==='Data View'",
                         fileInput('file1', 'Choose CSV File',
                                   accept=c('text/csv', 
                                            'text/comma-separated-values,text/plain', 
                                            '.csv')),
                         tags$hr(),
                         checkboxInput('header', 'Header', TRUE),
                         radioButtons('sep', 'Separator',
                                      c(Comma=',',
                                        Semicolon=';',
                                        Tab='\t'),
                                      ','),
                         radioButtons('quote', 'Quote',
                                      c(None='',
                                        'Double Quote'='"',
                                        'Single Quote'="'"),
                                      '"')
        ),
        
        # SliderbarPanel for t-test tab
        conditionalPanel(condition = "$('li.active a').first().html()==='Analysis'",
                         sliderInput("bins",
                                     "Numer of bins:",
                                     min = 1,
                                     max = 50,
                                     value = 2
                         ),
                         selectInput("var1", 
                                     label = "Please Select a Numerical Variable",
                                     ""
                         ),
                         
                         selectInput("var2", 
                                     label = "Please Select a Numerical Variable",
                                     ""
                         )     )  ),
    
    mainPanel(
        tabsetPanel(
            
            tabPanel('Data View', 
                     fluidRow(column(10, offset = 1,
                                     h2("Data Summary"),
                                     verbatimTextOutput('disc'))),
                     
                     fluidRow(column(10, offset = 1,
                                     h2("Data Structure"),
                                     verbatimTextOutput('str'))),
                     fluidRow(column(10, offset = 1,
                                     h2("Data Table"),
                                     tableOutput('contents')))      
            ),           
            tabPanel('Analysis',
                     fluidRow(column(10, offset = 1,
                                     plotOutput('graph'))),
                     
                     fluidRow(column(8, offset = 1,
                                     h2("Key summary statistics"),
                                     p("The observed sample statistics were:"),
                                     tableOutput('parametric'),
                                     
                                     h2("Covariance & Correlation"),
                                     p("The observed covariance:"),
                                     textOutput('cvalue'),
                                     br(),
                                     br(),
                                     p("Correlation :"),
                                     textOutput('corvalue'),
                                     br(),
                                     br(),
                                     p(" gwihyeonseo@naver.com " ),
                                     br(),
                                     br()
                                     ))
            )  ) ))

##


# shinyServer(function(input, output, session) {

server <- function(input, output, session){ 
    
    data <- reactive({
        inFile <- input$file1 
        if (is.null(inFile)){return(NULL)} 
        read.csv(inFile$datapath, header=input$header, sep=input$sep, 
                 quote=input$quote)
    })
    
    
    # Updata value user could select
    observe({
        updateSelectInput(
            session,
            "var1",
            choices=names(data()))
        
    })
    # Updata value user could select
    observe({
        updateSelectInput(
            session,
            "var2",
            choices=names(data()))
        
    })

    # Output a data table for the upload tab page
    output$contents <- renderTable({
        inFile <- input$file1 
        if (is.null(inFile))
            return(NULL)
        read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
        
    })
    
    # Output a histogram for the variables user chose
    output$graph <- renderPlot({
        var1 <- data()[,input$var1]
        var2 <- data()[,input$var2]
        if (is.null(var1)){return(NULL)}
        if (is.null(var2)){return(NULL)}
        
        plot(var1 ~ var2) 
    })
    
    # Output of discriptive summary of this variable
    output$disc <-  renderPrint({
        Data <- data()
        if (is.null(Data)){return(NULL)}
        summary(Data)
    })
    
    # Output of the data structure
    output$str <- renderPrint({
        Data <- data()
        if (is.null(Data)){return(NULL)}
        str(Data)
    })
    
    # Create a  
    covout <- reactive({
        var1 <- data()[,input$var1] 
        var2 <- data()[,input$var2]
        if (is.null(var1)){return(NULL)}
        if (is.null(var2)){return(NULL)}
        c1 <- cov(var1, var2)
        return(c1)
        
    })
    
    corout <- reactive({
        var1 <- data()[,input$var1] 
        var2 <- data()[,input$var2]
        if (is.null(var1)){return(NULL)}
        if (is.null(var2)){return(NULL)}
        c2 <- cor(var1, var2)
        return(c2)
        
    })
      
    
    # Output of one corvariance
    output$cvalue <- renderPrint({
        vals <- covout()
        if (is.null(vals)){return(NULL)}
        vals
    })
    
    output$corvalue <- renderPrint({
        vals <- corout()
        if (is.null(vals)){return(NULL)}
        vals
    })

    
    # Output of key statistical parametric
    output$parametric <- renderTable({
        var1 <- data()[,input$var1]
        if (is.null(var)){return(NULL)}
        var2 <- data()[,input$var2]
        if (is.null(var)){return(NULL)}
        mean1 <- mean(var1)
        mean2 <- mean(var2)
        standard_deviation1 <- sd(var1)
        standard_deviation2 <- sd(var2)
        standard_error1 <- sd(var1)/sqrt(length(var1))
        standard_error2 <- sd(var2)/sqrt(length(var2))
        parametric1 <- data.frame(mean = mean1, 
                                  standard_deviation=standard_deviation1, 
                                  standard_error=standard_error1)
        rownames(parametric1) <- input$var1
        parametric2 <- data.frame(mean = mean2, 
                                  standard_deviation=standard_deviation2, 
                                  standard_error=standard_error2)
        rownames(parametric2) <- input$var2
        return(rbind(parametric1,parametric2))
    })
    
    
    
}


## shinyApp

shinyApp(ui, server)
