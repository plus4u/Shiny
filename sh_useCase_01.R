### use case upload file, select analytics & report plot 
### option : default , sep

library(shiny)

ui <- fluidPage(
    
    titlePanel(h4("upload files & summary data")),
    sidebarLayout(
        
        sidebarPanel(
            fileInput("file","upload the file"),
            
            helpText("default max. file size is 5 MB"),
            tags$hr(),
            
            checkboxInput(inputId = "header",label = "Header", value = FALSE),
            
            br(),
            radioButtons(inputId = "analytics",label="Select analytics",choices = c("One t-test"=',', "oneWay ANOVA" = 'aov'))),
        
        mainPanel(
            uiOutput("tb")
            
    )
)) 

##

server <- function(input, output, session){
    
    data <- reactive({
        
        file1 <- input$file
        if(is.null(file1)){ return()}
#        read.table(file=file1$datapath,sep=input$sep,header = input$header,stringsAsFactors = input$stringsAsFactors)
#        read.table(file=file1$datapath,sep=",",header =input$header)
        
        read.table(file=file1$datapath)
        
    })
    
    
    output$filedf <- renderTable({
        if(is.null(data())){return()}
        input$file
    }) 
    
    output$sum <- renderTable({
        if(is.null(data())){return()}
        summary(data())
    })
    
    output$table <- renderTable({
        if(is.null(data())) {return()}
        data()
    })
    
    output$analytics <- renderTable({
        if(is.null(data())) {return()}
#        data()
#       t1 <- t.test(x$math, mu=50)
#        t.test(data()$math, mu=50)
##        summary(data())
        t.test(data(), mu=50)
        
    })
    
    output$tb <- renderUI ({
        if(is.null(data())) {return()}
        # h5("Powered by", tags$img(src='RStudio-Ball.png', height=200, width=200))
        
        else
            x <- data()
            tabsetPanel(
                tabPanel("About file",tableOutput("filedf")),
                tabPanel("Data",tableOutput("table")),
                tabPanel("Summary",tableOutput("sum")),
                tabPanel("Analytics",tableOutput("analytics"))
            )
        })
}
        

## shinyApp

shinyApp(ui, server)

###

