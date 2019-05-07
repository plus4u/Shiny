## PCA

library(shiny)
library(shinythemes)
library(psych)

ui <- fluidPage(
    
    theme = shinytheme("united"),
    titlePanel("PCA by ghSeo"),
    br(),
    br(),
    br(),
    
    sidebarPanel(
        conditionalPanel(condition = "$('li.active a').first().html()==='Data-Import'",
                         # sidebarLayout(
                         #    sidebarPanel(
                         h2("Import dataset:"), 
                         p("it is pca based on svd   "), 
 #                        tags$a(href = "http://en.wikipedia.org/wiki/Student's_t-test", "More Detail "),
                         br(), 
                         
                         fileInput('file', 'Choose CSV File',
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
        
        # SliderbarPanel for   tab
        conditionalPanel(condition ="$('li.active a').first().html()==='Analysis-PCA",
                         uiOutput("var_select"),
                         helpText("Note: Please assign proper dataset ")
                         
        )),
    
    
    mainPanel(
        tabsetPanel(
            tabPanel("Data-Import", DT::dataTableOutput('tbl')),

            tabPanel('Analysis-PCA',
                     fluidRow(column(8, offset = 1,
                                     h2("Key summary statistics"),
                                     p("The observed sample statistics were:"),
                                     #                                     tableOutput('parametric'),
                                     h2("Hypothesis of pca"), 
                                     p("prcomp-out :"),
                                     textOutput('prcomp-out'),
                                     br(),
                                     textOutput('sdev'),
                                     p("rotation "),
                                     textOutput('rotation')
                     ))),
            #                     br(),
            #                     br(),
            #                     br()),
            tabPanel('Data-Summary', 
                     fluidRow(column(10, offset = 1,
                                     h2("Summary"),
                                     verbatimTextOutput('test'))),
                     
                     fluidRow(column(10, offset = 1,
                                     h2("Summary"),
                                     verbatimTextOutput('summ'))),
                     
                     fluidRow(column(10, offset = 1,
                                     h2("Structure"),
                                     verbatimTextOutput('str'))),
                     br(),
                     br()            ),           
            
            tabPanel('Scree-plot',
                     plotOutput('scrPlot'))
        ))
)


# shinyServer(function(input, output, session) {

server <- function(input, output, session){ 
    
    data <- reactive({
        file1 <- input$file
        if(is.null(file1)){return()} 
#        read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
        read.table(file=file1$datapath, sep=input$sep, header = input$header)
        
    })  
    
    
    output$tbl = DT::renderDataTable({
        DT::datatable(data(), options = list(lengthChange = FALSE))
    })   
    
    
    output$var_select <- renderUI({
        checkboxGroupInput("var_selected","Select Variables", choices =as.list(names(data())))
    })
        
  
    
    
    # Updata value user could select
#    observe({
#        updateSelectInput(
#            session,
#            "var_selected",
#            choices=names(data()))
#    })

    # Output a t distribution plot
    output$scrPlot <- renderPlot({
        screeplot(fit) 
    })
    
    # Output of discriptive summary of this variable
    output$summ <-  renderPrint({
        if (is.null(data())){return(NULL)}
        summary(data())
    })
    
    # Output of the data structure
    output$str <- renderPrint({
        if (is.null(data())){return(NULL)}
        str(data())
    })
    
    # analysis output
#    output$prcomp-out <- renderPrint({
#        x <- data()[ , which(names(data()) %in% c(input$var_selected))]
#        fit <- prcomp(x, center=T, scale=T)
#        summary(fit)
#    })
    
    
#    # test  
#    output$test <- renderPrint({
#        data()[ , which(names(data()) %in% c(input$var_selected))]
#    })
    
    # Output of one  
    output$sdev <- renderPrint({
        fit$sdev
    })
    
    # Output of  
    output$rotation <- renderPrint({
        fit$rotation
    })
    
    # Output of key statistical parametric
#    output$parametric <- renderTable({
#        x <- data()[,input$var_selected]
#        if (is.null(var)){return(NULL)}
 #       mean1 <- mean(x) 
 #       standard_deviation1 <- sd(x)
 #       standard_error1 <- sd(var_selected)/sqrt(length(var_selected)) 
 #       parametric1 <- data.frame(mean = mean1,  standard_deviation=standard_deviation1 )
 #       rownames(parametric1) <- input$var_selected
 #       return(parametric1)}) 
    
    output$scrplot <- renderPlot({
        screeplot(fit) })
}

##

shinyApp(ui = ui, server = server)
 

