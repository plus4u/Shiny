# install.packages('caret')
# install.packages('rlang')

library(shiny)
library(caret)
library(rlang)


ui <- navbarPage("Reliability Model by ghs",
               tabPanel("Data Import",
                        sidebarLayout(
                            sidebarPanel( fileInput("file","Upload your CSV",multiple = FALSE),
                                          tags$hr(),
                                          h5(helpText("Select the read.table parameters below")),
                                          checkboxInput(inputId = 'header', label = 'Header', value = TRUE),
                                          checkboxInput(inputId = "stringAsFactors", "stringAsFactors", FALSE),
                                          radioButtons(inputId = 'sep', label = 'Separator', 
                                                       choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ',')
                        ),
                        
                        mainPanel(uiOutput("tb1"))
                        ) ),
               
               tabPanel("Analysis",
                        sidebarLayout(sidebarPanel(
#                            uiOutput("model_select"),
#                            uiOutput("var1_select"),
                            uiOutput("rest_var_select")),
                            
                            mainPanel( helpText("Your Selected variables"),
                                       verbatimTextOutput("other_val_show"))))
)

server <- function(input,output) { 
    data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()} 
    
    read.table(file=file1$datapath, sep=input$sep, header = input$header, stringsAsFactors = input$stringAsFactors)
    
})  

output$tb1 <- renderUI({
    tableOutput("table")
})

output$table <- renderTable({
    if(is.null(data())){return ()}
    data()
})

# output$model_select <- renderUI({
#     selectInput("modelselect","Select Algo",choices = c("Logistic_reg"="logreg","SVM"="svm"))
# })

# output$var1_select <- renderUI({
#     selectInput("ind_var_select","Select Independent Var", choices =as.list(names(data())),multiple = FALSE)
# })

output$rest_var_select <- renderUI({
    checkboxGroupInput("other_var_select","Select other Var", choices =as.list(names(data())))
})

output$other_val_show <- renderPrint({
    input$other_var_select
#    input$ind_var_select
#    f <- data()
    
    library(caret)

    #    form <- sprintf("%s~%s",input$ind_var_select,paste0(input$other_var_select,collapse=","))
#    form <- paste0(input$other_var_select,collapse=",")
#    form <- sprintf("%s,%s",input$ind_var_select,paste0(input$other_var_select,collapse=","))
#    form <- sprintf(input$ind_var_select, paste0(input$other_var_select))
    form <- paste0(input$other_var_select)
     print(form)  
    myx <- data.frame(data()[ , which(names(data()) %in% c(form))])
#    print(data.frame(data()[ , which(names(data()) %in% c(form))]))
#    print(myx)
#    print(data()[ , which(names(data()) %in% c(form))])


#    logreg <- glm(as.formula(form),family=binomial(),data=f)
#    print(summary(logreg))
    
   
    library(psych)
#    alp <- alpha(as.formula(form))
#    print(summary(alp))
    alpha(myx)
    
})

}

shinyApp(ui=ui,server=server)
 
