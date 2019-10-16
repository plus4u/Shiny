## 05 : plot output


library(shiny)

ui <- fluidPage(

# shinyUI(fluidPage(   
    
    titlePanel("plot output"),
    
    sidebarLayout(
        sidebarPanel(("Enter your name and ages"),
                     textInput("name","Enter your name",""),
                     sliderInput("bins","Select the number of bins",min = 1,max=100,value = 30, animate = T),
                     radioButtons("gender","Select your gender", list("female"="female","male"="male"),"")),
        
        mainPanel(
            ("mainPanel : Personal information"),
            
            textOutput("myname"),
            br(),
            textOutput("myage"),
            br(),
            textOutput("mygender"),
            
            plotOutput("distplot"))
        
        # "plot of faithful dataset" 
        
        
    )
)



## server 

server <- function(input, output, session){

# shinyServer(  function(input, output) {
        
    output$myname <-renderText(input$name)
    output$myage <-renderText(input$age)
    output$mygender <- renderText(input$gender)
    
    output$distplot <- renderPlot({
        x <- faithful[,2]
        bins <- seq(min(x),max(x), length.out = input$bins + 1)
        hist(x, breaks = bins, col='darkgray',border = 'white')
    })
    
}


## shinyApp

shinyApp(ui, server)


str(faithful)
 