###

library(shiny)


ui <- pageWithSidebar(
    
    headerPanel('Analysis of Variance'),
    
    sidebarPanel(
        tags$head(
            tags$script(src = 'https://c328740.ssl.cf1.rackcdn.com/mathjax/2.0-latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML', type = 'text/javascript')
        ),
        helpText('This module demonstrates ANOVA for this model
                 $$Y_{ij}=\\mu_{i}+\\epsilon_{ij}$$ where \\(\\mu\\) is the group
                 mean, and \\(\\epsilon\\) is the random error term
                 \\(\\sim N(0, \\sigma^2)\\). There are three groups \\(i=1,2,3\\) and n
                 individuals \\(j=1,2,\\cdots,n\\) in each group. The goal here is
                 to test the hypothesis \\(H_0: \\mu_1=\\mu_2=\\mu_3\\) We can use
                 F-test here: \\(F=MS_{between}/MS_{within}\\).'),
    sliderInput('mu1', '\\(\\mu_1\\)', min = -5, max = 5, value = 0, step = .1),
    sliderInput('mu2', '\\(\\mu_2\\)', min = -5, max = 5, value = 0, step = .1),
    sliderInput('mu3', '\\(\\mu_3\\)', min = -5, max = 5, value = 0, step = .1),
    sliderInput('sigma', '\\(\\sigma\\)', min = 0, max = 10, value = 1, step = .1),
    sliderInput('n', '\\(n\\)', min = 2, max = 100, value = 30, step = 1)
  ),

  mainPanel(
    helpText('The red bars denote group means.'),
    plotOutput('aovPlot', height = 'auto'),
    h3('ANOVA Table'),
    verbatimTextOutput('aovSummary')
  )
)

##
server <- function(input, output, session){
    
    # Make three independent, repeatable copies of rnorm so that the underlying
    # random values don't change
    rnorm1 = repeatable(rnorm)
    rnorm2 = repeatable(rnorm)
    rnorm3 = repeatable(rnorm)
    
    gen_data = reactive(function() {
        n = input$n; mu1 = input$mu1; mu2 = input$mu2; mu3 = input$mu3; s = input$sigma
        data.frame(y = c(rnorm1(n, mu1, s), rnorm2(n, mu2, s), rnorm3(n, mu3, s)),
                   group = rep(sprintf('mu%s', 1:3), each = n))
    })
    
    output$aovPlot = renderPlot(function() {
        d = gen_data()
        par(mar = c(4, 4, .1, .1), mgp = c(2.8, 1, 0), las = 1)
        plot(y ~ group, data = d, border = 'gray', horizontal = TRUE)
        points(group ~ y, data = d, col = rgb(0, 0, 0, .8))
        points(c(input$mu1, input$mu2, input$mu3), 1:3, pch = '|', cex = 2, col = 'red')
    }, width = 600, height = 300)
    
    output$aovSummary = renderPrint(function() {
        summary(aov(y ~ group, data = gen_data()))
    })
    
} 

## shinyApp

shinyApp(ui, server)

