library(shiny)
# install.packages("shinyWidgets")

library(shinyWidgets)

ui <- basicPage(
    sidebarPanel(
        pickerInput("locInput","Location", choices=c("New Mexico", "Colorado", "California"), options = list(`actions-box` = TRUE),multiple = T)
    )
)

server <- function(input, output) {
    
    observe({
        print(input$locInput)
    })
    
}

shinyApp (ui = ui, server = server)
