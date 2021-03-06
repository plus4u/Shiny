
library(shiny)

ui <- fluidPage(
    downloadLink("downloadData", "Download")
)

server <- function(input, output) {
    # Our dataset
    data <- mtcars
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
            write.csv(data, file)
        }
    )
}

shinyApp(ui, server)
}