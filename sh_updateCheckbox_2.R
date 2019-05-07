library(shiny) 

# vector with choices for select input

myChoices = names(mtcars)
 
    
    checkboxInput('all', 'select All/None', value = TRUE),
    
    # for the variable names from mtcars dataset
    checkboxGroupInput('mtcars', 'mtcars column variables', myChoices)
)

# server
 
    observe( {
        updateCheckboxGroupInput(
            session, 'mtcars', choices = myChoices,
            selected = if(input$all) myChoices
            
        ) })}
 