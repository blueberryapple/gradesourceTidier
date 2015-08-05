library(dplyr)
source("./justOnce.R")

shinyServer(function(input, output) {
  
  #uses input url to create table
  gradeTable <- reactive({
    validate(
      need(input$url != "", "")
    )
    
    assessment <- input$categories
    urlOptions <- urlBank(input$url)
    
    newUrl <-
    {
      if(assessment)
      {
        urlOptions[2]
      }
      
      else
      {
        urlOptions[1]
      }
    }
    
    newUrl %>%
      getGrades()
  })
  
  #filters table based on secret number input
  filteredTable <- reactive({
    #relates input for url with table
    input$url
    
    filter(gradeTable(), gradeTable()[,1] == input$number)
  })
  
  #uses the names of the table as options for select input
  selectChoices <- reactive({
    #relates input for url with the select options
    input$url
    
    #retrieves the list of possible graphs
    names(gradeTable())[-1]
  })
  
  #outputs the table
  output$grades <- renderTable({filteredTable()})
  
  #outputs the graph
  output$plotOverall <- renderPlot({
    xlabel <- 
    {
      if(input$categories)
      {
        "Points"
      }
      
      else
      {
        "Percent"
      }
    }
    
    selectedGraph <<- input[["graphOptions"]]
    
    gradeTable()[[selectedGraph]] %>%
      as.character() %>%
      as.numeric() %>%
      hist(main = selectedGraph, xlab = xlabel, 
           ylab = "Frequency", col = "skyblue", border = "white")
  })
  
  #outputs reactive select input
  output$graphs <- renderUI({
    selectInput("graphOptions", h3("Graph"), 
                choices = selectChoices())
  })
})