# ui.R

shinyUI(fluidPage(
  titlePanel("Gradesource Tidier"),
  
  sidebarPanel(
    textInput("url", label = h3("Gradesource Url")),
    helpText
    (
      "Just paste the gradesource url emailed by your instructor. By
    default, only the grades for categories are shown. However, you
    can select the option to view points for each assignment if needed."
    ),
    textInput("number", label = h3("Secret Number")),
    checkboxInput("categories", 
                  label = "Show me assignment scores instead"),
    uiOutput("graphs")
  ),
  
  mainPanel(
    tableOutput("grades"),
    plotOutput("plotOverall")
  )
))
