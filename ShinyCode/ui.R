library(shiny)

# Define UI for application that draws a histogram
pageWithSidebar(
  
  # Application title
  titlePanel("MFA"),
  
  sidebarPanel(
    selectInput("plot", "Select What to Plot", c("eigenvalue", "common factor scores", "partial factor scores", "loadings factor scores")),
    selectInput("col1", "x-axis", c(1:15)),
    selectInput("col2", "y-axis", c(1:15))
    ),
  
  
  mainPanel(
    plotOutput("MFAPlot")
  )
)
