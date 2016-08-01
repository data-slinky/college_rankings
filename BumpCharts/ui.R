library(shiny)

# Define UI for miles per gallon application
shinyUI(fluidPage(
  
  # Load the css
  includeCSS("bootstrap.css"),
  hr(),
  # Output the plot from the server
  plotOutput("plot1", width = "1200"),
  hr(),
  
  # Determine the number of colleges to include
  fluidRow(
    
    # Title of the project and link back to webpage
    column(3, titlePanel("College Rankings Bump Charts"), 
           mainPanel("by", a("data-slinky", href="http://data-slinky.com/project/3_College_rankings/"))),
    
    # Selecting the dataset
    column(4, selectInput("data", "Dataset:",
                c("Times Higher Education" = "timesData",
                  "Center for World University Rankings" = "cwurData",
                  "Shanghai Rankings" = "shanghaiData"), selected = "timesData")
    ),
    # Selecting the number of schools
    column(5, sliderInput("var1", "Top N schools:", min=1, max=20, value=10)
    )
  )
))