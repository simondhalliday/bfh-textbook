library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Competitive credit markets with equity"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("k",
                  "Equity (k):",
                  min = 0,
                  max = 1,
                  value = .5,
                  step = .1),
      #SH:2014.10.12:11:06: Changed this to a baseline of 1 increasing to 2
      #SH:2014.10.12:11:31: Edited it back to min=0 and max=1 because I was having issues. 
      sliderInput("mu",
                  HTML("Project Quality (&mu;):"),
                  min = 1,
                  max = 2,
                  value = 1.5,
                  step = .1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("linePlot")
    )
  )
))