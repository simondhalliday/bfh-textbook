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
                  min = 0,
                  max = 4,
                  value = 2,
                  step = .2),
      
      #sliderInput("rho",
       #           HTML("Interest Rate (&rho;):"),
        #          min = -1.5,
         #         max = 0.5,
          #        value = -.75,
           #       step = .15)
      
    tags$h5("How the model works"),
    tags$p("The borrower decides on their", 
           tags$strong("best response"), 
           "(the speed at which they run the machine (f)) considering:"),
    HTML("
      <ul>
      <li>the interest factor that they face (&delta;)</li> 
      <li>the quality of their project (&mu;)</li>
      <li>how much capital/equity they have (k)</li>
        </ul>
    "),
    tags$p("For the borrower to get funds, 
           their BRF must intersect or be tangent to the lender's", 
           tags$strong("iso-expected return function"),
           HTML("(&pi; = 1 + &rho; = &delta;(1-f))")
           )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("linePlot")
    )
  )
))