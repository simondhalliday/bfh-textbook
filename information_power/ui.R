library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Principal-Agent Problem: Competitive wine quality markets"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("delta",
                  "Disutilit of Agent's Effort (delta):",
                  min = 0,
                  max = 1,
                  value = 0.5,
                  step = 0.05),
      
    tags$h5("How the model works"),
    tags$p("The vintner (agent) decides on their", 
           tags$strong("best response,"), 
           "that is, the quality of wine (q) 
           given the price (p) announced by the
           wine merchant (principal), given by q = 1 - 2/p. 
           This is also called the agent's ", 
           tags$strong("incentive compatibility constraint."),""), 
    tags$p("For the vintner (agent) to sell wine to the wine merchant (principal),  
           the vintner's BRF must intersect or be tangent to the principal's", 
           tags$strong("maximum profit first order condition,"),
           "q=p/a, because the principal takes the agent's incentive compatibility 
           constraint as given when maximizing their profit. 
           The wine merchant's profit is affected by 
           the level of the mark-up on the wine, a. The higher the mark-up, 
           the higher the price the wine merchant will offer to the vintner."
           )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("linePlot")
    )
  )
))