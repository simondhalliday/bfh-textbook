library(shiny)

shinyServer(function(input, output) {
  
  output$linePlot <- renderPlot({
    brfFn <- function(delta, mu = 1, k = 0.5) {
      1/2 + (delta / (2 * mu)) * (1 - k)
    }
    
    isoreturnFn <- function(delta, mu = 1, rho=0.05) {
      1 - (1+rho)/delta
    }
  
    COL <- c("#46ACC8", "#E58601")
    par(mar =  c(5, 5, 4, 2))
    xlims <- c(1, 2)
    ylims <- c(0, 1)
    
    plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
         xlab = expression(paste("Interest factor, ", delta)),
         ylab = expression(paste("Probability of failure, ", f)),
         xaxt = "n", yaxt = "n", asp = 1, cex.lab = 1.2)
    
    axis(1, at = xlims)
    axis(2, at = ylims)
    
    npts <- 500
    xx <- seq(xlims[1], xlims[2], length.out = npts)
    lines(xx, brfFn(xx, mu = input$mu, k = input$k), col = COL[1], lwd = 4)
    lines(xx, isoreturnFn(xx, mu = input$mu), col = COL[2], lwd = 4)
    legend(.7, .15, legend = c(
                              expression(paste("f = 1/2 + ", delta, "(1 - k)/2", mu)),
                              expression(paste("f = 1 - (1 + ", rho, ")/", delta, ))),
           col = COL, lty = 1, lwd = 2, bty = "n")
    
    if(input$k == 0.5) {
      x_tangent <- input$mu
      lines(rep(x_tangent, 2), c(0, brfFn(x_tangent, mu = input$mu, k = input$k)),
            lty = 2, col = "darkgray")
      lines(c(0, x_tangent), rep(brfFn(x_tangent, mu = input$mu, k = input$k), 2),
            lty = 2, col = "darkgray")
      points(x_tangent, brfFn(x_tangent, mu = input$mu, k = input$k), 
             pch = 16, col = "gray45", cex = 1.5)
    }
    
    if(input$k > .5 & input$k < 1) {
      root <- sqrt((2 * input$k - 1) * input$mu^2)
      x_tan_low <- (root - input$mu)/(2 * input$k - 2)
      x_tan_high <- (-1 * root - input$mu)/(2 * input$k - 2)
      lines(rep(x_tan_low, 2), c(0, brfFn(x_tan_low, mu = input$mu, k = input$k)),
            lty = 2, col = "darkgray")
      lines(c(0, x_tan_low), rep(brfFn(x_tan_low, mu = input$mu, k = input$k), 2),
            lty = 2, col = "darkgray")
      lines(rep(x_tan_high, 2), c(0, brfFn(x_tan_high, mu = input$mu, k = input$k)),
            lty = 2, col = "darkgray")
      lines(c(0, x_tan_high), rep(brfFn(x_tan_high, mu = input$mu, k = input$k), 2),
            lty = 2, col = "darkgray")
      points(x_tan_low, brfFn(x_tan_low, mu = input$mu, k = input$k),
             pch = 16, col = "gray45", cex = 1.5)
    }
    
    if(input$k == 1) {
      x_tangent <- input$mu/2
      lines(rep(x_tangent, 2), c(0, brfFn(x_tangent, mu = input$mu, k = input$k)),
            lty = 2, col = "darkgray")
      lines(c(0, x_tangent), rep(brfFn(x_tangent, mu = input$mu, k = input$k), 2),
            lty = 2, col = "darkgray")
      points(x_tangent, brfFn(x_tangent, mu = input$mu, k = input$k), 
             pch = 16, col = "gray45", cex = 1.5)
    }

    }, height = 700, width = 700)
})

