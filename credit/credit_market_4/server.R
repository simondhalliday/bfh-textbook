library(shiny)

shinyServer(function(input, output) {
  
  output$linePlot <- renderPlot({
    brfFn <- function(delta, mu = 1, k = 0.5) {
       ((delta^2*k^2 + 3*mu)^(0.5) + delta*(1-k)) / (3 * mu)
    }
    
    #SH: 2014.10.12: 11:07 - I want to include this function as eventually
    #it will tell us what the value of 1+rho should be, but for now I've just
    #set rho and 1+rho to 1.05
    #tangencyFn <- function(delta, mu = 1, k = 0.5) {
    #  1 - (delta / (2 * mu)) * (1 - k)
    #}
    
    #isoreturnFn <- function(delta, mu=5, k=0.5) {
    #  1 - (1+rho)/delta
    #}
    
    isoreturnFn <- function(delta, pi=0.25) {
      sqrt(1 - (pi)/(delta))
    }
  
    COL <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
    par(mar =  c(5, 5, 4, 2))
    xlims <- c(0, 2.5)
    ylims <- c(0, 1)
    pi <- 1.05
    
    plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
         xlab = expression(paste("Interest factor, ", delta)),
         ylab = expression(paste("Probability of failure, ", f)),
         xaxt = "n", yaxt = "n", cex.lab = 1.2, bty = "n")
    
    npts <- 500
    xx1 <- seq(xlims[1], xlims[2], length.out = npts)
    xx2 <- seq(pi, xlims[2], length.out = npts)
    lines(xx1, brfFn(xx1, mu = input$mu, k = input$k), col = COL[2], lwd = 4)
    #lines(xx, tangencyFn(xx, mu = input$mu, k = input$k), col = COL[3], lwd = 4)
    lines(xx2, isoreturnFn(xx2, pi=pi), col = COL[1], lwd = 4)
    
    axis(1, at = xlims, pos = 0)
    axis(2, at = ylims, pos = 0)
    
#     legend("bottomright", inset = c(0, -.08), legend = c(
#                               expression(paste("f = 1/2 + ", delta, "(1 - k)/2", mu)),
#                               expression(paste("f = 1 - (1 + ", rho, ")/", delta, ))),
#            col = COL, lty = 1, lwd = 2, bty = "n")
#     
    # calculate points of intersection
    root <- sqrt(16 * input$k * pi + input$mu - 16 * pi)
    x_low <- (sqrt(input$mu) * root - input$mu) / (4 * (input$k - 1))
    x_high <- (-sqrt(input$mu) * root - input$mu) / (4 * (input$k - 1))
    y_low <- brfFn(x_low, mu = input$mu, k = input$k)
    y_high <- brfFn(x_high, mu = input$mu, k = input$k)
    
    # add lines/points of intersection
    lines(c(rep(x_low, 2), xlims[1]),
          c(0, rep(brfFn(x_low, mu = input$mu, k = input$k), 2)),
          lty = 2, col = "darkgray")
    lines(c(rep(x_high, 2), xlims[1]),
          c(0, rep(brfFn(x_high, mu = input$mu, k = input$k), 2)),
          lty = 2, col = "darkgray")
    points(c(x_low, x_high),
           c(y_low, y_high),
           pch = c(16, 1), col = COL[7], cex = 1.9)
    
    # add labels in margins
    if (!is.nan(x_low) & !is.infinite(x_low)) {
      mtext(round(x_low, 2), 1, at = x_low)
      mtext(round(x_high, 2), 1, at = x_high)
      mtext(round(y_low, 2), 2, at = y_low)
      mtext(round(y_high, 2), 2, at = y_high)
    }
    if (is.infinite(x_low)) {
      mtext("infinity", 1)
    }

add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

add_legend("topright", legend=c(
  expression(paste("f = 1/4 + ", delta, "(1 - k)/2", mu)), 
  expression(paste("f = 1/2 - (1 + ", rho, ")/", delta, ))),
  col = COL, lty = 1, lwd = 2, bty = "n")
    

    }, height = 500, width = 700)
})

