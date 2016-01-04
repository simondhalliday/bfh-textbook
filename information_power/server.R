library(shiny)

shinyServer(function(input, output) {
  
  output$linePlot <- renderPlot({
    brfFn <- function(p, delta=0.5) {
      1 - (2 * delta) /p
    }
    
    isov1a <- function(p, delta = 0.5, v = 2){
      (sqrt(p^2 - 4 * delta * v) - p + 2*v)/( 2 * v )
    }
    
    isov1b <- function(p, delta = 0.5, v = 2){
      (-sqrt(p^2 - 4 * delta * v) - p + 2*v)/( 2 * v )
    }
    
    isovc <- function(p, v = 2){
      p/2
    }
    
    solowCondition <- function(p, delta = 0.5){
      (p*delta/2)
    }
    
#     isoreturnFn <- function(p, pi=a-2*sqrt(2)*sqrt(a), a=8) {
#       (pi+p)/a 
#     }
    
    #isoreturnFn2 <- function(p, delta, delta = 0.5){
    #  (2*delta)/p
    #}
    
  #  isoreturnFn <- function(p) {
  #   -1 + p   
  #  }
    
  
    COL <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
    par(mar =  c(5, 5, 4, 2))
    xlims <- c(0, 5)
    ylims <- c(0, 1)

    
    plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
         xlab = expression(paste("Price, ", p)),
         ylab = expression(paste("Quality, ", q)),
         xaxt = "n", yaxt = "n", cex.lab = 1, bty = "n")
    
    npts <- 500
    xx1 <- seq(xlims[1], xlims[2], length.out = npts)
    xx2 <- seq(xlims[2], length.out = npts)
    lines(xx1, brfFn(xx1), col = COL[2], lwd = 4)
    #lines(xx1, isov1(xx1, delta=input$delta), col = COL[1], lwd = 4)
    lines(xx1, isov1a(xx1, delta=0.5), col = COL[1], lwd = 4)
    lines(xx1, isov1b(xx1, delta=0.5), col = COL[1], lwd = 4)
    lines(xx1, isov1c(xx1, delta=0.5), col = COL[1], lwd = 4)
    lines(xx1, solowCondition(xx1, delta=0.5), col = COL[3], lwd = 4)
    
    
    axis(1, at = xlims, pos = 0)
    axis(2, at = ylims, pos = 0)
        
    # calculate points of intersection
    #root <- sqrt(8 * input$k * pi + input$mu - 8 * pi)
#     x_low <- (0.5)*(input$delta - sqrt(input$delta) * sqrt(input$delta - 8)) 
#     x_high <- (0.5)*(input$delta + sqrt(input$delta) * sqrt(input$delta - 8)) 
#     y_low <- isoreturnFn(x_low, delta=input$delta)
#     y_high <- isoreturnFn(x_high, delta=input$delta)
    
    # add lines/points of intersection
#     lines(c(rep(x_low, 2), xlims[1]),
#           c(0, rep(isoreturnFn(x_low, delta=input$delta), 2)),
#          lty = 2, col = "darkgray")
#     lines(c(rep(x_high, 2), xlims[1]),
#           c(0, rep(isoreturnFn(x_high, delta=input$delta), 2)),
#           lty = 2, col = "darkgray")
#     points(c(x_low, x_high),
#            c(y_low, y_high),
#            pch = c(1, 16), col = COL[7], cex = 1.9)
    
    # add labels in margins
#    if (!is.nan(x_low) & !is.infinite(x_low)) {
#       mtext(round(x_low, 2), 1, at = x_low)
#       mtext(round(x_high, 2), 1, at = x_high)
#       mtext(round(y_low, 2), 2, at = y_low)
#       mtext(round(y_high, 2), 2, at = y_high)
#     }
#     if (is.infinite(x_low)) {
#       mtext("infinity", 1)
#     }

add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0), 
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

add_legend("topleft", legend=c(
  expression(paste("Agent's Indifference Curve: v = v*", )),
  expression(paste("Agent's Best Response: q = 1 - (2*delta/p)",)), 
  expression(paste("Solow Condition: q = p*(delta/2)",))
  ),
  col = COL, lty = 1, lwd = 2, cex=0.6, bty = "n")
    

    }, height = 500, width = 700)
})

