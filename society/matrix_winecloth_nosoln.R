require(shape)
pdf(file = "society/matrix_winecloth_nosoln.pdf", width = 9, height = 7)

par(mar =  c(0, 0, 0, 0))
xlims <- c(0, 8)
ylims <- c(0, 8)

#TextSizes
PlayerSize <- 3
StratSize <- 2.1
PayoffSize <- 3
p1fadelevel <- 0.2
p2fadelevel <- 0.2

#Payoffs are listed as c("NW", "SW", "NE", "SE")

#Considerations for Player 1
P1Name <- c("Great Britain")
P1Strat <- c("Cloth", "Wine")
P1Payoffs <- c("5", "4", "10", "2")
P1BR1 <- c("SW") #Must be NW or SW - the BR prints accordingly. 
P1BR2 <- c("SE") #Must be NE or SE - the BR prints accordingly. 
#NW = (5, 3); SW = (5,1); NE = (7, 3); SE = (7, 1)
P1bestsize <- 2.5 #Adjusts size of point indicating best response of P1

#Considerations for Player 2
P2Name <- c("Portugal")
P2Strat <- c("Cloth", "Wine")
P2Payoffs <- c("2", "4", "10", "5")
P2BR1 <- c("NW") #Must be NW or NE - the BR prints accordingly. 
P2BR2 <- c("SW") #Must be SW or SE - the BR prints accordingly. 
P2bestsize <- 4 #Adjusts size of circle indicating best response of P2

plot(0, 0, xlim = xlims, ylim = ylims, type = "n",
     xlab = expression(""),
     ylab = expression(""),
     xaxt = "n", 
     yaxt = "n", 
     cex.lab = 2, 
     bty = "n")



#Color and label for Player 1 and their strategies
polygon(c(0,0,2,2,0), c(0,4,4,0,0), col=rgb(0, 0, 1, p1fadelevel), density=NULL, border = NA)
text(1, 2, P1Name[1], srt = 90, cex = PlayerSize)

text(3, 3, P1Strat[1], cex = StratSize, srt = 90)
text(3, 1, P1Strat[2], cex = StratSize, srt = 90)


#Color and label for Player 2
polygon(c(4,4,8,8,4), c(8,6,6,8,8), col=rgb(1, 0, 0, p2fadelevel), density=NULL, border = NA)
text(6, 7, P2Name[1], cex = PlayerSize)

text(5, 5, P2Strat[1], cex = StratSize)
text(7, 5, P2Strat[2], cex = StratSize)

#NorthWest cell 
#P1
polygon(c(4,4,6,4), c(4,2,2,4), col=rgb(0, 0, 1, p1fadelevel), density=NULL, border = NA)
#P2
polygon(c(4,6,6,4), c(4,4,2,4), col=rgb(1, 0, 0, p2fadelevel), density=NULL, border = NA)

#P1
text(4.5, 2.5, P1Payoffs[1], cex = PayoffSize)
#P2
text(5.5, 3.5, P2Payoffs[1], cex = PayoffSize)

#SouthWest cell  
#P1
polygon(c(4,4,6,4), c(2,0,0,2), col=rgb(0, 0, 1, p1fadelevel), density=NULL, border = NA)
#P2
polygon(c(4,6,6,4), c(2,2,0,2), col=rgb(1, 0, 0, p2fadelevel), density=NULL, border = NA)

#P1
text(4.5, 0.5, P1Payoffs[2], cex = PayoffSize)
#P2
text(5.5, 1.5, P2Payoffs[2], cex = PayoffSize)


#NorthEast cell 
#P1
polygon(c(6,6,8,6), c(4,2,2,4), col=rgb(0, 0, 1, p1fadelevel), density=NULL, border = NA)
#P2
polygon(c(6,8,8,6), c(4,4,2,4), col=rgb(1, 0, 0, p2fadelevel), density=NULL, border = NA)
#P1
text(6.5, 2.5, P1Payoffs[3], cex = PayoffSize)
#P2
text(7.5, 3.5, P2Payoffs[3], cex = PayoffSize)

#SouthEast cell 
#P1
polygon(c(6,6,8,6), c(2,0,0,2), col=rgb(0, 0, 1, p1fadelevel), density=NULL, border = NA)
#P2
polygon(c(6,8,8,6), c(2,2,0,2), col=rgb(1, 0, 0, p2fadelevel), density=NULL, border = NA)
#P1
text(6.5, 0.5, P1Payoffs[4], cex = PayoffSize)
#P2
text(7.5, 1.5, P2Payoffs[4], cex = PayoffSize)


#Lines for the Table
segments(8, 0, 8, 6, lty = 1, col = "black")
segments(4, 6, 8, 6, lty = 1, col = "black")
segments(2, 0, 2, 4, lty = 1, col = "black")
segments(2, 0, 8, 0, lty = 1, col = "black")
segments(2, 4, 8, 4, lty = 1, col = "black")
segments(4, 0, 4, 6, lty = 1, col = "black")
segments(2, 2, 8, 2, lty = 1, col = "black")
segments(6, 0, 6, 6, lty = 1, col = "black")

#Diagonals
#NW
segments(4, 4, 6, 2, lty = 1, col = "white")
#NE
segments(6, 4, 8, 2, lty = 1, col = "white")
#SW
segments(4, 2, 6, 0, lty = 1, col = "white")
#Se
segments(6, 2, 8, 0, lty = 1, col = "white")


# #Best responses 
# #P1 
# if (P1BR1[1] == "NW"){
#   points(5, 3, pch = 16, col = "black", cex = P1bestsize)
# } else{points(5, 1, pch = 16, col = "black", cex = P1bestsize)
# }
# 
# if (P1BR2[1] == "NE"){
#   points(7, 3, pch = 16, col = "black", cex = P1bestsize)
# } else{points(7, 1, pch = 16, col = "black", cex = P1bestsize)
# }
# 
# 
# #P2
# if (P2BR1[1] == "NW"){
#   points(5, 3, pch = 1, col = "black", cex = P2bestsize)
# } else{points(7, 3, pch = 1, col = "black", cex = P2bestsize)
# }
# 
# if (P2BR2[1] == "SW"){
#   points(5, 1, pch = 1, col = "black", cex = P2bestsize)
# } else{points(7, 1, pch = 1, col = "black", cex = P2bestsize)
# }

dev.off()

