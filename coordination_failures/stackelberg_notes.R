ebSt <- function(alpha, beta) {
  (alpha - alpha^2*beta)/(1 - 2*alpha*beta^2)
}


eaSt <- function(alpha, beta) {
  (alpha - 2 * alpha^2 * beta^2 - alpha^2 * beta + alpha^3 * beta^2)/(1 - 2*alpha*beta^2)
  
}

ebSt(alpha = 16, beta = 1/24) + eaSt(alpha = 16, beta = 1/24)

eaCn <- function(alpha, beta) {
  alpha/(1 + alpha*beta)
}

ebCn <- function(alpha, beta) {
  alpha/(1 + alpha*beta)
}

eaSTAleads <- function(alpha, beta) {
(alpha - alpha^2 * beta)/(1 - 2 * alpha^2 * beta^2)
}

ebSTAleads <- function(alpha, beta) {
  (alpha - alpha^3 * beta^2 - alpha^2 * beta)/(1 - 2 * alpha^2 * beta^2 )
}

#For beta = 1/48
eaSTAleads(alpha = 16, beta = 1/48)
ebSTAleads(alpha = 16, beta = 1/48)

#For beta = 1/32
ea32 <- eaSTAleads(alpha = 16, beta = 1/32); ea32
eb32 <- ebSTAleads(alpha = 16, beta = 1/32); eb32
uA(ea = ea32, eb = eb32, alpha = 16, beta = 1/32)
uB(ea = ea32, eb = eb32, alpha = 16, beta = 1/32)

#For beta = 1/24
ea24 <- eaSTAleads(alpha = 16, beta = 1/24); ea24
eb24 <- ebSTAleads(alpha = 16, beta = 1/24); eb24
uA(ea = ea24, eb = eb24, alpha = 16, beta = 1/24)
uB(ea = ea24, eb = eb24, alpha = 16, beta = 1/24)


eaCn(alpha = 16, beta = 1/48)
ebCn(alpha = 16, beta = 1/48)

eaCn32 <- eaCn(alpha = 16, beta = 1/32); eaCn32
ebCn32 <- ebCn(alpha = 16, beta = 1/32); ebCn32
uA(ea = eaCn32, eb = ebCN32, alpha = 16, beta = 1/32)
uB(ea = eaCn32, eb = ebCN32, alpha = 16, beta = 1/32)


uA <- function(ea, eb, alpha = 16, beta = 1/48) {
  alpha*(1 - beta*eb)*ea - 0.5*(ea)^2
}

uB <- function(ea, eb, alpha = 16, beta = 1/48) {
  alpha*(1 - beta*ea)*eb - 0.5*(eb)^2
}

uA(ea = 13.71429, eb = 16.13393, alpha = 16, beta = 1/48)
uB(ea = 13.71429, eb = 16.13393, alpha = 16, beta = 1/48)

ebCn(alpha = 16, beta = 1/24) + eaCn(alpha = 16, beta = 1/24)
