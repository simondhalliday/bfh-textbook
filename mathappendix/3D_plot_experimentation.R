require(dplyr)
require(plot3D)
require(lattice)


function1 <- function (x, y) {
  return (-x^2 - y^2)
}

x <- seq(-10, 10, length= 30)
y <- x 
z <- outer(x, y, function1)
z[is.na(z)] <- 1

wireframe(z, drape=T, col.regions=rainbow(100))
