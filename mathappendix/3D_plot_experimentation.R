require(dplyr)
require(plot3D)
require(lattice)


function1 <- function (x, y) {
  return (-2*x^2 + 4*x + x*y - y - y^2)
}

x <- seq(-10, 10, length= 30)
y <- x 
z <- outer(x, y, function1)
z[is.na(z)] <- 1

wireframe(z, drape=T, col.regions=rainbow(100))


x <- seq(-10, 10, length= 30)
y <- x
z <- outer(x, y, function1)
z[is.na(z)] <- 1
op <- par(bg = "white")
persp(x, y, z, theta = 30, phi = 30, expand = 0.5, col = "lightblue")