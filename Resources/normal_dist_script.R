
rm(list=ls())
library(RColorBrewer)
library(rgl)
library(plot3D)
library(mvtnorm)
library(latex2exp)
cols <- brewer.pal(9, "Set1")

# Generate n observations from a normal distribution
set.seed(123)
n <- 100
mu1 <- 3
sigma1 <- 1
X1 <- rnorm(n, mu1, sigma1)

# Plot it
xgrid <- seq(min(X1), max(X1), length.out = 100)
par(mar=c(4,4,2,2), family = 'serif')
hist(X1, breaks = 15, xlab = TeX("$X_1$"), main = '', border = 'white', col = 'gray', probability = T)
points(X1, rep(0, n), pch = 3)
lines(xgrid, dnorm(xgrid, mu1, sigma1), lwd = 2)

# Generate n observations from another normal distribution
mu2 <- 5
sigma2 <- 4
X2 <- rnorm(n, mu2, sigma2)

par(mar=c(4,4,2,2), family = 'serif')
plot(X1, X2, asp = 1, pch = 16, xlab = TeX("$X_1$"), ylab = TeX("$X_2$"))
abline(v = 0, lty = 2)
abline(h = 0, lty = 2)

# Let us show the contour lines
xgrid <- seq(min(X1), max(X1), length.out = 100)
ygrid <- seq(min(X2), max(X2), length.out = 100)
my_norm <- function(x, y, mean, sigma){return(dmvnorm(cbind(x, y), mean, sigma))}
f <- outer(xgrid, ygrid, my_norm, mean = c(3, 5), sigma = diag(c(1, 3)))
contour(xgrid, ygrid, f, nlevels = 10, add = T, col = cols[1], lwd = 2)


nbcol <- 100
nrz <- nrow(f)
ncz <- ncol(f)
color <- alpha(rev(rainbow(nbcol, start = 0/6, end = 4/6)), 0.8)
zfacet <- f[-1, -1] + f[-1, -ncz] + f[-nrz, -1] + f[-nrz, -ncz]
zcol <- cut(zfacet, nbcol)

persp(xgrid, ygrid, f, 
      col = color[zcol], xlab = 'X1', ylab = 'X2', zlab = "density", theta = 60, 
      phi = 15, r = 3, border = NA)


persp3d(xgrid, ygrid, f, col = cols[1], xlab = "", ylab = "", zlab = "",
        alpha = 0.8, ticktype = "detailed")
points3d(X1, X2, rep(0, n), pch = 16, add = TRUE)

# How are X1 and X2 related?
cor(X1, X2)


# Let's generate bivariate normal draws (correlated)
mu <- c(mu1, mu2)
Sigma <- matrix(2 * c(1, 0.75, 0.75, 1), 2, 2, byrow = T)
X_joint <- rmvnorm(n, mu, Sigma)
xgrid <- seq(min(X_joint[,1]), max(X_joint[,1]), length.out = 100)
ygrid <- seq(min(X_joint[,2]), max(X_joint[,2]), length.out = 100)


par(mar=c(4,4,2,2), family = 'serif')
plot(X_joint, asp = 1, pch = 16, xlab = TeX("$X_1$"), ylab = TeX("$X_2$"))
abline(v = 0, lty = 2)
abline(h = 0, lty = 2)
f <- outer(xgrid, ygrid, my_norm, mean = mu, sigma = Sigma)
contour(xgrid, ygrid, f, nlevels = 10, add = T, col = cols[1], lwd = 2)

cov(X_joint); Sigma
cor(X_joint)

# Contours are slices
persp3d(xgrid, ygrid, f, col = cols[1], xlab = "", ylab = "", zlab = "",
        alpha = 0.8, ticktype = "detailed")
points3d(X_joint[,1], X_joint[,2], rep(0, n), pch = 16, add = TRUE)


plot_2Dnormal <- function(n, mu, Sigma){
  X <- rmvnorm(n, mu, Sigma)
  xgrid <- seq(min(X[,1]), max(X[,1]), length.out = 100)
  ygrid <- seq(min(X[,2]), max(X[,2]), length.out = 100)
  
  par(mar=c(4,4,2,2), family = 'serif')
  plot(X, asp = 1, pch = 16, xlab = TeX("$X_1$"), ylab = TeX("$X_2$"))
  abline(v = mean(X[,1]), lty = 2)
  abline(h = mean(X[,2]), lty = 2)
  my_norm <- function(x, y, mean, sigma){return(dmvnorm(cbind(x, y), mean, sigma))}
  f <- outer(xgrid, ygrid, my_norm, mean = mu, sigma = Sigma)
  contour(xgrid, ygrid, f, nlevels = 10, add = T, col = cols[1], lwd = 2)
}

set.seed(123)
plot_2Dnormal(100, c(1, 3), diag(2))
plot_2Dnormal(100, c(1, 3), diag(c(5, 1)))
plot_2Dnormal(100, c(1, 3), diag(c(1, 5)))
plot_2Dnormal(100, c(1, 3), matrix(c(1, 0.7, 0.7, 1), 2, 2, byrow = T))
plot_2Dnormal(100, c(1, 3), matrix(c(1, -0.7, -0.7, 1), 2, 2, byrow = T))
plot_2Dnormal(100, c(1, 3), matrix(c(1, 0.95, 0.95, 1), 2, 2, byrow = T))

