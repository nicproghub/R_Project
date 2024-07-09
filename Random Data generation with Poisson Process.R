library(spatstat)
par(mfrow=c(1,2))
# generate 4 random numbers with [1,10]
# corresponding to x & y of the two centers
a<-runif(4,min = 0, max = 10)
a
# defined intensity for group 1 & distance constraints to the centers  
lambda1 <- function(x, y) {50*as.numeric((abs(x -a[1])<2) & (abs(y -a[2])<2))}
plot(rpoispp(lambda1, win=owin(c(0,10),c(0,10))), main = "Inhomogeneous Poisson 1")
# defined intensity for group 2 & distance constraints to the centers  
lambda2 <- function(x, y) {50*as.numeric((abs(x -a[3])<2) & (abs(y -a[4])<2))}
plot(rpoispp(lambda2, win=owin(c(0,10),c(0,10))),add=TRUE)

# 2nd simulations 
a<-runif(4,min = 0, max = 10)
a
lambda1 <- function(x, y) {50*as.numeric((abs(x -a[1])<2) & (abs(y -a[2])<2))}
plot(rpoispp(lambda1, win=owin(c(0,10),c(0,10))), main="Inhomogeneous Poisson 2")

lambda2 <- function(x, y) {50*as.numeric((abs(x -a[3])<2) & (abs(y -a[4])<2))}
plot(rpoispp(lambda2, win=owin(c(0,10),c(0,10))),add=TRUE)