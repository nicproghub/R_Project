library(spatstat)
data(nbfires)
str(nbfires)
summary(nbfires)
plot(nbfires)


# Remove duplicate records
#new <- nbfires[!duplicated(nbfires)]

# Produce density plots for each of fire types.
plot(density(split(nbfires, f="fire.type")), main ="Density for each fire types")

# Consider only forest fires
forestfire<- nbfires[nbfires$marks$fire.type == "forest"]
str(forestfire)
forestfire1 <- unmark(forestfire)
# Get the image of empty space distance
emp <- distmap(forestfire1)
# Plot the image 
plot(emp, main="Empty space distances for forest fire")
plot(forestfire1, add = TRUE)


# Consider NOT forest fires
noforestfire<- nbfires[!nbfires$marks$fire.type == "forest"]
str(noforestfire)
noforestfire1 <- unmark(noforestfire)
# Get the image of empty space distance
emp <- distmap(noforestfire1)
# Plot the image 
plot(emp, main="Empty space distances for forest fire")
plot(noforestfire1, add = TRUE)


# Compute and plot the F function for the locations of forest fires
f <- Fest(forestfire1) 
plot(f, main="F Function for the locations of forest fires")

# Compute and plot the G function for the locations of forest fires
g <- Gest(forestfire1) 
plot(g, main="G Function for the locations of forest fires")

# Remove marks information
forestfire1 <- unmark(forestfire)
# Fit an inhomogeneous Poisson model with an intensity that is a polynomial of degree 3 of x and y coordinates
fit <- ppm(forestfire1, ~polynom(x, y, 3)) 
fit
par(mfrow=c(1,1))
plot(fit, se = FALSE)
plot(forestfire1, pch= 3, add = TRUE)

cdf.test(fit, function(x,y){y})