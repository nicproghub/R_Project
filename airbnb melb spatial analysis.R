# https://data.insideairbnb.com/australia/vic/melbourne/2024-03-16/visualisations/listings.csv
# Load CSV file in R and show the first 6 rows
library(spatstat)
airbnb <- read.csv("listings.csv")
head(airbnb)

adj_airbnb <- airbnb
# Use jitter function to differentiate duplicate locations
adj_airbnb$latitude <- jitter(adj_airbnb$latitude,1)
str(adj_airbnb)

# Find the range of longitude and latitude
lng_r <- range(adj_airbnb$longitude)
lat_r <- range(adj_airbnb$latitude)
# Create ppp object 
myppp <- ppp(adj_airbnb$longitude, adj_airbnb$latitude, c(lng_r[1], lng_r[2]), c(lat_r[1], lat_r[2]))
# Investigate intensity
summary(myppp)
print(summary(myppp)$intensity)



# c. Plot your results and a contour plot for the estimated intensity.
# Estimate density
den <- density(myppp)
den

# Log transformation of the intensity values
log_den <- log(den)

# Plot original estimate density
par(mfrow = c(1,2))
plot(den, log=TRUE)
plot(log_den)
par(mfrow = c(1,1))

# Plot density with points
plot(den, main =" Density (log) of listing Airbnb in Melbourne",log=TRUE)
plot(myppp, add=TRUE, CEX = 0.5)

# Plot contour for estimated intensity
plot(den, main =" Contour Density (log) of listing Airbnb in Melbourne", log=TRUE)
contour(den, add=TRUE, log=TRUE)


# Convert the density estimate to an intensity function
lambda <- as.im(den)
# Simulate a Poisson process 
simulated_ppp <- rpoispp(lambda)
# Plot the simulated point pattern
plot(simulated_ppp, main = "Simulated Poisson Process with Estimate Intensity")


# Plot the estimated intensity
lambda <- as.im(den)/100  
# Simulate a Poisson process 
simulated_ppp <- rpoispp(lambda)
# Plot the simulated point pattern
plot(den/100, main = "Estimated Intensity (den/100) and Simulated Point Process with lambda/100")


lambda <- log(as.im(den)) 
# Simulate a Poisson process 
simulated_ppp <- rpoispp(lambda)
plot(den, log=TRUE, main = "Estimated Intensity (log) and Simulated Point Process")
plot(simulated_ppp, add = TRUE, cex = 0.5, pch = 2)
par(mfrow=c(1,1))
