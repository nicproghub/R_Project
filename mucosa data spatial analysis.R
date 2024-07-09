library(spatstat)
data(mucosa)
plot(mucosa)

# mucosa data analysis#
summary(mucosa)

# Estimate intensity using kernel density estimation
den <- density(mucosa)
# Plot the estimated intensity
plot(den, main="Estimated Intensity of Mucosa Data")

# Plot data points and contour plot
plot(den, main="Contour Plot of Estimated Intensity")
contour(den, add=TRUE)
plot(mucosa, add = TRUE, cex = 0.5)


# Select a suitable bandwidth
bandwidth <- bw.ppl(mucosa)
bandwidth
bandwidth3 <- bw.diggle(mucosa)

# Compute the kernel density estimation
intensity_estimation <- density(mucosa, sigma = bandwidth)
# Plot the estimated intensity
plot(intensity_estimation, main = "Estimated Intensity of Cell Locations")
plot(mucosa, add = TRUE, cex = 0.5)

# Simulated a Poisson process with den
sim_pp <- rpoispp(den)
plot(sim_pp,main="Simulated Poisson Process with Estimated Intensity")

# split data
plot(split(mucosa))
# plot intensities for splited groups
plot(density(split(mucosa)),main = "Intensities of ECL & other")

mucosa_ecl <- subset(mucosa, marks(mucosa) == "ECL")
plot(mucosa_ecl)

# Calculate the cross-type pair correlation for "ECL" & "Other"
p <- pcfcross(mucosa, "ECL", "other")
plot(p, main="Cross-type Pair Correlation Function (ECL vs Other)")


# spatial Kolmogorov–Smirnov test
# remove marks
mucosa2 <- unmark(mucosa)
str(mucosa2)
#mucosa2 <- mucosa[!duplicated(mucosa)]
#str(mucosa2)
# perform Kolmogorov–Smirnov test on x-coordinate
KS_x <- cdf.test(mucosa2, function(x,y) {x})
KS_x
plot(KS_x)

# perform Kolmogorov–Smirnov test on y-coordinate
KS_y <- cdf.test(mucosa2, function(x,y) {y}) 
KS_y
plot(KS_y)
