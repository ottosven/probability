set.seed(123)
d1 = rnorm(10000)
d2 = rt(10000,2)

d1std = (d1-mean(d1))/sd(d1)
d2std = (d2-mean(d2))/sd(d2)
png("./img/kurtosis.png", width = 800, height = 400)
par(cex=3, cex.axis=1.5, cex.main = 2)
par(mfrow = c(1, 2))
hist(d1std, xlim = c(-6,6), ylim = c(0,1200), breaks=50, main = "normal kurtosis")
hist(d2std, xlim = c(-6,6), ylim = c(0,1200), breaks = 200, main = "high kurtosis")
dev.off()

library(moments)
kurtosis(d1std)
kurtosis(d2std)








# Set seed for reproducibility
set.seed(123)

# ---------------------------
# Generate Data
# ---------------------------

# Light-Tailed Data using Uniform Distribution
light_tailed <- runif(1000, min = -sqrt(3), max = sqrt(3))  # Mean 0, variance 1

# Normal Distribution Data
normal_data <- rnorm(10000, mean = 0, sd = 1)

# Heavy-Tailed Data using t-Distribution
heavy_tailed <- rt(10000, df = 4)/sqrt(4/2)  # Low degrees of freedom for heavy tails

# ---------------------------
# Plot Histograms
# ---------------------------

# Adjust the output image size and resolution
png("./img/kurtosis.png", width = 600, height = 400, res = 100)

# Adjust text sizes for better readability
par(cex = 1.5, cex.axis = 1, cex.main = 1.3)

# Set up plotting area: 1 row, 3 columns
par(mfrow = c(1, 2))

m = 20
m2 = 4.5

# # Histogram for Light-Tailed Distribution
# hist(light_tailed,
#      main = "Light-tailed Distribution",
#      xlab = "Value",
#      probability = TRUE,
#      xlim = c(-m, m),
#      ylim = c(0,0.45),
#      breaks = seq(-m, m, length.out = 15))

# Histogram for Normal Distribution
hist(normal_data,
     main = "Normal-tailed Distribution",
     xlab = "Value",
     probability = TRUE,
     xlim = c(-m2, m2),
     ylim = c(0,0.52),
     breaks = seq(-m, m, length.out = 100))

# Histogram for Heavy-Tailed Distribution
hist(heavy_tailed,
     main = "Heavy-tailed Distribution",
     xlab = "Value",
     probability = TRUE,
     xlim = c(-m2, m2),
     ylim = c(0,0.52),
     breaks = seq(-m, m, length.out = 100))

# Reset plotting area to default
par(mfrow = c(1, 1))

# Close the PNG device to save the file
dev.off()
