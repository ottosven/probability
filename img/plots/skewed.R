# Set seed for reproducibility
set.seed(123)

# ---------------------------
# Generate Skewed Data
# ---------------------------

# Right-Skewed Data using Gamma Distribution
right_skewed <- rgamma(1000, shape = 1.6, scale = 2)

# Left-Skewed Data using Beta Distribution
left_skewed <- 3*rbeta(1000, shape1 = 8, shape2 = 1.5)

# Symmetric Data using Normal Distribution
symmetric <- rnorm(1000, mean = 0, sd = 1)

# ---------------------------
# Plot Histograms
# ---------------------------

png("./img/skewed.png", width = 1000, height = 300)
par(cex=3, cex.axis=1.5, cex.main = 2)
# Set up plotting area: 1 row, 2 columns
par(mfrow = c(1, 3))

# Histogram for Right-Skewed Distribution
hist(right_skewed,
     main = "Right-Skewed Distribution",
     xlab = "Value",
     probability = TRUE)

# Histogram for Symmetric Distribution
hist(symmetric,
     main = "Symmetric Distribution",
     xlab = "Value",
     probability = TRUE)

# Histogram for Left-Skewed Distribution
hist(left_skewed,
     main = "Left-Skewed Distribution",
     xlab = "Value",
     probability = TRUE)
dev.off()
