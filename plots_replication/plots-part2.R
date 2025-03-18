library(ClassData)


## ####################
## CEF of wage given education
## ####################
png("./plots/CEF_wageedu.png", width = 480, height = 300)
# Set plot parameters
par(mar = c(2, 2, 0.9, 0.5), cex = 1.6, lwd = 5, cex.axis = 0.9)
# Define E[Y|Z=b] for each education level b
wage = allbus2023$wage
ed = allbus2023$edu
y <- c(10,12,13,14,16,18,21)
N <- length(y)
mwage <- matrix(0,N,1)
for (i in 1:N){
  wagei <- wage[which(ed==y[i])]
  mwage[i] <- mean(wagei, na.rm=T)
}
# Create new plot
plot.new()
plot.window(xlim = c(9, 22), ylim = c(0.0, 30))
# Add axes
axis(side = 1, at = seq(9, 22, by = 1))
axis(side = 2, at = seq(0, 30, by = 5))
# Plot vertical lines for each CEF value
for (i in 1:length(y)) {
  lines(c(y[i], y[i]), c(0, mwage[i]))
}
# Add text annotation
text(13,23,"E[Y|Z=b]", cex = 0.75)
# Close device
dev.off()




## ####################
## CEF of wage given experience
## ####################
png("./plots/CEF_wageex.png", width = 480, height = 300)
# Set plot parameters
par(mar = c(2, 2, 0.9, 0.5), cex = 1.6, lwd = 5, cex.axis = 0.9)
# Define E[Y|Z=b] for each education level b
wage = allbus2023$wage
ex = allbus2023$exper

n6 <- length(wage)^(-1/6)
he <- 2*sd(ex, na.rm=T)*n6
x <- seq(0,50,1)
N <- length(x)
m <- matrix(0,N,1)
for (i in 1:N){
  d <- dnorm((ex-x[i])/he)
  m[i] <- mean(d*wage, na.rm=T)/mean(d, na.rm=T)
}
# Create new plot
plot.new()
plot.window(xlim = c(0,50), ylim = c(0,30))
plot(x,m,type="l",bty="n",xlab="",ylab="",lwd=4)
text(35,23,"E[Y|Z=b]", cex = 0.75)
# Close device
dev.off()



## ####################
## CEF of wage given experience
## ####################
png("./plots/CEF_wageex_simplified.png", width = 480, height = 300)
# Set plot parameters
par(mar = c(2, 2, 0.9, 0.5), cex = 1.6, lwd = 5, cex.axis = 0.9)
# Define E[Y|Z=b] for each education level b
m = 19 + 0.5*seq(0,50) - 0.013*seq(0,50)^2
# Create new plot
plot.new()
plot.window(xlim = c(0,50), ylim = c(0,30))
plot(x,m,type="l",bty="n",xlab="",ylab="",lwd=4)
text(35,23,"E[Y|Z=b]", cex = 0.75)
# Close device
dev.off()
