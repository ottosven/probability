library(ClassData)


## ####################
## CDF of coin
## ####################
png("./plots/CDF_coin.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
y <- c(0,1)
n <- length(y)
p <- matrix(0.5,2,1)
p[1] = 0.5
p[2] = 0.5
p <- cumsum(p)
plot.new()
plot.window(c(-1,2),c(0.0,1))
axis(side=1,seq(-2,2,1))
axis(side=2,seq(-1,1,.2))
points(y[1],0,pch=1,col="black", main="test")
points(y[1],p[1],pch=19,col="black")
lines(c(-2,y[1]),c(0,0))
lines(c(1,25),c(1,1))
for (i in 2:n){
  points(y[i],p[i-1],pch=1,col="black")
  points(y[i],p[i],pch=19,col="black")
  lines(c(y[i-1],y[i]),c(p[i-1],p[i-1]))
}
text(-0.93,.95,"F(a)",cex=.75)
dev.off()



## ####################
## CDF of education
## ####################
png("./plots/CDF_edu.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
education = allbus2023$edu
#Education CDF
y <- c(4, 10, 12, 13, 14, 16, 18, 21)
n <- length(y)
p <- matrix(0,n,1)
p[1] <- mean(education <= y[1],na.rm = TRUE)
for (i in 2:n){
  p[i] <- mean(education == y[i],na.rm= TRUE)
}
p <- cumsum(p)
plot.new()
plot.window(c(4,22),c(0.0,1))
axis(side=1,seq(-2,22,2))
axis(side=2,seq(-1,1,.2))
points(y[1],0,pch=1,col="black", main="test")
points(y[1],p[1],pch=19,col="black")
lines(c(0,y[1]),c(0,0))
lines(c(21,25),c(1,1))
for (i in 2:n){
  points(y[i],p[i-1],pch=1,col="black")
  points(y[i],p[i],pch=19,col="black")
  lines(c(y[i-1],y[i]),c(p[i-1],p[i-1]))
}
text(4.5,.95,"F(a)",cex=.75)
dev.off()


## ####################
## CDF of wage
## ####################
png("./plots/CDF_wage.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
## bootstrap from wage data
wage= rep(na.omit(allbus2023$wage),10)
set.seed(42)
wage = wage*(abs(rnorm(length(wage), mean = 1, sd=0.05)))
wage[which(wage == 0)] = abs(rnorm(length(which(wage == 0)),0,2))
# Wage CDF
x <- seq(0,50,0.5)
n <- length(x)
F <- matrix(1,n,1)
for (i in 1:n)   F[i] <- mean(wage <= x[i],na.rm = TRUE)
plot(x,F, lwd = 2, type="l",lty=1,xlab="",ylab="",xaxt="n", yaxt="n", xlim=c(0,50),ylim=c(0,1),title=NULL,xaxs="i",yaxs="i",bty="n")
axis(side=1,seq(0,50,10),cex.axis=.75)
axis(side=2,seq(0,1,0.2),cex.axis=.75)
text(3,0.95,"F(a)",cex=.75)
dev.off()


## ####################
## PMF of education
## ####################
# Create Probability Mass Function (PMF) Plot for Education Data
# Probabilities provided in the problem statement
# Output PNG file
png("./plots/PMF_edu.png", width = 480, height = 300)
# Set plot parameters
par(mar = c(2, 2, 0.9, 0.5), cex = 1.6, lwd = 5, cex.axis = 0.9)
# Define education levels and their probabilities
y <- c(4, 10, 12, 13, 14, 16, 18, 21)
p <- c(0.008, 0.055, 0.393, 0.079, 0.145, 0.078, 0.218, 0.024)
# Create new plot
plot.new()
plot.window(xlim = c(4, 22), ylim = c(0.0, 0.5))
# Add axes
axis(side = 1, at = seq(4, 22, by = 2))
axis(side = 2, at = seq(0, 0.5, by = 0.1))
# Plot vertical lines for each probability
for (i in 1:length(y)) {
  # Vertical line representing the probability mass
  lines(c(y[i], y[i]), c(0, p[i]))
}
# Add text annotation
text(4.2, 0.485, "π(a)", cex = 0.75)
# Close device
dev.off()



## ####################
## PDF of wage
## ####################
png("./plots/PDF_wage.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
## bootstrap from wage data
wage= rep(na.omit(allbus2023$wage),10)
set.seed(42)
wage = wage*(abs(rnorm(length(wage), mean = 1, sd=0.05)))
wage[which(wage == 0)] = abs(rnorm(length(which(wage == 0)),0,2))

den <- density(wage,from=0,to=100,adjust=2,na.rm = TRUE)
i20 <- 103
i30 <- 154
d20 <- den$y[i20]
d30 <- den$y[i30]
dd <- den$y[i20:i30]
dx <- den$x[i20:i30]
plot(den,type="l",lty=1,xaxs="i",yaxs="i",ylim=c(0,.09),xlim=c(-0.1,50),ylab="",xlab="",bty="n",main="",cex.axis=.75)
lines(den$x,den$y)
lines(c(0,0),c(0,den$y[1]))
text(2,.075,"f(a)",cex=.75)
dev.off()


## ####################
## conditional CDF of repeated coin
## ####################
png("./plots/CCDF_coin_b0.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
y <- c(0,1)
n <- length(y)
p <- matrix(0.5,2,1)
p[1] = 0.5
p[2] = 0.5
p <- cumsum(p)
plot.new()
plot.window(c(-1,2),c(0.0,1))
axis(side=1,seq(-2,2,1))
axis(side=2,seq(-1,1,.2))
points(y[1],0,pch=1,col="black", main="test")
points(y[1],p[1],pch=19,col="black")
lines(c(-2,y[1]),c(0,0))
lines(c(1,25),c(1,1))
for (i in 2:n){
  points(y[i],p[i-1],pch=1,col="black")
  points(y[i],p[i],pch=19,col="black")
  lines(c(y[i-1],y[i]),c(p[i-1],p[i-1]))
}
text(-0.8,.95,expression(F[Y*"|"*Z] * "(a|b=0)"),cex=.75)
dev.off()
png("./plots/CCDF_coin_b1.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
y <- c(0,1)
n <- length(y)
p <- matrix(0.5,2,1)
p[1] = 0.5
p[2] = 0.5
p <- cumsum(p)
plot.new()
plot.window(c(-1,2),c(0.0,1))
axis(side=1,seq(-2,2,1))
axis(side=2,seq(-1,1,.2))
points(y[1],0,pch=1,col="black", main="test")
points(y[1],p[1],pch=19,col="black")
lines(c(-2,y[1]),c(0,0))
lines(c(1,25),c(1,1))
for (i in 2:n){
  points(y[i],p[i-1],pch=1,col="black")
  points(y[i],p[i],pch=19,col="black")
  lines(c(y[i-1],y[i]),c(p[i-1],p[i-1]))
}
text(-0.8,.95,expression(F[Y*"|"*Z] * "(a|b=1)"),cex=.75)
dev.off()


## ####################
## PMF of coin
## ####################
png("./plots/PMF_coin.png", width = 480, height = 300)
# Set plot parameters similar to the PMF_edu plot
par(mar = c(2, 2, 0.9, 0.5), cex = 1.6, lwd = 5, cex.axis = 0.9)
# Define coin outcomes and their conditional probabilities given b=0
y <- c(0, 1)
p <- c(0.5, 0.5)  # Equal probability for fair coin
# Create new plot
plot.new()
plot.window(xlim = c(-1, 2), ylim = c(0.0, 0.6))
# Add axes
axis(side = 1, at = seq(-1, 2, by = 1))
axis(side = 2, at = seq(0, 0.6, by = 0.1))
# Plot vertical lines for each probability
for (i in 1:length(y)) {
  # Vertical line representing the probability mass
  lines(c(y[i], y[i]), c(0, p[i]))
}
# Add text annotation
text(-0.95, 0.58, expression(pi * "(a)"), cex = 0.75)
# Close device
dev.off()


## ####################
## Conditional PMF of repeated coin
## ####################
# Create Probability Mass Function (PMF) Plot for Coin Conditional on b=0
# Output PNG file
png("./plots/CPMF_coin_b0.png", width = 480, height = 300)
# Set plot parameters similar to the PMF_edu plot
par(mar = c(2, 2, 0.9, 0.5), cex = 1.6, lwd = 5, cex.axis = 0.9)
# Define coin outcomes and their conditional probabilities given b=0
y <- c(0, 1)
p <- c(0.5, 0.5)  # Equal probability for fair coin
# Create new plot
plot.new()
plot.window(xlim = c(-1, 2), ylim = c(0.0, 0.6))
# Add axes
axis(side = 1, at = seq(-1, 2, by = 1))
axis(side = 2, at = seq(0, 0.6, by = 0.1))
# Plot vertical lines for each probability
for (i in 1:length(y)) {
  # Vertical line representing the probability mass
  lines(c(y[i], y[i]), c(0, p[i]))
}
# Add text annotation
text(-0.8, 0.58, expression(pi[Y*"|"*Z] * "(a|b=0)"), cex = 0.75)
# Close device
dev.off()
# Create Probability Mass Function (PMF) Plot for Coin Conditional on b=1
# Output PNG file
png("./plots/CPMF_coin_b1.png", width = 480, height = 300)
# Set plot parameters
par(mar = c(2, 2, 0.9, 0.5), cex = 1.6, lwd = 5, cex.axis = 0.9)
# Define coin outcomes and their conditional probabilities given b=1
y <- c(0, 1)
p <- c(0.5, 0.5)  # Equal probability for fair coin
# Create new plot
plot.new()
plot.window(xlim = c(-1, 2), ylim = c(0.0, 0.6))
# Add axes
axis(side = 1, at = seq(-1, 2, by = 1))
axis(side = 2, at = seq(0, 0.6, by = 0.1))
# Plot vertical lines for each probability
for (i in 1:length(y)) {
  # Vertical line representing the probability mass
  lines(c(y[i], y[i]), c(0, p[i]))
}
# Add text annotation
text(-0.8, 0.58, expression(pi[Y*"|"*Z] * "(a|b=1)"), cex = 0.75)
# Close device
dev.off()



## ####################
## Conditional PMF of education given married
## ####################
# First plot: Conditional PMF of Education given Married=1
# Output PNG file
png("./plots/CPMF_edu_married.png", width = 480, height = 300)
# Set plot parameters
par(mar = c(2, 2, 0.9, 0.5), 
    cex = 1.6, 
    lwd = 5, 
    cex.axis = 0.9)
# Assuming data is available from the statisticsdata package
education = allbus2023$edu
mar = allbus2023$marr
# Define education levels
y <- c(4, 10, 12, 13, 14, 16, 18, 21)
n <- length(y)
# For demonstration, let's set some example probabilities
# In a real scenario, you would calculate these from your data:
education_married = ifelse(mar == 1, education, NA)
p_married <- numeric(n)
p_married[1] <- mean(education_married == y[1], na.rm = TRUE)
for (i in 2:n) {
  p_married[i] <- mean(education_married == y[i], na.rm = TRUE)
}
# Create new plot
plot.new()
plot.window(xlim = c(4, 22), ylim = c(0.0, 0.5))
axis(side = 1, at = seq(4, 22, by = 2))
axis(side = 2, at = seq(0, 0.5, by = 0.1))
# Plot vertical lines for each probability
for (i in 1:length(y)) {
  # Vertical line representing the probability mass
  lines(c(y[i], y[i]), c(0, p_married[i]), col = "black")
}
text(5.3, 0.48, expression(pi[Y*"|"*Z] * "(a|b=1)"), cex = 0.75)
# Close device
dev.off()
# Second plot: Conditional PMF of Education given Married=0
# Output PNG file
png("./plots/CPMF_edu_unmarried.png", width = 480, height = 300)
# Set plot parameters
par(mar = c(2, 2, 0.9, 0.5), 
    cex = 1.6, 
    lwd = 5, 
    cex.axis = 0.9)
education_unmarried = ifelse(mar == 0, education, NA)
p_unmarried <- numeric(n)
p_unmarried[1] <- mean(education_unmarried == y[1], na.rm = TRUE)
for (i in 2:n) {
  p_unmarried[i] <- mean(education_unmarried == y[i], na.rm = TRUE)
}
# Create new plot
plot.new()
plot.window(xlim = c(4, 22), ylim = c(0.0, 0.5))
axis(side = 1, at = seq(4, 22, by = 2))
axis(side = 2, at = seq(0, 0.5, by = 0.1))
# Plot vertical lines for each probability
for (i in 1:length(y)) {
  # Vertical line representing the probability mass
  lines(c(y[i], y[i]), c(0, p_unmarried[i]))
}
text(5.3, 0.48, expression(pi[Y*"|"*Z] * "(a|b=0)"), cex = 0.75)
# Close device
dev.off()




## ####################
## Conditional CDFs of education given married
## ####################
png("./plots/CCDF_edu_marr.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
education = allbus2023$edu
mar = allbus2023$marr
education = ifelse(mar==1, education, NA)
#Education CDF
y <- c(4, 10, 12, 13, 14, 16, 18, 21)
n <- length(y)
p <- matrix(0,n,1)
p[1] <- mean(education <= y[1],na.rm = TRUE)
for (i in 2:n){
  p[i] <- mean(education == y[i],na.rm= TRUE)
}
p <- cumsum(p)
plot.new()
plot.window(c(4,22),c(0.0,1))
axis(side=1,seq(-2,22,2))
axis(side=2,seq(-1,1,.2))
points(y[1],0,pch=1,col="black", main="test")
points(y[1],p[1],pch=19,col="black")
lines(c(0,y[1]),c(0,0))
lines(c(21,25),c(1,1))
for (i in 2:n){
  points(y[i],p[i-1],pch=1,col="black")
  points(y[i],p[i],pch=19,col="black")
  lines(c(y[i-1],y[i]),c(p[i-1],p[i-1]))
}
education = statisticsdata::allbus21$education
education = ifelse(mar==0, education, NA)
#Education CDF
y <- c(4, 10, 12, 13, 14, 16, 18, 21)
n <- length(y)
p <- matrix(0,n,1)
p[1] <- mean(education <= y[1],na.rm = TRUE)
for (i in 2:n){
  p[i] <- mean(education == y[i],na.rm= TRUE)
}
p <- cumsum(p)
points(y[1],0,pch=2,col="black", main="test", lwd = 1)
points(y[1],p[1],pch=17,col="black")
lines(c(0,y[1]),c(0,0))
lines(c(21,25),c(1,1))
for (i in 2:n){
  points(y[i],p[i-1],pch=2,col="blue", lwd = 1)
  points(y[i],p[i],pch=17,col="blue", lwd = 1)
  lines(c(y[i-1],y[i]),c(p[i-1],p[i-1]), lwd = 1)
}
text(19,.6,expression(F[Y*"|"*Z] * "(a|b=1)"),cex=.75)
text(14,.8,expression(F[Y*"|"*Z] * "(a|b=0)"),cex=.75, col="blue")
dev.off()


## ####################
## Conditional PDFs of wage given experience and female
## ####################
png("./plots/CPDF_wage_ex_fem.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
wage = allbus2023$wage
ex = allbus2023$exper
fem = allbus2023$female
wage1 = ifelse(fem==1, wage, NA)
wage2 = ifelse(fem!=1, wage, NA)
ex1 = ifelse(fem==1, ex, NA)
ex2 = ifelse(fem!=1, ex, NA)
getF = function(wage, ex){
  n6 <- length(wage/100)^(-1/6)
  hw <- .9*sd(wage,na.rm = TRUE)*n6
  he <- 1.5*sd(ex, na.rm=TRUE)*n6
  x <- c(0,10,25)
  g = c(0,1)
  y <- seq(0,50,.25)
  N <- length(y)
  f <- matrix(0,N,3)
  for (i in 1:3){
    d <- dnorm((ex-x[i])/he)
    fx <- mean(d,na.rm = TRUE)
    for (j in 1:N) {
      fy <- mean(d*dnorm((wage-y[j])/hw),na.rm=TRUE)/hw
      f[j,i] <- fy/fx
    }
  }
  f
}
y <- seq(0,50,.25)
f1 = getF(wage1, ex1)
f2 = getF(wage2, ex2)
plot(y,f1[,2],type="l",lty=1,xlim=c(-0.1,45),ylim=c(0,.15),xaxs="i",yaxs="i",ylab="",xlab="",bty="n",xaxt="n")
lines(y,f2[,2], col="blue")
axis(side=1,seq(-10,70,10),cex.axis=.75)
text(18.5,0.115,"f(a|b=10,c=1)",cex=.75)
text(27,.05,"f(a|b=10,c=0)",cex=.75, col = "blue")
dev.off()


## ####################
## Conditional CDFs of wage given experience and female
## ####################
png("./plots/CCDF_wage_ex_fem.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
wage = allbus2023$wage
ex = allbus2023$exper
fem = allbus2023$female
wage1 = ifelse(fem==1, wage, NA)
wage2 = ifelse(fem!=1, wage, NA)
ex1 = ifelse(fem==1, ex, NA)
ex2 = ifelse(fem!=1, ex, NA)
getF = function(wage, ex){
  n6 <- length(wage/100)^(-1/6)
  hw <- .9*sd(wage,na.rm = TRUE)*n6
  he <- 1.5*sd(ex, na.rm=TRUE)*n6
  x <- c(0,10,25)
  g = c(0,1)
  y <- seq(0,50,.25)
  N <- length(y)
  f <- matrix(0,N,3)
  for (i in 1:3){
    d <- dnorm((ex-x[i])/he)
    fx <- mean(d,na.rm = TRUE)
    for (j in 1:N) {
      fy <- mean(d*dnorm((wage-y[j])/hw),na.rm=TRUE)/hw
      f[j,i] <- fy/fx
    }
  }
  f
}
y <- seq(0,50,.25)
f1 = getF(wage1, ex1)
f2 = getF(wage2, ex2)
plot(y,cumsum(f1[,2])/sum(f1[,2]),type="l",lty=1,xlim=c(-0.1,45),ylim=c(0,1),xaxs="i",yaxs="i",ylab="",xlab="",bty="n",xaxt="n")
lines(y,cumsum(f2[,2])/sum(f2[,2]), col="blue")
axis(side=1,seq(-10,70,10),cex.axis=.75)
text(10,.8,"F(a|b=10,c=1)",cex=.75)
text(16,.2,"F(a|b=10,c=0)",cex=.75, col="blue")
dev.off()


## ####################
## Conditional PDFs wage given education
## ####################
png("./plots/CPDF_wage_edu.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
wage = allbus2023$wage
ed = allbus2023$edu
wage1 <- wage[which(ed==12)]
wage2 <- wage[which(ed==14)]
wage3 <- wage[which(ed==18)]
x <- c(seq(0,9,1),seq(10,40,2.5),seq(45,80,5))
n <- length(x)
f1 <- density(wage1,from=0,to=80,adjust=2.,na.rm = TRUE)
f2 <- density(wage2,from=0,to=80,adjust=2,na.rm = TRUE)
f3 <- density(wage3,from=0,to=80,adjust=2,na.rm = TRUE)
plot(f1$x,f1$y,type="l",lty=1,xlim=c(-.1,45),ylim=c(0,.11),xlab="",ylab="",xaxt="n",yaxt="n",title=NULL,xaxs="i",yaxs="i",bty="n")
lines(f2$x,f2$y)
lines(f3$x,f3$y)
axis(side=1,seq(-10,100,10),cex.axis=.75)
axis(side=2,seq(0,0.1,0.02),cex.axis=.75)  # Added y-axis
lines(c(0,0),c(0,f1$y[1]))
text(14,.098,"f(a|b=12)",cex=.75)
text(17.5,.077,"f(a|b=14)",cex=.75)
text(27,.040,"f(a|b=18)",cex=.75)
dev.off()




## ####################
## Conditional CDFs wage given education
## ####################
png("./plots/CCDF_wage_edu.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
wage = allbus2023$wage
ed = allbus2023$edu

wage1 <- wage[which(ed==12)]
wage2 <- wage[which(ed==14)]
wage3 <- wage[which(ed==18)]
x <- c(seq(0,9,1),seq(10,40,2.5),seq(45,80,5))
n <- length(x)
f1 <- density(wage1,from=0,to=80,adjust=2.,na.rm = TRUE)
f2 <- density(wage2,from=0,to=80,adjust=2,na.rm = TRUE)
f3 <- density(wage3,from=0,to=80,adjust=2,na.rm = TRUE)
plot(f1$x,cumsum(f1$y)/sum(f1$y),type="l",lty=1,xlim=c(-.1,45),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n",title=NULL,xaxs="i",yaxs="i",bty="n")
lines(f2$x,cumsum(f2$y)/sum(f2$y))
lines(f3$x,cumsum(f3$y)/sum(f3$y))
axis(side=1,seq(-10,100,10),cex.axis=.75)
axis(side=2,seq(-1,1,.2))
lines(c(0,0),c(0,f1$y[1]))
text(13,.83,"F(a|b=12)",cex=.70)
text(20,0.55,"F(a|b=14)",cex=.70)
text(18,.25,"F(a|b=18)",cex=.7)
dev.off()