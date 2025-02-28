## ####################
## example 1 coin toss
## ####################
png("./img/sec02-ex1.png")
par(mar = c(0,0,0,0), cex=3, lwd=3)
plot.new()
plot.window(c(-5,5),c(-5,5))
W1 <- spatstat.geom::ellipse(a=5,b=5,centre=c(0,0),phi=0,npoly=1024)
plot(W1,add=TRUE)
text(-2,1,expression(heads))
text(2,-1,expression(tails))
text(4,4,expression(italic(S)))
dev.off()


## ####################
## example 2 gender
## ####################
png("./img/sec02-ex2.png")
par(mar = c(0,0,0,0), cex=3, lwd=3)
plot.new()
plot.window(c(-5,5),c(-5,5))
W1 <- spatstat.geom::ellipse(a=5,b=5,centre=c(0,0),phi=0,npoly=1024)
plot(W1,add=TRUE)
text(-2,0,expression(female))
text(2,-2,expression(male))
text(2,2,expression(diverse))
text(4,4,expression(italic(S)))
dev.off()

## ####################
## example 3 education
## ####################
png("./img/sec02-ex3.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
education = statisticsdata::allbus21$education
#Education PMF
y <- c(4,9,10,12,13,16,18,20)
n <- length(y)
p <- matrix(0,n,1)
p[1] <- mean(education <= y[1],na.rm = TRUE)
for (i in 2:n){
  p[i] <- mean(education == y[i],na.rm= TRUE)
}
L = 1:8
x <- seq(0,9,.001)
w <- .2
yy <- matrix(0,length(x),n)
for (i in 1:n){
  yy[,i] <- (abs(x-i) < w)*p[i]
}
plot(x,yy[,1],type="l",lty=1,xaxs="i",yaxs="i",ylab="",yaxt="n",xlab="",xaxt="n",bty="n",ylim=c(0,.4))
axis(side=1,at=1:8,labels=c("L1","L2","L3","L4", "L5", "L6", "L7", "L8"))
for (i in 1:n){
  lines(x,yy[,i])
}
axis(side=2,seq(0,.4,.1), lwd=2)
dev.off()



## ####################
## example 1 transform
## ####################
png("./img/sec02-ex1transf.png", width = 960, height = 480)
par(mar = c(0,0,0,0), cex=3, lwd=3)
plot.new()
plot.window(c(-5,5),c(-5,5))
W1 <- spatstat.geom::ellipse(a=2,b=4,centre=c(-3,0),phi=0,npoly=1024)
plot(W1,add=TRUE)
arrows(0,0,4,0,angle=20,length=.1)
# arrows(3,0,0,0,angle=20,length=.1)
points(1,0,pch=19,col="black",cex=.75)
points(3,0,pch=19,col="black",cex=.75)
text(3,-.5,"1",cex=.75)
text(1,-.5,"0",cex=.75)
text(-4,0,"heads",cex=.75)
text(-2,0,"tails",cex=.75)
diagram::curvedarrow((c(-2,0.4)),(c(1,.3)),lty=1,arr.pos=1,curve=-.3,arr.type="simple",angle=50)
diagram::curvedarrow((c(-4,.4)),(c(3,.3)),lty=1,arr.pos=1,curve=-.3,arr.type="simple",angle=40)
text(-4.8,2.5,expression(italic(S)),cex=.8)
text(3.8,0.5,"\u211D",cex=.8)
dev.off()



## ####################
## example 3 education PMF
## ####################
png("./img/sec02-ex3PMF.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
education = statisticsdata::allbus21$education
#Education PMF
y <- c(4,9,10,12,13,16,18,20)
n <- length(y)
p <- matrix(0,n,1)
p[1] <- mean(education <= y[1],na.rm = TRUE)
for (i in 2:n){
  p[i] <- mean(education == y[i],na.rm= TRUE)
}
L = 1:8
x <- seq(3,22,.001)
w <- .2
yy <- matrix(0,length(x),n)
for (i in 1:n){
  yy[,i] <- (abs(x-y[i]) < w)*p[i]
}
plot(x,yy[,1],type="l",lty=1,xaxs="i",yaxs="i",ylab="",yaxt="n",xlab="",xaxt="n",bty="n",ylim=c(0,.4))
# axis(side=1,at=1:8,labels=c("L1","L2","L3","L4", "L5", "L6", "L7", "L8"))
axis(side=1,seq(3,22,2))
for (i in 1:n){
  lines(x,yy[,i])
}
axis(side=2,seq(0,.4,.1), lwd=2)
dev.off()



## ####################
## example 1 coin PMF
## ####################
png("./img/sec02-ex1PMF.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
# Coin PMF
y <- c(0,1)
n <- length(y)
p <- matrix(0.5,2,1)
x <- seq(-1,3,.001)
w <- .05
yy <- matrix(0,length(x),n)
for (i in 1:n){
  yy[,i] <- (abs(x-y[i]) < w)*p[i]
}
plot(x,yy[,1],type="l",lty=1,xaxs="i",yaxs="i",ylab="",yaxt="n",xlab="",xaxt="n",bty="n",ylim=c(0,0.6))
axis(side=1,seq(-1,3,1))
for (i in 1:n){
  lines(x,yy[,i])
}
axis(side=2,seq(0,0.6,.1), lwd=2)
dev.off()


## ####################
## example 1 female PMF
## ####################
png("./img/sec02-ex2PMF.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
# female PMF
y <- c(0,1)
n <- length(y)
p <- matrix(0.5,2,1)
p[1] = 0.493
p[2] = 0.507
x <- seq(-1,3,.001)
w <- .05
yy <- matrix(0,length(x),n)
for (i in 1:n){
  yy[,i] <- (abs(x-y[i]) < w)*p[i]
}
plot(x,yy[,1],type="l",lty=1,xaxs="i",yaxs="i",ylab="",yaxt="n",xlab="",xaxt="n",bty="n",ylim=c(0,0.6))
axis(side=1,seq(-1,3,1))
for (i in 1:n){
  lines(x,yy[,i])
}
axis(side=2,seq(0,0.6,.1), lwd=2)
dev.off()




## ####################
## example 1 coin CDF
## ####################
png("./img/sec02-ex1CDF.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
# female CDF
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
text(2,.9,"F(a)",cex=.75)
dev.off()




## ####################
## example 2 female CDF
## ####################
png("./img/sec02-ex2CDF.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
# female CDF
y <- c(0,1)
n <- length(y)
p <- matrix(0.5,2,1)
p[1] = 0.493
p[2] = 0.507
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
text(2,.9,"F(a)",cex=.75)
dev.off()



## ####################
## example 3 education CDF
## ####################
png("./img/sec02-ex3CDF.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
education = statisticsdata::allbus21$education
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
text(21,.9,"F(a)",cex=.75)
dev.off()




## ####################
## example 4 wage CDF
## ####################
png("./img/sec02-ex4CDF.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
## bootstrap from wage data
wage= rep(na.omit(statisticsdata::allbus21$wage),100)
set.seed(42)
wage = wage*(abs(rnorm(length(wage), mean = 1, sd=0.05)))
wage[which(wage == 0)] = abs(rnorm(length(which(wage == 0)),0,2))
# Wage CDF
x <- seq(0,40,0.5)
n <- length(x)
F <- matrix(1,n,1)
for (i in 1:n)   F[i] <- mean(wage <= x[i],na.rm = TRUE)

plot(x,F, lwd = 2, type="l",lty=1,xlab="",ylab="",xaxt="n", yaxt="n", xlim=c(0,50),ylim=c(0,1),title=NULL,xaxs="i",yaxs="i",bty="n")
axis(side=1,seq(0,40,10),cex.axis=.75)
axis(side=2,seq(0,1,0.2),cex.axis=.75)
# lines(c(13.41,13.41),c(0, mean(wage < 13.41)),lwd=0.75)
# lines(c(17.95,17.95),c(0, mean(wage < 17.95)),lwd=0.75)
text(48,.9,"F(a)",cex=.75)
dev.off()



## ####################
## example 4 wage CDF arrows
## ####################
png("./img/sec02-ex4CDF2.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
## bootstrap from wage data
wage= rep(na.omit(statisticsdata::allbus21$wage),100)
set.seed(42)
wage = wage*(abs(rnorm(length(wage), mean = 1, sd=0.05)))
wage[which(wage == 0)] = abs(rnorm(length(which(wage == 0)),0,2))
# Wage CDF
x <- seq(0,40,0.5)
n <- length(x)
F <- matrix(1,n,1)
for (i in 1:n)   F[i] <- mean(wage <= x[i],na.rm = TRUE)
plot(x,F, lwd = 2, type="l",lty=1,xlab="",ylab="",xaxt="n", yaxt="n", xlim=c(0,50),ylim=c(0,1),title=NULL,xaxs="i",yaxs="i",bty="n")
axis(side=1,seq(0,40,10),cex.axis=.75)
axis(side=2,seq(0,1,0.2),cex.axis=.75)
y <- c(10,20,30)
for (i in 1:3) {
  arrows(y[i],mean(wage < y[i]),0,mean(wage < y[i]),angle=20,length=.1,lty=1,lwd=0.75)
  lines(c(y[i],y[i]),c(0,mean(wage < y[i])),lwd=0.75)
}
text(48,.9,"F(a)",cex=.75)
dev.off()


## ####################
## example 4 wage quantiles arrows
## ####################
png("./img/sec02-ex4CDF3.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
## bootstrap from wage data
wage= rep(na.omit(statisticsdata::allbus21$wage),100)
set.seed(42)
wage = wage*(abs(rnorm(length(wage), mean = 1, sd=0.05)))
wage[which(wage == 0)] = abs(rnorm(length(which(wage == 0)),0,2))
# Wage CDF
x <- seq(0,40,0.5)
n <- length(x)
F <- matrix(1,n,1)
for (i in 1:n)   F[i] <- mean(wage <= x[i],na.rm = TRUE)
plot(x,F, lwd = 2, type="l",lty=1,xlab="",ylab="",xaxt="n", yaxt="n", xlim=c(0,50),ylim=c(0,1),title=NULL,xaxs="i",yaxs="i",bty="n")
axis(side=1,seq(0,40,10),cex.axis=.75)
axis(side=2,seq(0,1,0.2),cex.axis=.75)

p <- c(.1,.5,.9)
q <- quantile(wage,p,na.rm = TRUE)
arrows(q[1],p[1],q[1],0,angle=20,length=.1,lty=1,lwd=0.75)
arrows(q[2],p[2],q[2],0,angle=20,length=.1,lty=1,lwd=0.75)
arrows(q[3],p[3],q[3],0,angle=20,length=.1,lty=1,lwd=0.75)
lines(c(0,q[1]),c(p[1],p[1]),lwd=0.75)
lines(c(0,q[2]),c(p[2],p[2]),lwd=0.75)
lines(c(0,q[3]),c(p[3],p[3]),lwd=0.75)
text(48,.9,"F(a)",cex=.75)
dev.off()



## ####################
## example 3 education quantiles
## ####################
png("./img/sec02-ex3Q.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
education = statisticsdata::allbus21$education
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
text(21,.9,"F(a)",cex=.75)

p <- c(.1,.5,.9)
q <- quantile(education,p,na.rm = TRUE)
arrows(q[1],p[1],q[1],-0.04,angle=20,length=.1,lty=1,lwd=0.75)
arrows(q[2],p[2],q[2],-0.04,angle=20,length=.1,lty=1,lwd=0.75)
arrows(q[3],p[3],q[3],-0.04,angle=20,length=.1,lty=1,lwd=0.75)
lines(c(0,q[1]),c(p[1],p[1]),lwd=0.75)
lines(c(0,q[2]),c(p[2],p[2]),lwd=0.75)
lines(c(0,q[3]),c(p[3],p[3]),lwd=0.75)

dev.off()



## ####################
## Standard normal pdf
## ####################
png("./img/sec02-N(0,1).png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
x <- seq(-6,6,by=0.01)
f1 <- dnorm(x)
f2 = dt(x,5)
plot(x,f1,type="l",lty=1,xaxs="i",xlim=c(-5,5),ylab="",xlab="",bty="n")
lines(x,f2, lty=2)
axis(side=1,seq(-8,8,2))
dev.off()



## ####################
## example 4 wage PDF
## ####################
png("./img/sec02-ex4PDF.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
## bootstrap from wage data
wage= rep(na.omit(statisticsdata::allbus21$wage),100)
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
plot(den,type="l",lty=1,xaxs="i",yaxs="i",ylim=c(0,.09),xlim=c(-0.1,40),ylab="",xlab="",bty="n",main="",cex.axis=.75)
polygon(c(20,20,dx,30,30),c(0.0001,d20,dd,d30,0.0001),col=gray(.8),border=NA)
lines(den$x,den$y)
abline(h=0)
lines(c(20,20),c(0,d20))
lines(c(30,30),c(0,d30))
text(34,0.02,"P(20<Y<30)=0.163",cex=.75)
arrows(32.5,(d20+d30)/2,23.5,(d20+d30)/2-.005,angle=20,length=.1)
lines(c(0,0),c(0,den$y[1]))
text(7,.055,"f(a)",cex=.75)
dev.off()




# Create Probability Mass Function (PMF) Plot for Education Data
# Probabilities provided in the problem statement

# Output PNG file
png("./img/sec02-ex3PMF.png", width = 480, height = 300)

# Set plot parameters
par(mar = c(2, 2, 0.9, 0.5), cex = 1.6, lwd = 3, cex.axis = 0.9)

# Define education levels and their probabilities
y <- c(4, 10, 12, 13, 14, 16, 18, 21)
p <- c(0.008, 0.048, 0.392, 0.072, 0.155, 0.071, 0.225, 0.029)

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
text(21, 0.45, "π(a)", cex = 0.75)

# Close device
dev.off()