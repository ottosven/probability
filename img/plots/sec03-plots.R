## ####################
## Bivariate joint distribution calculation
## ####################
png("./img/sec03-jointCDF1.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
x <- c(-5,2,2,-5)
y <- c(2,2,-5,-5)
plot.new()
plot.window(c(-5,5),c(-5,5))
polygon(x,y,col=gray(.8),border=NA)
abline(h=0)
abline(v=0)
points(2,2,pch=19,col="black")
arrows(2,2,2,-5,angle=20,length=.15)
arrows(2,2,-5,2,angle=20,length=.15)
text(3,2.2,"(a,b)",cex=.75)
lab=expression(paste(P,"(",Y<=a,", ",Z<=b,")"))
text(-2.2,-1.5,lab,cex=.75)
dev.off()



## ####################
## Probability and Distribution Functions
## ####################
png("./img/sec03-jointCDF2.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
x <- c(-2,3,3,-2)
y <- c(3,3,-2,-2)
plot.new()
plot.window(c(-5,5),c(-5,5))
polygon(x,y,col=gray(.8),border=NA)
abline(h=0)
abline(v=0)
points(3,3,pch=19,col="black",cex=.75)
points(3,-2,pch=19,col="black",cex=.75)
points(-2,3,pch=19,col="black",cex=.75)
points(-2,-2,pch=19,col="black",cex=.75)
lines(c(-2,3),c(-2,-2))
lines(c(-2,3),c(3,3))
lines(c(-2,-2),c(-2,3))
lines(c(3,3),c(-2,3))
text(4,3.2,"(b,d)",cex=.75)
text(4,-2.2,"(b,c)",cex=.75)
text(-3,3.2,"(a,d)",cex=.75)
text(-3,-2.2,"(a,c)",cex=.75)
text(5,-.3,"Y",cex=.75)
text(.25,5,"Z",cex=.75)
points(3,0,pch=19,col="black",cex=.75)
points(-2,0,pch=19,col="black",cex=.75)
points(0,3,pch=19,col="black",cex=.75)
points(0,-2,pch=19,col="black",cex=.75)
text(-2.25,.25,"a",cex=.75)
text(3.25,.3,"b",cex=.75)
text(.25,-2.25,"c",cex=.75)
text(.25,3.3,"d",cex=.75)
lab <- expression(paste(P,"(a<",Y<=b,", c<",Z<=d,")"))
text(0.6,1.1,lab,cex=.75)
dev.off()





## ####################
## Joint CDF wage and experience
## ####################
png("./img/sec03-WageExCDF.png", width = 480, height = 480)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
wage= rep(na.omit(statisticsdata::allbus21$wage),100)
set.seed(42)
wage = wage*(abs(rnorm(length(wage), mean = 1, sd=0.05)))
wage[which(wage == 0)] = abs(rnorm(length(which(wage == 0)),0,2))
ex = statisticsdata::allbus21$experience
hrwage = wage
F <- function (x,y) mean((wage <= x)*(ex <= y),na.rm = TRUE)
y <- seq(0,70,2)
x <- c(seq(0,10,2),seq(12.5,40,2.5))
xn <- length(x)
yn <- length(y)
z <- matrix(0,xn,yn)
for (j in 1:xn){
  for (i in 1:yn){
    z[j,i] <- F(x[j],y[i])
  }}
pmat <- persp(x,y,z,theta=-30,phi=10,d=1.5,ticktype="detailed",box=FALSE,xlim=c(-5,40),ylim=c(-5,70),col=gray(.8))
x.axis <- seq(0,40,by=10)
y.axis <- seq(0,70,by=10)
lines(trans3d(x.axis,0,0,pmat))
lines(trans3d(0,y.axis,0,pmat))
tick.start <- trans3d(x.axis, 0, 0, pmat)
tick.end   <- trans3d(x.axis,-2, 0, pmat)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
labels <- as.character(x.axis)
label.pos <- trans3d(x.axis-2, -4, 0, pmat)
text(label.pos$x, label.pos$y, labels=labels, adj=c(0, NA),cex=.75)
y.axis <- seq(0,70,by=10)
tick.start <- trans3d(0, y.axis, 0, pmat)
tick.end   <- trans3d(-2, y.axis, 0, pmat)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
labels <- as.character(y.axis)
label.pos <- trans3d(-6, y.axis, 0, pmat)
text(label.pos$x, label.pos$y, labels=labels, adj=c(0, NA),cex=.75)
text(trans3d(24,-12,0,pmat),"Wage",cex=.75)
text(trans3d(-18,10,0,pmat),"Experience",cex=.75)
dev.off()



## ####################
## Marginal CDF experience
## ####################
png("./img/sec03-ExCDF.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
wage= rep(na.omit(statisticsdata::allbus21$wage),100)
set.seed(42)
wage = wage*(abs(rnorm(length(wage), mean = 1, sd=0.05)))
wage[which(wage == 0)] = abs(rnorm(length(which(wage == 0)),0,2))
ex = statisticsdata::allbus21$experience
hrwage = wage
x <- seq(0,70,2)
n <- length(x)
F <- matrix(1,n,1)
for (i in 1:n)   F[i] <- mean(ex <= x[i],na.rm = TRUE)
y <- c(10,20,30,40,50, 60, 70)
Fy <- matrix(1,length(y),1)
for (i in 1:length(y)) Fy[i] <- mean(hrwage <= y[i])
plot(x,F, type="l",lty=1,xlab="",ylab="",xaxt="n", yaxt="n", xlim=c(0,70),ylim=c(0,1),title=NULL,xaxs="i",yaxs="i",bty="n")
axis(side=1,seq(0,70,10),cex.axis=.75)
axis(side=2,seq(0,1,0.2),cex.axis=.75)
text(47,.9,"F(a)",cex=.75)
dev.off()




## ####################
## Joint PDF wage and experience
## ####################
png("./img/sec03-WageExPDF.png", width = 480, height = 480)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
wage= rep(na.omit(statisticsdata::allbus21$wage),100)
set.seed(42)
wage = wage*(abs(rnorm(length(wage), mean = 1, sd=0.05)))
wage[which(wage == 0)] = abs(rnorm(length(which(wage == 0)),0,2))
ex = statisticsdata::allbus21$experience
hrwage = wage
n6 <- length(wage)^(-1/6)
hw <- 1.1*sd(wage,na.rm = TRUE)*n6
he <- 1.5*sd(ex,na.rm = TRUE)*n6
F <- function (x,y) mean(dnorm((ex-y)/he)*dnorm((wage-x)/hw),na.rm=TRUE)/hw/he
y <- c(seq(0,8,1),seq(10,70,2))
x <- seq(0,40,1)
xn <- length(x)
yn <- length(y)
z <- matrix(0,xn,yn)
for (j in 1:xn){
  for (i in 1:yn){
    z[j,i] <- F(x[j],y[i])
  }}
pmat <- persp(x,y,z,theta=-30,phi=10,d=1.5,ticktype="detailed",box=FALSE,xlim=c(-5,40),ylim=c(-5,70),col=gray(.8))
x.axis <- seq(0,40,by=10)
tick.start <- trans3d(x.axis, 0, 0, pmat)
tick.end   <- trans3d(x.axis,-2, 0, pmat)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
labels <- as.character(x.axis)
label.pos <- trans3d(x.axis-2, -4, 0, pmat)
text(label.pos$x, label.pos$y, labels=labels, adj=c(0, NA),cex=.75)
y.axis <- seq(0,70,by=10)
tick.start <- trans3d(0, y.axis, 0, pmat)
tick.end   <- trans3d(-2, y.axis, 0, pmat)
segments(tick.start$x, tick.start$y, tick.end$x, tick.end$y)
labels <- as.character(y.axis)
label.pos <- trans3d(-6, y.axis, 0, pmat)
text(label.pos$x, label.pos$y, labels=labels, adj=c(0, NA),cex=.75)
text(trans3d(24,-12,0,pmat),"Wage",cex=.75)
text(trans3d(-18,10,0,pmat),"Experience",cex=.75)
lines(trans3d(x.axis,0,0,pmat))
lines(trans3d(0,y.axis,0,pmat))
dev.off()




## ####################
## Conditional densities wage and experience
## ####################
png("./img/sec03-WageExCondPDF.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 2, cex.axis=0.9)
wage = statisticsdata::allbus21$wage
ex = statisticsdata::allbus21$experience
n6 <- length(wage/100)^(-1/6)
hw <- .9*sd(wage,na.rm = TRUE)*n6
he <- 1.5*sd(ex, na.rm=TRUE)*n6
x <- c(0,10,25)
y <- seq(0,60,.25)
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
plot(y,f[,1],type="l",lty=1,xlim=c(-0.1,60),ylim=c(0,.14),xaxs="i",yaxs="i",ylab="",xlab="",bty="n",xaxt="n")
lines(y,f[,2])
lines(y,f[,3])
lines(c(0,0),c(0,f[1,1]))
axis(side=1,seq(-10,70,10),cex.axis=.75)
text(18,.123,"f(a|b=0)",cex=.75)
text(21,.1,"f(a|b=10)",cex=.75)
text(28,.04,"f(a|b=25)",cex=.75)
dev.off()




## ####################
## PDF experience
## ####################
png("./img/sec03-ExPDF.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 2, cex.axis=0.9)
ex = statisticsdata::allbus21$experience
den <- density(ex,from=0,to=100,adjust=2,na.rm = TRUE)
plot(den,type="l",lty=1,xaxs="i",yaxs="i",ylim=c(0,.021),xlim=c(-0.1,90),ylab="",xlab="",bty="n",main="",cex.axis=.75)
lines(den$x,den$y)
text(20,.016,"f(a)",cex=.75)
abline(h=0)
dev.off()



## ####################
## Conditional CDF wage edu
## ####################
png("./img/sec03-CondCFDWageEdu.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 2, cex.axis=0.9)
wage= rep(statisticsdata::allbus21$wage,100)
set.seed(42)
wage = wage*(abs(rnorm(length(wage), mean = 1, sd=0.05)))
wage[which(wage == 0)] = abs(rnorm(length(which(wage == 0)),0,2))
ed = rep(statisticsdata::allbus21$education,100)

wage1 <- wage[which(ed==12)]
wage2 <- wage[which(ed==14)]
wage3 <- wage[which(ed==18)]
# wage4 <- wage[which(ed==21)]
x <- c(seq(0,9,1),seq(10,50,2.5))
n <- length(x)
F1 <- matrix(1,n,1)
F2 <- matrix(1,n,1)
F3 <- matrix(1,n,1)
# F4 <- matrix(1,n,1)
for (i in 1:n){
  F1[i] <- mean(wage1 <= x[i],na.rm = TRUE)
  F2[i] <- mean(wage2 <= x[i],na.rm = TRUE)
  F3[i] <- mean(wage3 <= x[i],na.rm = TRUE)
  # F4[i] <- mean(wage4 <= x[i],na.rm = TRUE)
}
plot(x,F1, type="l",lty=1,xlab="",ylab="",xaxt="n", yaxt="n",ylim=c(0,1),title=NULL,xaxs="i",yaxs="i",bty="n")
lines(x,F2)
lines(x,F3)
# lines(x,F4)
axis(side=1,seq(0,100,10),cex.axis=.75)
axis(side=2,seq(0,1,0.2),cex.axis=.75)
text(12,.9,"F(a| b=12)",cex=.70)
text(25.5,.81,"F(a| b=14)",cex=.70)
text(22,.45,"F(a| b=18)",cex=.7)
dev.off()




## ####################
## Conditional PDF wage edu
## ####################
png("./img/sec03-CondPDFWageEdu.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 2, cex.axis=0.9)
wage= rep(statisticsdata::allbus21$wage,100)
set.seed(42)
wage = wage*(abs(rnorm(length(wage), mean = 1, sd=0.05)))
wage[which(wage == 0)] = abs(rnorm(length(which(wage == 0)),0,2))
ed = rep(statisticsdata::allbus21$education,100)

wage1 <- wage[which(ed==12)]
wage2 <- wage[which(ed==14)]
wage3 <- wage[which(ed==18)]
# wage4 <- wage[which(ed==21)]
x <- c(seq(0,9,1),seq(10,40,2.5),seq(45,80,5))
n <- length(x)
f1 <- density(wage1,from=0,to=80,adjust=2.,na.rm = TRUE)
f2 <- density(wage2,from=0,to=80,adjust=2,na.rm = TRUE)
f3 <- density(wage3,from=0,to=80,adjust=2,na.rm = TRUE)
# f4 <- density(wage4,from=0,to=80,adjust=1,na.rm = TRUE)
plot(f1$x,f1$y,type="l",lty=1,xlim=c(-.1,35),ylim=c(0,.11),xlab="",ylab="",xaxt="n",yaxt="n",title=NULL,xaxs="i",yaxs="i",bty="n")
lines(f2$x,f2$y)
lines(f3$x,f3$y)
# lines(f4$x,f4$y)
axis(side=1,seq(-10,100,10),cex.axis=.75)
lines(c(0,0),c(0,f1$y[1]))
text(14,.098,"f(a| b=12)",cex=.75)
text(17,.077,"f(a| b=14)",cex=.75)
text(27,.040,"f(a| b=18)",cex=.75)
dev.off()




## ####################
## CEF wage experience
## ####################
png("./img/sec03-CEFwageex.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 2, cex.axis=0.9)
wage= rep(statisticsdata::allbus21$wage,100)
set.seed(42)
wage = wage*(abs(rnorm(length(wage), mean = 1, sd=0.05)))
wage[which(wage == 0)] = abs(rnorm(length(which(wage == 0)),0,2))
n6 <- length(wage)^(-1/6)
he <- 2*sd(ex,na.rm = TRUE)*n6
x <- seq(0,45,1)
N <- length(x)
m <- matrix(0,N,1)
for (i in 1:N){
  d <- dnorm((ex-x[i])/he)
  m[i] <- mean(d*wage,na.rm = T)/mean(d,na.rm=T)
}
wd <- 1.4
plot(x,m,type="l",lty=1,xaxs="i",yaxs="i",ylab="",xlab="",bty="n",xaxt="n",ylim=c(0,35),lwd=wd)
axis(side=1,seq(0,60,10),lwd=wd)
axis(side=2,seq(0,60,10),lwd=wd)
text(35,28,"E[Y|Z=b]")
dev.off()



## ####################
## CEF wage edu
## ####################
png("./img/sec03-CEFwageedu.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 2, cex.axis=0.9)
wage= rep(statisticsdata::allbus21$wage,100)
set.seed(42)
wage = wage*(abs(rnorm(length(wage), mean = 1, sd=0.05)))
wage[which(wage == 0)] = abs(rnorm(length(which(wage == 0)),0,2))
ed.new <- ed*(ed>8) + matrix(8,length(ed),1)*(ed<=8)
y <- c(8,9,10,11,12,13,14,16,18,20)
N <- length(y)
mwage <- matrix(0,N,1)
for (i in 1:N){
  wagei <- wage[which(ed.new==y[i])]
  mwage[i] <- mean(wagei,na.rm=T)
}
x <- seq(7,21,.001)
w <- .2
yy <- matrix(0,length(x),N)
for (i in 1:N){
  yy[,i] <- (abs(x-y[i]) < w)*mwage[i]
}
plot(x,yy[,1],type="l",lty=1,xaxs="i",yaxs="i",ylab="",xlab="",xaxt="n",bty="n",ylim=c(0,26),lwd=wd)
for (i in 1:N) lines(x,yy[,i],lwd=wd)
axis(side=1,seq(6,22,2),lwd=wd)
axis(side=2,seq(0,60,10),lwd=wd)
text(13,24,"E[Y|Z=b]")
dev.off()





