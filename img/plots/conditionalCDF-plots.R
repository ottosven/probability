

## ####################
## Conditional densities wage given experience and female
## ####################
png("./img/WageExFemCondPDF.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 2, cex.axis=0.9)
wage = statisticsdata::allbus21$wage
ex = statisticsdata::allbus21$experience
fem = statisticsdata::allbus21$female

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
lines(y,f2[,2])
axis(side=1,seq(-10,70,10),cex.axis=.75)
text(20,.123,"f(a|b=10,c=1)",cex=.75)
text(27,.05,"f(a|b=10,c=0)",cex=.75)
dev.off()


## ####################
## Conditional CDF wage given experience and female
## ####################
png("./img/WageExFemCondCDF.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 2, cex.axis=0.9)
wage = statisticsdata::allbus21$wage
ex = statisticsdata::allbus21$experience
fem = statisticsdata::allbus21$female

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
lines(y,cumsum(f2[,2])/sum(f2[,2]))
axis(side=1,seq(-10,70,10),cex.axis=.75)
text(10,.8,"F(a|b=10,c=1)",cex=.75)
text(16,.2,"F(a|b=10,c=0)",cex=.75)
dev.off()





## ####################
## Conditional PDF wage edu
## ####################
png("./img/WageEduCondPDF.png", width = 480, height = 300)
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
text(14,.098,"f(a|b=12)",cex=.75)
text(17,.077,"f(a|b=14)",cex=.75)
text(27,.040,"f(a|b=18)",cex=.75)
dev.off()



## ####################
## Conditional CDF wage edu
## ####################
png("./img/WageEduCondCDF.png", width = 480, height = 300)
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
plot(f1$x,cumsum(f1$y)/sum(f1$y),type="l",lty=1,xlim=c(-.1,35),ylim=c(0,1),xlab="",ylab="",xaxt="n",yaxt="n",title=NULL,xaxs="i",yaxs="i",bty="n")
lines(f2$x,cumsum(f2$y)/sum(f2$y))
lines(f3$x,cumsum(f3$y)/sum(f3$y))
# lines(f4$x,f4$y)
axis(side=1,seq(-10,100,10),cex.axis=.75)
axis(side=2,seq(-1,1,.2))
lines(c(0,0),c(0,f1$y[1]))
# text(14,.098,"F(a|b=12)",cex=.75)
# text(17,.077,"F(a|b=14)",cex=.75)
# text(27,.040,"F(a|b=18)",cex=.75)
text(12,.83,"F(a|b=12)",cex=.70)
text(19.6,.71,"F(a|b=14)",cex=.70)
text(17,.25,"F(a|b=18)",cex=.7)
dev.off()


# 
# ## ####################
# ## Conditional CDF female edu
# ## ####################
# png("./img/sec02-ex2CDF.png", width = 480, height = 300)
# par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
# # female CDF
# y <- c(0,1)
# n <- length(y)
# p <- matrix(0.5,2,1)
# p[1] = 0.493
# p[2] = 0.507
# p <- cumsum(p)
# plot.new()
# plot.window(c(-1,2),c(0.0,1))
# axis(side=1,seq(-2,2,1))
# axis(side=2,seq(-1,1,.2))
# points(y[1],0,pch=1,col="black", main="test")
# points(y[1],p[1],pch=19,col="black")
# lines(c(-2,y[1]),c(0,0))
# lines(c(1,25),c(1,1))
# for (i in 2:n){
#   points(y[i],p[i-1],pch=1,col="black")
#   points(y[i],p[i],pch=19,col="black")
#   lines(c(y[i-1],y[i]),c(p[i-1],p[i-1]))
# }
# text(2,.9,"F(a)",cex=.75)
# dev.off()






## ####################
## Conditional CDF edu married
## ####################
png("./img/EduMarrCondCDF.png", width = 480, height = 300)
par(mar = c(2, 2, 0.9, 0.5), cex=1.6, lwd = 3, cex.axis=0.9)
education = statisticsdata::allbus21$education
mar = statisticsdata::allbus21$married
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
# plot.new()
# plot.window(c(4,22),c(0.0,1))
# axis(side=1,seq(-2,22,2))
# axis(side=2,seq(-1,1,.2))
points(y[1],0,pch=2,col="black", main="test", lwd = 1)
points(y[1],p[1],pch=17,col="black")
lines(c(0,y[1]),c(0,0))
lines(c(21,25),c(1,1))
for (i in 2:n){
  points(y[i],p[i-1],pch=2,col="blue", lwd = 1)
  points(y[i],p[i],pch=17,col="blue", lwd = 1)
  lines(c(y[i-1],y[i]),c(p[i-1],p[i-1]), lwd = 1)
}
text(18,.6,"F(a|b=1)",cex=.75)
text(15,.9,"F(a|b=0)",cex=.75, col="blue")

dev.off()







################################################################

library(haven)
library(tidyverse)
#read in Allbus Data
allbus2021 <- as.data.frame(read_stata("./data/ZA5280_v2-0-0.dta"))

#explore dataset
names(allbus2021)

#show attributes
for (col in names(allbus2021)) {
  label <- attributes(allbus2021[[col]])$label
  print(paste("Column:", col, "| Label:", label))
}

#extract necessary variables
allbus2021_selected <- allbus2021 %>%
  select(age,
         sex,
         german,
         dn07,
         mstat,
         di01a,
         de18,
         iscd11,
         dw15,
         dw37)

#mutate necessary variables (education, wage, experience, Gender-dummy, migrationbackgroung-dummy, married-dummy)

#age
'-32 Nicht generierbar'


#married dummy
#mstat
#Welchen Familienstand haben Sie?
#1 Verheiratet und mit Ehepartner zusammenlebend
#2 Verheiratet und getrennt lebend
#3 Verwitwet
#4 Geschieden
#5 Ledig

#education years
# -32 Nicht generierbar
#1 Level 1 - Primary education
#2 Level 2 - Lower secondary education
#3 Level 3 - Upper secondary education
#4 Level 4 - Post secondary non-tertiary education
#5 Level 5 - Short-cycle tertiary education
#6 Level 6 - Bachelors or equivalent level
#7 Level 7 - Masters or equivalent level
#8 Level 8 - Doctoral or equivalent level

#dw15,dw37 Arbeitsstunden pro Woche
#-41 Datenfehler
#-10 Befragter ist nicht hauptberuflich erwerbstätig bzw. nebenberuflich tätig (Code 3, 4, -9 in work)
#-42 Datenfehler: Mehrfachnennung
#-9 Keine Angabe


allbus_lecture <- allbus2021_selected %>%
  #age
  mutate(age = ifelse(age==-32, NA, age)) %>% 
  #married dummy
  mutate(married = ifelse(mstat %in% c(1, 2), 1, 0)) %>% 
  #migration dummy
  mutate(germanyborn=ifelse(german==1, 1, 0)) %>% 
  #gender-dummy
  mutate(female=ifelse(sex==2, 1, 0)) %>% 
  #income - net monthly income (in EUR)
  mutate(income = ifelse(di01a %in% c(-41, -15, -9, -7), NA, ifelse(di01a == -50, 0, di01a))) %>% 
  #working hours per week
  mutate(workinghours_hj= ifelse(dw15 %in% c(-41,-42, -9), NA, ifelse(dw15== -10 , 0 ,dw15))) %>% 
  #working hours per week(Nebenjob)
  mutate(workinghours_nj= ifelse(dw37 %in% c(-41,-42, -9), NA, ifelse(dw37== -10 , 0 ,dw37))) %>% 
  #working hours per week(sum)
  mutate(workinghours= workinghours_hj+workinghours_nj) %>% 
  #wage/per hour
  mutate(wage = income / 4.35,  # Assuming 4.35 weeks in a month
         wage = wage/ workinghours) %>% 
  mutate(wage = replace(wage, is.infinite(wage) | is.nan(wage), NA)) %>% 
  #education(Bildungsjahre)
  mutate(education_years = case_when(
    iscd11 == 1 ~ 4,      # Level 1 - Primary education (4 years)
    iscd11 == 2 ~ 9,      # Level 2 - Lower secondary education (9 years)
    iscd11 == 3 ~ 10,     # Level 3 - Upper secondary education (10 years)
    iscd11 == 4 ~ 12,     # Level 4 - Post secondary non-tertiary education (12 years)
    iscd11 == 5 ~ 13,     # Level 5 - Short-cycle tertiary education (13 years)
    iscd11 == 6 ~ 16,     # Level 6 - Bachelor's or equivalent level (16 years)
    iscd11 == 7 ~ 18,     # Level 7 - Master's or equivalent level (18 years)
    iscd11 == 8 ~ 20,      # Level 8 - Doctoral or equivalent level (20 years)
    iscd11 == -32 ~ NA
  )) %>% 
  mutate(experience=age-education_years-6,
         experience= ifelse(experience==-1,0,experience)) %>% 
  select(-c("sex","german","dn07","mstat","di01a","de18","iscd11","workinghours_hj","workinghours_nj","dw15","dw37"))

#save as csv
#write.csv(allbus_lecture, file = "allbus_lecture.csv", row.names = FALSE)

education <- allbus_lecture[,8]
ed=education
wage <- allbus_lecture[,7]
hrwage=wage
lnwage <- log(hrwage)
logwage = lnwage
ex <- allbus_lecture[,9]



png("./img/fig-Conditionalexpectation-1.png", width = 320, height = 300)
# Wage given experience CEF
n6 <- length(wage)^(-1/6)
he <- 2*sd(ex, na.rm=T)*n6
x <- seq(0,50,1)
N <- length(x)
m <- matrix(0,N,1)
for (i in 1:N){
  d <- dnorm((ex-x[i])/he)
  m[i] <- mean(d*wage, na.rm=T)/mean(d, na.rm=T)
}
wd <- 1.4
plot(x,m,type="l",lty=1,xaxs="i",yaxs="i",ylab="",xlab="",bty="n",xaxt="n",ylim=c(0,55),lwd=wd)
axis(side=1,seq(0,60,10),lwd=wd)
axis(side=2,seq(0,60,10),lwd=wd)
text(40,28,"E[Y|Z=b]")
dev.off()

#####################################

png("./img/fig-Conditionalexpectation-2.png", width = 320, height = 300)
# Wage given education CEF
ed.new <- ed*(ed>8) + matrix(8,length(ed),1)*(ed<=8)
y <- c(8,9,10,11,12,13,14,16,18,20)
N <- length(y)
mwage <- matrix(0,N,1)
for (i in 1:N){
  wagei <- wage[which(ed.new==y[i])]
  mwage[i] <- mean(wagei, na.rm=T)
}
x <- seq(7,21,.001)
w <- .2
yy <- matrix(0,length(x),N)
for (i in 1:N){
  yy[,i] <- (abs(x-y[i]) < w)*mwage[i]
}
plot(x,yy[,1],type="l",lty=1,xaxs="i",yaxs="i",ylab="",xlab="",xaxt="n",bty="n",ylim=c(0,25),lwd=wd)
for (i in 1:N) lines(x,yy[,i],lwd=wd)
axis(side=1,seq(6,22,2),lwd=wd)
axis(side=2,seq(0,60,10),lwd=wd)
text(13,23,"E[Y|Z=b]")
#title(main="Conditional expectations of wage given education", cex.main=0.9)
dev.off()


## ####################
## conditional CDFs coin
## ####################
png("./img/condCDFcoin0.png", width = 480, height = 300)
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
text(1.85,.9,"F(a|b=0)",cex=.75)
dev.off()
png("./img/condCDFcoin1.png", width = 480, height = 300)
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
text(1.85,.9,"F(a|b=1)",cex=.75)
dev.off()






##########################################


# Create Conditional Probability Mass Function (PMF) Plots for Education Data
library(statisticsdata)

# Output PNG file
png("./img/EduMarrCondPMF_SideBySide.png", width = 800, height = 300)

# Set up side-by-side plots
par(mfrow = c(1, 2), 
    mar = c(2, 2, 0.9, 0.5), 
    cex = 1.6, 
    lwd = 3, 
    cex.axis = 0.9)

# Prepare data
education = statisticsdata::allbus21$education
mar = statisticsdata::allbus21$married

# Define education levels
y <- c(4, 10, 12, 13, 14, 16, 18, 21)
n <- length(y)

# Plot for Conditional Y given Z=1 Group
# Subset data for married individuals
education_married = ifelse(mar == 1, education, NA)

# Calculate probabilities for married group
p_married <- numeric(n)
p_married[1] <- mean(education_married == y[1], na.rm = TRUE)
for (i in 2:n) {
  p_married[i] <- mean(education_married == y[i], na.rm = TRUE)
}

# First plot window
plot.new()
plot.window(xlim = c(4, 22), ylim = c(0.0, 0.5))
axis(side = 1, at = seq(4, 22, by = 2))
axis(side = 2, at = seq(0, 0.5, by = 0.1))
title(expression(pi[Y*"|"*Z==1]))

# Plot vertical lines for married group
for (i in 1:length(y)) {
  lines(c(y[i], y[i]), c(0, p_married[i]), col = "black")
}

# Plot for Conditional Y given Z=0 Group
# Subset data for unmarried individuals
education_unmarried = ifelse(mar == 0, education, NA)

# Calculate probabilities for unmarried group
p_unmarried <- numeric(n)
p_unmarried[1] <- mean(education_unmarried == y[1], na.rm = TRUE)
for (i in 2:n) {
  p_unmarried[i] <- mean(education_unmarried == y[i], na.rm = TRUE)
}

# Second plot window
plot.new()
plot.window(xlim = c(4, 22), ylim = c(0.0, 0.5))
axis(side = 1, at = seq(4, 22, by = 2))
axis(side = 2, at = seq(0, 0.5, by = 0.1))
title(expression(pi[Y*"|"*Z==0]))

# Plot vertical lines for unmarried group
for (i in 1:length(y)) {
  lines(c(y[i], y[i]), c(0, p_unmarried[i]), col = "blue")
}

# Close device
dev.off()