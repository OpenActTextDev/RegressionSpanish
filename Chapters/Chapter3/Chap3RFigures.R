#  FILENAME IS Chap3RFigures.txt  ;

setwd("c://BOOK3//CUPBook//Chapter3")
#setwd("R://BOOK3//CUPBook//Chapter3")
library(HH)

Term = read.csv(choose.files(), header=TRUE)

#  PICK THE SUBSET OF THE DATA CORRESPONDING TO TERM PURCHASE
Term2 = subset(Term, FACE > 0)
names(Term2)
attach(Term2)

LNFACE <- log(FACE)
LNINCOME <- log(INCOME)

#  FIGURE 3.1
postscript("F3TermLifeTwoPlotsB.ps", width=8, height=4)
par(mfrow=c(1, 2), cex=1.1, mar=c(4.1,4,1.5,1))
plot(INCOME, FACE, ylab="", las=1, yaxt="n", xaxt="n", xlab="INCOME (in Millions)")
mtext("FACE (in Millions)", side=2, at=15200000, las=1, cex=1.1, adj=.4)
axis(2,at=seq(0,14000000,2000000), labels=c("0", "2", 
    "4", "6", "8","10","12","14"), las=1)
axis(1,at=seq(0,11000000,1000000), labels=c("0","1", "2","3", 
    "4","5", "6","7","8","9","10","11"))
plot(LNINCOME, LNFACE, ylab="", las=1)
mtext("LNFACE", side=2, at=16.8, las=1, cex=1.1, adj=1.1)
export.eps("F3TermLifeTwoPlots.eps", width=8,height=4)
dev.off()

#  FIGURE 3.2
postscript("F3TermLifeSMatrix.ps",width=8, height=8)
par(mar=c(4.1,2.1,2.1,2.1), cex=1.1)
Term1<-data.frame(NUMHH, EDUCATION, LNINCOME, LNFACE)
pairs(Term1,upper.panel=NULL, gap=0,cex.labels=1.25, las=1)
export.eps("F3TermLifeSMatrix.eps", width=8,height=8)
dev.off()

#  FIGURE 3.3 THREE DIMENSIONAL PLANE
postscript("F33DPlane.ps",width=8, height=8)
par(mar=c(0,2,0,1), cex=0.9)
EDUCATION <- seq(3, 16, length=15)
LNINCOME <- seq(5, 15, length=15)
f <- function(EDUCATION,LNINCOME) {r <- 5 + 0.221*EDUCATION + 0.354*LNINCOME}
LNFACE <- outer(EDUCATION, LNINCOME, f)
persp(EDUCATION, LNINCOME, LNFACE, theta = 30, phi = 30, expand = 0.5, ticktype="detailed")
export.eps("F33DPlane.eps", width=4,height=4)
dev.off()

#  FIGURE 3.4  ADDED VARIABLE PLOT
detach(Term2)
Refrig = read.csv(choose.files(), header=TRUE)
attach(Refrig)

model.refrig1 <- lm(PRICE ~ RSIZE + FSIZE + SHELVES + FEATURES)
summary(model.refrig1)
model.refrig2 <- lm(ECOST ~ RSIZE + FSIZE + SHELVES + FEATURES)
summary(model.refrig2)

postscript("F3RefrigAddedVarPlot.ps")
par(mar=c(4.1,4,.1,1), cex=1.1)
plot(residuals(model.refrig2),residuals(model.refrig1) ,xlab=expression(e[1]),ylab="", las=1)
mtext(expression(e[2]), side=2, at=0, line=3, las=1, cex=1.1)
export.eps("F3RefrigAddedVarPlot.eps", width=6,height=4)
dev.off()


#  FIGURE 3.5 LETTER PLOT
attach(Term2)
LNFACE <- log(FACE)
LNINCOME <- log(INCOME)
MAR0 <- 1*(Term2$MARSTAT == 0)

num <- length(MARSTAT)
modelM0 <- lm(LNFACE ~ LNINCOME + MAR0)
summary(modelM0)

CLASSMAR0<-character(length=num )
for (i in 1:num)
{CLASSMAR0[i]<-if(MAR0[i]==0) "o" else if (MAR0[i]==1) "S"}

#test <- 1:num
#testsamp2 <- sample(test, 50)
#write.csv(testsamp2, file = "Chap3RandomSample.csv")
testsamp4 <- (read.csv(file = "Chap3RandomSample.csv", header=TRUE))[,2]

#postscript("F3LinesLetterPlot.ps",width=8, height=8)


par(mar=c(4.1,3.1,2,.1), cex=1.1)
plot(LNINCOME[testsamp4],LNFACE[testsamp4],xlab="LNINCOME", ylab="",las=1,
type="p", pch=as.character(CLASSMAR0[testsamp4]), cex=0.7)
mtext("LNFACE", side=2, at=16.8, las=1, adj=.5, cex=1.1)
Ey1 <- modelM0$coefficients[1]+modelM0$coefficients[2]*LNINCOME
Ey2 <- Ey1 + modelM0$coefficients[3]
lines(LNINCOME,Ey1)
lines(LNINCOME,Ey2)
text(6.5,15.2,expression(widehat(LNFACE)),cex=.7)
text(8.4,15.1,expression(" = 5.09 + 0.634 LNINCOME"),cex=.7)
text(11.8,9.2,expression(widehat(LNFACE)),cex=.7)
text(12.6,8.7,expression("= 4.29 + 0.634 LNINCOME"),cex=.7)
arrows(6.4, 14.8, 7,9.7, code=2, angle=10, length=0.2)
arrows(13.3,9, 12.8, 12, code=2, angle=10, length=0.2)


export.eps("F3LinesLetterPlot.eps", width=5,height=4)
dev.off()

#  FIGURE 3.6
postscript("F3Interaction.ps")
X1 <- seq(3, 16, length=15)
X2 <- seq(5, 15, length=15)
f <- function(X1,X2) {y <- 50 + .2*X1 + .3*X2 + 1*(X1-10)*(X2-10) }
y <- outer(X1, X2, f)
par(mar=c(0,2.7,0,.1))
persp(X1, X2, y, theta = 30, phi = 30, expand = 0.5, 
    ticktype="detailed")
export.eps("F3Interaction.eps", width=5,height=4)
dev.off()

#  INTERACT WITH A BINARY VARIABLE  
modelM0Interact <- lm(LNFACE ~ LNINCOME+MAR0+MAR0*LNINCOME)
summary(modelM0Interact)
Ey3 <- modelM0Interact$coefficients[1]+modelM0Interact$coefficients[2]*LNINCOME
Ey4 <- Ey3 + modelM0Interact$coefficients[3]+modelM0Interact$coefficients[4]*LNINCOME

#  FIGURE 3.7
#postscript("F3LetterInteract.ps",width=8, height=8)
par(mar=c(4.1,3.1,2,.1), cex=1.1)
plot(LNINCOME[testsamp4],LNFACE[testsamp4],xlab="LNINCOME", ylab="",las=1,
type="p", pch=as.character(CLASSMAR0[testsamp4]), cex=0.7)
mtext("LNFACE", side=2, at=16.8, las=1, adj=.5, cex=1.1)
lines(LNINCOME,Ey3)
lines(LNINCOME,Ey4)
text(6.5,15.2,expression(widehat(LNFACE)),cex=.7)
text(8.4,15.1,expression(" = 5.78 + 0.573 LNINCOME"),cex=.7)

text(11.6,9.2,expression(widehat(LNFACE)),cex=.7)
text(12.4,8.7,expression("= -1.51 + 1.185 LNINCOME"),cex=.7)

arrows(6.4, 14.8, 7  ,9.8, code=2, angle=10, length=0.2)
arrows(11.1, 8.9, 9.2,9.2, code=2, angle=10, length=0.2)


export.eps("F3LetterInteract.eps", width=5,height=4)
dev.off()



#  FIGURE 3.8
postscript("F3Curvilinear.ps")
X1 <- seq(3, 16, length=15)
X2 <- seq(5, 15, length=15)
f <- function(X1,X2) {y <- 50 + 2*X1 + 3*X2 + 3*(X1-10)*(X2-10)- .4*(X1-10)^2+.4*(X2-10)^2}
y <- outer(X1, X2, f)
par(mar=c(0,2.1,0,.1))
persp(X1, X2, y, theta = 30, phi = 30, expand = 0.5, 
   ticktype="detailed")
export.eps("F3Curvilinear.eps", width=5,height=4)
dev.off()

#  FIGURE 3.9
x <- seq(90000,105000,20)
Ey <- 20 + 1*x + 2*(x-97500)*(x>97500)
postscript("F3Charity.ps")
par(mar=c(4.1,3.1,.1,1), cex=1.3)
plot(x,Ey, type="l",yaxt="n", ylab="",  las=1)
mtext("E y", side=2,las=1, line=1.5,cex=1.1)
text(102000,102000,expression(beta[1]+beta[2]),cex=1.1)
text(94000,92000,expression(beta[1]),cex=1.1)
export.eps("F3Charity.eps", width=5,height=4)
dev.off()

#  FIGURE 3.10
postscript("F3Break.ps")
par(mar=c(4.1,4.5,.1,.1))
x <- seq(50,150,.1)
Ey <- 20 + 2*x - 1*(x-100)*(x>100) - 20*(x>100)
plot(x,Ey, type="p",ylab="", font.lab=1, cex.lab=1.1, cex=.25, las=1)
mtext("E y", side=2,las=1, line=2.8,cex=1.1)
export.eps("F3Break.eps", width=5,height=4)
dev.off()



#  FIGURE 3.11
LifeExp = read.csv(choose.files(),header=TRUE)
#  REMOVE MISSING LIFEEXPs
LifeExp3 = subset(LifeExp, !is.na(LIFEEXP) )   
attach(LifeExp3)
lnHEALTH <- log(HEALTHEXPEND)
Xmat1 <- cbind(FERTILITY,PUBLICEDUCATION,lnHEALTH)
Xymat1 <- data.frame(REGION,COUNTRY,LIFEEXP, Xmat1)
Xymat1.good <- na.omit(Xymat1) 
rm(lnHEALTH)
detach(LifeExp3)
attach(Xymat1.good)

#  ADDED VARIABLE PLOT
model4a <- lm(LIFEEXP ~ FERTILITY+lnHEALTH)
model4b <- lm(PUBLICEDUCATION ~ FERTILITY+lnHEALTH)
plot(residuals(model4b),residuals(model4a),
xlab="residuals(PUBLICEDUCATION)",ylab="residuals(LIFEEXP)")
export.eps("UNLife2.eps", width=5,height=4) 
dev.off()

#  FIGURE 3.11 - NORMAL PLOT
par(mar=c(4.2,3,1.5,.1),cex=1.5)
x <- seq(-4, 4, 0.01)
y= dnorm(x)
plot(x,y, type="l", yaxt="n", ylab="",  las=1, cex=1.4)
mtext("Standard Normal Density", side=2, at=.435,las=1.5, adj=.2,cex=1.8)
dev.off()


#  FIGURE 3.12 - CHI-SQUARE PLOT
par(mar=c(4.2,3,1.5,.1),cex=1.5)
x <- seq(0, 20, 0.01)
y= dchisq(x,3)
plot(x,y, type="l", yaxt="n", ylab="",  las=1, cex=1.4)
mtext("Chi-Square Density", side=2, at=.265,las=1.3, adj=.2,cex=1.8)
y1= dchisq(x,5)
y2= dchisq(x,10)
lines(x,y1)
lines(x,y2)
text(4,.2, "df=3",cex=1.4)
text(6,.15, "df=5",cex=1.4)
text(12,.1, "df=10",cex=1.4)
dev.off()

#  FIGURE 3.13 - t-DISTRIBUTION PLOT
par(mar=c(4.2,3,1.5,.1),cex=1.5)
x <- seq(-4, 4, 0.01)
y= dnorm(x)
plot(x,y, type="l", yaxt="n", ylab="",  las=1, cex=1.4)
mtext("t-Distribution Density",  side=2, at=.435,las=1.5, adj=.2,cex=1.8)

y1= dt(x,1)
y2= dt(x,5)
y3= dt(x,10)
lines(x,y1)
lines(x,y2)
lines(x,y3)
text(0,.1, "df=1",cex=1.4)
#text(6,.15, "df=5",cex=1.4)
text(2.5,.38, "df=infinity",cex=1.4)
arrows(1.2, .38, 0.33,.38, code=2, angle=10, length=0.2)
arrows(-0.6, .1, -1.4,.1, code=2, angle=10, length=0.2)
dev.off()

#  FIGURE 3.14 - F-DISTRIBUTION PLOT

par(mar=c(4.2,3,1.5,.1),cex=1.5)
x <- seq(0, 4, 0.01)
y= df(x,25,25)
plot(x,y, type="l", yaxt="n", ylab="",  las=1, cex=1.4)
mtext("F-Distribution Density", side=2, at=1.17,las=1.3, adj=.2,cex=1.8)
#y1= df(x,2,25)
y2= df(x,5,2)
y3= df(x,2,5)
#lines(x,y1)
lines(x,y2)
lines(x,y3)
text(2.5,.8, "df1=25, df2=25",cex=1.4)
text(6,.15, "df=5",cex=1.4)
text(12,.1, "df=10",cex=1.4)
dev.off()

