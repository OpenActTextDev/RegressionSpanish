#  FILENAME IS Chap2RFigures.txt  ;

#setwd("c://BOOK3//CUPBook//Chapter2")
setwd("R://BOOK3//CUPBook//Chapter2")
library(HH)
Lot = read.csv(choose.files(),header=TRUE)
attach(Lot)
names(Lot)

#  FIGURE 2.1
postscript("F2HistPopSales.ps")
par(mfrow=c(1, 2), cex=1.3, mar=c(4.1,3.1,1.2,1))
hist(POP, main="", ylab="", las=1)
mtext("Frequency", side=2, at=30, las=1, cex=1.3, adj=.6)
hist(SALES, main="", ylab="", las=1)
mtext("Frequency", side=2, at=34, las=1, cex=1.3, adj=.6)
export.eps("F2HistPopSales.eps", width=6,height=4)
dev.off()

#  TABLE 2.1 SUMMARY STATISTICS
options(digits=5)
Xymat <- data.frame(cbind(POP,SALES))   
meanSummary <- sapply(Xymat, mean,  na.rm=TRUE) 
sdSummary   <- sapply(Xymat, sd,    na.rm=TRUE) 
minSummary  <- sapply(Xymat, min,   na.rm=TRUE) 
maxSummary  <- sapply(Xymat, max,   na.rm=TRUE) 
medSummary  <- sapply(Xymat, median,na.rm=TRUE) 
summvar <- cbind(meanSummary, medSummary, sdSummary, minSummary, maxSummary)
summvar

#  FIGURE 2.2, WITH CORRELATIONS
postscript("F2SalesVsPoP.ps")
par(mar=c(4.1,3.8,2,1),cex=1.1)
plot(POP, SALES, ylab="", las=1)
mtext("SALES",side=2, at=36000, las=1, cex=1.1)
export.eps("F2SalesVsPoP.eps", width=4,height=4)
dev.off()

#  FIGURE 2.3

#figure 2.3
#simulation
S<-vector(mode="numeric",length=1000)
C<-vector(mode="numeric",length=1000)
Var<-vector(mode="numeric",length=1000)
CTE<-vector(mode="numeric",length=1000)
for (i in 1:1000){
	for (j in 1:1000){
	S[j]<-100*exp(.08*10+.15*(10^.5)*rnorm(1))
	C[j]<-exp(-.06*10)*max(0,110-S[j])	
	}
	C<-sort(C)
	Var[i]<-C[950]
	CTE[i]<-mean(C[950:1000])	
}

model<-lm(CTE~Var)
b0<-round(model$coef[1],digits=3)
b1<-round(model$coef[2],digits=3)
R2<-round(summary(model)$r.squared,digits=4)

plot(Var, CTE,
xlab=expression(paste("VaR Estimates")) ,
ylab=expression(paste("CTE Estimates")),
xlim=c(0,12),ylim=c(8,20),xaxs="i",yaxs="i",pch=20,cex=0.4)
lines(Var,model$fitted,lwd=.5)
abline(h=c(10,12,14,16,18,20),col="grey")
export.eps("VarCTEFig.eps", width=4,height=4)
dev.off()


cor(POP, SALES)

#  SECTION 2.1 OUTPUT
model.basiclinearreg<-lm(SALES ~ POP)
summary(model.basiclinearreg)

#  SECTION 2.5.2 CONFIDENCE INTERVALS
confint(model.basiclinearreg)
confint(model.basiclinearreg, level=.90)

#  SECTION 2.5.3 PREDICTION INTERVALS
newdata <- data.frame(POP <- 10000)
predict(model.basiclinearreg, newdata, interval="prediction")
predict(model.basiclinearreg, newdata, interval="prediction", level=.90)
#  PROVIDES THE 97.5TH PERCENTILE OF A T-DISTRIBUTION, JUST FOR CHECKING
qt(.975, 48)

#  FIGURE 2.4
postscript("F2NormalCurve.ps")
par(mar=c(2.1,.2,.2,.2),cex=1.2)
x <- seq(-2.5, 2.5, by = 0.01)
y <- dnorm(x, sd=0.8)
plot(y, x, xlim=c(0, 3), ylim=c(-3, 5), type="l", xaxt="n", yaxt="n", xlab="", ylab="")
lines(y+1, x+1)
lines(y+2, x+2)
axis(1, c(0, 1, 2), labels=c(expression(x[1]), expression(x[2]),expression(x[3])), cex=1.2)
abline(0, 1)

segments(0, -3, 0, 5, lty=2)
segments(1, -3, 1, 5, lty=2)
segments(2, -3, 2, 5, lty=2)

points(0, 0.3, pch=19)
points(1, 0.5, pch=19)
points(2, 1.8, pch=19)

arrows(0.5, 3, 0.6, 0.6, code=2, angle=10, length=0.2)
text(0.6, 3.5, "True Unknown \n Regression Line", cex=.9)
arrows(1.5, -1.6, 1.2, -0.1, code=2, angle=10, length=0.2)
text(1.5, -2.3, "Each Response Tends \n To Fall Near The Height Of \n The Regression Line", cex=.9)
text(2.6, -.2, "The Center Of Each Normal \n Curve Is At The Height OF \n The Regression Line", cex=.9)
export.eps("F2NormalCurve.eps", width=8,height=4)
dev.off()

#  FIGURE 2.5
postscript("F2ANOVADecomp.ps")
par(mar=c(2.2,2.1,.2,.2),cex=1.2)
x <- seq(-4, 4, len=101)
y <- x
plot(x, y, type = "l", xlim=c(-3, 4), xaxt="n", yaxt="n", xlab="", ylab="")
axis(1, at = c(-1, 1),lab = expression(bar(x), x))
axis(2, at = c(-1, 1, 3),lab = expression(bar(y), hat(y), y), las=1)
abline(-1, 0, lty = 2)
segments(-4, 1, 1, 1, lty=2)
segments(-4, 3, 1, 3, lty = 2)
segments(1, -4, 1, 3, lty = 2)
segments(-1, -4, -1, -1, lty = 2)

points(1, 3, cex=1.5, pch=19)

arrows(1.0, 1, 1.0, 3, code = 3, lty = 1, angle=15, length=0.12, lwd=2)
text(1.3, 2.2,   expression( y-hat(y)),cex=0.8) 
#text(.3,2.2,"'unexplained' deviation", cex=.8) 
arrows(1.0, -1, 1.0, 1, code = 3, lty = 1, angle=15, length=0.12, lwd=2)
text(1.7, 0, expression(hat(y)-bar(y) == b[1](x-bar(x)) ), cex=0.8 )
#text(2.1, -0.5, " 'explained' deviation", cex=0.8)
#arrows(.9, -1, .9, 3, code = 3, lty = 1, angle=15, length=0.12, lwd=2)
#text(.6, 1.2, cex=0.8  , expression( y-bar(y)))#"total deviation")
arrows(-1, -1.0, 1, -1.0, code = 3, lty = 1, angle=15, length=0.12, lwd = 2)
text(0, -1.3, expression( x-bar(x)), cex=0.8  )
text(3.5, 2.7, expression( hat(y)== b[0]+ b[1]*x), cex=0.8  )
export.eps("F2ANOVADecomp.eps", width=8,height=4)
dev.off()

#  FIGURE 2.6 HERE

postscript("F2BasicLSRE1.ps")
par(mar=c(3.8,2.8,1,1), cex=1.3)
x <- c(1,2,2.3,2.5,1.5,1.7,2.6,2.8,.9,.88,.8,1.2,1.3,1.45,1.8,2.2,2.1)
y <- c(.5,2.2,2.6,2.5,.8,1.5,2.3,2.4,.75,.7,1.3,1.5,1.7,2.3,2.3,2.7,1.25)
plot(x,y, xlim=c(0.5,3), ylim=c(0,3.5), bty="l", xaxt="n", yaxt="n", ylab="", xlab="")
mtext("y", side=2, at=3.5, line=2, las=1, cex=1.3)
mtext("x", side=1, line=2, cex=1.3)
a <- seq(.75,2.75, by = .001)
b = a
lines(a,b)
export.eps("F2BasicLSRE1.eps", width=4,height=4)
dev.off()

postscript("F2BasicLSRE2.ps")
par(mar=c(3.8,2.8,1,1), cex=1.3)
x <- c(1,2,2.3,2.5,1.5,1.7,2.6,2.8,.9,.88,.8,1.2,1.3,1.45,1.8,2.2,2.45)
y <- c(1,2,2.3,2.5,1.2,1.6,2.4,2.6,1.1,1.11,1.2,1.3,1.45,1.6,1.95,2.3,2.7)
plot(x,y, xlim=c(0.5,3), ylim=c(0,3.5), bty="l", xaxt="n", yaxt="n", ylab="", xlab="")
mtext("y", side=2, at=3.5, line=2, las=1, cex=1.3)
mtext("x", side=1, line=2, cex=1.3)
a <- seq(.75,2.75, by = .001)
b = a
lines(a,b)
export.eps("F2BasicLSRE2.eps", width=4,height=4)
dev.off()

postscript("F2BasicLSRE3.ps")
par(mar=c(3.8,2.8,1,1), cex=1.3)
x <- c(1,2,2.3,2.5,1.5,1.7,2.6,2.8,2.6,1.5,2,1.2,1.3,1.45,1.8,2.2,2.45)
y <- c(1,2,2.3,2.5,1.2,1.6,2.4,2.6,2,2,2.4,1.3,1.55,1.6,1.95,1.6,2.7)
plot(x,y, xlim=c(-1.5,5), ylim=c(-2,5.5), bty="l", xaxt="n", yaxt="n", ylab="", xlab="")
mtext("y", side=2, at=5.1, line=2, las=1, cex=1.3)
mtext("x", side=1, line=2, cex=1.3)
a <- seq(-.5,4.5, by = .001)
b = a
lines(a,b)
export.eps("F2BasicLSRE3.eps", width=4,height=4)
dev.off()

#  SECTION 2.6 OUTLIER EXAMPLE
OUTLR = read.csv(choose.files(),header=TRUE)
str(OUTLR)

#  FIGURE 2.7
postscript("F2Outlier.ps")
par(mar=c(4.1,3.1,1.1,.1), cex=1.3)
plot(OUTLR$X, OUTLR$Y, xlab="x", ylab="", xlim=c(0, 10), ylim=c(2, 9), las=1)
mtext("y", at=5.5,side=2,las=1,cex=1.3, line=2.3)
points(4.3, 8.0)
text(4.7, 8.0, "A", cex=1.3)
points(9.5, 8.0)
text(9.9, 8.0, "B", cex=1.3)
points(9.5, 2.5)
text(9.9, 2.5, "C", cex=1.3)
export.eps("F2Outlier.eps", width=6,height=4)
dev.off()

#  TABLE 2.6
model.outlr0 <- lm(OUTLR$Y ~ OUTLR$X, subset=-c(20,21,22))
summary(model.outlr0)
model.outlrA <- lm(OUTLR$Y ~ OUTLR$X, subset=-c(21,22))
summary(model.outlrA)
model.outlrB <- lm(OUTLR$Y ~ OUTLR$X, subset=-c(20,22))
summary(model.outlrB)
model.outlrC <- lm(OUTLR$Y ~ OUTLR$X, subset=-c(20,21))
summary(model.outlrC)

#  TABLE 2.7
model.basiclinearreg<-lm(SALES ~ POP, Lot)
summary(model.basiclinearreg)
model.Kenosha<-lm(SALES ~ POP, Lot, subset=-c(9))
summary(model.Kenosha)

#  FIGURE 2.8
postscript("F2PlotWithKenosha.ps")
par(mar=c(4.1,3.9,2,1),cex=1.1)
plot(POP, SALES, ylab="", las=1)
mtext("SALES", side=2, at=36000,cex=1.1, las=1)
text(5000, 24000, "Kenosha")
export.eps("F2PlotWithKenosha.eps", width=4,height=4)
dev.off()

#  FIGURE 2.9
postscript("F2QQplotsKenosha.ps")
par(mfrow=c(1, 2), mar=c(4.1,3.9,1.7,1),cex=1.1)
qqnorm(residuals(model.basiclinearreg), main="", ylab="", las=1)
mtext("Sample Quantiles", side=2,at=20500,las=1,cex=1.1, adj=.5)
qqnorm(residuals(model.Kenosha), main="", ylab="", las=1)
mtext("Sample Quantiles", side=2,at=9050,las=1,cex=1.1, adj=.5)
export.eps("F2QQplotsKenosha.eps", width=8,height=4)
dev.off()

#  SECTION 2.7 OUTPUT

detach(Lot)
CAPM <- read.csv(choose.files(),header=TRUE)
attach(CAPM)
str(CAPM)
model.CAPM<-lm(LINCOLN ~ MARKET)
anova(model.CAPM)
summary(model.CAPM)


#  DELETE OBSERVATION NUMBER 22, OCTOBER 1987 CRASH
model.CAPM<-lm(CAPM$LINCOLN ~ CAPM$MARKET, subset = -c(22) )
summary(model.CAPM)

#  FIGURE 2.10
postscript("F2TimeSeriesPlots.ps")

par(mar=c(4.1,3.1,2,1),cex=1.1, las=1)
foo <- ts(CAPM, freq = 12, start = c(1986, 1))
ts.plot(foo[,2], foo[,3], xlab="Year", ylab="", type="o", lty=c(1, 2))
mtext("Monthly Return", side=2, at=.38,las=1,cex=1.1, adj=.5)
legend(1986, 0.3, c("LINCOLN", "MARKET"), lty=1:2, cex=0.5)
export.eps("F2TimeSeriesPlots.eps", width=6,height=4)
dev.off()

#  FIGURE 2.11
postscript("F2LincolnvsMarket.ps")

par(mar=c(4.1,3.1,1.4,0.2),cex=1.1, las=1)
plot(MARKET, LINCOLN, xlab="MARKET", ylab="", xlim=c(-0.3, 0.2), ylim=c(-0.3, 0.4),las=1)
mtext("LINCOLN", side=2,at=0.46,las=1, cex=1.1, adj=.5)
reg <- lm(LINCOLN ~ MARKET)
abline(reg)
arrows(-0.22, -0.1, -0.22, -0.22,length=0.1, angle = 10)
text(-0.22, -0.08, "OCTOBER, 1987 CRASH", cex=0.8)
arrows(0.1, 0.02, 0, -0.27,length=0.1, angle = 10)
arrows(0.1, 0.02, 0.06, 0.3,length=0.1, angle = 10)
text(0.16, 0.02, "1990 OUTLIERS", cex=0.8)
export.eps("F2LincolnvsMarket.eps", width=6,height=4)
dev.off()

#  FIGURE 2.12
LifeExp = read.csv(choose.files(),header=TRUE)
#  REMOVE MISSING LIFEEXPs
LifeExp3 = subset(LifeExp, !is.na(LIFEEXP) )   
attach(LifeExp3)
postscript("UNLife1.ps")
plot(FERTILITY, LIFEEXP)
export.eps("UNLife1.eps", width=5,height=4)
dev.off()
