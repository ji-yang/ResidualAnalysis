
data <- as.matrix(read.table("TireTread.txt"))
x1 <- data[,2]
x2 <- data[,3]
x3 <- data[,4]
y <- data[,1]

model1 <- glm(y~x1+x2+x3)
summ <- summary(model1)
anova1 <- aov(model1)
tab <- summary(anova1)

print(summ)
print(tab)

resid <- model1$resid
fits <- model1$fitted
#############Test of Regression Significance
SS <- tab[[1]]["Sum Sq"]
df <- tab[[1]]["Df"]

SSR <- sum(SS[1:3,1])
dfR <- sum(df[1:3,1])

SSE <- SS[4,1]
dfE <- df[4,1]

MSR <- SSR/dfR
MSE <- SSE/dfE

F.stat <- MSR/MSE
f.crit <- qf(0.05, dfR, dfE, lower.tail=FALSE)

if(F.stat > f.crit){
	print("Reject H0")
}else{
	print("Do Not Reject H0")
}
##############Residual Plots#############
pdf.options()
pdf("ResidualPlotsProb32.pdf")
par(mfrow=c(3,2))
plot(fits, resid, xlab="Fitted Values", ylab="Residuals", main="Residuals vs Fits")
abline(a=0,b=0)
plot(x1, resid, xlab="Silica Level", ylab="Residuals", main="Residuals vs Silica Level")
abline(a=0, b=0)
plot(x2, resid, xlab="Saline Level", ylab="Residuals", main="Residuals vs Saline Level")
abline(a=0,b=0)


plot(x3, resid, xlab="Sulfur Level", ylab="Residuals", main="Residuals vs Sulfur Level")
abline(a=0,b=0)
qqnorm(resid, main="Normal Probability Plot of the Residuals")
dev.off()


##########Lack of Fit Test
ybar <- c(102,120,117,198,103,132,132,139,rep(139.1667, 6))
SSPE <- sum((y-ybar)^2)
SSLOF <- sum((ybar-fits)^2)

dfPE <- 5
dfLOF <- 5


MSPE <- SSPE/dfPE
MSLOF <- SSLOF/dfLOF

F.stat <- MSLOF/MSPE
crit.F <- qf(0.05, dfLOF, dfPE, lower.tail=FALSE)
if(F.stat > crit.F){
	print("Reject H0")
}else{
	print("Do Not Reject H0")
}