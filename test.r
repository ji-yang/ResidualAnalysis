
#############Tourism Expenditures##########
#############1 Estimating Coefficients#########
tourism <- read.table("Tourism.txt")
y <- tourism$Expenditure
x <- tourism$Income
model1 <- glm(y ~ x)
print(summary(model1))
#############2 Carrying out test for beta.1#####
beta.1 <- model1$coef[2]
SSE <- sum((model1$resid)^2)
n <- length(tourism$Expenditure)
MSE <- SSE/(n-2)
S.xx <- sum((x-mean(x))^2)
t.stat <- beta.1/sqrt(MSE/S.xx)
crit.t <- qt(.975, n-2)
if(abs(t.stat) > crit.t){
  print("Reject H0")
}else{
  print("Do Not Reject H0")
}
#############3 95% confidence interval###########
m.e <- crit.t*sqrt(MSE/S.xx)
ci.low.slope <- beta.1-m.e
ci.up.slope <- beta.1 + m.e
ci.slope <- c(ci.low.slope, ci.up.slope)
print(ci.slope)
beta.0 <- model1$coef[1]
me.int <- m.e*sqrt(sum(x^2)/n)
SE.int <- me.int / crit.t
ci.low.int <- beta.0-me.int
ci.up.int <- beta.0+me.int
ci.int <- c(ci.low.int, ci.up.int)
print(ci.int)
#############4 residual plot######################
res <- model1$residuals
fits <- model1$fitted.values
plot(x, res, ylab="Residuals", xlab="Income", main="Plot of Residuals vs Income")
abline(0,0)
plot(fits, res, xlab="Fitted Values", ylab="Residuals", main="Plot of Residuals vs Fitted Values")
abline(0,0)
qqnorm(res, main="Normal Probability Plot of Residuals")
#############5 lack of fit test###################
ybar <- y
ybar[8] <- (ybar[8]+ybar[11])/2
ybar[11] <- ybar[8]
ybar[3] <- (ybar[3]+ybar[18])/2
ybar[18] <- ybar[3]
dfPE <- 2
dfLOF <- 16
SSPE <- sum((y-ybar)^2)
SSLOF <- sum((ybar-fits)^2)
MSPE <- SSPE/dfPE
MSLOF <- SSLOF/dfLOF
f.stat <- MSLOF/MSPE
crit.f <- qf(0.05, dfLOF, dfPE, lower.tail=FALSE)
if(f.stat > crit.f){
  print("Reject H0")
}else{
  print("Do Not Reject H0")
}
#############6 test of through the origin########
t.stat2 <- beta.0/SE.int
crit.t2 <- qt(.975, n-1)
if(abs(t.stat2) > crit.t2){
  print("Reject H0")
}else{
  print("Do Not Reject H0")
}
model2 <- glm(y ~ 0 + x)
t.stat3 <- summary(model2)$coef[,1]/summary(model2)$coef[,2]
crit.t3 <- qt(.975, n-1)
if(abs(t.stat3) > crit.t3){
  print("Reject H0")
}else{
  print("Do Not Reject H0")
}

##################Health Expenditures##############
#############1 linear model########################
health <- read.table("Healthexp.txt")
y1 <- health$Health.Expenditures
x1 <- health$Internet.Users.per.100
x2 <- health$Expected.Years.of.Education
model3 <- glm(y1 ~ x1 + x2)
print(summary(model3))
#############2 scatter plot########################
plot(x2, x1, ylab="Internet Users per 100 people", xlab="Expected Years of Education")
#############3 significance test###################
anova <- aov(model3)
tab <- summary(anova)
SS <- tab[[1]]["Sum Sq"]
SSR <- SS[1,1] + SS[2,1]
SSE1 <- SS[3,1]
df <- tab[[1]]["Df"]
df.R <- df[1,1] + df[2,1]
df.E <- df[3,1]
F.0 <- (SSR/df.R)/(SSE1/df.E)
crit.F <- qf(0.05, df.R, df.E, lower.tail=FALSE)
if(F.0 > crit.F){
  print("Reject H0")
}else{
  print("Do Not Reject H0")
}
ones <- rep(1, length(y1))
X <- cbind(ones, x1, x2)
inv <- solve(t(X)%*%X)
coeff <- model3$coef
MS.res <- SSE1/df.E
t.stat4 <- coeff[3]/sqrt(MS.res*inv[3,3])
crit.t4 <- qt(.975, 93)
if(abs(t.stat4) > crit.t4){
  print("Reject H0")
}else{
  print("Do Not Reject H0")
}
#############4 95% confidence interval##########
lower.ci.i <- coeff[1] - crit.t4*sqrt(MS.res*inv[1,1])
upper.ci.i <- coeff[1] + crit.t4*sqrt(MS.res*inv[1,1])
ci.i <- c(lower.ci.i, upper.ci.i)
print(ci.i)
lower.ci.x1 <- coeff[2] - crit.t4*sqrt(MS.res*inv[2,2])
upper.ci.x1 <- coeff[2] + crit.t4*sqrt(MS.res*inv[2,2])
ci.x1 <- c(lower.ci.x1, upper.ci.x1)
print(ci.x1)
lower.ci.x2 <- coeff[3] - crit.t4*sqrt(MS.res*inv[3,3])
upper.ci.x2 <- coeff[3] + crit.t4*sqrt(MS.res*inv[3,3])
ci.x2 <- c(lower.ci.x2, upper.ci.x2)
print(ci.x2)
#############5 95% confidence interval############
x.0 <- c(1, 27, 13.5)
y.hat.0 <- t(x.0)%*%coeff
var.y.hat.0 <- MS.res*t(x.0)%*%inv%*%x.0
lower.conf <- y.hat.0 - crit.t4*sqrt(var.y.hat.0)
upper.conf <- y.hat.0 + crit.t4*sqrt(var.y.hat.0)
ci <- c(lower.conf, upper.conf)
print(ci)
#############6 95% predict interval###############
pred.var <- MS.res*(1+t(x.0)%*%inv%*%x.0)
lower.pi <- y.hat.0 - crit.t4*sqrt(pred.var)
upper.pi <- y.hat.0 + crit.t4*sqrt(pred.var)
pi <- c(lower.pi, upper.pi)
print(pi)
#############7 residual plot######################
res2 = model3$residuals
fits2 <- model3$fitted.values
qqnorm(res2, main="Normal Probability Plot of Residuals")
plot(x1, res2, ylab="Residuals", xlab="Internet.Users.per.100", main="Plot of Residuals vs Internet.Users.per.100")
abline(0,0)
plot(x2, res2, ylab="Residuals", xlab="Expected.Years.of.Education", main="Plot of Residuals vs Expected.Years.of.Education")
abline(0,0)
plot(fits2, res2, xlab="Fitted Values", ylab="Residuals", main="Plot of Residuals vs Fitted Values")
abline(0,0)
