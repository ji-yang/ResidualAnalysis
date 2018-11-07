data <- as.matrix(read.table("mileage.txt"))

MPG <- data[,1]
displacement <- data[,2]
barrels <- data[,7]

###########################Fit the model (Part a)######################
model <- glm(MPG~displacement+barrels)
print(summary(model))

########################Test for Significance of the Regression (B)

anova <- aov(model)
tab <- summary(anova)

SS <- tab[[1]]["Sum Sq"]

SSR <- SS[1,1] + SS[2,1]
SSE <- SS[3,1]

df <- tab[[1]]["Df"]
df.R <- df[1,1] + df[2,1]
df.E <- df[3,1]

F.0 <- (SSR/df.R)/(SSE/df.E)

crit.F <- qf(0.05, df.R, df.E, lower.tail=FALSE)
if(F.0 > crit.F){
	print("Reject H0")
}else{
	print("Do Not Reject H0")
}	

#############Confidence Interval (C)##########
ones <- rep(1, length(MPG))
X <- cbind(ones, displacement, barrels)
inv <- solve(t(X)%*%X)
coeff <- model$coef
MS.res <- SSE/df.E
crit.t <- qt(0.025, df.E, lower.tail=FALSE)

lower.ci <- coeff[2] - crit.t*sqrt(MS.res*inv[2,2])
upper.ci <- coeff[2] + crit.t*sqrt(MS.res*inv[2,2])

ci <- c(lower.ci, upper.ci)

##########Tests for beta.1 and beta.6
beta.hat.1 <- coeff[2]
beta.hat.6 <- coeff[3]

t.1 <- beta.hat.1/sqrt(MS.res*inv[2,2])
t.2 <- beta.hat.6/sqrt(MS.res*inv[3,3])

if(abs(t.1) > crit.t){
	print("Reject H01")
}else{
	print("Do Not Reject H01")
}

if (abs(t.2) > crit.t){
	print("Reject H06")
}else{
	print("Do Not Reject H06")
}

###################Mean Response Confidence Interval#######
x.0 <- c(1,275,2)
y.hat.0 <- t(x.0)%*%coeff
var.y.hat.0 <- MS.res*t(x.0)%*%inv%*%x.0
lower.conf <- y.hat.0 - crit.t*sqrt(var.y.hat.0)
upper.conf <- y.hat.0 + crit.t*sqrt(var.y.hat.0)

ci <- c(lower.conf, upper.conf)
print(ci)

##############Prediction Interval########
x.0 <- c(1,257,2)
y.hat.0 <- t(x.0)%*%coeff
pred.var <- MS.res*(1+t(x.0)%*%inv%*%x.0)
lower.pi <- y.hat.0 - crit.t*sqrt(pred.var)
upper.pi <- y.hat.0 + crit.t*sqrt(pred.var)

pi <- c(lower.pi, upper.pi)
print(pi)