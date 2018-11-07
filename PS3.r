
##4
c=read.table("vote1.txt",na.string=".")
#attach(c4.1)

##(iii)
voteA=c[[4]]
lexpendA=c[[8]]
lexpendB=c[[9]]
prtystrA=c[[7]]
model1=lm(voteA~lexpendA+lexpendB+prtystrA)
summary(model1)
#calculate the t statistic for Ho: beta.lexpendA=0 by extracting betahat.lexpendA and se(betahat.lexpendA)
betahat.lexpendA=coef(model1)[2]## betahat.lexpendA is the second element of coef(model1)
se.betahat.lexpendA=sqrt(vcov(model1)[2,2]) ## standard error of betahat.lexpendA
t.betahat.lexpendA=betahat.lexpendA/se.betahat.lexpendA  ## t-ratio for Ho: beta.lexpendA=0  
t.betahat.lexpendA
df=length(voteA)-4 ## df=n-4 
p.valueA=2*(1-pt(t.betahat.lexpendA,df)) ## calculating the p-value
p.valueA
#calculate the t statistic for Ho: beta.lexpendB=0 by extracting betahat.lexpendB and se(betahat.lexpendB)
betahat.lexpendB=coef(model1)[3]## betahat.lexpendB is the third element of coef(model1)
se.betahat.lexpendB=sqrt(vcov(model1)[3,3]) ## standard error of betahat.lexpendB
t.betahat.lexpendB=betahat.lexpendB/se.betahat.lexpendB  ## t-ratio for Ho: beta.lexpendB=0  
t.betahat.lexpendB
p.valueB=2*pt(t.betahat.lexpendB,df) ## calculating the p-value
p.valueB

##(iv)
model2=lm(voteA~lexpendA+I(lexpendB-lexpendA)+prtystrA)
summary(model2)
#calculate the t statistic for Ho: teta1=0 by extracting teta1hat and its se
teta1hat=coef(model2)[2]## teta1hat is the second element of coef(model2)
se.teta1hat=sqrt(vcov(model2)[2,2]) ## standard error of teta1hat
t.ratio=teta1hat/se.teta1hat  ## t-ratio for Ho: teta1=0  
t.ratio
p.value=2*(pt(t.ratio,df)) ## calculating the p-value
p.value
#detach(c4.1)


##5
c4.6=read.table("wage2.txt",na.string=".")
#attach(c4.6)

##(ii)
lwage=V17
educ=V5
exper=V6
tenure=V7
model=lm(lwage~educ+exper+I(exper+tenure))
summary(model)
confint(model,level=0.95)[3,]## 95% confidence interval for coefficient of exper
#detach(c4.6)


##6
ksubs = read.table("401ksubs.txt")
#attach(ksubs)
nettfa=V7
age=V5
inc=V2
fsize=V6

##(i)
n=length(fsize[fsize==1])## number of single-person households
n

##(ii)
nettfa.lm=lm(nettfa~inc+age, subset=(fsize==1))
summary(nettfa.lm)

## or we could do the regression by first restricting the dataframe to observations with fsize=1
ksubs1=subset(ksubs, fsize==1)## keep only the data for families where fsize=1
#detach(ksubs)
#attach(ksubs1)
nettfa=V7
age=V5
inc=V2
nettfa.lm=lm(nettfa~inc+age)
summary(nettfa.lm)

##(iv)
#calculate the t statistic for Ho: beta.age=1 by extracting beta.agehat and its se
beta.agehat=coef(nettfa.lm)[3]## beta.agehat is the third element of coef(nettfa.lm)
se.beta.agehat=sqrt(vcov(nettfa.lm)[3,3]) ## standard error of beta.agehat
t.ratio=(beta.agehat-1)/se.beta.agehat  ## t-ratio for Ho: beta.age=1  
t.ratio
df=n-3
p.value=pt(t.ratio,df) ## calculating the p-value
p.value

##(v)
model=lm(nettfa~inc)
summary(model)
cor(age,inc) ## correlation of age and income
#detach(ksubs1)

