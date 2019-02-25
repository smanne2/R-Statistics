rm(list=ls())
library("psych")

##Problem 1a and Problem 1b
P1<-read.clipboard()
P1$MixingTechnique <-factor(P1$MixingTechnique)
str(P1)
P1anova <- aov(Strength~MixingTechnique, data=P1)
summary(P1anova)

##Problem 1c
P1_lm<-lm(Strength ~MixingTechnique,data=P1)
summary(P1_lm)

P1residuals = P1_lm$residuals
qqnorm(P1residuals) 
qqline(P1residuals)


pnorm(5.6055)
pnorm(0.1065 + 5.499e-3*1000)

##Problem 2a
library("ISLR")
str(Default)
attach(Default)
glm1 = glm(default~balance, family= binomial)
summary(glm(default~balance, family= binomial))

##Problem 2b

#Problem 2c
p2c = data.frame(balance=1000)
predict(glm1,p2c , type = 'response' )
0.005752145

#Problem 2d
p2d = data.frame(balance=2000)
predict(glm1,p2d , type = 'response' )
0.5857694 

#Problem 2e
glm2e = glm(default~student, family= binomial)
summary(glm2e)

#Problem 2f
#Problem 2g
p2g = data.frame( student = "Yes")
p2gpred = predict(glm2e,p2g , type = 'response' )
p2gpred
0.04313859 

#Problem 2h
p2h = data.frame( student = "No")
p2hpred = predict(glm2e,p2h , type = 'response' )
p2hpred
0.02919501

#Problem 2i
glm2i = glm(default~balance + income + student, family= binomial)
summary(glm2i)

#Problem 2k
p2k = data.frame(student = "Yes", balance= 1500, income = 40000)
p2kpred = predict(glm2i,p2k , type = 'response')
p2kpred
0.05788194

#Problem 2l
p2l = data.frame(student = "No", balance= 1500, income = 40000)
p2lpred = predict(glm2i,p2l , type = 'response')
p2kpred
0.1049919 
