####Class 10 BMI504####
setwd("~/Desktop/BMI 504/BMI504-SP18")
###read in datafile 
Ex1<-read.csv("EXA_C09_S03_01.csv")

###Correlation of two variables
Ex1_1<-Ex1
colnames(Ex1_1)<-c("Subject", "Waist_Circumference", "Deep_Abdominal_AT")

##pearson
cor(Ex1_1)

##spearman
cor(Ex1_1, method="spearman")

###Example 1 Simple Linear Regression###

##Exploratory Data analysis: Scatterplot
plot(Ex1_1$Waist_Circumference, Ex1_1$Deep_Abdominal_AT,pch=21,  bg="cyan", main="Waist Circumference (cm) and Deep Abdominal AT of 109 Men", xlab="Waist Circumference (cm)", ylab="Deep Abdominal AT area (cm^2)")

#find the descriptive statistics of Y and X
cbind(min(Ex1_1$Deep_Abdominal_AT), mean(Ex1_1$Deep_Abdominal_AT), sd(Ex1_1$Deep_Abdominal_AT), max(Ex1_1$Deep_Abdominal_AT))
cbind(min(Ex1_1$Waist_Circumference), mean(Ex1_1$Waist_Circumference), sd(Ex1_1$Waist_Circumference), max(Ex1_1$Waist_Circumference))

#Simple Linear Regression Model
Ex1_model=lm(Deep_Abdominal_AT~Waist_Circumference, data=Ex1_1)
summary(Ex1_model)
##confidence intervals for coefficients
confint(Ex1_model)
##Point estimation when x=82
Pt_est=Ex1_model$coefficients[1]+Ex1_model$coefficients[2]*82
plot(Ex1$X, Ex1$Y,pch=21,  bg="cyan", main="Waist Circumference (cm) and Deep Abdominal AT of 109 Men", xlab="Waist Circumference (cm)", ylab="Deep Abdominal AT area (cm^2)")
abline(lm(Y~X, data=Ex1))
points(82, Pt_est, bg="red", pch=21)

##ANOVA table
anova(Ex1_model)
####check residuals
par(mfrow=c(2,2))
plot(Ex1_model)
par(mfrow=c(1,1))
## prediction of an observation not from a new trial
new.pred <- data.frame(Waist_Circumference=82)
predict.lm(Ex1_model, new.pred,interval="confidence" )

####prediction of "new" observation x=92
new.pred <- data.frame(Waist_Circumference=92)
predict.lm(Ex1_model, new.pred,interval="prediction" )


###lack of fit####
Reduced<-lm(Deep_Abdominal_AT~Waist_Circumference, data=Ex1_1)#fit reduced model
Full<-lm(Deep_Abdominal_AT~0+as.factor(Waist_Circumference), data=Ex1_1) #fit full model
anova(Reduced, Full) #get lack-of-fit test

