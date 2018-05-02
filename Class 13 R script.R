####Class 13 BMI504####
setwd("~/Desktop/BMI 504/BMI504-SP18")

########Multiple Regression######
Ex2<-read.csv("EXA_C10_S03_01.csv")
par(mfrow=c(1,2))
plot(Ex2$AGE, Ex2$CDA,pch=21,  bg="cyan", main="Age and CDA of 71 Patients", xlab="Age", ylab="CDA")
plot(Ex2$EDLEVEL, Ex2$CDA,pch=21,  bg="darkorange", main="Education Level and CDA of 71 Patients", xlab="Age", ylab="CDA")

#scatterplot matrix
pairs(Ex2)
#descriptive statistics
summary(Ex2)
install.packages("pastecs")
library(pastecs)
stat.desc(Ex2)

##correlation-pearson because function our scatterplots are linear functions
cor(Ex2)

###regression model
Ex2_model<-lm(CDA~AGE+EDLEVEL, data=Ex2)
summary(Ex2_model)
##confidence intervals for coefficients
confint(Ex2_model)
###Sum of squares table
anova(Ex2_model)

#full vs. reduced F-test for getting rid of education level
reduced<-lm(CDA~AGE, data=Ex2)
full<-lm(CDA~AGE+EDLEVEL, data=Ex2)
anova(reduced, full) #compare the models

####residuals
par(mfrow=c(2,2))
plot(Ex2_model)

###partial correlation
install.packages("ppcor")
library(ppcor)
#partial correlations
pcor(Ex2)
pcor.test(Ex2$CDA, Ex2$AGE,Ex2$EDLEVEL)
##check
ry1=cor(Ex2, method="pearson")[3,1] #age and cda
ry2=cor(Ex2, method="pearson")[3,2] #Education level and cda
r12=cor(Ex2, method="pearson")[2,1] #age and education level
r_y1.2=(ry1-(ry2*r12))/sqrt((1-(ry2)^2)*((1-(r12)^2)))
r_y1.2

###example with binary predictors
Ex3=read.table("birthsmokers.txt", sep="", header=T)

Ex3$Smoke_r[Ex3$Smoke=="yes"]<-1
Ex3$Smoke_r[Ex3$Smoke=="no"]<-0

Ex3_model=lm(Wgt~Gest+Smoke, data=Ex3)
summary(Ex3_model)
anova(Ex3_model)
require(ggplot2)

ggplot(Ex3, aes(Gest, Wgt,colour=Smoke_r)) + 
  geom_abline(intercept=-2389.575-244.544, slope=143.1, colour) + geom_abline(intercept=-2389.575, slope=143.1, colour)+
  geom_point()

#######evaluate collinearity
## Evaluate Collinearity
#vif(fit) # variance inflation factors 
#sqrt(vif(fit)) > 2 # problem?

###mallow's cp
#install.packages("leaps")
#library(leaps)
