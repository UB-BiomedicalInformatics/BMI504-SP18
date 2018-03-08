#For questions 1,2,4,5:
#1. Check Assumptions (with things such as plots, visual inspection, knowledge of type of data)
#2. Analyze the data using a specific test
#3. When doing a hypothesis test, state your hypotheses in words and equations (symbols). What is your significance level?
#4. When making Confidence Intervals, state your confidence level and CI should be two-sided.
#5. State a conclusion and interpret your conclusion.
#Note: report both hypothesis test p-value and confidence interval
library(MASS)
#Dataset 1

install.packages("MethComp")
library(MethComp)
scint<-data(scint)
################
#Measurements of the relative kidney function (=renal function) for 111 patients. The percentage of the total renal function present in the left kidney is determined by one reference method, DMSA (static) and by one of two dynamic methods, DTPA or EC.
#Five variables
#meth-Measurement method, a factor with levels DMSA, DTPA, EC.
#item-Patient identification.
#y-Percentage of total kidney function in the left kidney.
#age-Age of the patient.
#sex-Sex of the patient, a factor with levels F, M.

#Question 1: For patients who were measured with the reference standards DMSA and DTPA, was there a difference in total kidney function?

#Question 2: We want to know if there is a difference in the percentage of total kidney function in the left kidney between males and females for the DMSA reference method. Is there a difference in ages for these two groups?
