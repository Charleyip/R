#Homework 3 
install.packages("tidyverse")
setwd("C:/Users/CHARLEY/Desktop/homework3")
library(haven)
data_CD = read_dta('CollegeDistance.dta')
head(data_CD)
regre1 = lm(data = data_CD, ed~dist+female+black+hispanic+dadcoll+momcoll+tuition)
summ(regre1, robust = TRUE, digit = 4)
regre2 = lm(data = data_CD, log(ed)~dist+female+black+hispanic+dadcoll+momcoll+log(tuition))
summ(regre2, robust = TRUE, digit = 4)
#check for omitted variable bias
regre3 = lm(data = data_CD, tuition~momcoll+dadcoll+hispanic+black+female)
summ(regre3, robust = TRUE, digit = 4)
#1test
regre_test1 = lm(data = data_CD, tuition~urban)
summ(regre_test1 , robust = TRUE, digit = 4)
#2test
regre_test2 = lm(data = data_CD, tuition~stwmfg80)
summ(regre_test2 , robust = TRUE, digit = 4)
#3test
regre_test3 = lm(data = data_CD, tuition~stwmfg80)
summ(regre_test3 , robust = TRUE, digit = 4)

#(e)We should do interactions 
regre_inter1 = lm(data = data_CD, ed~(black*dadcoll)+(black*momcoll))
linearHypothesis(regre_inter1, c('momcoll', 'black:momcoll = 0'),white.adjust = 'hc1')
linearHypothesis(regre_inter1, c('dadcoll', 'black:dadcoll = 0'),white.adjust = 'hc1')

#p is not significant








#3.2 
data_ISU = read_dta('Insurance.dta')
summary(data_ISU)
data_ISU$age_2 = data_ISU$age^2
data_ISU$age_3 = data_ISU$age^3
library(car)
library(jtools)
install.packages("stargazer")
library(stargazer)
lpm<-lm(insured~selfemp + age +age_2 +deg_ged +deg_hs +deg_ba+ deg_ma +deg_phd +deg_oth+ race_wht+ race_ot+ reg_ne+ reg_so+ reg_we+ male+ married, data = data_ISU)
summ(lpm, robust = 'HC1', digit = 4)
lpm1 = lm(insured~selfemp + age, data = data_ISU)
lpm2 = lm(insured~selfemp + age +age_2, data = data_ISU)
lpm3 = lm(insured~selfemp + age +age_2 + deg_ged +deg_hs +deg_ba+ deg_ma +deg_phd +deg_oth ,data = data_ISU)
lpm4 = lm(insured~selfemp + age +age_2 +deg_ged +deg_hs +deg_ba+ deg_ma +deg_phd +deg_oth+ race_wht+ race_ot, data = data_ISU)
lpm5 = lm(insured~selfemp + age +age_2 +deg_ged +deg_hs +deg_ba+ deg_ma +deg_phd +deg_oth+ race_wht+ race_ot+ reg_ne+ reg_so+ reg_we, data = data_ISU)

library("jtools")
install.packages("huxtable")
library(huxtable)
install.packages("officers")
library(officer)#(different formate document)
install.packages("flextable")
library(flextable)
export_summs(lpm1,lpm2,lpm3,lpm4,lpm5, lpm, robust=TRUE,digits=3,to.file = "xlsx",file.name="table2.xlsx")












lpm_shit = lm(insured~age+age_2+age_3, data = data_ISU)
summ(lpm1, robust = TRUE, digit = 4)
#we do F test and we find that it should be nonlinear
linearHypothesis(lpm1, c('age_2 = 0', 'age_3 = 0' ), white.adjust = 'hc1')

#b probit model
probit = glm(insured~selfemp + age +age_2 +deg_ged +deg_hs +deg_ba+ deg_ma +deg_phd +deg_oth+ race_wht+ race_ot+ reg_ne+ reg_so+ reg_we+ male+ married, data = data_ISU,family = binomial(link = probit))
probit1 = glm(insured~selfemp + age, data = data_ISU,family = binomial(link = probit))
probit2 = glm(insured~selfemp + age +age_2, data = data_ISU,family = binomial(link = probit))
probit3 = glm(insured~selfemp + age +age_2 + deg_ged +deg_hs +deg_ba+ deg_ma +deg_phd +deg_oth ,data = data_ISU,family = binomial(link = probit))
probit4 = glm(insured~selfemp + age +age_2 +deg_ged +deg_hs +deg_ba+ deg_ma +deg_phd +deg_oth+ race_wht+ race_ot, data = data_ISU,family = binomial(link = probit))
probit5 = glm(insured~selfemp + age +age_2 +deg_ged +deg_hs +deg_ba+ deg_ma +deg_phd +deg_oth+ race_wht+ race_ot+ reg_ne+ reg_so+ reg_we, data = data_ISU,family = binomial(link = probit))
export_summs(probit1,probit2,probit3,probit4,probit5, probit, robust=TRUE,digits=4,to.file = "xlsx",file.name="table3.xlsx")
#c
probit6 = glm(insured~selfemp + age +deg_ged +deg_hs +deg_ba+ deg_ma +deg_phd +deg_oth+ race_wht+ race_ot+ reg_ne+ reg_so+ reg_we+ male+ married, data = data_ISU,family = binomial(link = probit))

probit7 = glm(insured~selfemp + age, data = data_ISU,family = binomial(link = probit))
probit8 = glm(insured~selfemp + age + deg_ged +deg_hs +deg_ba+ deg_ma +deg_phd +deg_oth ,data = data_ISU,family = binomial(link = probit))
probit9 = glm(insured~selfemp + age +deg_ged +deg_hs +deg_ba+ deg_ma +deg_phd +deg_oth+ race_wht+ race_ot, data = data_ISU,family = binomial(link = probit))
probit10 = glm(insured~selfemp + age +deg_ged +deg_hs +deg_ba+ deg_ma +deg_phd +deg_oth+ race_wht+ race_ot+ reg_ne+ reg_so+ reg_we, data = data_ISU,family = binomial(link = probit))
export_summs(probit7,probit8,probit9,probit10,probit6, robust=TRUE,digits=4,to.file = "xlsx",file.name="table4.xlsx")

#logit
logit = glm(insured~selfemp + age +deg_ged +deg_hs +deg_ba+ deg_ma +deg_phd +deg_oth+ race_wht+ race_ot+ reg_ne+ reg_so+ reg_we+ male+ married, data = data_ISU,family = binomial(link = logit))

logit1 = glm(insured~selfemp + age, data = data_ISU,family = binomial(link = logit))
logit2 = glm(insured~selfemp + age + deg_ged +deg_hs +deg_ba+ deg_ma +deg_phd +deg_oth ,data = data_ISU,family = binomial(link = logit))
logit3 = glm(insured~selfemp + age +deg_ged +deg_hs +deg_ba+ deg_ma +deg_phd +deg_oth+ race_wht+ race_ot, data = data_ISU,family = binomial(link = logit))
logit4 = glm(insured~selfemp + age +deg_ged +deg_hs +deg_ba+ deg_ma +deg_phd +deg_oth+ race_wht+ race_ot+ reg_ne+ reg_so+ reg_we, data = data_ISU,family = binomial(link = logit))
logit_iter1 = glm(data = data_ISU, insured~selfemp + age  +deg_ged +deg_hs +deg_ba+ deg_ma +deg_phd +deg_oth+ race_wht+ race_ot+ reg_ne+ reg_so+ reg_we+ male+ married + (selfemp*married), family = binomial(link = logit))
export_summs(logit1,logit2,logit3,logit4,logit,logit_iter1, robust=TRUE,digits=4,to.file = "xlsx",file.name="table_inter1.xlsx")

linearHypothesis(logit_iter1, c('married', 'selfemp:married' ), white.adjust = 'hc1')

export_summs(logit1,logit2,logit3,logit4,logit, robust=TRUE,digits=4,to.file = "xlsx",file.name="table_5.xlsx")
#
lpm100<-lm(insured~selfemp + age  +deg_ged +deg_hs +deg_ba+ deg_ma +deg_phd +deg_oth+ race_wht+ race_ot+ reg_ne+ reg_so+ reg_we+ male+ married, data = data_ISU)
summ(lpm, robust = 'HC1', digit = 4)
lpm101 = lm(insured~selfemp + age, data = data_ISU)

lpm102 = lm(insured~selfemp + age  + deg_ged +deg_hs +deg_ba+ deg_ma +deg_phd +deg_oth ,data = data_ISU)
lpm103= lm(insured~selfemp + age + deg_ged +deg_hs +deg_ba+ deg_ma +deg_phd +deg_oth+ race_wht+ race_ot, data = data_ISU)
lpm104 = lm(insured~selfemp + age +deg_ged +deg_hs +deg_ba+ deg_ma +deg_phd +deg_oth+ race_wht+ race_ot+ reg_ne+ reg_so+ reg_we, data = data_ISU)

export_summs(lpm101,lpm102,lpm103,lpm104,lpm100,robust=TRUE,digits=4,to.file = "xlsx",file.name="table7.xlsx")

lpm_iter1 = lm(data = data_ISU, insured~selfemp + age  +deg_ged +deg_hs +deg_ba+ deg_ma +deg_phd +deg_oth+ race_wht+ race_ot+ reg_ne+ reg_so+ reg_we+ male+ married + (selfemp*married))
export_summs(lpm101,lpm102,lpm103,lpm104,lpm100,lpm_iter1,robust=TRUE,digits=4,to.file = "xlsx",file.name="table8.xlsx")
linearHypothesis(lpm_iter1, c('married', 'selfemp:married' ), white.adjust = 'hc1')

