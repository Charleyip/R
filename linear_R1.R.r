#setwd('C:/Users/CHARLEY/Desktop/R_T2data')
library(haven)
caschool<-read_dta('C:/Users/CHARLEY/Desktop/caschool.dta')
summary(caschool)
library(ggplot2)
ggplot(caschool, aes(x = str, 
                     y = testscr)) + geom_point() + geom_smooth(method = 'lm')
ggplot(caschool, aes(x = str, 
                     y = testscr)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)+ggtitle('california district data')
ggplot(caschool, aes(x = str, 
                     y = testscr)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)+ggtitle('california district data') + scale_y_continuous(name = 'score', breaks = seq(600, 720, 20), limits = c(600, 720))

#regression
reg<-lm(testscr ~ str, data = caschool)
reg
summary(reg)
coef(reg)
caschool$yhat<-predict(reg)
caschool$uhat<-residuals(reg)
confint(reg, level = 0.99)
caschool$yhat


#heteroskedasticity 
install.packages('lmtest')
install.packages('car')
library(lmtest)
library(car)
coeftest(reg, vcov = hccm)
install.packages('sandwish')
coeftest(reg, vcov = vcovHC(reg, type = 'HC1'))


install.packages('jtools')
library(jtools)
summ(reg)
summ(reg, robust = TRUE, digit = 3)



library(sandwich)
library(lmtest)
cov1<-vcov(reg, type = 'HC1')
robust_se1 <- sqrt(diag(cov1))

