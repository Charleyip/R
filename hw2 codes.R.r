library(haven)
data <-read_dta('C:/Users/CHARLEY/Desktop/attendence2016.dta')
data = na.omit(data)
summary(data)
head(data)
str(data)
as.data.frame(data)
summary(data)
reg1<-lm(attend~hw1, data = data)
summary(reg1)
reg1
install.packages('lmtest')
library(lmtest)
library(car)
coeftest(reg1, vcov = hccm)
install.packages('jtools')
library(jtools)
install.packages('Rcpp')
library(Rcpp)
summ(reg1)
summ(reg1, robust = 'HC1', confint = TRUE, digits = 3)
library(ggplot2)
ggplot(data, aes(x = hw1, y = attend)) + geom_point() + geom_smooth(method = 'lm')+ggtitle('damn')
boxplot(data$attend)
boxplot(data$hw1)
#eliminate the outliers
Q1 <- quantile(data$attend, .25)
Q3 <- quantile(data$attend, .75)
IQR <- IQR(data$attend)

#only keep rows in dataframe that have values within 1.5*IQR of Q1 and Q3
df <- subset(data, data$attend> (Q1 - 1.5*IQR) & data$attend< (Q3 + 1.5*IQR))
#check it again
boxplot(df$attend)

#the same to hw1 column
Q1 <- quantile(df$hw1, .25)
Q3 <- quantile(df$hw1, .75)
IQR <- IQR(df$hw1)
df <- subset(df, df$hw1> (Q1 - 1.5*IQR) & df$hw1< (Q3 + 1.5*IQR))
boxplot(df$hw1)

#regression 
reg2<-lm(attend~hw1, data = df)
summary(reg2)
summ(reg2, robust = 'HC1', confint = TRUE, digits = 3)

library(dplyr)
df2 = filter(data,attend<200&hw1<300)
reg_new<-lm(df2$attend~df2$hw1, data = df)
summ(reg_new)
install.packages("huxtable")
install.packages("officers")#(different formate document)
install.packages("flextable")
export_summs(reg,robust=TRUE,digits=3,to.file = "docx",file.name="b.docx")
#create a new column called hw1_100
df2$hw1_100 = df2$hw1 / 100
head(df2$hw1_100)
reg3<-lm(df2$attend~df2$hw1_100, data = df2)
summ(reg3, robust = 'HC1', confint = TRUE, digit = 3)


#
reg4<-lm(df2$attend~df2$hw1 + df2$entry_GPA, data = df2)
summ(reg4, robust = 'HC1', confint = TRUE, digit = 3)
head(df2)
df = na.omit(df)
reg5<-lm(attend~black+white, data = df2)
summ(reg5, robust = 'HC1', confint = TRUE, digit = 3)

