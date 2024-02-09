#(a)
library(haven)
data<-read_dta('C:/Users/CHARLEY/Desktop/wbdr.dta')
summary(data, np.rm = TRUE)
mean(data$gnppc, na.rm = TRUE)
#the mean of GNP is 5340.887
sd(data$gnppc, na.rm = TRUE)
#the standard deviation of GNP is 9307.99
mean(data$illit_t, na.rm = TRUE)
#the mean of illiteracy rate is 20.949
sd(data$illit_t, na.rm = TRUE)
#the stand deviation of illiteracy rate is 21.786
mean(data$mort_inf, na.rm = TRUE)
#the mean of mortality rate is 41.169
sd(data$mort_inf, na.rm = TRUE)
#the standard deviation of mortality rate is 38.405
mean(data$mort_5, na.rm = TRUE)
#the mean is 62.486
sd(data$mort_inf, na.rm = TRUE)

#(b)
#data1<-table(data$gnppc77, data$mort_5, dnn = c('GNP1977', 'under 5 mr'))
data1 = data.frame(data$gnppc77, data$mort77, data$mort_5, data$illit_t)
colnames(data1) = c('GNP1977', 'MORT1977', 'MORT_5', 'ILLIT_R')
data1<-na.omit(data1)
data1
data2<-data1[order(data1$GNP1977),]
#data4 is for 50 most rich conturies
data4<-data1[order(data1$GNP1977, decreasing = TRUE),]
data3<-data2[1:50,]
data3
data4<-data4[1:50,]
mean(data3$ILLIT_R)
#mean is 34.5
min(data3$ILLIT_R)
#min is 2
max(data3$ILLIT_R)
#max is 79
mean(data3$MORT1977)
#mean of infant mortality is 99.5
mean(data3$MORT_5)
#mean of infant mortality under 5 is 98.12

data4

mean(data4$MORT1977)
#the mean is 34.2
mean(data4$MORT_5)
#the mean is 17.04
mean(data4$ILLIT_R)
#the mean is 7,96 for illiteracy rate
#we can find that the mortality rate in rich countries is much fewer that that of poor countries
cor(data1$GNP1977, data1$MORT1977)
#the correlation is -0.582
cor(data1$GNP1977, data1$ILLIT_R)
#the correlation is -0.449

#(c)
median(data$gnppcppp, na.rm = TRUE)
mean(data$gnppcppp, na.rm = TRUE)
#THE median is 3775, and the mean is 6534.867, which indicates that the data is positively skewed
hist(data$gnppcppp)
