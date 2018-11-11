#IODS, Fall 2018
# Week 2 exercises, 6.11.2018
#Pekka Tölli

library(dplyr)

#Data wrangling
#The dataset of this week exercise contains 183 observations across 60 variables. The variables are questions of different kinds + a bunch of demographic/background variables such as age and gender
data1 <- read.table("http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt", sep="\t", header=TRUE)
dim(data1)
str(data1)
head(data1)

#gender, age, attitude, deep, stra, surf and points
deep_questions <- c("D03", "D11", "D19", "D27", "D07", "D14", "D22", "D30","D06",  "D15", "D23", "D31")
deep_columns <- select(data1, one_of(deep_questions))
data1$deep <- rowMeans(deep_columns)

surface_questions <- c("SU02","SU10","SU18","SU26", "SU05","SU13","SU21","SU29","SU08","SU16","SU24","SU32")
surface_columns <- select(data1, one_of(surface_questions))
data1$surf <- rowMeans(surface_columns)

strategic_questions <- c("ST01","ST09","ST17","ST25","ST04","ST12","ST20","ST28")
strategic_columns <- select(data1, one_of(strategic_questions))
data1$stra <- rowMeans(strategic_columns)

keep_columns <- c("gender","Age","Attitude", "deep", "stra", "surf", "Points")
data2<- select(lrn14, one_of(keep_columns))
str(data2)
data2<-filter(data2, points > 0)
str(data2)
head(data2)

write.csv(data2, file = "data2")
read.csv("data2")
str(data2)
head(data2)




#____________________________________________________
#Analysis
read.csv("data2")
str(data2)
head(data2)

#data2 consists of 166 observations of students answers across 7 variables, originally with 1-5 likert scale (excluding demographics and points). 
#Variables include demograhpics such as gender and age as well as other variables measuring studying attitude, learning strategies and points from the exercises and exam. 

#2
install.packages("ggplot2")
install.packages("GGally")
library("ggplot2")
library("GGally")
plot(data2)


summary(data2$Age)
summary(data2$Attitude)
#boxplot(data2$Attitude)
summary(data2$deep)
#boxplot(data2$deep)
summary(data2$stra)
#boxplot(data2$stra)
summary(data2$surf)
#boxplot(data2$surf)
summary(data2$Points)
#boxplot(data2$Points)

par(mfrow = c(2,2))
plot(data2$Attitude, data2$points, main="Attitude vs points", xlab="Attitude", ylab="Points")
plot(data2$deep, data2$points, main="Deep learning vs points", xlab="Deep", ylab="Points")
plot(data2$surf, data2$points, main="Surface approach vs points", xlab="Surface", ylab="Points")
plot(data2$surf, data2$points, main="Strategic approach vs points", xlab="Strategic", ylab="Points")


pairs(data2[-1], col = data2$gender)
p <- ggpairs(data2, mapping = aes(col = gender, alpha = 0.3), lower = list(combo = wrap("facethist", bins = 20)))
p

#Based on the graphical overview it appears: average points are around 25 for both genders. Participants did use by large 
#the deep learning strategy, to some extent strategic approach. 
# Points are most correlated with attitude and to some extent strategic approach. 

data2

#3 Regession model
model1 <- lm(Points ~ Attitude + stra + surf , data = data2)
summary(model1)
#The model is a good start. The statistical test gives us an overview to the model: coefficient by explanatory variable, 
#the significance of each variable, std. error by variable and total correlation coefficient / r-squared  
#the model explains ~13% of the (adj. r-squared). It appears the Attitude has the highest significanse, with *** p-value
#(<0.001). 
# variable surf does not have statistically significant relationship with the dependent variable, so let's omit that. and create a new model

model2 <- lm(Points ~ Attitude + stra , data = data2)
summary(model2)


#4 - interpretation

#based on the model2, it appears that studying attitude and strategic approach explain around 13.5% of the variation of points. 
#Based on the model, the points can be estimated by having 0.38 times attitude + 1.934 times the strategic approach scores + 2.67 (intercept)


#5 - validation

par(mfrow = c(2,2))
plot(model2, which = c(1,2,5))

#The model assumes linear relationship between the points and the attitude and strategic approach. In addition, it assumes 
#that the residual errors are normally distributed and not correlated, and they have constant variance. 
#Based on Normal Q-Q plot it looks the normality assumption holds well in the middle of the quantiles 
#but at the bottom quintiles it is questionable. 
#Variance constancy is not either a clear cut as it looks there is something fishy with the deeply negative residuals
#Leverage seems good, no outliers with high leverage appear