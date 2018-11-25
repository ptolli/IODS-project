#IODS, Fall 2018
# Week 4 exercises, 23.11.2018
#Pekka Tölli

#Data wrangling

library(dplyr)
library(ggplot2)
library(MASS)

hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

str(hd)
dim(hd)
summary(hd)
head(hd)

str(gii)
dim(gii)
summary(gii)


#In hd there are 195 observations across 8 variables. 
#In gii there are 195 observations across 10 variables. 

names(hd) <- c("HDI_Rank", "Country", "HDI", "Life_exp", "Education_exp", "Education_mean", "GNI", "GNI_HDI_rank")
names(gii) <- c("GII_rank", "Country", "GII", "Maternal_mortality", "A_Birth_rate", "share_in_parliament", "F_Second_Edu", "M_Second_Edu", "F_labor_force", "M_labor_force")
gii$edu2ratio <- gii$F_Second_Edu/gii$M_Second_Edu
gii$labor_ratio <- gii$F_labor_force/gii$M_labor_force
head(gii)


join_by <- c("Country")
human <- inner_join(hd, gii, by = join_by, suffix=c(".hd",".gii"))
head(human)
str(human)