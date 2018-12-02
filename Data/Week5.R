#IODS, Fall 2018
# Week 5 exercises, 2.12.2018
#Pekka Tölli

#Data wrangling

library(dplyr)
library(ggplot2)
library(MASS)

human <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human1.txt", stringsAsFactors = F)

str(human)
dim(human)
summary(human)
head(human)

#We have a data set human with 195 observations across 19 variables. Most of them are numeric continuous (e.g. life expectancy, 
#proportion of females with at least secondary education). A couple of variables are integers, e.g. development index. In addition, there is variable country
#which is character string. 
#It appears the GNI is stored as character string as the comma breaks the numeric value in the R interpretation. Let's change the data to numeric with 
#string manipulation
library(stringr)
human$GNI_ <- str_replace(human$GNI, pattern=",", replace ="")
human$GNI_ <-as.numeric(human$GNI_)
str(human)

keep <- c("Country", "Edu2.FM", "Labo.FM", "Edu.Exp", "Life.Exp", "GNI", "Mat.Mor", "Ado.Birth", "Parli.F")
human <- dplyr::select(human, one_of(keep))

#Let's remove the missing values
complete.cases(human)
data.frame(human[-1], comp = complete.cases(human))
human_ <- filter(human, complete.cases(human))
tail(human_, 55)
str(human_)

#The last 7 rows appear to be regions, not countries. Remove those. 
last <- nrow(human_) - 7
human_ <- human[1:last,]

#Change the row names to country names
rownames(human_) <- human_$Country
human_ <- dplyr::select(human_, -Country)
human <-human_
str(human_)
str(human)

#Done :)