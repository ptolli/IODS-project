#IODS, Fall 2018
# Week 3 exercises, 15.11.2018
#Pekka Tölli

library(dplyr)
library(ggplot2)

alc_data1 <- read.table("C:/Users/pekka/Documents/GitHub/IODS-project/Data/alc_data.csv", sep=",", header=TRUE)

#Data wrangling
data1 <- read.table("c:/Users/pekka/Documents/GitHub/IODS-project/Data/student-mat.csv", sep=";", header=TRUE)
dim(data1)
str(data1)
head(data1)

data2 <- read.table("c:/Users/pekka/Documents/GitHub/IODS-project/Data/student-por.csv", sep=";", header=TRUE)
dim(data2)
str(data2)
head(data2)

join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")
math_por <- inner_join(data1, data2, by = join_by, suffix=c(".mat",".por"))

dim(math_por)
str(math_por)
head(math_por)
glimpse(math_por)

colnames(math_por)

alc <- select(math_por, one_of(join_by))
glimpse(alc)
notjoined_columns <- colnames(math)[!colnames(math) %in% join_by]

for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}

glimpse(alc)

#6. 
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)
g1 <- ggplot(data = alc, aes(x = alc_use, fill = sex))
g1 + geom_bar()

alc <- mutate(alc, high_use = alc_use > 2)

g2 <- ggplot(alc, aes(high_use))
g2 + facet_wrap("sex") + geom_bar()


write.csv(alc, file = "alc_data")
alc_data <-read.csv("alc_data")
str(alc_data)
head(alc_data)

