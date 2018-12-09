#IODS, Fall 2018
# Week 6 exercises, 7.12.2018
#Pekka Tölli

#Data wrangling

library(dplyr)
library(ggplot2)
library(tidyr)

BPRS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt", sep  =" ", header = T)
RATS <- read.table("https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt", sep  ="\t", header = T)

str(BPRS)
dim(BPRS)
summary(BPRS)
head(BPRS)

#In BPRS we have a data set of 41 observations across 11 variables containing measure of mental well-being of individuals. 
#Variables include: type of treatment, person id and 9 time related variables, one for each week from 0 to 8. 

str(RATS)
dim(RATS)
summary(RATS)
head(RATS)

#In RATS there are 16 observations across 13 variables with rat weight logs across weeks. 
#Variables include ID of a rat, group the rat belongs and 11 time-related variables.

#Let's change the category variables to factors to enable conversion to long form:
BPRS$treatment <- factor(BPRS$treatment)
BPRS$subject <- factor(BPRS$subject)

RATS$Group <- factor(RATS$Group)
RATS$ID <- factor(RATS$ID)

# Now, the conversion of data from wide form to long form 
BPRSL <-  BPRS %>% gather(key = weeks, value = bprs, -treatment, -subject)
BPRSL <-  BPRSL %>% mutate(week = as.integer(substr(BPRSL$weeks, 5, 5)))

RATSL <- RATS %>%
  gather(key = WD, value = Weight, -ID, -Group) %>%
  mutate(Time = as.integer(substr(WD,3,4))) 


# Look at the new data 
str(BPRSL)
glimpse(BPRSL)
head(BPRSL)
summary(BPRSL)

str(RATSL)
glimpse(RATSL)
head(RATSL)
summary(RATSL)

#Commentary of differences between wide and long form: In the wide form, each observation is stored as own row with 
#separate variables for each point of time. E.g. in BPRS there is one row for each invididual and the data is stored in 11 variables, of which 9
#represent the point of measurement (week). Comparably, in RATS, there is one row per observed rat and 11 variables for individual measurement of weight. 

#In BPRS wide form we have a data set of 41 observations across 11 variables containing measure of mental well-being of individuals. 
#Variables include: type of treatment, person id and 9 time related variables, one for each week from 0 to 8. 

#In RATS wide form there are 16 observations across 13 variables with rat weight logs across weeks. 
#Variables include ID of a rat, group the rat belongs and 11 time-related variables.

#The long form effectively reduces the number of columns (variables) and increases rows. For example, in BPRSL, the mentioned 9 time-related variables are
# now in the rows. Two first variables of BPRSL are factors and define observations to different categories. 
#The other 3 have week number as character, bprs value and the week number as integer. 

# The benefit of long form data is that we can easily collect observations for a specific week but also for specific individuals to conduct a longitudinal analysis.
# Furthermore, e.g. ANOVA, is easier to run. 

#The long format of BPRS (BPRSL) has 360 rows and 5 columns i.e. variables.
#The long format of RATS (RATSL) has 176 rows and 5 columns i.e. variables.

# Save data:
write.table(RATSL, file = "rats.txt", sep = "\t")
write.table(BPRSL, file = "bprs.txt", sep = "\t")