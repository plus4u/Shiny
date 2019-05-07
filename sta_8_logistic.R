### logistic regression    

library(readxl)

myd <- read_excel("work_r/source/10_logistic.xls", col_names = TRUE)

# data cleansing : norminal
library(dplyr)

myd2 <- myd %>% select(gume, location, edu)
str(myd2)

myd2$location <- as.factor(myd$location)
myd2$edu <- as.factor(myd$edu)
myd2$gume <- as.factor(myd$gume)

head(myd2)

# gume 0, 1

myd2$gume <- ifelse (myd2$gume == 1 , 0, 1)
myd2$gume <- as.factor(myd2$gume)


str(myd2)

summary(myd2)


fit <- glm( gume ~ location + edu, data=myd2, family = "binomial")

str(fit)

summary(fit) 


##

# contingency table : xtabs(~ x + y, data) 

xtabs(~ location + edu + gume, data=myd2)
 