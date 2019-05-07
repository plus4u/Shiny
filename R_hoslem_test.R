### hoslem.test


library(readxl)

# x <- read_excel("C:/R/work_r/source/1_ttest.xls", col_names = TRUE)

x <- read.csv("C:/R/work_r/source/10_logistic_2.csv") 

y <- x$gume
x1 <- x$location 
x2 <- x$edu 

# install.packages("ResourceSelection")
library(ResourceSelection)


m <- glm(y ~ x1+x2, family=binomial)

# hoslem.test(x, y, g = 10)  / x a numeric vector of observations, binary (0/1) # y expected values.
# g = number of bins to use to calculate quantiles.


hoslem.test(m$y, fitted(m), g=10) # p-value = 0.9995

hoslem.test(y, fitted(m), g=10) 

fitted(m)

head(x1)
x1
y

##  
m <- glm(y ~ x1, family=binomial)

hoslem.test(y, fitted(m), g=10) #  

str(m)

?hoslem.test
## hoslem.test(x1, y, g = 10)  ## p value error 
 