## replace values in data frame

x1 <- c(1:5)
x2 <- c(1,1,2,2,2)
x <- c("a", "b", "c", "d", "f")
x
x3 <- data.frame(cbind(x1,x2, x))

x3$x2 <- ifelse(x3$x2 == 1 , 0, 1)
x3
str(x3)

##


dat <- data.frame(x=c(1,2), y=c(3,4), z=c(5,6)) 

dat

dat$z <- dat$z * 2

dat$y <- ifelse (dat$y == 4 , 0, 10)


##

library(dplyr)

recode(x3, a=0, b=1)
x3




x=c(1:25)

x[23]="X"
x

x.new = ifelse(x=="X", 23, x)
x.new
x.new=as.numeric(x.new)

##

x <- data.frame(name = letters[1:5],
                 re1 = factor(c("yes","no","yes","no","yes")),
                 re2 = factor(c("yes","no","yes","no","yes")),
                 re3 = factor(c("yes","no","yes","no","yes")))
x

recode2(x, fields = c('re1', 're2'), recodes = "'yes' = 1; 'no' = 2", as.factor.result = TRUE)




## 
x1=c(1, 1, 1)
x2 = c(2, 2, 2)

x3 = cbind(x1,x2)
x3

x3$x1 = ifelse(x1=="1", 0, x1)
x3$x2 = ifelse(x2=="2", 1, x2)

x3

x.new=as.numeric(x.new)