### 1. simple linear regression   2. multi variables 

library(readxl)

myd <- read_excel("work_r/source/8_simple_lr.xls", col_names = TRUE)

fit <- lm( revenue ~ advertise, data=myd)

str(fit)

summary(fit)

## 

anova (fit)
fit$coef
deviance( fit)
effects ( fit )
fitted ( fit )
vcov ( fit )
confint ( fit )






##

plot(revenue ~ advertise, data=myd)

abline(fit,col="blue")


## 2. 

myd <- read_excel("work_r/source/9_multivar_lr.xlsx", col_names = TRUE)

fit <- lm( mj ~ yg + pe + uy, data=myd)

summary(fit)

install.packages("lmtest")
library(lmtest)

dwtest(mj ~ yg + pe + uy, data=myd)



##  Durbin-Watson

## generate regressor and dependent variable
x <- rep(c(-1,1), 50)
y1 <- 1 + x + err1

## perform Durbin-Watson test
dwtest(y1 ~ x)

err2 <- filter(err1, 0.9, method="recursive")
y2 <- 1 + x + err2
dwtest(y2 ~ x)
