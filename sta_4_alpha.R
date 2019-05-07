### reliability  / correlation analysis

## 1. alpha

myd <- read_excel("work_r/source/6_pca.xls", col_names = TRUE)

myd <- myd %>% select(2 : 16)   # 15

pairs(myd, panel=panel.smooth)

Q <- myd[, 1 : 3]   # q1 ~ q3

alpha(Q)

a <- alpha(Q)
str(a)

a$total
a$raw_alpha

library(psy)
cronbach(Q)


## 2. cor

# mean q1~q3, q4~q6

myd <- read_excel("work_r/source/6_pca.xls", col_names = TRUE)

myd <- myd %>% select(2 : 16)  

x1 <- rowMeans(myd[, 1:3])
x2 <- rowMeans(myd[, 4:6])

cor(x1,x2)
cor.test(x1,x2)

## 1. data 

Q <- data.frame(
    Q1=c(1,4,2,3,4,2,3,4,3,2), 
    Q2=c(2,4,1,2,4,1,2,5,2,1), 
    Q3=c(2,5,1,3,3,2,3,4,2,2))

pairs(Q, panel=panel.smooth)

# 2. cronbach : install.packages("psy")  /  alpha ()

library(psy)

cronbach(Q)

library(psych)
alpha(Q)

a <- alpha(Q)
str(a)

a$total

View(a$scores)



#### correlation analysis


# 1. 

df <- read.csv(file = "http://goo.gl/HKnl74", header = T)

head(df)
str(df) 


# handling NA

colSums(is.na(df))


attach(df)

plot(overall, rides)  # 1. correlation between overall satisfaction and rides 

cov(overall, rides)   # 2. covariance of the both

cor(overall, rides, use = 'complete.obs', method = 'pearson')   # 3. correlation 

plot(df[,4:8])

cor(df[,4:8])  # 4. 

corrplot(cor(df[,4:8]))  # 5. 

#2.  install.packages("corrplot")

library(corrplot)


## compare excel : rank()  sort()  

rank(sort(x))


a1 <- c(4,5,6,5.5,4.5)
a2 <- c(3,7,8,6.5, 7.5)

m = matrix(c(4,5,6,5.5,4.5,3,7,8,6.5, 7.5), nrow = 5)
m

cor(m, method = 'spearman') # refer to excel 


# 3. general test


a1 <- c(1:6)
a2 <- c(2,3,4, 4, 5,5)
d1 <- data.frame(a1,a2)

cor(d1, method = "pearson")  # excel value = 0.960182909

cor(a1, a2, method = "pearson")

cor.test(a1,a2,conf.level = 0.95, method =c("pearson"))

cov(a1, a2, method = c("pearson")) 
