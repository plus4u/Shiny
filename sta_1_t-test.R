### t-test : 1. one sample, 2. paired  3. 2 maker independent    4. prop.test vs wilcox.test()

## 1. one sample

library(readxl)
x <- read_excel("C:/R/work_r/source/1_ttest.xls", col_names = TRUE)

str(x)


t.test(x$liter, mu=250)

# plot

hist(x$liter)

plot(t.test(x$liter, mu=250))

#

xp <- x$liter 

h <- hist(xp, breaks=10, col="red", xlab="Liter", 
        main="Histogram with liter") 

xfit<-seq(min(xp),max(xp),length=300)

yfit<-dnorm(xfit,mean=mean(xp),sd=sd(xp)) 
yfit <- yfit*diff(h$mids[1:2])*length(xp) # consider frequency 

lines(xfit, yfit, col="blue", lwd=2)


#

yfit<-dnorm(xfit,mean=mean(xp),sd=sd(xp)) 
head(yfit)  #  [1] 0.0005919773 0.0006206666 

yfit <- yfit*diff(h$mids[1:2])*length(xp) 
head(yfit) # [1] 1.775932 1.862000 

h$mids[1:2]
length(xp) 
head(yfit)
str(yfit)

# boxplot

boxplot(x$liter)

# 
par(mfrow=c(1,2))    # set the plotting area into a 1*2 array

xp <- x$liter 

h <- hist(xp, breaks=10, col="red", xlab="Liter", 
          main="Histogram with liter") 


boxplot((x$liter), main="Boxplot")

# pie(max.temp, main="Piechart", radius=1)



## 2. paired my_data = myd

myd <- read_excel("work_r/source/2_pttest.xls", col_names = TRUE)


t.test(myd$before, myd$after, var.equal=T, paired=T)   

cor(myd$before, myd$after, method = "pearson")


# Plot weight by group and color by group
str(myd)


# Violin Plots
install.packages("vioplot")
library(vioplot)

x1 <- myd[, 2]
str(x1)

x1$group = "before"
names(x1)<-c("score","group")

str(x1)

x2 <- myd[, 3]
x2$group = "after"
names(x2)<-c("score","group")

x2

x12 <- rbind(x1, x2)

boxplot(score~group,data=x12, main="Score by group", 
        xlab="Groups", ylab="Scores")


## 2-3 vioplot 

x1 <- myd$before
x2 <- myd$after 

vioplot(x1, x2, names=c("before", "after"), col="gold")
title("Violin Plots of scores")

summary(myd)


### below error 

library("ggpubr")

ggboxplot(myd, x = "group", y = "score", color = "group", palette = c("#00AFBB", "#E7B800"),
          order = c("before", "after"), ylab = "Score", xlab = "Groups")

## 3. two independent 

x <- read_excel("work_r/source/3_2ittest.xls", col_names = TRUE)

t.test(x$hour ~ x$maker)   
 
## plot

boxplot(hour~maker,data=x, main="Score by maker", 
        xlab="makers", ylab="hours")



### others 

## 1.   one sample t-test
# t.test(y,mu=3) # Ho: mu=3


a = c(175, 168, 168, 190, 156, 181, 182, 175, 174, 179)
b = c(185, 169, 173, 173, 188, 186, 175, 174, 179, 180)
d = c(177, 169, 172, 190, 160, 180, 185, 179, 174, 181)

c <- a+20

var.test(a,b) # check equal variance  / F = 2.1028, num df = 9, denom df = 9, p-value = 0.2834 = equal

t.test(a, mu=170) # t = 1.6247, df = 9, p-value = 0.1387 


## 2. independent 2-group t-test

 # t.test(y~x) # where y is numeric and x is a binary factor

 # t.test(y1,y2) # where y1 and y2 are numeric

 # F test to compare two variances


t.test(a,b,var.equal=T)  # t = -0.94737, df = 18, p-value = 0.356

t.test(a,b, var.equal=T, alt='two.sided')

t.test(a,c, var.equal=T, alt='less')   # t = -4.7867, df = 18, p-value = 7.381e-05

t.test(a,c, var.equal=T, alt='greater') # t = -4.7867, df = 18, p-value = 0.9999

summary(a)

summary(b)


## 3. paired t-test

# t.test(y1,y2,paired=TRUE) # where y1 & y2 are numeric

t.test(a, d, var.equal=T, paired=T)  # t = -3.2426, df = 9, p-value = 0.01012
 

## 

require(graphics)

t.test(1:10, y = c(7:20))      # P = .00001855
t.test(1:10, y = c(7:20, 200)) # P = .1245    -- NOT significant anymore

## Classical example: Student's sleep data

ls()
data()

summary(sleep)

plot(extra ~ group, data = sleep)
## Traditional interface
with(sleep, t.test(extra[group == 1], extra[group == 2]))
## Formula interface
t.test(extra ~ group, data = sleep)


## datasets :: pressure 
plot(pressure)

plot(pressure ~ temperature, data = pressure)
text(150, 600, "Pressure (mm Hg)\nversus\nTemperature (Celsius)")

str(pressure)



## 4.  independent two population proportions test : prop.test() 

prop <- c(0.30, 0.40) # proportion of events
n <- c(500, 600) # number of trials
x <- prop*n # number of events
x  

prop.test(x = x, # number of events
                n = n, # number of trials
                alternative = c("two.sided"), # alternative = c("two.sided", "less", "greater")
                conf.level = 0.95) # confidence level (= 1- significance level 'alpha')

# 
base  <- c( 200, 200, 200, 200 )
smokers  <- c( 83, 90, 129, 70 )
patients <- c( 86, 93, 136, 82 )

prop.test(smokers, patients)

prop.test(x= patients, n = base)
prop.test(x= smokers, n = patients)

## independent two population proportions test : prop.test()
