### 16 tools

## which 

x <- read.csv('C:/R/work_r/source/data-01-test-score_2.csv')

head(x)

which(x$x1>=90)  # extract index

x[, which(names(x1) %in% c(x1, x2)) ]

head(x1)  # 열의 개수가 0이고 행의 개수가 25인 데이터 프레임입니다.

x1 <- x[ , which(names(x) %in% c("x1", "x2"))]

x <- data()[ , which(names(data()) %in% c(input$var_selected))]

prcomp(x, center = T, scale= T)

### subset 

education <- read.csv("https://vincentarelbundock.github.io/Rdatasets/csv/robustbase/education.csv", stringsAsFactors = FALSE)
colnames(education) <- c("X","State","Region","Urban.Population","Per.Capita.Income","Minor.Population","Education.Expenditures")
View(education)

ed_exp1 <- education[c(10:21),c(2,6:7)]

head(ed_exp1)

ed_exp3 <- education[which(education$Region == 2),names(education) %in% c("State","Minor.Population","Education.Expenditures")]

## as.formula paste

# These are the variable names:

measurevar <- "y"
groupvars  <- c("x1","x2","x3")

# This creates the appropriate string:
paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ ")

# [1] "y ~ x1 + x2 + x3"

## 

# This returns the formula:
as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
#> y ~ x1 + x2 + x3
#> <environment: 0x3361710>


# select variables v1, v2, v3
myvars <- c("v1", "v2", "v3")
str(myvars)

summary(myvars)

newdata <- mydata[myvars]

# another method
myvars <- paste("v", 1:3, sep="")
newdata <- mydata[myvars]

# select 1st and 5th thru 10th variables
newdata <- mydata[c(1,5:10)]


## reliability alpha 2. cronbach : install.packages("psy")  /  alpha ()

Q <- data.frame(
    Q1=c(1,4,2,3,4,2,3,4,3,2), 
    Q2=c(2,4,1,2,4,1,2,5,2,1), 
    Q3=c(2,5,1,3,3,2,3,4,2,2))

pairs(Q, panel=panel.smooth)

library(psy)

cronbach(Q)

?cronbach

library(psych)

x <- (Q$Q1, Q$Q2)

alpha(Q)

a <- alpha(Q)

?alpha

str(a)
a$total



## import

xy <- read.csv("C:/R/work_r/source/data-01-test-score_2.csv", header=T)

y1 <- xy$x1
x1 <- xy$x2

cov(y1, x1)
cor(y1, x1)

## reliability

alpha(xy)

## cov

data("iris")


x1 <- iris$Sepal.Length
y1 <- iris$Sepal.Width

x <- cov(x1, y1)

cor.test(x1, y1)

cov(Sepal.Length, Sepal.Width, data=iris)  # error unused argument 

str(x)

