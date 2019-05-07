### plot

# NOT RUN {

require(stats) # for lowess, rpois, rnorm

plot(cars)
str(cars)
lines(lowess(cars))

plot(sin, -pi, 2*pi) # see ?plot.function

## Discrete Distribution Plot:

plot(table(rpois(100, 5)), type = "h", col = "red", lwd = 10,
     main = "rpois(100, lambda = 5)")

## Simple quantiles/ECDF, see ecdf() {library(stats)} for a better one:

plot(x <- sort(rnorm(10)), type = "s", main = "plot(x, type = \"s\")")

points(x, cex = .9, col = "dark red")

##

plot(1:100, (1:100) ^ 2, main = "plot(1:100, (1:100) ^ 2)")

## box plot 

library(ggplot2)

ToothGrowth$dose <- as.factor(ToothGrowth$dose)
head(ToothGrowth)

# Basic box plot

p <- ggplot(ToothGrowth, aes(x=dose, y=len)) + geom_boxplot()
p
# Rotate the box plot
p + coord_flip()

# Notched box plot
ggplot(ToothGrowth, aes(x=dose, y=len)) + geom_boxplot(notch=TRUE)
# Change outlier, color, shape and size

ggplot(ToothGrowth, aes(x=dose, y=len)) + geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=4)

##

x <- c(1:100)

quantile(x)  #  1.00  25.75  50.50  75.25 100.00 

x <- c(0:100) # 0   25   50   75  100 

summary(x)
sd(x)
var(x)

boxplot(x)

x <- append(x, 150)

boxplot(x, outline = F)

? boxplot

y <- rnorm(100)
y

y <- sample(1:100, 50, replace=T)
y
boxplot(y)
identify(rep(1, length(y)), y, labels = seq_along(y))

plot(y)

## dataframe

y <- data.frame("v1" = c(1, 2, 3, 4, 5),
                     "v2" = c(24, 25, 42, 56, 22), "v3"=c(11:15))
boxplot(v3 ~ v1, y)

plot(v3 ~ v1,y)

plot(data=y, v3~v1, type = "c", lty = 1, lwd = 1, col = "blue")  

?plot$type

# 

x <- seq(0.5, 1.5, 0.25)
y <- rep(1, length(x))
plot(x, y, type="n")

points(x, y, type="b")
points(x, y, type="o", lty=3)
points(x, y, type="h")

# 
# 1. Create some variables
x <- 1:10
y1 <- x*x
y2  <- 2*y1

# 2. Plot a first line
plot(x, y1, type = "b", frame = FALSE, pch = 19,
     col = "red", xlab = "x", ylab = "y", 
     lty = 1, lwd = 1)

# 3. Add a second line
lines(x, y2, pch = 18, col = "blue", type = "b", 
      lty = 2, lwd = 1)

# 4. Add a legend to the plot and set legend lty
legend("topleft", legend = c("Line 1", "Line 2"),
       col = c("red", "blue"), lty = 1:2, cex = 0.8)



## 

y <- data.frame()

x <- c("name", "age", "gender")
colnames(y) <- x

#

df <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("name", "age", "gender")
colnames(df) <- x

# 

sample(state.name, 10)

##

# 1. Create some variables
x <- 1:10
y1 <- x*x
y2  <- 2*y1

# 2. Plot a first line
plot(x, y1, type = "b", frame = FALSE, pch = 19,
     col = "red", xlab = "x", ylab = "y", 
     lty = 1, lwd = 1)

# 3. Add a second line
lines(x, y2, pch = 18, col = "blue", type = "b", 
      lty = 2, lwd = 1)


##
plot(
    1:25, 
    cex  = 3,
    lwd  = 3,
    pch  = 1:25, 
    col  = rainbow(25), 
    bg   = c(rep(NA, 20), terrain.colors(5)),
    main = "plot(1:25, pch = 1:25, ...)"
)


##

plot(
    (1:100) ^ 2, 
    type = "l", 
    main = 'plot((1:100) ^ 2, type = "l")'
)

##

plot(
    (1:100) ^ 2, 
    type = "l", 
    lty  = "dashed",
    lwd  = 3,
    col  = "chocolate", 
    main = 'plot((1:100) ^ 2, type = "l", lty = "dashed", ...)'
)

##

with(
    cars, 
    plot(speed, dist, main = "with(cars, plot(speed, dist))")
)

##

plot(extra ~ group, sleep, main = "plot(extra ~ group, sleep)")

?plot

## sleep 


