### subset

x <- data.frame(x=1:5, y=2:6, z=3:7, u=4:8)
x

x1 <- x[ , which(names(x) %in% c("z","u"))]

print(x1)

library(psych) 

alpha(x1)


d <- data.frame(x = letters[1:5],y = runif(5))
d[d[,'x'] == 'c',]