### view data

data(iris)
View(iris)
str(iris)
iris[1:5,"Sepal.Width"]


library(tidyverse)

my_data <- as_tibble(iris)

iris %>% select(1,3)

my_data %>% select(Sepal.Length:Petal.Length)


iris(Sepal.Length,Petal.Length)

iris[3,2]

iris[, 1:3]

iris[1:3, 1]

iris[Sepal.Length,Petal.Length]
