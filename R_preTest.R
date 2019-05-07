

data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, 
         quote=input$quote)

x <- read.csv("C:/R/work_r/source/3_2_sample.csv", header = TRUE)
x <- read.csv("C:/R/work_r/source/4_oneway_anova.csv", header = TRUE)

names(x)

t2 <- aov(var1 ~ var2, alternative = input$tail, var.equal = ve, conf.level = conf)

str(x)
aov( satisfaction ~ conv, data=x)

fit <- aov( satisfaction ~ conv, data=x)

str(fit)

summary(fit) 

sum_test = unlist(summary(fit))
sum_test
names(sum_test)
sum_test["Pr(>F)1"]

sum_test["F value1"]
 