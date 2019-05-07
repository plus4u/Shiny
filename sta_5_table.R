### cross table   chi-square

library(readxl)
myd <- read_excel("work_r/source/7_table_chisquare.xls", col_names = TRUE)

str(myd)

x <- myd[c("location", "gume")]
str(x)
 
table(x)

summary(table(x))
