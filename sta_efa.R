###  error 

# install.packages("devtools")
# install.packages("shiny")

library(devtools)
library(shiny)

devtools::install_github("PsyChiLin/EFAshiny")

remove.packages(Rcpp)

EFAshiny::EFAshiny()

###

remove.packages("EFAshiny") # error
remove.packages("R.matlab")

