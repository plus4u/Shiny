### 1. prcomp   2. principal  3. factanal 
# https://www.datacamp.com/community/tutorials/pca-analysis-r 


## 1.  prcomp(x, scale = FALSE)  /  princomp(x, cor = FALSE, scores = TRUE)
# princomp : spectral decomposition approach (EVD)   / cor =F, scorese = T 
# principal (x1,  n, rotate = "Varimax", score=T,   method = "regression")
# fit <- factanal(mydata, 3, rotation="varimax")


myd <- read_excel("work_r/source/6_pca.xls", col_names = TRUE)

str(myd)

library(dplyr)

myd <- myd %>% select(2 : 16) 

head(myd)

## factanal

fit <- factanal(myd, 5, rotation="varimax")

print(fit, digits=2, cutoff=.3, sort=TRUE)

# plot factor 1 by factor 2 
load <- fit$loadings[,1:2] 
plot(load,type="n") # set up plot 
text(load,labels=names(myd),cex=.7) # add variable names


## principal

fit <- psych::principal(myd, rotate="varimax", nfactors=5, scores=TRUE)

print(fit$scores[1:5,])  # Scores returned by principal()


## principal(r, nfactors = 1, residuals = FALSE,rotate="varimax",n.obs=NA, covar=FALSE, scores=TRUE,missing=FALSE,impute="median",oblique.scores=TRUE,method="regression",...) 

library(psych)

fit <- principal(myd, nfactors = 5, residuals = FALSE,rotate="oblimin", covar=FALSE,
scores=TRUE, oblique.scores=TRUE,method="regression")

summary(fit)
print(fit$scores[1:5,])

# prcomp(x, retx = TRUE, center = TRUE, scale. = FALSE,  tol = NULL, rank. = NULL, ...)

fit <- prcomp(myd, retx = TRUE, center = TRUE, scale. = FALSE)

summary(fit)

fit <- principal(myd, rotate = "Varimax", score=T,   method = "regression")









## 1. prcomp 

str(mtcars)
head(mtcars)

mtcars_pca <- mtcars[, c(1:7,10,11)]

head(mtcars_pca)

mtcars_pca <- prcomp(mtcars[,c(1:7,10,11)], center = TRUE,scale. = TRUE)

summary(mtcars_pca)
 
##


##


