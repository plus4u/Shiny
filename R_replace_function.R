

##

ize = 
    function (d, columns = names(d), izer = as.factor) 
    { 
        d[columns] = lapply(d[columns], izer) 
        d 
    } 
d = data.frame(x=1:10, y=1:10, z =1:10) 

str( ize(d, 'z') ) # y is now a factor 
str( ize(d, 1:2, `cumsum`) ) # x and y are affected 

d

## function 

recode2 <- function ( data, fields, recodes, as.factor.result = FALSE ) {
    for ( i in which(names(data) %in% fields) ) { # iterate over column indexes that are present in the passed dataframe that are also included in the fields list
        data[,i] <- car::recode( data[,i], recodes, as.factor.result = as.factor.result )
    }
    data
}

dam <- data.frame(name = letters[1:5],
                  re1 = factor(c("yes","no","yes","no","yes")),
                  re2 = factor(c("yes","no","yes","no","yes")),
                  re3 = factor(c("yes","no","yes","no","yes")))

recode2(dam, fields = c('re1', 're2'), recodes = "'yes' = 1; 'no' = 2", as.factor.result = TRUE)



##
