pob = c(13.9, 11.5, 16.7, 14.4, 14.6, 15.1)

mean(pob)

var(pob)


var(pob)*(length(pob)-1)/length(pob)

# con reemplazamiento

sample(pob, 3, replace=TRUE)



help(choose)

devtools::install_github("randy3k/arrangements")


install.packages("arrangements")
require("arrangements")
combinations(6, 3, pob)


#ncol(combn(6,3))

combinations(5, 2, replace=TRUE)

t(combn(6,3))

rep(pob, 3)
expand.grid() 


