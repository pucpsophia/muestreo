install.packages("arrangements")
install.packages("combinat")
library("combinat")
require("arrangements")
options(digits = 5)


# previous 




# 1600 urnas seleccionadas de una gran poblacion 
# 812 votaron por el candidato opositor 
# 480 votaron por el candidato estatal 
# 50 votaron en blanco
# 258 votaron invalido

alpha = 0.05

N <- dim(apipop)[1]
n <- 500


load("TallaS.RData")