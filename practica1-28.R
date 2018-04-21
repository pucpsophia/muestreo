install.packages("arrangements")
install.packages("combinat")
install.packages("survey")
library("combinat")
require("arrangements")
library(survey)
options(digits = 20)

# pregunta 26



tallas <- read.csv(file="tallas.csv", header=TRUE, sep=",")
head(tallas)
disTS = svydesign(id=~1, fpc=rep(700,35) , data = tallas)
tallas_var = svyvar (~estatura, disTS)
tallas_ds = sqrt(tallas_var)

N = 700
alpha = 0.05
n = 35
z = qnorm(1-alpha/2)

e <- z * (tallas_ds / sqrt(35)) * sqrt(1 - 35 / 700)


# 1600 urnas seleccionadas de una gran poblacion 
# 812 votaron por el candidato opositor 
# 480 votaron por el candidato estatal 
# 50 votaron en blanco
# 258 votaron invalido

# nuestra muestra es de 1600 de una muestra mayor que consideraremos infinita 
# definimos nuestra muestra 
# 1 votaron candidato opositor
# 2 votaron candidato estatal
# 3 votaron en blanco
# 4 votaron invalido 

n <- 1600
sample =  rep(c("a", "b", "c", "d"), c(812, 480, 50, 258) )
m = matrix(data = sample, nrow = 1600, ncol = 1)
d = as.data.frame(m)
dis = svydesign(id=~1,  data = d)
v_var = svyvar(~V1, dis)
v_sd = sqrt(v_var)
e = z * (v_sd / sqrt(n)) 

confint(v_mean)
print(e)
