install.packages("arrangements")
install.packages("combinat")
library("combinat")
require("arrangements")
options(digits = 5)
# clues 
# https://www.scribd.com/doc/17381074/VARIANZA-MUESTRAL
#======================== Parte A  =================
# Muestreo con reemplazamiento  
# pob =  c(12, 32, 18, 37, 22, 18, 28) datos material clase

pob <- c(13.9, 11.5, 16.7, 14.4, 14.6, 15.1)
var_n_func <- function(vec){
  v_mean <- mean(vec)
  v_var <- sum( (vec - v_mean) ^ 2 ) / length(vec) 
  return(v_var)
}
pob_media = mean(pob)
pob_var_n = var_n_func(pob) 
pob_var_n1 = var(pob)
p <- permutations(x=pob, k =3, replace=TRUE)
m <- as.matrix(p)
media <- as.matrix(apply(m,1,mean))
var <- as.matrix(apply(m,1,var))
prob <- as.matrix(rep ( 1 / nrow(m), nrow(m)))
espacio <- cbind(m, media[,1], var[,1], prob[, 1]) 
colnames(espacio) <- c("X1", "X2", "X3", "media", "s2", "prob")
head(espacio)
media_muestral = c(sum(espacio[,"media"]*espacio[,"prob"])) # E(X) , tambien igual a sum(espacio[,"media"]) / nrow(espacio)
varianza_muestral = c(sum(espacio[,"s2"]*espacio[,"prob"]))
varianza_media_muestral = sum(((espacio[,"media"] - sum(espacio[,"media"]*espacio[,"prob"]))^2)*espacio[,"prob"])
sample_total = nrow(espacio)
sample_14 =  length(which(espacio[,"media"] > 14))
prob_14  = sample_14 / sample_total
print(paste0( "probabilidad de elementos > 14 " , prob_14))
# estimador insesgado por media poblacional = media de la muestra
sprintf(" insesgado media poblacional %s , media muestral %s", pob_media, media_muestral )
sprintf(" insesgado varianza poblacional %s , varianza muestral %s", pob_var_n, varianza_muestral)

# ====================== Parte A =============
# Muestreo sin reemplazamiento 

pob <- c(13.9, 11.5, 16.7, 14.4, 14.6, 15.1)

pob_media = mean(pob)
pob_var_n = sum(( pob - mean(pob) ) ^ 2)/ length(pob) 
pob_var_n1 = var(pob)

p = combinations(x=pob, k =3, replace=FALSE)
m <- as.matrix(p)
media <- as.matrix(apply(m,1,mean))
var <- as.matrix(apply(m,1, var))
prob <- as.matrix(rep ( 1 / nrow(m), nrow(m)))
espacio <- cbind(m, media[,1], var[,1], prob[, 1]) 
colnames(espacio) <- c("X1", "X2", "X3", "media", "s2", "prob")

head(espacio)
media_muestral = c(sum(espacio[,"media"]*espacio[,"prob"]))
varianza_muestral = c(sum(espacio[,"s2"]*espacio[,"prob"]))
varianza_media_muestral = sum(((espacio[,"media"] - sum(espacio[,"media"]*espacio[,"prob"]))^2)*espacio[,"prob"])

sample_total = nrow(espacio)
sample_14 =  length(which(espacio[,"media"] > 14))
prob_14  = sample_14 / sample_total
print(paste0( "probabilidad de elementos > 14 " , prob_14))

sprintf(" insesgado media poblacional %s , media muestral %s", pob_media, media_muestral )
sprintf(" insesgado varianza poblacional %s , varianza muestral %s", pob_var_n1 , varianza_muestral)


#======================== B Part =================
# Con reemplazamiento 
# Muestreo con reemplazamiento  
var_mediana_fun <- function(vec){
  return( sum( ( vec - median(vec) ) ^ 2 ) / length(vec) )
}
var_mediana_n1_fun <- function(vec){
  return( sum( ( vec - median(vec) ) ^ 2 ) / (length(vec) - 1) )
}
pob <- c(13.9, 11.5, 16.7, 14.4, 14.6, 15.1)
pob_media = mean(pob)
pob_mediana = median(pob)
pob_var_n = sum( ( pob - mean(pob) ) ^ 2 ) / length(pob) 
pob_var_n1 = var(pob)
pob_var_mediana = var_mediana_fun(pob)
pob_var_mediana_n1 =  var_mediana_n1_fun(pob)
p <- permutations(x=pob, k =3, replace=TRUE)
m <- as.matrix(p)

media <- as.matrix(apply( m , 1 , mean ) )
var <- as.matrix(apply( m, 1 , var ) )
median <- as.matrix( apply ( m , 1 , median ))
var_median <- as.matrix( apply ( m , 1 , var_mediana_fun ))
prob <- as.matrix(rep ( 1 / nrow(m), nrow(m) ) )
espacio <- cbind(m, media[,1], var[,1], median[, 1], var_median[, 1] , prob[, 1]) 
colnames(espacio) <- c("X1", "X2", "X3", "media", "s2", "mediana", "var_median", "prob")
head(espacio)

media_muestral = c(sum(espacio[,"media"]*espacio[,"prob"]))
median_muestral = c(sum(espacio[,"mediana"]*espacio[,"prob"]))
varianza_muestral = c(sum(espacio[,"s2"]*espacio[,"prob"]))
varianza_median = c(sum(espacio[,"var_median"]*espacio[,"prob"]))
varianza_media_muestral = sum(((espacio[,"media"] - sum(espacio[,"media"]*espacio[,"prob"]))^2)*espacio[,"prob"])
varianza_mediana_muestral = sum(((espacio[,"mediana"] - sum(espacio[,"mediana"]*espacio[,"prob"]))^2)*espacio[,"prob"])

sprintf(" insesgado mediana poblacional %s , mediana muestral %s", pob_mediana, median_muestral )
sprintf(" insesgado varianza mediana poblacional %s , varianza mediana %s", pob_var_mediana , varianza_median)


# =========== Muestreo aleatorio simple sin reemplazamiento

var_mediana_fun <- function(vec){
  return( sum( ( vec - median(vec) ) ^ 2 ) / length(vec) )
}
var_mediana_n1_fun <- function(vec){
  return( sum( ( vec - median(vec) ) ^ 2 ) / (length(vec) - 1) )
}

pob <- c(13.9, 11.5, 16.7, 14.4, 14.6, 15.1)
pob_media = mean(pob)
pob_mediana = median(pob)
pob_var_n = sum((pob-mean(pob))^2)/ length(pob) 
pob_var_n1 = var(pob)-
pob_var_mediana = var_mediana_fun(pob)
pob_var_mediana_n1 =  var_mediana_n1_fun(pob)

p = combinations(x=pob, k =3, replace=FALSE)
m <- as.matrix(p)
media <- as.matrix(apply(m,1,mean))
var <- as.matrix(apply(m,1,var))
var_median <- as.matrix( apply ( m , 1 , var_mediana_n1_fun ))
median <- as.matrix(apply(m,1,median))
prob <- as.matrix(rep ( 1 / nrow(m), nrow(m)))
espacio <- cbind(m, media[,1], var[,1], median[, 1], var_median[, 1] , prob[, 1]) 
colnames(espacio) <- c("X1", "X2", "X3", "media", "s2", "mediana", "var_median", "prob")
head(espacio)
media_muestral = c(sum(espacio[,"media"]*espacio[,"prob"]))
median_muestral = c(sum(espacio[,"mediana"]*espacio[,"prob"]))
varianza_muestral = c(sum(espacio[,"s2"]*espacio[,"prob"]))
varianza_median = c(sum(espacio[,"var_median"]*espacio[,"prob"]))
varianza_media_muestral = sum(((espacio[,"media"] - sum(espacio[,"media"]*espacio[,"prob"]))^2)*espacio[,"prob"])
varianza_mediana_muestral = sum(((espacio[,"mediana"] - sum(espacio[,"mediana"]*espacio[,"prob"]))^2)*espacio[,"prob"])

sprintf(" insesgado mediana poblacional %s , mediana muestral %s", pob_mediana, median_muestral )
sprintf(" insesgado varianza mediana poblacional %s , varianza mediana %s", pob_var_mediana_n1 , varianza_median)


# ================== Parte C =====================
# MAS con reemplzamiento 
pob = c(13.9, 11.5, 16.7, 14.4, 14.6, 15.1)
pob_size = length(pob)

p = permutations(x=pob, k =3, replace=TRUE)

# 0.018 0.310 0.549
m = matrix(pob, nrow = 6, ncol = 1)
prob <- as.matrix(rep ( 1 / nrow(m), nrow(m)))
facum = cumsum( prob[ ,1])
espacio = cbind(m, prob, facum) 
colnames(espacio) <- c("X1", "prob", "pacum")

head(espacio)

a <- findInterval(0.018, espacio[,"pacum"]) + 1 
b <- findInterval(0.310, espacio[,"pacum"]) + 1
c <- findInterval(0.549, espacio[,"pacum"]) + 1

muestra = c( espacio[a, 1],espacio[b, 1],espacio[c, 1])
muestra
mean(muestra)


# MAS sin reemplzamiento 
pob = c(13.9, 11.5, 16.7, 14.4, 14.6, 15.1)
pob_size = length(pob)
p = permutations(x=pob, k =3, replace=TRUE)
# 0.018 0.310 0.549
m = matrix(pob, nrow = 6, ncol = 1)
prob <- as.matrix(rep ( 1 / nrow(m), nrow(m)))
facum = cumsum( prob[ ,1])
espacio = cbind(m, prob, facum) 
colnames(espacio) <- c("X1", "prob", "pacum")
head(espacio)
a <- findInterval(0.018, espacio[,"pacum"]) + 1
muestra_a =  espacio[a,1 ]    
espacio <- espacio[ -c(a),]
espacio[,"prob"] <- as.matrix(rep ( 1 / nrow(espacio), nrow(espacio)))
espacio[,"pacum"] <- cumsum(espacio[,"prob"])
espacio
b <- findInterval(0.310, espacio[,"pacum"]) + 1
muestra_b =  espacio[b, 1]    
espacio <- espacio[ -c(b),]
espacio[,"prob"] <- as.matrix(rep ( 1 / nrow(espacio), nrow(espacio)))
espacio[,"pacum"] <- cumsum(espacio[,"prob"])
espacio
c <- findInterval(0.549, espacio[,"pacum"]) + 1
muestra_c =  espacio[c,1]    
muestra = c(muestra_a, muestra_b, muestra_c)
muestra
mean(muestra)











