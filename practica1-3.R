install.packages("arrangements")
require("arrangements")


#======================== A Part =================
# Muestreo con reemplazamiento  


pob = c(13.9, 11.5, 16.7, 14.4, 14.6, 15.1)
#combinations(x=pob, k =3, replace=TRUE)

p = permutations(x=pob, k =3, replace=TRUE)
#permutation with replacement 

m = matrix(p, nrow = 216, ncol = 3)

media = (matrix(m[,1]) + matrix(m[,2]) +matrix(m[,3]))/3

pob_media = cbind(m, media[,1]) 

#numero de muestras
sample_total = nrow(pob_media)
#muestras con mas de 14
sample_14 =  length(which(pob_media[,4] > 14))
#probabilidad de media muestral > 14
prob_14  = sample_14 / sample_total
print(paste0( "prob with replacement rows > 14 " , prob_14))



# ===================================
# Muestreo sin reemplazamiento 

pob = c(13.9, 11.5, 16.7, 14.4, 14.6, 15.1)

p = combinations(x=pob, k =3, replace=FALSE)

m = matrix(p, nrow = 20, ncol = 3)
media = (matrix(m[,1]) + matrix(m[,2]) +matrix(m[,3]))/3
pob_media = cbind(m, media[,1]) 


#numero de muestras
sample_total = nrow(pob_media)
print(paste0( "total rows " , sample_total))

#muestras con mas de 14
sample_14 =  length(which(pob_media[,4] > 14))
print(paste0( "total rows > 14 " , sample_14))
#probabilidad de media muestral > 14
prob_14  = sample_14 / sample_total
print(paste0( "prob with replacement rows > 14 " , prob_14))


#======================== B Part =================
# b halle la varianza de la media anterior  y compruebe  que se cumple la proposicion 2.2
#muestreo aleatorio simple con reemplazamiento 


pob = c(13.9, 11.5, 16.7, 14.4, 14.6, 15.1)
#combinations(x=pob, k =3, replace=TRUE)

p = permutations(x=pob, k =3, replace=TRUE)

m = matrix(p, nrow = 216, ncol = 3)
media = (matrix(m[,1]) + matrix(m[,2]) +matrix(m[,3]))/3
pob_media = cbind(m, media[,1]) 

sample_prob = matrix(rep ( 1 / nrow(pob_media), nrow(pob_media)),nrow = nrow(pob_media), ncol=1)
nrow(sample_prob)
pob_media_prob = cbind(pob_media, sample_prob) 


sample_variance = pob_media_prob[,4] ** 2 

pob_media_prob_variance = cbind(pob_media_prob, sample_variance) 

#numero de muestras
sample_total = nrow(pob_media_prob_variance)
print(paste0( "total samples " , sample_total  ))
#muestras con mas de 14
sample_14 =  length(which(pob_media_prob_variance[,4] > 14))
print(paste0( "total rows > 14 " , sample_14))
#probabilidad de media muestral > 14
prob_14  = sample_14 / sample_total
print(paste0( "prob with replacement rows > 14 " , prob_14))

head(pob_media_prob_variance)

hope_sample_mean = sum(pob_media_prob_variance[, 4] * pob_media_prob_variance[, 5])
print(paste0( "hope_sample_mean " , hope_sample_mean))

# varianza de la media muestral 

hope_sample_variance = sum(pob_media_prob_variance[, 5] * pob_media_prob_variance[, 6]) - (hope_sample_mean) ** 2
print(paste0( "hope_sample_mean " , hope_sample_variance))


#cociente varianza poblacional y n 
pob_mean = mean(pob)
pob_variance = sum( (pob - pob_mean) ** 2) / length(pob)
v_y = pob_variance / 3
print(paste0( "variance_y " , v_y))
# weird variance_pob = var(pob)


#Muestreo Aleatorio Simple Sin Reemplazamiento 

pob = c(13.9, 11.5, 16.7, 14.4, 14.6, 15.1)

p = combinations(x=pob, k =3, replace=FALSE)

m = matrix(p, nrow = 20, ncol = 3)
media = (matrix(m[,1]) + matrix(m[,2]) +matrix(m[,3]))/3
pob_media = cbind(m, media[,1]) 

pob_size = nrow(pob_media)

sample_prob = matrix(rep ( 1 / pob_size, pob_size), nrow = pob_size, ncol=1)

pob_media_prob = cbind(pob_media, sample_prob) 

sample_variance = pob_media_prob[,4] ** 2 

pob_media_prob_variance = cbind(pob_media_prob, sample_variance) 

#numero de muestras
sample_total = nrow(pob_media_prob_variance)
print(paste0( "total samples " , sample_total  ))
#muestras con mas de 14
sample_14 =  length(which(pob_media_prob_variance[,4] > 14))
print(paste0( "total rows > 14 " , sample_14))
#probabilidad de media muestral > 14
prob_14  = sample_14 / sample_total
print(paste0( "prob with replacement rows > 14 " , prob_14))


hope_sample_mean = sum(pob_media_prob_variance[, 4] * pob_media_prob_variance[, 5])

print(paste0( "hope_sample_mean " , hope_sample_mean))

# varianza de la media muestral 

hope_sample_variance = sum(pob_media_prob_variance[, 5] * pob_media_prob_variance[, 6]) - (hope_sample_mean) ** 2
print(paste0( "hope_sample_mean " , hope_sample_variance))

#media poblacional
mean(pob)

# varianza poblacion con n -1 
variance_pob = var (pob)
v_y = (1 - 3/6) * (variance_pob /3)


#======================== C Part =================
# Suponga que para estimar la media del nivel de hemoglobina en estos 6
# pacientes se propusiera la mediana de los valores observados en la muestra.
# ¿Sería un estimador insesgado? ¿Tiene este una menor varianza que la media
# muestral?

# ============
# Con reemplazamiento 

pob = c(13.9, 11.5, 16.7, 14.4, 14.6, 15.1)
pob_size = length(pob)

p = permutations(x=pob, k =3, replace=TRUE)
m = matrix(p, nrow = 216, ncol = 3)
sample_prob = matrix(rep ( 1 / 216, 216 ), nrow = 216 , ncol=1)
pob_prob = cbind(m, sample_prob) 


vec_median = vector()
vec_median_square = vector()
for (row in 1:nrow(pob_prob)) {
    vector <- c(pob_prob[row,1:3])
    vec_median[row] <- median(vector)
    vec_median_square[row] <- vec_median[row] ** 2
}
pob_prob_median = cbind( cbind(pob_prob, matrix(vec_median, nrow = 216 , ncol=1))

pob_prob_median_varianza = cbind(pob_prob_median, matrix(vec_median_square, nrow = 216 , ncol=1))                


#numero de muestras
sample_total = nrow(pob_prob_median_varianza)
print(paste0( "total samples " , pob_prob_median_varianza  ))

hope_sample_median = sum(pob_prob_median_varianza[, 4] * pob_prob_median_varianza[, 5])
print(paste0( "hope_sample_median " , hope_sample_median))

# varianza de la mediana muestral 

hope_sample_variance = sum(pob_prob_median_varianza[, 4] * pob_prob_median_varianza[, 6]) - (hope_sample_mean) ** 2
print(paste0( "hope_sample_median_variance" , hope_sample_variance))


#cociente varianza poblacional y n 
pob_mean = mean(pob)

# Si la mediana muestral fuera un estimador insesgado de la media se cumpliría que:
# E(mediana muestral) = media poblacional

pob_mean
hope_sample_median

# Se verifica que ambos valores no son iguales, por lo tanto la mediana muestral no es un
# estimador insesgado de la media poblacional.

# =========== Muestreo aleatorio simple sin reemplazamiento

pob = c(13.9, 11.5, 16.7, 14.4, 14.6, 15.1)
pob_size = length(pob)

p = combinations(x=pob, k =3, replace=FALSE)

m = matrix(p, nrow = 20, ncol = 3)
sample_prob = matrix(rep ( 1 / 20, 20 ), nrow = 20 , ncol=1)
pob_prob = cbind(m, sample_prob) 


vec_median = vector()
vec_median_square = vector()
for (row in 1:nrow(pob_prob)) {
  vector <- c(pob_prob[row,1:3])
  vec_median[row] <- median(vector)
  vec_median_square[row] <- vec_median[row] ** 2
}
pob_prob_median = cbind(pob_prob, matrix(vec_median, nrow = 20 , ncol=1))
                         
pob_prob_median_varianza = cbind(pob_prob_median, matrix(vec_median_square, nrow = 20 , ncol=1))                


#numero de muestras
sample_total = nrow(pob_prob_median_varianza)
print(paste0( "total samples " , sample_total  ))

hope_sample_median = sum(pob_prob_median_varianza[, 4] * pob_prob_median_varianza[, 5])
print(paste0( "hope_sample_median " , hope_sample_median))

# varianza de la mediana muestral 

hope_sample_variance = sum(pob_prob_median_varianza[, 4] * pob_prob_median_varianza[, 6]) - (hope_sample_median) ** 2

hope_sample_variance = sum( (pob_prob_median_varianza[, 5] -  hope_sample_median) ** 2 ) / nrow(pob_prob_median_varianza)

#1/n sum(X-x)2

print(paste0( "hope_sample_median_variance" , hope_sample_variance))

#cociente varianza poblacional y n 
pob_mean = mean(pob)

# Si la mediana muestral fuera un estimador insesgado de la media se cumpliría que:
# E(mediana muestral) = media poblacional

# ================== Case D =====================
# MAS con reemplzamiento 
pob = c(13.9, 11.5, 16.7, 14.4, 14.6, 15.1)
pob_size = length(pob)

p = permutations(x=pob, k =3, replace=TRUE)

# 0.018 0.310 0.549
# 1, 4, 6








