# practica API database , muestreo por conglomerados 
# conglomerados de una etapa 


library(survey)
library(sampling)

data(api)
attach(apipop)

K = dim(apipop)[1]  # 6194
N = dim(table(apipop$dnum)) # 757
n = 15  # conglomerados seleccionados 

aux1 = cluster( apipop, clustername = c("dnum"), n , description = T ) # obtencion de la muestra 

head(aux1)  # probabilidad es n/N todos misma probabilidad 15/757

samplec1 = getdata(apipop, aux1) # obtener la data 

dim (samplec1) # 15 distritos que incluyen 90 colegiospuede cambiar 

Mhat = dim(aux1)[1]   # el numero de colegios seleccionados 

aux = data.frame(fpc = rep(N, Mhat), pw = rep(K/Mhat, Mhat))

samplec1 = cbind(numc=1:Mhat, samplec1, aux)

head(samplec1)

dclus1 <- svydesign(id=~dnum, fpc = ~fpc, data = samplec1)  # fpc identifica sin reemplzamiento 757


dclus1

svytotal(~enroll, dclus1) # estama cantidad de matriculados en el estado

# dclus SE = raiz de la varianza del estimador IC =  +- 1.96 * SE 


svymean(~api00, dclus1)

# Muestreo bietapico

#  n = 40 numero de dsitritos 
#  m = 5 numero de colegios de cada distrito

# apiclus2


# ejercicio de conglomerados con probabilidades de primer orden y seleccion de poison


x <-  c(300, 200, 100, 1000, 150, 500)

300 / sum(x)

pik = inclusionprobabilities(x, 2)
(pik > runif(6))
UPpoisson(pik)








