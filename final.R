
# PARTE A
N = 80
M = 20
p = -0.005

((N * M  - 1) / ( M  * ( N - 1) )) * ( 1 + p * (M - 1) )
# PARTE B
y = c(28.326,49.555,35.521,24.712,38.293,18.752,29.456,27.228,14.554)
sort(y)
pik = inclusionprobabilities(y,4)

UPsystematicpi2(pik)

library(combinat)
pisppt <- function(X,n) {
  N = length(X)
  XT = sum(X)
  m = combn(X,n) # Requiere del paquete combinat
  m = apply(m,2,permn)
  m = matrix(unlist(m),ncol=n,byrow=TRUE)
  nm = dim(m)[1]
  p=0
  for (j in 1:nm) {
    p[j] = prod(m[j,])/(XT*prod(XT-cumsum(m[j,1:n-1])))
  }
  pi1=0
  pi2=matrix(0,N,N)
  for (i in 1:(N-1)){
    aux1 = (m==X[i])
    index = which(apply(1*aux1,1,sum)==1)
    pi1[i] = sum(p[index])
    for (j in (i+1):N){
      aux2 = (m==X[j])
      aux2 = 1*aux2[index,]
      pi2[i,j] = sum(p[index[which(apply(aux2,1,sum)==1)]])
    }}
  pi1[N] = n-sum(pi1)
  pi2 = pi2+t(pi2)
  list(pi1,pi2)
}

res = pisppt(y, 4)

res[[1]]
res[[2]]



# pregunta 2 

N = 60  # containers 
K = 6000 # cajas de frutas

# primera etapa seleccionar al hazar y con remplazamiento 4 containers  y luego 3 cajas en las se registra el peso 


