# tamanio muestra para proporcion , diapositiva 20 del capitulo 2

# desea estimar T (tao) , error no mayor a 30 alumnos y confianza 95% , poblacion = 1200

# deducir margen de error

alpha = 0.05
N = 1200
d_error = 30 

z = qnorm(1-alpha/2)
e = 30 / 1200
p = 0.5 # regla conservadora

n = ( z ^ 2 *  p * ( 1 - p ) * N) /( z^2 * p * (1-p) + e^2 * ( N - 1 ) )

# uso survey
library(survey)
data(api)
apipop[1:3,]
N = dim(apipop)[1]
n = 100
index1 = sample(N, 100)
# fpc tamanio de la poblacion N  pw los pesos N/n

sample1 = apipop[index1,]

aux = data.frame( fpc = rep(N, 100) , pw = rep(N/n,100))
sample1 = cbind(sample1, aux)
head(sample1)

design =  svydesign(id=~1, fpc = ~fpc, data = sample1)

svytotal( ~enroll, design)  
svymean( ~stype, design)  

means1 =  svymean( ~api00+api99, design)  
svycontrast(means1, c(api00=1, api99=-1))


sample2 = apipop[sample(N,100, replace = TRUE), ]
sample2 = cbind(sample2, aux)

designmasc = svydesign(id=~1, weights = ~pw, data = sample2)

svytotal(~enroll, designmasc)
svymean(~stype, designmasc)

# censo estudiantil, estimar el rendimiento medio en matematicas (M), comprension lectora (L) historia y geografia (H)
# margen de error no mayor a 5 puntos y nivel confianza 95

# piloto

library(survey)
set.seed(12345)
setwd('F:/muestreo')

library(foreign)
ce2s16Am = read.spss("ce2s16Am.sav", use.value.labels=TRUE)
ce2s16Am = as.data.frame(ce2s16Am)
save(ce2s16Am,file=ce2s16Am.RData)
head(ce2s16Am)

N = dim(ce2s16Am)[1]
index1 = sample(N,30)
mp16Am = ce2s16Am[index1,]
dismp = svydesign(id=~1,fpc=rep(N,30),data=mp16Am)
sigmae2_L = coef(svyvar(~M500_M,dismp,na.rm=T))
sigmae2_M = coef(svyvar(~M500_L,dismp,na.rm=T))
sigmae2_H = coef(svyvar(~M500_H,dismp,na.rm=T))
e = 5


d = 25 * N / (qnorm(0.975)^2)


n1 = z ^ 2 * sigmae2_L * N / ( z ^ 2 * sigmae2_L + e ^ 2 * N )
n2 = z ^ 2 * sigmae2_M * N / ( z ^ 2 * sigmae2_M + e ^ 2 * N )
n3 = z ^ 2 * sigmae2_H * N / ( z ^ 2 * sigmae2_H + e ^ 2 * N )

n = ceiling(max(n1,n2,n3))


set.seed(12345)
index = sample(N,n)
m16Am = ce2s16Am[index,]
disem = svydesign(id=~1,fpc=rep(N,n),data=m16Am)
mean_L = svymean(~M500_L,disem,na.rm=T)
mean_M = svymean(~M500_M,disem,na.rm=T)
mean_H = svymean(~M500_H,disem,na.rm=T)
meanp_L = svymean(~Grupo_L,disem,na.rm=T)
meanp_M = svymean(~Grupo_M,disem,na.rm=T)
meanp_H = svymean(~Grupo_HGE,disem,na.rm=T)
mean_L


pr = rbind(meanp_L,meanp_M,meanp_H)
colnames(pr) = c("Previo al inicio","Inicio","En proceso","Satisfactorio")








