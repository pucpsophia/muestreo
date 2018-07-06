# muestreo por conglomerados 

library("sampling")
library("survey")


k = 6194 # unidades secundarias colegios 
N = 757 # unidades primarias (distritos escolares)

n = 15 # numero de clusters 


data(api)
apipop

index = sampling::cluster(apipop, clustername= c("dnum"), n , description=T)

str(index)

dim(index)
sample =  getdata(apipop, index)

Mhat = dim(sample)[1]

aux = data.frame(fpc= rep(N, Mhat), pw = rep(k/Mhat, Mhat)) # fpc colocarle cuantos distritos hay , pesos de muestreo

sample_1 = cbind(sample, aux)

design = svydesign(ids=~dnum, fpc = ~fpc, data = sample_1)
svymean(x = ~api00, design = design, deff=T)
test =  svytotal(x = ~api00, design = design , deff=T)
confint(test)

alpha = 0.05
z <- qnorm ( 1 - alpha / 2 )

# CI using SE
c(coef(test) - z *  SE(test)[1] , coef(test) + z * SE(test)[1])

# multietapico 

n = 40
m = 5 

design = svydesign(ids=~dnum + snum, fpc=~fpc1 + fpc2 ,  data =  apiclus2)
svymean(~api00, design)











