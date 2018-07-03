# Lista de ejercicios 3 Final
# (Tecnicas de muestreo)
# Este ejercicio esta basado en una actual investigacion sobre el uso de cierta metodologia de construccion
# BIM en el pais. El estudio se hizo en la ciudad de Lima con el fin de poder estimar a un nivel de confianza
# del 95% y un error de 0.05, la proporcion de obras en Lima que hacian uso de esta metodologia. El diseno
# empleado fue uno estratificado por conglomerados bietapico. Los estratos estuvieron conformados por las
# divisiones urbanas: Lima Top, Lima Moderna, Lima Centro, Lima Norte, Lima Sur y el Callao. Cada estrato
# se dividio en sectores A, B y C de acuerdo al nivel socio-economico y dependiendo tambien si habian obras
# en ellas. En algunos casos se colapsaron sectores. En cada estrato se tomo un MASs de sectores y dentro de
# cada sector un MASs de obras. La data del muestreo y el marco muestral (estimado por datos de Capeco)
# se encuentra en la intranet bajo el nombre de DATAEX. La variable principal de investigacion aqui es BIM,
# que indica si la obra encuestada hace o no uso de esta metodologia. Se pide entonces lo siguiente:
  # a) Estime la proporcion de obras de construccion en Lima Metropolitana que hacen uso de la metodologia
  # BIM, reportando su intervalo de confianza al 95%. (2.0 puntos)
  # b) Halle la estimacion de la proporcion del numero de obras de construccion en Lima Top que hacen uso de
  # la metodologia BIM, junto con su error estandar de estimacion estimado. (1.0 punto)
  # c) Suponga que en lugar de haberse empleado este dise~no para Lima Top, usted hubiese empleado un
  # muestreo ppt de 4 sectores, para luego, encuestar a todas las obras de los sectores de Lima Top seleccionados.
  # Implemente este dise~no, reportando la proporcion del numero de obras de construccion en Lima Top que
  # hacen uso de la metodologia BIM, junto con su error estandar de estimacion estimado. Compare finalmente
  # los errores de estimacion de este dise~no con los del anteriormente tomado.
# NOTA: Una vez que seleccione el sector, use la estimacion de DATAEX tomada para este sector a fin de
# imputar su proporcion del uso del BIM. En caso que el sector no halla sido seleccionado en DATAEX (es
# decir, cuando vea que el numero de obras encuestadas es 0), impute esta proporcion simulando ella de una
# distribucion Beta de parametros ?? = 2 y ?? = 8. (2.0 puntos)

alpha = 0.05
e = 0.05

install.packages("data.table")
install.packages("sampling")
install.packages("survey")
install.packages("foreign")

install.packages("NHANES")
library(NHANES)
data(NHANES)
library(survey)
library(data.table)

setwd("F:/muestreo")

setwd("/Users/gvalderrama/Documents/muestreo/")

setwd("/Users/gregory/Documents/pucp/muestreo/")

dataset <- read.csv(file= "DATAEX.csv", header=TRUE, sep=";")
head(dataset)
str(dataset)
datatable = as.data.table(dataset)
table(dataset$ESTRATO)
table(dataset$DISTRITO)
table(dataset$SECTOR)


length(table(dataset$ESTRATO))

data(api)

dstrat<-svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
table(apistrat$stype, apistrat$pw)
unique(apistrat$pw)

## multistage sampling has no effect when fpc is not given, so
## these are equivalent.
dclus2wr <- svydesign(id=~dnum+snum, weights=weights(dclus2), data=apiclus2)
dclus2wr2<-svydesign(id=~dnum, weights=weights(dclus2), data=apiclus2)

table(apiclus2$stype)

str(apiclus2)




table(apistrat$stype,apistrat$fpc)
dim(apistrat)

dclus1<-svydesign(id=~dnum, weights=~pw, data=apiclus1, fpc=~fpc)
table(apiclus1$dnum)
dim(apiclus1)

NObras = 2716
NMuestra = 1219
K = dim(datatable)[1]
NEstratos = length(levels(dataset$ESTRATO))
NDistritos =  length(levels(dataset$DISTRITO))
NSector =  length(levels(dataset$SECTOR))



levels(dataset$CONG)
levels(dataset$BIM)
dim(dataset)



n = 3

help("runif")

help("punif")
# check missing data 
nads = na.omit(datatable)
dim(datatable)
dim(nads)
# order by strato
datatable <- datatable[ order ( datatable$ESTRATO ) , ]

datatable [ , nest := .N , by = 'ESTRATO']

datatable [, Fact:=interaction(ESTRATO,DISTRITO, SECTOR)]

datatable[1:10, ]

dataset[ , 'ESTRATO' ]
str(dataset)

design = svydesign(id=~DISTRITO + SECTOR, strata = ~ESTRATO, data = datatable)

svymean(~BIM, design)

load(NHANES)







str(NHANES)
