# file.edit('~/.Renviron') r under proxy 
# NO_PROXY=
# http_proxy=
# https_proxy=
# HTTP_PROXY=
# HTTPS_PROXY=
install.packages("data.table")
install.packages("sampling")
install.packages("survey")
install.packages("foreign")

library(data.table)
library(sampling)
library(survey)
library(foreign)

# setwd("/Users/gvalderrama/Documents/muestreo")
# setwd("/Users/gregory/Documents/pucp/muestreo")
setwd("F:/muestreo")
load("ce2s16Cz.rdata")
ce2s16Cz=as.data.table(ce2s16Cz)
ce2s16Cz = ce2s16Cz[!is.na(ce2s16Cz$M500_M),]
ce2s16Cz = ce2s16Cz[!is.na(ce2s16Cz$Sexo),]
ce2s16Cz = ce2s16Cz[!is.na(ce2s16Cz$Area),]
ce2s16Cz = ce2s16Cz[!is.na(ce2s16Cz$Gestion),]
ce2s16Cz[, Sexo:= droplevels ( Sexo ) ]
levels(ce2s16Cz$Area) # "Urbana" "Rural" 
levels(ce2s16Cz$Gestion) # "Estatal"    "No estatal"
levels(ce2s16Cz$Sexo) # "Hombre" "Mujer" 
# Estratos
ce2s16Cz[, Estrato:=interaction(Area,Gestion)]
levels(ce2s16Cz$Estrato) # "Urbana.Estatal"    "Rural.Estatal"     "Urbana.No estatal" "Rural.No estatal" 

# Crear fpc
ce2s16Cz = ce2s16Cz[ order ( ce2s16Cz$Estrato ) , ]
ce2s16Cz [ , fpc := .N , by = Estrato]
# Longitud de los estratos 
Nh = as.numeric ( table ( ce2s16Cz$Estrato ) )
# Obtener Muestra Piloto
set.seed(12345)
sigma_h = vector(length = 4)
estratos = levels(ce2s16Cz$Estrato)
sigma_h[1] = sd ( ce2s16Cz$M500_M [ ce2s16Cz$Estrato == estratos[1] ] [sample(Nh[1],10)] )
sigma_h[2] = sd ( ce2s16Cz$M500_M [ ce2s16Cz$Estrato == estratos[2] ] [sample(Nh[2],10)] )
sigma_h[3] = sd ( ce2s16Cz$M500_M [ ce2s16Cz$Estrato == estratos[3] ] [sample(Nh[3],10)] )
sigma_h[4] = sd ( ce2s16Cz$M500_M [ ce2s16Cz$Estrato == estratos[4] ] [sample(Nh[4],10)] )
# Hallar Ah neyman
ah = Nh * sigma_h / sum ( Nh * sigma_h )
# Hallar longitud de los estratos
N <- nrow(ce2s16Cz)
e <- 5
alpha = 0.05
z <- qnorm ( 1 - alpha / 2 )
d = N * e / z
n = sum( ( ( Nh * sigma_h ) ^ 2 ) / ah ) / ( d ^ 2 + sum( Nh * sigma_h ^ 2 ) )
nh = ceiling(ah * n) # 917 324 195   3

# Ordenar segun estratos
ce2s16Cz = ce2s16Cz[ order ( ce2s16Cz$Estrato ) , ]
#muestreo
m <- sampling::strata(ce2s16Cz, c("Estrato"), size= nh, method="srswor")
muestra <- getdata( ce2s16Cz , m)
disMAE = svydesign( id = ~1, strata = ~Estrato, fpc = ~fpc, data = muestra)
# svy para dominios 
svyby(formula = ~M500_M,by = ~Sexo, design = disMAE, FUN = svymean)
# Sexo   M500_M       se
# Hombre Hombre 550.1259 2.859766
# Mujer   Mujer 539.5873 2.830011


# Parte B

# Colapsar Estratos
ce2s16Cz [ , Estrato2:= as.factor( 
    c('Estatal', 'Estatal', 'No estatal', 'No estatal')
      [ match(Estrato,c('Urbana.Estatal','Rural.Estatal','Urbana.No estatal','Rural.No estatal'))])]

ce2s16Cz [ , fpc2 := .N, by=Estrato2]

muestra[ , Estrato2:= ce2s16Cz$Estrato2[ ID_unit ] ]

muestra[ , fpc2:= ce2s16Cz$fpc2[ ID_unit ] ]


disMAE_Sexo = svydesign( id = ~1, strata = ~Estrato, fpc = ~fpc, data = muestra)

estimacion <- svyby(formula = ~M500_M, by = ~Sexo + Estrato2, design = disMAE_Sexo , FUN = svymean)

#  Sexo   Estrato2   M500_M       se
# Hombre.Estatal    Hombre    Estatal 545.0589 3.080072
# Mujer.Estatal      Mujer    Estatal 534.8360 2.945809
# Hombre.No estatal Hombre No estatal 580.7998 7.712171
# Mujer.No estatal   Mujer No estatal 569.8986 9.380232

mujer_no_estatal = estimacion$M500_M[estimacion$Estrato2 == 'No estatal' & estimacion$Sexo=='Mujer']

mujer_estatal =  estimacion$M500_M[estimacion$Estrato2=='Estatal' & estimacion$Sexo=='Mujer']

(mujer_no_estatal - mujer_estatal) + z * sqrt(
                estimacion$se[estimacion$Estrato2=='No estatal' & estimacion$Sexo=='Mujer'] ^ 2 +
                estimacion$se[estimacion$Estrato2=='Estatal' & estimacion$Sexo=='Mujer'] ^ 2)  * c(-1, 1)

#15.79248 54.33288

hombre_no_estatal = estimacion$M500_M[estimacion$Estrato2 == 'No estatal' & estimacion$Sexo=='Hombre']
hombre_estatal =  estimacion$M500_M[estimacion$Estrato2=='Estatal' & estimacion$Sexo=='Hombre']

(hombre_no_estatal - hombre_estatal) + z * sqrt(
  estimacion$se[estimacion$Estrato2=='No estatal' & estimacion$Sexo=='Hombre'] ^ 2 +
    estimacion$se[estimacion$Estrato2=='Estatal' & estimacion$Sexo=='Hombre'] ^ 2)  * c(-1, 1)

# 19.46436 52.01733

