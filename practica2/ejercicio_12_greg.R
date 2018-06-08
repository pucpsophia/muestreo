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

setwd("/Users/gvalderrama/Documents/muestreo")
#setwd("/Users/gregory/Documents/pucp/muestreo")
dataset = read.spss("ce2s16Am.sav", to.data.frame=TRUE)

str(dataset)
dim(dataset)

Pop = dataset

unique(Pop$Area)  # Urbana Rural
unique(Pop$GestionE) # Estatal No estatal 

Pop$Estrato = interaction( Pop$Area, Pop$GestionE )

Pop = Pop[order(Pop$Estrato),]

table(Pop$Estrato)
options(internet.info = 0)

# case cuzco

load("ce2s16Cz.rdata")

ce2s16Cz=as.data.table(ce2s16Cz)

# clean data

ce2s16Cz = ce2s16Cz[!is.na(ce2s16Cz$M500_M),]

ce2s16Cz = ce2s16Cz[!is.na(ce2s16Cz$Sexo),]

ce2s16Cz = ce2s16Cz[!is.na(ce2s16Cz$Area),]

ce2s16Cz = ce2s16Cz[!is.na(ce2s16Cz$Gestion),]

ce2s16Cz[, Sexo:=droplevels(Sexo)]

str(ce2s16Cz$M500_M)
levels(ce2s16Cz$Area)
levels(ce2s16Cz$Gestion)
levels(ce2s16Cz$Sexo)

# Estratos

ce2s16Cz[, Estrato:=interaction(Area,Gestion)]

levels(ce2s16Cz$Estrato)

# Crear fpc

ce2s16Cz = ce2s16Cz[ order ( ce2s16Cz$Estrato ) , ]

ce2s16Cz [ , fpc := .N , by = Estrato]

# Longitud de los estratos 

Nh = as.numeric ( table ( ce2s16Cz$Estrato ) )

stopifnot( sum( Nh ) == nrow(ce2s16Cz) ) # validar estratos tienen la toda la poblacion

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

nh = ceiling(ah * n)

# Ordenar segun estratos

ce2s16Cz = ce2s16Cz[ order ( ce2s16Cz$Estrato ) , ]

#muestreo
set.seed(12345)

m <- sampling::strata(ce2s16Cz, c("Estrato"), size= nh, method="srswor")
samp <- getdata( ce2s16Cz , m)

# design

disMAE = svydesign( id = ~1, strata = ~Estrato, fpc = ~fpc, data=samp)

# svy package
svyby(formula = ~M500_M,by = ~Sexo, design = disMAE, FUN = svymean)

# variables de dominio Sexo 

N_d = table(ce2s16Cz$Sexo)

N_h = table(ce2s16Cz$Estrato)

n_h = nh

N_dh = table( ce2s16Cz$Sexo , ce2s16Cz$Estrato )

mu_dh = ce2s16Cz [ , mean(M500_M), by = c("Sexo", "Estrato") ] 

var_dh = ce2s16Cz [ , var(M500_M) , by = c("Sexo", "Estrato") ]

mu_d = ce2s16Cz[ , mean(M500_M), by = c("Sexo") ]

#Varianza para los dominios (Sexo)----
var_dom = vector( length = 2)

names(var_dom)=c('Hombre','Mujer')

dom = 'Hombre'

var_dom[dom] = ( 1 / N_d [ dom ] ^ 2 ) * sum ( ( N_h ^ 2 / n_h ) * ( 1 - n_h / N_h ) * (((N_dh[dom,]-1)/(N_h-1))*var_dh[Sexo==dom,V1]+(N_dh[dom,]/(N_h-1))*(1-N_dh[dom,]/N_h)*(mu_dh[Sexo==dom,V1]-mu_d[Sexo==dom,V1])^2))

dom='Mujer'

var_dom[dom]=(1/N_d[dom]^2)*sum((N_h^2/n_h)*(1-n_h/N_h)*(((N_dh[dom,]-1)/(N_h-1))*var_dh[Sexo==dom,V1]+(N_dh[dom,]/(N_h-1))*(1-N_dh[dom,]/N_h)*(mu_dh[Sexo==dom,V1]-mu_d[Sexo==dom,V1])^2))

sqrt(var_dom) # Para compararlo con el que hallamos por la f?rmula de dominios


#Lo que creo que el profesor hizo en el solpar... Creo que est? super mal----
# aggregate(samp$M500_M,by=list(samp$Sexo),FUN=sd)


