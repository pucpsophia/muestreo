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

# case cuzco

load("ce2s16Cz.rdata")

ce2s16Cz=as.data.table(ce2s16Cz)

# clean data

ce2s16Cz = ce2s16Cz[!is.na(ce2s16Cz$M500_M),]

ce2s16Cz = ce2s16Cz[!is.na(ce2s16Cz$Sexo),]

ce2s16Cz = ce2s16Cz[!is.na(ce2s16Cz$Area),]

ce2s16Cz = ce2s16Cz[!is.na(ce2s16Cz$Gestion),]

ce2s16Cz[, Sexo:= droplevels ( Sexo ) ]

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

muestra <- getdata( ce2s16Cz , m)

disMAE = svydesign( id = ~1, strata = ~Estrato, fpc = ~fpc, data = muestra)

# svy para dominios 

svyby(formula = ~M500_M,by = ~Sexo, design = disMAE, FUN = svymean)


# Parte B

# Colapsar Estratos

ce2s16Cz [ , Estrato2:= as.factor( c('Estatal', 'Estatal', 'No estatal', 'No estatal') [ match(Estrato,c('Urbana.Estatal','Rural.Estatal','Urbana.No estatal','Rural.No estatal'))])]

ce2s16Cz [ , fpc2 := .N, by=Estrato2]

muestra[ , Estrato2:= ce2s16Cz$Estrato2[ ID_unit ] ]

muestra[ , fpc2:= ce2s16Cz$fpc2[ ID_unit ] ]

disMAE2 = svydesign(id=~1, strata=~Estrato2, fpc=~fpc2, data=muestra)

n_h <- table(muestra$Estrato2)

N_h <- table(ce2s16Cz$Estrato2)

N_dh <- table( muestra$Sexo, muestra$Estrato2) * matrix( c( N_h , N_h ), nrow = 2, byrow = T) / matrix( c( n_h, n_h), nrow = 2, byrow = T)

N_d <- rowSums(N_dh)

mu_dh <- muestra[ , mean(M500_M) , by=.(Sexo,Estrato2) ]

var_dh <- muestra[ , var(M500_M) , by=.(Sexo,Estrato2)]

# Esperanza de la media 

mu_d=numeric(2)

names(mu_d) <- c('Hombre','Mujer')

dom='Hombre'

mu_d[dom] <- sum((N_h/n_h) * muestra[Sexo==dom,sum(M500_M),by=Estrato2]$V1)/sum((N_h/n_h) * muestra[Sexo==dom,length(M500_M),by=Estrato2]$V1)

dom='Mujer'

mu_d[dom] <- sum((N_h/n_h) * muestra[Sexo==dom,sum(M500_M),by=Estrato2]$V1)/sum((N_h/n_h) * muestra[Sexo==dom,length(M500_M),by=Estrato2]$V1)

# Varianza para los dominios (Sexo)

var_dom=numeric(2)

names(var_dom) <- c('Hombre','Mujer')

dom='Hombre'

var_dom[dom] <- (1/N_d[dom]^2)*sum((N_h^2/n_h)*(1-n_h/N_h)*(((N_dh[dom,]-1)/(N_h-1))*var_dh[Sexo==dom,V1]+(N_dh[dom,]/(N_h-1))*(1-N_dh[dom,]/N_h)*(mu_dh[Sexo==dom,V1]-mu_d[dom])^2))

dom <- 'Mujer'

var_dom[dom] <- (1/N_d[dom]^2)*sum((N_h^2/n_h)*(1-n_h/N_h)*(((N_dh[dom,]-1)/(N_h-1))*var_dh[Sexo==dom,V1]+(N_dh[dom,]/(N_h-1))*(1-N_dh[dom,]/N_h)*(mu_dh[Sexo==dom,V1]-mu_d[dom])^2))

# Hallamos un interalo de confianza

Y_mujer <- mu_dh[ Sexo=='Mujer', V1]

n_dh <- table(muestra$Sexo,muestra$Estrato2)

var_Y_mujer <- (1-n_dh[2,] / N_dh[2,]) * var_dh [ Sexo == 'Mujer',V1] / n_dh[2,]

c((Y_mujer[1] - Y_mujer[2]) - qnorm(0.975) * sqrt(sum(var_Y_mujer)), (Y_mujer[1] - Y_mujer[2]) + qnorm(0.975) * sqrt(sum(var_Y_mujer)))


Y_hombre <-  mu_dh[Sexo=='Hombre',V1]

n_dh <- table( muestrap$Sexo, muestra$Estrato2)

var_Y_hombre <- (1-n_dh[2,] / N_dh[2,]) * var_dh[Sexo=='Hombre',V1] / n_dh[2,]

c ( (Y_hombre[1] - Y_hombre[2]) - qnorm(0.975) * sqrt ( sum(var_Y_hombre ) ), (Y_hombre[1]-Y_hombre[2]) + qnorm(0.975) * sqrt ( sum(var_Y_hombre ) ) ) 






