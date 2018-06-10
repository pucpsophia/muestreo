install.packages("data.table")
install.packages("sampling")
install.packages("survey")
install.packages("foreign")

library(data.table)
library(sampling)
library(survey)
library(foreign)

setwd("F:/muestreo")

# caso cuzco

load("ce2s16Cz.rdata")

ce2s16Cz=as.data.table(ce2s16Cz)

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


Nh = as.numeric ( table ( ce2s16Cz$Estrato ) )

set.seed(45235)

sigma_h = vector(length = 4)

estratos = levels(ce2s16Cz$Estrato)

sigma_h[1] = sd ( ce2s16Cz$M500_M [ ce2s16Cz$Estrato == estratos[1] ] [sample(Nh[1],10)] )
sigma_h[2] = sd ( ce2s16Cz$M500_M [ ce2s16Cz$Estrato == estratos[2] ] [sample(Nh[2],10)] )
sigma_h[3] = sd ( ce2s16Cz$M500_M [ ce2s16Cz$Estrato == estratos[3] ] [sample(Nh[3],10)] )
sigma_h[4] = sd ( ce2s16Cz$M500_M [ ce2s16Cz$Estrato == estratos[4] ] [sample(Nh[4],10)] )


ah = Nh * sigma_h / sum ( Nh * sigma_h )

N <- nrow(ce2s16Cz)

e <- 5

alpha = 0.05

z <- qnorm ( 1 - alpha / 2 )

d <- N * e / z

n <- sum( ( ( Nh * sigma_h ) ^ 2 ) / ah ) / ( d ^ 2 + sum( Nh * sigma_h ^ 2 ) )

nh <- ceiling ( ah * n )

ce2s16Cz = ce2s16Cz[ order ( ce2s16Cz$Estrato ) , ]

m <- sampling::strata(ce2s16Cz, c("Estrato"), size= nh, method="srswor")

muestra <- getdata( ce2s16Cz , m)

N_d = table(ce2s16Cz$Sexo)

N_h = table(ce2s16Cz$Estrato)

n_h = nh

# variables de dominio Sexo 

N_d = table(ce2s16Cz$Sexo)

N_h = table(ce2s16Cz$Estrato)

n_h = nh

N_dh = table( muestra$Sexo, muestra$Estrato ) * matrix( c(N_h,N_h), nrow = 2, byrow = T ) / matrix( c(nh,nh) , nrow = 2, byrow = T)

N_d = rowSums(N_dh)

mu_dh = muestra[,mean(M500_M),by=.(Sexo,Estrato)]

var_dh = muestra[,var(M500_M),by=.(Sexo,Estrato)]

#Varianza para los dominios (Sexo)----

mu_d=numeric(2)

names(mu_d)=c('Hombre','Mujer')

dom='Hombre'

mu_d[dom]=sum((N_h/n_h)*muestra[Sexo==dom,sum(M500_M),by=Estrato]$V1)/sum((N_h/n_h)*muestra[Sexo==dom,length(M500_M),by=Estrato]$V1)

dom='Mujer'

mu_d[dom]=sum((N_h/n_h)*muestra[Sexo==dom,sum(M500_M),by=Estrato]$V1)/sum((N_h/n_h)*muestra[Sexo==dom,length(M500_M),by=Estrato]$V1)


var_dom=numeric(2)

names(var_dom)=c('Hombre','Mujer')

dom='Hombre'

var_dom[dom]=(1/N_d[dom]^2)*sum((N_h^2/n_h)*(1-n_h/N_h)*(((N_dh[dom,]-1)/(N_h-1))*var_dh[Sexo==dom,V1]+(N_dh[dom,]/(N_h-1))*(1-N_dh[dom,]/N_h)*(mu_dh[Sexo==dom,V1]-mu_d[dom])^2))

dom='Mujer'

var_dom[dom]=(1/N_d[dom]^2)*sum((N_h^2/n_h)*(1-n_h/N_h)*(((N_dh[dom,]-1)/(N_h-1))*var_dh[Sexo==dom,V1]+(N_dh[dom,]/(N_h-1))*(1-N_dh[dom,]/N_h)*(mu_dh[Sexo==dom,V1]-mu_d[dom])^2))

# error estandar de estimacion

sqrt(var_dom)

# parte B

# Colapsar Estratos

ce2s16Cz [ , Estrato2:= as.factor( c('Estatal', 'Estatal', 'No estatal', 'No estatal') [ match(Estrato,c('Urbana.Estatal','Rural.Estatal','Urbana.No estatal','Rural.No estatal'))])]

ce2s16Cz [ , fpc2 := .N, by=Estrato2]

muestra[ , Estrato2:= ce2s16Cz$Estrato2[ ID_unit ] ]

muestra[ , fpc2:= ce2s16Cz$fpc2[ ID_unit ] ]

disMAE2 = svydesign(id=~1, strata=~Estrato2, fpc=~fpc2, data=muestra)

mu_d=numeric(2)

names(mu_d) <- c('Hombre','Mujer')

mean_data_frame <- as.data.frame(svyby(formula = ~M500_M,by = ~Sexo,design = disMAE2,FUN = svymean))

mu_d['Mujer'] <- as.numeric(mean_data_frame[ c(which(mean_data_frame$Sexo == "Mujer")) , "M500_M"]) 

mu_d['Hombre'] <- as.numeric(mean_data_frame[ c(which(mean_data_frame$Sexo == "Mujer")) , "M500_M"]) 


# Varianza para los dominios (Sexo)

var_dom=numeric(2)

names(var_dom) <- c('Hombre','Mujer')

dom='Hombre'

var_dom[dom] <- ( 1 / N_d[dom]^2)*sum((N_h^2/n_h)*(1-n_h/N_h)*(((N_dh[dom,]-1)/(N_h-1))*var_dh[Sexo==dom,V1]+(N_dh[dom,]/(N_h-1))*(1-N_dh[dom,]/N_h)*(mu_dh[Sexo==dom,V1]-mu_d[dom])^2))

dom <- 'Mujer'

var_dom[dom] <- ( 1 / N_d[dom]^2)*sum((N_h^2/n_h)*(1-n_h/N_h)*(((N_dh[dom,]-1)/(N_h-1))*var_dh[Sexo==dom,V1]+(N_dh[dom,]/(N_h-1))*(1-N_dh[dom,]/N_h)*(mu_dh[Sexo==dom,V1]-mu_d[dom])^2))


# IC

Y_mujer <- mu_dh[ Sexo=='Mujer', V1]

n_dh <- table(muestra$Sexo,muestra$Estrato2)

var_Y_mujer <- (1-n_dh[2,] / N_dh[2,]) * var_dh [ Sexo == 'Mujer',V1] / n_dh[2,]

c((Y_mujer[1] - Y_mujer[2]) - qnorm(0.975) * sqrt(sum(var_Y_mujer)), (Y_mujer[1] - Y_mujer[2]) + qnorm(0.975) * sqrt(sum(var_Y_mujer)))


Y_hombre <-  mu_dh[Sexo=='Hombre',V1]

n_dh <- table( muestrap$Sexo, muestra$Estrato2)

var_Y_hombre <- (1-n_dh[2,] / N_dh[2,]) * var_dh[Sexo=='Hombre',V1] / n_dh[2,]

c ( (Y_hombre[1] - Y_hombre[2]) - qnorm(0.975) * sqrt ( sum(var_Y_hombre ) ), (Y_hombre[1]-Y_hombre[2]) + qnorm(0.975) * sqrt ( sum(var_Y_hombre ) ) ) 


