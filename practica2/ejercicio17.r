install.packages("arrangements")
install.packages("combinat")
install.packages("survey")
install.packages("sampling")
install.packages("survey")

library(survey)
library(sampling)
library("combinat")
require("arrangements")
options(digits = 5)
setwd("f:/muestreo")
movies <- read.csv(file= "movies.csv", header=TRUE, sep=",")
head(movies)

str(movies)

nrow(movies)

str(d)


# pregunta 25 anterior 
# La Internet Movie Database (IMDb) es una base de datos en lnea que almacena informacion relacionada con peliculas, 
# personal de equipo de produccion (incluyendo directores y productores), actores, series de television, programas de television, videojuegos, actores de
# doblaje y, mas recientemente, personajes cticios que aparecen en los medios de entretenimiento
# visual. Recibe mas de 100 millones de usuarios unicos al mes y cuenta con una version
# movil. Una de sus secciones, \The IMDb Top 250" es destinada a ser un listado de las 250
# pelculas con mejor calicacion, basado en calicaciones de los usuarios registrados del sitio
# web. En esta seccion cada pelcula aparece con una estrella y un ranking de a lo mas 10 puntos.
# Debajo de este ranking uno puede acceder a las calicaciones otorgadas por los usuarios
# en forma de un histograma. La intencion de este miniproyecto es estimar, con un margen de
# error de a lo mas 0.1 puntos y un nivel de conanza del 95 %, la desviacion estandar media
# (como medida de controversia) de los rankings asignados a estas 250 pelculas.


# pregunta 17
# Considere el ejercicio 26 del captulo anterior y replique este estudio, pero ahora utilizando
# un MAE con asignacion de Neyman, donde su variable de estraticacion sera el a~no
# de la pelcula. Concretamente considere en un primer estrato a aquellas pelculas que sean
# anteriores a 1970, otro estrato con pelculas entre los 70 y anteriores a los 80, un tercer
# estrato con pelculas de entre los 80 y anteriores a los 90 y un estrato nal con pelculas de
# los 90 hasta la actualidad.

s_function <- function(vec){
  d <- rep(c(1:10), vec)
  m <- mean(d)
  s2 <- (1/ (length(d) -1) ) * sum ( (d - m) **2)
  res <- sqrt(s2)
  return ( res )
}


movies[c(which(movies$year  < 1970)), 'estrato'] <- 1
movies[c(which(movies$year  >= 1970  & movies$year < 1980)), 'estrato'] <- 2
movies[c(which(movies$year  >= 1980  & movies$year < 1990)), 'estrato'] <- 3
movies[c(which(movies$year  >= 1990)), 'estrato'] <- 4


movies_d <- cbind(movies, sd = c(apply(movies[,3:12] , 1, s_function)))

str(movies_d)


movies_d = movies_d[order(movies_d$estrato),]
table(movies_d$estrato)

# muestra piloto 

piloto_estrato_1 <- movies_d[sample(which(movies_d$estrato == 1), 10), ]
piloto_estrato_2 <- movies_d[sample(which(movies_d$estrato == 2), 10), ]
piloto_estrato_3 <- movies_d[sample(which(movies_d$estrato == 3), 10), ]
piloto_estrato_4 <- movies_d[sample(which(movies_d$estrato == 4), 10), ]





Nh_estrato_1  = nrow(movies_d[which(movies_d$estrato == 1),])
Nh_estrato_2  = nrow(movies_d[which(movies_d$estrato == 2),])
Nh_estrato_3  = nrow(movies_d[which(movies_d$estrato == 3),])
Nh_estrato_4  = nrow(movies_d[which(movies_d$estrato == 4),])

stopifnot( sum(Nh_estrato_1,  Nh_estrato_2, Nh_estrato_3, Nh_estrato_4) == 250)

sigma_h_estrato_1 = sd(piloto_estrato_1$sd)
sigma_h_estrato_2 = sd(piloto_estrato_2$sd)
sigma_h_estrato_3 = sd(piloto_estrato_3$sd)
sigma_h_estrato_4 = sd(piloto_estrato_4$sd)


sum_Nh = (Nh_estrato_1 * sigma_h_estrato_1) +  (Nh_estrato_2 * sigma_h_estrato_2) + (Nh_estrato_3 * sigma_h_estrato_3) + (Nh_estrato_4 * sigma_h_estrato_4)

ah_1 = ( Nh_estrato_1 * sigma_h_estrato_1)/sum_Nh
ah_2 = ( Nh_estrato_2 * sigma_h_estrato_2)/sum_Nh
ah_3 = ( Nh_estrato_3 * sigma_h_estrato_3)/sum_Nh
ah_4 = ( Nh_estrato_4 * sigma_h_estrato_4)/sum_Nh


N  = 250
e = 0.1
alpha = 0.05
z= qnorm(1-alpha/2)

d = N*e/z

sum_n_a =  ( Nh_estrato_1 * sigma_h_estrato_1 ) ^ 2 / ah_1 + ( Nh_estrato_2 * sigma_h_estrato_2 ) ^ 2 / ah_2 + ( Nh_estrato_3 * sigma_h_estrato_3 ) ^ 2 / ah_3 + ( Nh_estrato_4 * sigma_h_estrato_4 ) ^ 2 / ah_4
sum_n =   ( Nh_estrato_1 * sigma_h_estrato_1 ^ 2) + ( Nh_estrato_2 * sigma_h_estrato_2 ^ 2)  + ( Nh_estrato_3 * sigma_h_estrato_3 ^ 2)  + ( Nh_estrato_4 * sigma_h_estrato_4 ^ 2)  
n = sum_n_a/(d ^ 2 + sum_n)

nh_1 = ceiling(ah_1 * n)
nh_2 = ceiling(ah_2 * n)
nh_3 = ceiling(ah_3 * n)
nh_4 = ceiling(ah_4 * n)

# check if the value is 1 
# nh_2 = 2

# http://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
options(survey.lonely.psu="adjust")

m <- sampling::strata(movies_d, c("estrato"), size= c(nh_1, nh_2, nh_3, nh_4), method="srswor")
me16Am = getdata(movies_d,m)

table(is.na(me16Am$sd))

aux = data.frame(fpc = c(rep(Nh_estrato_1,nh_1),rep(Nh_estrato_2,nh_2), rep(Nh_estrato_3,nh_3), rep(Nh_estrato_4,nh_4)))
sampleMAE = cbind(num = 1:( nh_1+ nh_2 + nh_3 + nh_4  ),me16Am,aux)

dis16MAE = svydesign(id=~1,strata=~estrato,fpc=~fpc,data=sampleMAE)



means1 = svymean(~sd,dis16MAE, deff=T)

means1


