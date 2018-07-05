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
  # c) Suponga que en lugar de haberse empleado este diseno para Lima Top, usted hubiese empleado un
  # muestreo ppt de 4 distrito sectores, para luego, encuestar a todas las obras de los sectores de Lima Top seleccionados.
  # Implemente este dise~no, reportando la proporcion del numero de obras de construccion en Lima Top que
  # hacen uso de la metodologia BIM, junto con su error estandar de estimacion estimado. Compare finalmente
  # los errores de estimacion de este diseno con los del anteriormente tomado.
# NOTA: Una vez que seleccione el sector, use la estimacion de DATAEX tomada para este sector a fin de
# imputar su proporcion del uso del BIM. En caso que el sector no halla sido seleccionado en DATAEX (es
# decir, cuando vea que el numero de obras encuestadas es 0), impute esta proporcion simulando ella de una
# distribucion Beta de parametros ?? = 2 y ?? = 8. (2.0 puntos)

install.packages("data.table")
install.packages("sampling")
install.packages("survey")
install.packages("foreign")

library(sampling)
library(survey)
library(data.table)
library(foreign)

setwd("F:/muestreo")

setwd("/Users/gvalderrama/Documents/muestreo/")

setwd("/Users/gregory/Documents/pucp/muestreo/")

dataset <- read.csv(file= "DATAEX.csv", header=TRUE, sep=";")

head(dataset)
str(dataset)

levels(dataset$ESTRATO)
levels(dataset$DISTRITO)
levels(dataset$SECTOR)



datatable = as.data.table(dataset)
datatable <- datatable[ order ( datatable$ESTRATO ) , ]


table(datatable$ESTRATO)
table(datatable$DISTRITO)
table(datatable$SECTOR)


datatable [ , NESTRATO := 0]
datatable [ , NDISTRITO := 0]
datatable [ , NSECTOR := 0]

str(datatable)

# check missing data 
nads = na.omit(datatable)
dim(datatable)
dim(nads)



# ================ TOTAL DE OBRAS POR ESTRATO =============================

NESTRATO_LIMA_TOP <- 423  # total obras por estrato
NESTRATO_LIMA_MODERNA <- 360  # total obras por estrato
NESTRATO_LIMA_CENTRO <- 98  # total  obras por estrato
NESTRATO_LIMA_ESTE <- 75  # total  obras por estrato
NESTRATO_LIMA_NORTE <- 134  # total  obras por estrato
NESTRATO_LIMA_SUR <- 94  # total  obras por estrato
NESTRATO_CALLAO <- 32  # total  obras por estrato
datatable[which(datatable$ESTRATO == "LIMA TOP", arr.ind=T), "NESTRATO" ] = NESTRATO_LIMA_TOP
datatable[which(datatable$ESTRATO == "LIMA MODERNA", arr.ind=T), "NESTRATO" ] = NESTRATO_LIMA_MODERNA
datatable[which(datatable$ESTRATO == "LIMA CENTRO", arr.ind=T), "NESTRATO" ] = NESTRATO_LIMA_CENTRO
datatable[which(datatable$ESTRATO == "LIMA ESTE", arr.ind=T), "NESTRATO" ] = NESTRATO_LIMA_ESTE
datatable[which(datatable$ESTRATO == "LIMA NORTE", arr.ind=T), "NESTRATO" ] = NESTRATO_LIMA_NORTE
datatable[which(datatable$ESTRATO == "LIMA SUR", arr.ind=T), "NESTRATO" ] = NESTRATO_LIMA_SUR
datatable[which(datatable$ESTRATO == "CALLAO", arr.ind=T), "NESTRATO" ] = NESTRATO_CALLAO


# ================ TOTAL DE OBRAS POR DISTRITO =============================

# DISTRITO LIMA TOP
datatable[which(datatable$ESTRATO == "LIMA TOP" & datatable$DISTRITO == "MIRAFLORES"), "NDISTRITO" ] = 133
datatable[which(datatable$ESTRATO == "LIMA TOP" & datatable$DISTRITO == "SAN ISIDRO"), "NDISTRITO" ] = 58
#  molina 
datatable[which(datatable$ESTRATO == "LIMA TOP" & datatable$DISTRITO == "SURCO"), "NDISTRITO" ] = 136
datatable[which(datatable$ESTRATO == "LIMA TOP" & datatable$DISTRITO == "SAN BORJA"), "NDISTRITO" ] = 53
datatable[which(datatable$ESTRATO == "LIMA TOP" & datatable$DISTRITO == "BARRANCO"), "NDISTRITO" ] = 34

# DISTRITO LIMA MODERNA

datatable[which(datatable$ESTRATO == "LIMA MODERNA" & datatable$DISTRITO == "JESUS MARIA"), "NDISTRITO" ] = 60
datatable[which(datatable$ESTRATO == "LIMA MODERNA" & datatable$DISTRITO == "LINCE"), "NDISTRITO" ] = 54
datatable[which(datatable$ESTRATO == "LIMA MODERNA" & datatable$DISTRITO == "MAGDALENA"), "NDISTRITO" ] = 64
datatable[which(datatable$ESTRATO == "LIMA MODERNA" & datatable$DISTRITO == "PUEBLO LIBRE"), "NDISTRITO" ] = 53
datatable[which(datatable$ESTRATO == "LIMA MODERNA" & datatable$DISTRITO == "SAN MIGUEL"), "NDISTRITO" ] = 82
datatable[which(datatable$ESTRATO == "LIMA MODERNA" & datatable$DISTRITO == "SURQUILLO"), "NDISTRITO" ] = 47

# DISTRITO LIMA CENTRO  

datatable[which(datatable$ESTRATO == "LIMA CENTRO" & datatable$DISTRITO == "CERCADO LIMA"), "NDISTRITO" ] = 24
datatable[which(datatable$ESTRATO == "LIMA CENTRO" & datatable$DISTRITO == "BRENIA"), "NDISTRITO" ] = 34
datatable[which(datatable$ESTRATO == "LIMA CENTRO" & datatable$DISTRITO == "LA VICTORIA"), "NDISTRITO" ] = 37
# san luis 0

# DISTRITO LIMA ESTE 

datatable[which(datatable$ESTRATO == "LIMA ESTE" & datatable$DISTRITO == "ATE"), "NDISTRITO" ] = 36
# Cieneguilla
# Chaclacayo
# Lurigancho
datatable[which(datatable$ESTRATO == "LIMA ESTE" & datatable$DISTRITO == "SANTA ANITA"), "NDISTRITO" ] = 7
datatable[which(datatable$ESTRATO == "LIMA ESTE" & datatable$DISTRITO == "EL AGUSTINO"), "NDISTRITO" ] = 3
# San Juan de Lurigancho

# DISTRITO NORTE 

datatable[which(datatable$ESTRATO == "LIMA NORTE" & datatable$DISTRITO == "CARABAYLLO"), "NDISTRITO" ] = 23
datatable[which(datatable$ESTRATO == "LIMA NORTE" & datatable$DISTRITO == "COMAS"), "NDISTRITO" ] = 42
datatable[which(datatable$ESTRATO == "LIMA NORTE" & datatable$DISTRITO == "LOS OLIVOS"), "NDISTRITO" ] = 57
# puente piedra
datatable[which(datatable$ESTRATO == "LIMA NORTE" & datatable$DISTRITO == "SMP"), "NDISTRITO" ] = 6
datatable[which(datatable$ESTRATO == "LIMA NORTE" & datatable$DISTRITO == "ANCON/INDEP"), "NDISTRITO" ] = 2

# DISTRITO SUR 

datatable[which(datatable$ESTRATO == "LIMA SUR" & datatable$DISTRITO == "CHORRILLOS"), "NDISTRITO" ] = 35
datatable[which(datatable$ESTRATO == "LIMA SUR" & datatable$DISTRITO == "LURIN/PACH/VES"), "NDISTRITO" ] = 13
datatable[which(datatable$ESTRATO == "LIMA SUR" & datatable$DISTRITO == "SAN JUAN MIRAFLORES"), "NDISTRITO" ] = 11
#Pucusana
datatable[which(datatable$ESTRATO == "LIMA SUR" & datatable$DISTRITO == "PUNTA HERMOSA/NEGRA"), "NDISTRITO" ] = 13
datatable[which(datatable$ESTRATO == "LIMA SUR" & datatable$DISTRITO == "SAN BARTOLO"), "NDISTRITO" ] = 14

# Santa María del Mar


# DISTRITO CALLAO
datatable[which(datatable$ESTRATO == "CALLAO" & datatable$DISTRITO == "BELLAVISTA"), "NDISTRITO" ] = 10
datatable[which(datatable$ESTRATO == "CALLAO" & datatable$DISTRITO == "CALLAO"), "NDISTRITO" ] = 14
# La perla
datatable[which(datatable$ESTRATO == "CALLAO" & datatable$DISTRITO == "VENTANILLA"), "NDISTRITO" ] = 2


# ================ TOTAL DE OBRAS POR SECTOR =============================


# DISTRITO LIMA TOP

datatable[which(datatable$ESTRATO == "LIMA TOP" & datatable$DISTRITO == "MIRAFLORES" & datatable$SECTOR == "A" ), "NSECTOR" ] = 34
datatable[which(datatable$ESTRATO == "LIMA TOP" & datatable$DISTRITO == "MIRAFLORES" & datatable$SECTOR == "B" ), "NSECTOR" ] = 77
datatable[which(datatable$ESTRATO == "LIMA TOP" & datatable$DISTRITO == "MIRAFLORES" & datatable$SECTOR == "C" ), "NSECTOR" ] = 23

datatable[which(datatable$ESTRATO == "LIMA TOP" & datatable$DISTRITO == "SAN ISIDRO" & datatable$SECTOR == "A" ), "NSECTOR" ] = 49
datatable[which(datatable$ESTRATO == "LIMA TOP" & datatable$DISTRITO == "SAN ISIDRO" & datatable$SECTOR == "B" ), "NSECTOR" ] = 9
#  molina 
datatable[which(datatable$ESTRATO == "LIMA TOP" & datatable$DISTRITO == "SURCO" & datatable$SECTOR == "A" ), "NSECTOR" ] = 66
datatable[which(datatable$ESTRATO == "LIMA TOP" & datatable$DISTRITO == "SURCO" & datatable$SECTOR == "B" ), "NSECTOR" ] = 50
datatable[which(datatable$ESTRATO == "LIMA TOP" & datatable$DISTRITO == "SURCO" & datatable$SECTOR == "C" ), "NSECTOR" ] = 20

datatable[which(datatable$ESTRATO == "LIMA TOP" & datatable$DISTRITO == "SAN BORJA" & datatable$SECTOR == "A" ), "NSECTOR" ] = 19
datatable[which(datatable$ESTRATO == "LIMA TOP" & datatable$DISTRITO == "SAN BORJA" & datatable$SECTOR == "B" ), "NSECTOR" ] = 34
datatable[which(datatable$ESTRATO == "LIMA TOP" & datatable$DISTRITO == "SAN BORJA" & datatable$SECTOR == "C" ), "NSECTOR" ] = 1

datatable[which(datatable$ESTRATO == "LIMA TOP" & datatable$DISTRITO == "BARRANCO" & datatable$SECTOR == "A" ), "NSECTOR" ] = 34


# DISTRITO LIMA MODERNA


datatable[which(datatable$ESTRATO == "LIMA MODERNA" & datatable$DISTRITO == "JESUS MARIA" & datatable$SECTOR == "A" ), "NSECTOR" ] = 31
datatable[which(datatable$ESTRATO == "LIMA MODERNA" & datatable$DISTRITO == "JESUS MARIA" & datatable$SECTOR == "B" ), "NSECTOR" ] = 24
datatable[which(datatable$ESTRATO == "LIMA MODERNA" & datatable$DISTRITO == "JESUS MARIA" & datatable$SECTOR == "C" ), "NSECTOR" ] = 6

datatable[which(datatable$ESTRATO == "LIMA MODERNA" & datatable$DISTRITO == "LINCE" & datatable$SECTOR == "A" ), "NSECTOR" ] = 54

datatable[which(datatable$ESTRATO == "LIMA MODERNA" & datatable$DISTRITO == "MAGDALENA" & datatable$SECTOR == "A" ), "NSECTOR" ] = 34
datatable[which(datatable$ESTRATO == "LIMA MODERNA" & datatable$DISTRITO == "MAGDALENA" & datatable$SECTOR == "C" ), "NSECTOR" ] = 30

datatable[which(datatable$ESTRATO == "LIMA MODERNA" & datatable$DISTRITO == "PUEBLO LIBRE" & datatable$SECTOR == "A" ), "NSECTOR" ] = 22
datatable[which(datatable$ESTRATO == "LIMA MODERNA" & datatable$DISTRITO == "PUEBLO LIBRE" & datatable$SECTOR == "B" ), "NSECTOR" ] = 17
datatable[which(datatable$ESTRATO == "LIMA MODERNA" & datatable$DISTRITO == "PUEBLO LIBRE" & datatable$SECTOR == "C" ), "NSECTOR" ] = 13

datatable[which(datatable$ESTRATO == "LIMA MODERNA" & datatable$DISTRITO == "SAN MIGUEL" & datatable$SECTOR == "A" ), "NSECTOR" ] = 19
datatable[which(datatable$ESTRATO == "LIMA MODERNA" & datatable$DISTRITO == "SAN MIGUEL" & datatable$SECTOR == "B" ), "NSECTOR" ] = 19
datatable[which(datatable$ESTRATO == "LIMA MODERNA" & datatable$DISTRITO == "SAN MIGUEL" & datatable$SECTOR == "C" ), "NSECTOR" ] = 44


datatable[which(datatable$ESTRATO == "LIMA MODERNA" & datatable$DISTRITO == "SURQUILLO" & datatable$SECTOR == "A" ), "NSECTOR" ] = 13
datatable[which(datatable$ESTRATO == "LIMA MODERNA" & datatable$DISTRITO == "SURQUILLO" & datatable$SECTOR == "B" ), "NSECTOR" ] = 6
datatable[which(datatable$ESTRATO == "LIMA MODERNA" & datatable$DISTRITO == "SURQUILLO" & datatable$SECTOR == "C" ), "NSECTOR" ] = 28



# DISTRITO LIMA CENTRO  

datatable[which(datatable$ESTRATO == "LIMA CENTRO" & datatable$DISTRITO == "CERCADO LIMA" & datatable$SECTOR == "A" ), "NSECTOR" ] = 14
datatable[which(datatable$ESTRATO == "LIMA CENTRO" & datatable$DISTRITO == "CERCADO LIMA" & datatable$SECTOR == "B" ), "NSECTOR" ] = 10


datatable[which(datatable$ESTRATO == "LIMA CENTRO" & datatable$DISTRITO == "BRENIA" & datatable$SECTOR == "A" ), "NSECTOR" ] = 23
datatable[which(datatable$ESTRATO == "LIMA CENTRO" & datatable$DISTRITO == "BRENIA" & datatable$SECTOR == "B" ), "NSECTOR" ] = 4
datatable[which(datatable$ESTRATO == "LIMA CENTRO" & datatable$DISTRITO == "BRENIA" & datatable$SECTOR == "C" ), "NSECTOR" ] = 6

datatable[which(datatable$ESTRATO == "LIMA CENTRO" & datatable$DISTRITO == "LA VICTORIA" & datatable$SECTOR == "A" ), "NSECTOR" ] = 37

# san luis 0


# DISTRITO LIMA ESTE 

datatable[which(datatable$ESTRATO == "LIMA ESTE" & datatable$DISTRITO == "ATE" & datatable$SECTOR == "A" ), "NSECTOR" ] = 36

# Cieneguilla
# Chaclacayo
# Lurigancho

datatable[which(datatable$ESTRATO == "LIMA ESTE" & datatable$DISTRITO == "SANTA ANITA" & datatable$SECTOR == "A" ), "NSECTOR" ] = 7
datatable[which(datatable$ESTRATO == "LIMA ESTE" & datatable$DISTRITO == "EL AGUSTINO" & datatable$SECTOR == "A" ), "NSECTOR" ] = 3
# San Juan de Lurigancho

# DISTRITO NORTE 

datatable[which(datatable$ESTRATO == "LIMA NORTE" & datatable$DISTRITO == "CARABAYLLO" & datatable$SECTOR == "A" ), "NSECTOR" ] = 23
datatable[which(datatable$ESTRATO == "LIMA NORTE" & datatable$DISTRITO == "COMAS" & datatable$SECTOR == "A" ), "NSECTOR" ] = 42

datatable[which(datatable$ESTRATO == "LIMA NORTE" & datatable$DISTRITO == "LOS OLIVOS" & datatable$SECTOR == "A" ), "NSECTOR" ] = 10
datatable[which(datatable$ESTRATO == "LIMA NORTE" & datatable$DISTRITO == "LOS OLIVOS" & datatable$SECTOR == "B" ), "NSECTOR" ] = 44
datatable[which(datatable$ESTRATO == "LIMA NORTE" & datatable$DISTRITO == "LOS OLIVOS" & datatable$SECTOR == "C" ), "NSECTOR" ] = 3

# puente piedra
datatable[which(datatable$ESTRATO == "LIMA NORTE" & datatable$DISTRITO == "SMP" & datatable$SECTOR == "A" ), "NSECTOR" ] = 6
datatable[which(datatable$ESTRATO == "LIMA NORTE" & datatable$DISTRITO == "ANCON/INDEP" & datatable$SECTOR == "A" ), "NSECTOR" ] = 4

# DISTRITO SUR 


datatable[which(datatable$ESTRATO == "LIMA SUR" & datatable$DISTRITO == "CHORRILLOS" & datatable$SECTOR == "A" ), "NSECTOR" ] = 5
datatable[which(datatable$ESTRATO == "LIMA SUR" & datatable$DISTRITO == "CHORRILLOS" & datatable$SECTOR == "C" ), "NSECTOR" ] = 30

datatable[which(datatable$ESTRATO == "LIMA SUR" & datatable$DISTRITO == "LURIN/PACH/VES" & datatable$SECTOR == "A" ), "NSECTOR" ] = 13
datatable[which(datatable$ESTRATO == "LIMA SUR" & datatable$DISTRITO == "SAN JUAN MIRAFLORES" & datatable$SECTOR == "A" ), "NSECTOR" ] = 11
#Pucusana
datatable[which(datatable$ESTRATO == "LIMA SUR" & datatable$DISTRITO == "PUNTA HERMOSA/NEGRA" & datatable$SECTOR == "A" ), "NSECTOR" ] = 13
datatable[which(datatable$ESTRATO == "LIMA SUR" & datatable$DISTRITO == "SAN BARTOLO" & datatable$SECTOR == "A" ), "NSECTOR" ] = 14
# Santa María del Mar


# DISTRITO CALLAO

datatable[which(datatable$ESTRATO == "CALLAO" & datatable$DISTRITO == "BELLAVISTA" & datatable$SECTOR == "A" ), "NSECTOR" ] = 10
datatable[which(datatable$ESTRATO == "CALLAO" & datatable$DISTRITO == "CALLAO" & datatable$SECTOR == "A" ), "NSECTOR" ] = 14
datatable[which(datatable$ESTRATO == "CALLAO" & datatable$DISTRITO == "VENTANILLA" & datatable$SECTOR == "A" ), "NSECTOR" ] = 2
# La perla


table(datatable$NESTRATO)
table(datatable$NDISTRITO)
table(datatable$NSECTOR)



alpha = 0.05
e = 0.05


# W1 probabilidad de selecionar el estrato
# W2 probabilidad de seleccionar el distrito dato que selecionamos el estrato
# w3 probabilidad de seleccionar el sector dado que seleccionamos el distrito y el estrato


NTotal_Estrato = sum(NESTRATO_LIMA_TOP, NESTRATO_LIMA_MODERNA, NESTRATO_LIMA_CENTRO, NESTRATO_LIMA_ESTE, NESTRATO_LIMA_NORTE, NESTRATO_LIMA_SUR, NESTRATO_CALLAO)

datatable [ , FPC := NESTRATO ]
datatable [ , FPC2 := NDISTRITO ]

datatable [ , WEIGHT_I := 1 / (NESTRATO / NTotal_Estrato) ]
datatable [ , WEIGHT_J_I := 1 / (NDISTRITO / NESTRATO) ]
datatable [ , WEIGHT_K_J_I := 1 / (NSECTOR / NDISTRITO) ]
datatable [ , WEIGHT := (WEIGHT_I * WEIGHT_J_I * WEIGHT_K_J_I)]


# order by strato


design = svydesign(id=~DISTRITO + SECTOR, fpc = ~FPC + FPC2 , strata = ~ESTRATO, data = datatable, weights = ~WEIGHT)

design

# estaimacion por metodo tradicional linealizacion
options(survey.lonely.psu = "adjust")
options(survey.lonely.psu = "certainty")
mean = svymean(~BIM, design = design, deff = T)
confint(mean)


# estyimacion BRR 
jkn = as.svrepdesign(design=design, type="JKn")
svymean(~BIM, design = jkn, deff = T)
# bootstrap 
boot = as.svrepdesign(design=design, type="subbootstrap")
svymean(~BIM, design = boot, deff = T)


top_sample = datatable[which(datatable$ESTRATO == "LIMA TOP" ),  ] 
top_design = svydesign(id=~DISTRITO + SECTOR, fpc = ~FPC + FPC2 , data = top_sample, weights = ~WEIGHT)
top_mean = svymean(~BIM, design = top_design, deff = T)
confint(top_mean)

# part c 

datatable
datatable [ , nest := .N , by = 'ESTRATO']

datatable [, Fact:=interaction(ESTRATO,DISTRITO, SECTOR)]



data(api)
apipop[1:2,]
dim(apipop)[1] # 6194

apistrat[1:2, ]
sum(unique(apistrat$fpc)) # 4421 1018 755 = 6194
table(apistrat$stype) # 200
dim(apistrat)[1] # 200
head(apistrat)

unique(apiclus1$fpc) # 757
dim(apiclus1)[1] # 183
apiclus1$dnum
head(apiclus1)

dim(apiclus2) # 126
unique(apiclus2$fpc1) # 757
table(apiclus2$fpc2) # 

