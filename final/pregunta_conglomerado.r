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
  # muestreo ppt de 4 distrito sectores, para luego, encuestar a todas las obras de los sectores (A, B, C) de Lima Top seleccionados.
  # Implemente este diseno, reportando la proporcion del numero de obras de construccion en Lima Top que
  # hacen uso de la metodologia BIM, junto con su error estandar de estimacion estimado. Compare finalmente
  # los errores de estimacion de este diseno con los del anteriormente tomado.
# NOTA: Una vez que seleccione el distrito sector, use la estimacion de DATAEX tomada para este sector a fin de
# imputar su proporcion del uso del BIM. En caso que el sector no halla sido seleccionado en DATAEX (es
# decir, cuando vea que el numero de obras encuestadas es 0), impute esta proporcion simulando ella de una
# distribucion Beta de parametros alpha = 2 y beta = 8. (2.0 puntos)


setwd("F:/muestreo")

setwd("/Users/gvalderrama/Documents/muestreo/")

setwd("/Users/gregory/Documents/pucp/muestreo/")

install.packages("data.table")
install.packages("sampling")
install.packages("survey")
install.packages("foreign")

library(sampling)
library(survey)
library(data.table)
library(foreign)

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

fpc1 = 14 # total de obras en lima top  
fpc2 = # numero de obras SECTOR

weights = 
  
ids=~CONG+NUM


Parte A  


Stratificado - Conglomerado Bietapico

Strata = ESTRATO 
ids=~CONG+NUM












  

ids=~CONG+NUM, strata=ESTRATO 

head(datatable)
str(datatable)
datatable [ , NESTRATO := 0]
datatable [ , NDISTRITO := 0]
datatable [ , NSECTOR := 0]
datatable [ , NCONG := 0]

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
# Cieneguilla # Chaclacayo # Lurigancho
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
# Santa Maria del Mar
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
# Cieneguilla # Chaclacayo # Lurigancho
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
# Santa Maria del Mar
# DISTRITO CALLAO
datatable[which(datatable$ESTRATO == "CALLAO" & datatable$DISTRITO == "BELLAVISTA" & datatable$SECTOR == "A" ), "NSECTOR" ] = 10
datatable[which(datatable$ESTRATO == "CALLAO" & datatable$DISTRITO == "CALLAO" & datatable$SECTOR == "A" ), "NSECTOR" ] = 14
datatable[which(datatable$ESTRATO == "CALLAO" & datatable$DISTRITO == "VENTANILLA" & datatable$SECTOR == "A" ), "NSECTOR" ] = 2
# La perla

#  Valores conglomerados por la variables CONG
datatable[which(datatable$ESTRATO == "LIMA TOP" & datatable$SECTOR == "A" ), "NCONG" ] =  208
datatable[which(datatable$ESTRATO == "LIMA TOP" & datatable$SECTOR == "B" ), "NCONG" ] =  172
datatable[which(datatable$ESTRATO == "LIMA TOP" & datatable$SECTOR == "C" ), "NCONG" ] =  44

datatable[which(datatable$ESTRATO == "LIMA MODERNA" & datatable$SECTOR == "A" ), "NCONG" ] =  173
datatable[which(datatable$ESTRATO == "LIMA MODERNA" & datatable$SECTOR == "B" ), "NCONG" ] =  66
datatable[which(datatable$ESTRATO == "LIMA MODERNA" & datatable$SECTOR == "C" ), "NCONG" ] =  121

datatable[which(datatable$ESTRATO == "LIMA CENTRO" & datatable$SECTOR == "A" ), "NCONG" ] =  77
datatable[which(datatable$ESTRATO == "LIMA CENTRO" & datatable$SECTOR == "B" ), "NCONG" ] =  14
datatable[which(datatable$ESTRATO == "LIMA CENTRO" & datatable$SECTOR == "C" ), "NCONG" ] =  6

datatable[which(datatable$ESTRATO == "LIMA ESTE" & datatable$SECTOR == "A" ), "NCONG" ] =  75
datatable[which(datatable$ESTRATO == "LIMA NORTE" & datatable$SECTOR == "A" ), "NCONG" ] =  89
datatable[which(datatable$ESTRATO == "LIMA NORTE" & datatable$SECTOR == "B" ), "NCONG" ] =  44
datatable[which(datatable$ESTRATO == "LIMA NORTE" & datatable$SECTOR == "C" ), "NCONG" ] =  3

datatable[which(datatable$ESTRATO == "LIMA SUR" & datatable$SECTOR == "A" ), "NCONG" ] =  64
datatable[which(datatable$ESTRATO == "LIMA SUR" & datatable$SECTOR == "C" ), "NCONG" ] =  30

datatable[which(datatable$ESTRATO == "CALLAO" & datatable$SECTOR == "A" ), "NCONG" ] =  32

head(datatable)
# Validation 
unique(datatable[which(datatable$ESTRATO == "LIMA TOP" ), "NCONG" ])  # 208, 172

datatable_con = datatable
datatable_con [ , FPC := NESTRATO ] # numero de obras por estrato
datatable_con [ , FPC2 := NCONG ] # numero de obras por distrito estrato


datatable [ , WEIGHT_I := 1 / (NESTRATO / NTotal_Estrato) ]
datatable [ , WEIGHT_J_I := 1 / (NDISTRITO / NESTRATO) ]
datatable [ , WEIGHT := (WEIGHT_I * WEIGHT_J_I)]


con_design = svydesign(id=~CONG, strato=~ESTRATO, fpc = ~FPC2 , data = datatable_con)
mean = svymean(~BIM, con_design )

# PART B 

svyby(formula = ~BIM,by = ~ESTRATO, design = con_design, FUN = svymean)

# ASUMIENDO MODELO COMPLEJO dos etapas 


datatable [ , FPC := NESTRATO ]
datatable [ , FPC2 := NCONG ]

datatable [ , WEIGHT_I := 1 / (NESTRATO / NTotal_Estrato) ]
datatable [ , WEIGHT_J_I := 1 / (NSECTOR / NCONG ) ]
datatable [ , WEIGHT := (WEIGHT_I * WEIGHT_J_I)]

# order by strato
design = svydesign(id=~CONG , fpc=~FPC2, strata = ~ESTRATO, data = datatable, weights = ~WEIGHT)
svymean(~BIM, design)


# asumiendo modelo complejo tres etapas

table(datatable$NESTRATO)
table(datatable$NDISTRITO)
table(datatable$NSECTOR)

# W1 probabilidad de selecionar el estrato
# W2 probabilidad de seleccionar el distrito dato que selecionamos el estrato
# w3 probabilidad de seleccionar el sector dado que seleccionamos el distrito y el estrato

NTotal_Estrato = sum(NESTRATO_LIMA_TOP, NESTRATO_LIMA_MODERNA, NESTRATO_LIMA_CENTRO, NESTRATO_LIMA_ESTE,
                     NESTRATO_LIMA_NORTE, NESTRATO_LIMA_SUR, NESTRATO_CALLAO)

datatable [ , FPC := NESTRATO ]
datatable [ , FPC2 := NCONG ]

datatable [ , WEIGHT_I := 1 / (NESTRATO / NTotal_Estrato) ]
datatable [ , WEIGHT_J_I := 1 / (NDISTRITO / NESTRATO) ]
datatable [ , WEIGHT_K_J_I := 1 / (NSECTOR / NDISTRITO) ]
datatable [ , WEIGHT := (WEIGHT_I * WEIGHT_J_I * WEIGHT_K_J_I)]

# order by strato
design = svydesign(id=~DISTRITO + SECTOR, fpc = ~FPC + FPC2 , strata = ~ESTRATO, data = datatable, weights = ~WEIGHT)
design
# estaimacion por metodo tradicional linealizacion
options(survey.lonely.psu = "adjust")
mean = svymean(~BIM, design = design, deff = T)
confint(mean)
# estimacion JKN 
jkn = as.svrepdesign(design=design, type="JKn")
svymean(~BIM, design = jkn, deff = T)
# estimacion bootstrap 
boot = as.svrepdesign(design=design, type="subbootstrap")
svymean(~BIM, design = boot, deff = T)

# PART B 

top_sample = datatable[which(datatable$ESTRATO == "LIMA TOP" ),  ] 
top_design = svydesign(id=~DISTRITO + SECTOR, fpc = ~FPC + FPC2 , data = top_sample, weights = ~WEIGHT)
top_mean = svymean(~BIM, design = top_design, deff = T)
top_mean
confint(top_mean)

# ---    part c 

ppt_sample  = datatable[ which(datatable$ESTRATO == "LIMA TOP"), c("NUM","BIM","ESTRATO", "DISTRITO", "SECTOR", "CONG", "NDISTRITO", "NSECTOR") ] 
ppt_sample [ , imputation := 0 ]
# imputation miraflores c 133 obras en el distrito, 23 obras en sector C
n_distrito_sector = 23
prop <- rbeta(1, 2, 8)
prop_yes <- ceiling(n_distrito_sector * prop)
prop_no <- n_distrito_sector - prop_yes
for (index in rep(1,prop_yes)) {
  ppt_sample = rbind(ppt_sample, list( nrow(ppt_sample) + 1, "SI", "LIMA TOP", "MIRAFLORES", "C", "MIRAFLORESC", 133, n_distrito_sector, 1 ) )        
}
for (index in rep(1,prop_no)) {
  ppt_sample = rbind(ppt_sample, list( nrow(ppt_sample) + 1, "NO", "LIMA TOP", "MIRAFLORES", "C", "MIRAFLORESC", 133, n_distrito_sector, 1 ) )        
}
# imputation molina A 9 obras en el distrito, 6 obras en sector A
n_distrito_sector = 6
prop <- rbeta(1, 2, 8)
prop_yes <- ceiling(n_distrito_sector * prop)
prop_no <- n_distrito_sector - prop_yes
for (index in rep(1,prop_yes)) {
  ppt_sample = rbind(ppt_sample, list( nrow(ppt_sample) + 1, "SI", "LIMA TOP", "MOLINA", "A", "MOLINAA", 9, n_distrito_sector, 1 ) )        
}
for (index in rep(1,prop_no)) {
  ppt_sample = rbind(ppt_sample, list( nrow(ppt_sample) + 1, "NO", "LIMA TOP", "MOLINA", "A", "MOLINAA", 9, n_distrito_sector, 1 ) )        
}

# imputation molina B 9 obras en el distrito, 2 obras en sector B
n_distrito_sector = 2
prop <- rbeta(1, 2, 8)
prop_yes <- ceiling(n_distrito_sector * prop)
prop_no <- n_distrito_sector - prop_yes
for (index in rep(1,prop_yes)) {
  ppt_sample = rbind(ppt_sample, list( nrow(ppt_sample) + 1, "SI", "LIMA TOP", "MOLINA", "B", "MOLINAB", 9, n_distrito_sector, 1 ) )        
}
for (index in rep(1,prop_no)) {
  ppt_sample = rbind(ppt_sample, list( nrow(ppt_sample) + 1, "NO", "LIMA TOP", "MOLINA", "B", "MOLINAB", 9, n_distrito_sector, 1 ) )        
}

# imputation santiago de surco C 136 obras en el distrito, 20 obras en sector C
n_distrito_sector = 20
prop <- rbeta(1, 2, 8)
prop_yes <- ceiling(n_distrito_sector * prop)
prop_no <- n_distrito_sector - prop_yes
for (index in rep(1,prop_yes)) {
  ppt_sample = rbind(ppt_sample, list( nrow(ppt_sample) + 1, "SI", "LIMA TOP", "SURCO", "C", "SURCOC", 136, n_distrito_sector, 1 ) )        
}
for (index in rep(1,prop_no)) {
  ppt_sample = rbind(ppt_sample, list( nrow(ppt_sample) + 1, "NO", "LIMA TOP", "SURCO", "C", "SURCOC", 136, n_distrito_sector, 1 ) )        
}

# imputation san borja C 53 obras en el distrito, 1 obras en sector C
n_distrito_sector = 1
prop <- rbeta(1, 2, 8)
prop_yes <- ceiling(n_distrito_sector * prop)
prop_no <- n_distrito_sector - prop_yes
for (index in rep(1,prop_yes)) {
  ppt_sample = rbind(ppt_sample, list( nrow(ppt_sample) + 1, "SI", "LIMA TOP", "SAN BORJA", "C", "SAN BORJAC", 53, n_distrito_sector, 1 ) )        
}
for (index in rep(1,prop_no)) {
  ppt_sample = rbind(ppt_sample, list( nrow(ppt_sample) + 1, "NO", "LIMA TOP", "SAN BORJA", "C", "SAN BORJAC", 53, n_distrito_sector, 1 ) )        
}

# limpieza de factores
ppt_sample$CONG = factor(ppt_sample$CONG)
ppt_sample <- ppt_sample[order( ppt_sample$CONG ) , ]
data.frame(table(ppt_sample$CONG))[ ,2 ]
DIS_SECTOR = sort(sapply(unique(ppt_sample$CONG), as.character))
TAM_SECTOR = vector(length = length(DIS_SECTOR))
BIM_SI_SECTOR = vector(length = length(DIS_SECTOR))
BIM_NO_SECTOR = vector(length = length(DIS_SECTOR))
PROP_SECTOR = vector(length = length(DIS_SECTOR))
i = 1
for (sector in DIS_SECTOR) {
  TAM_SECTOR[i] = dim(ppt_sample[ which(ppt_sample$CONG == sector), ])[1]
  BIM_SI_SECTOR[i] = dim(ppt_sample[ which(ppt_sample$CONG == sector & ppt_sample$BIM == "SI"), ])[1]  
  BIM_NO_SECTOR[i] = dim(ppt_sample[ which(ppt_sample$CONG == sector & ppt_sample$BIM == "NO"), ])[1]
  PROP_SECTOR[i] =   BIM_SI_SECTOR[i]  / (BIM_SI_SECTOR[i] + BIM_NO_SECTOR[i]) # calculate proportions
  i = i + 1
}

# funcion hallar probabilidades de primer y segundo orden
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

ppt_obras =data.frame(DIS_SECTOR, TAM_SECTOR, BIM_SI_SECTOR, BIM_NO_SECTOR,  PROP_SECTOR)
probs = pisppt(as.numeric( TAM_SECTOR ),4)
index = UPrandomsystematic(probs[[1]])
ppt_obras_sample = getdata(ppt_obras, index)
ppt_obras_pik = getdata(probs[[1]], as.logical(index))[, "data"]

bim_result = HTestimator(ppt_obras_sample[,"PROP_SECTOR"], ppt_obras_pik) / 14
bim_result

pik_2 =  probs[[2]][as.logical(index), as.logical(index)]
diag(pik_2) = ppt_obras_pik 
sta =  sqrt(varHT(ppt_obras_sample[,"PROP_SECTOR"] , pik_2)) / 14

alpha = 0.05
z <- qnorm ( 1 - alpha / 2 )

c(bim_result - z * sta, bim_result + sta)



PIK <- inclusionprobabilities(TAM_SECTOR, 4)
p_1[[1]]


sum(PIK) # 4



PIK2 = UPsystematicpi2(PIK)
index = UPrandomsystematic(PIK)
ppt_obras_sample = getdata(ppt_obras, index)
bim_result = HTestimator(ppt_obras_sample[,"PROP_SECTOR"], ppt_obras_sample[,"PIK"]) 
pik2_index = PIK2[as.logical(index), as.logical(index)]
# stimated error sqrt of variance
sta =  sqrt(varHT(ppt_obras_sample[,"PROP_SECTOR"] , pik2_index, 2))











  
sum(subset(data.frame(table(ppt_sample$CONG)), Freq > 0)[ ,2 ])

droplevels(ppt_sample, exclude = if(anyNA(levels(ppt_sample)))

ppt_sample [ , fpc := .N , by = Estrato]




dim(ppt_sample)
head(ppt_sample)

mstage(datatable, stage = list("cluster", "cluster"), varnames = list("DISTRITO", "SECTOR"), size = list())

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

rbeta(1, 2, 8)
runif(2)
