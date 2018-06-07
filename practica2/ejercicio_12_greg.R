library(data.table)
library(sampling)
library(survey)


dataset = read.spss("ce2s16Am.sav", to.data.frame=TRUE)

str(dataset)
dim(dataset)

Pop = dataset

unique(Pop$Area)  # Urbana Rural
unique(Pop$GestionE) # Estatal No estatal 

Pop$Estrato = interaction( Pop$Area, Pop$GestionE )

Pop = Pop[order(Pop$Estrato),]

table(Pop$Estrato)



# caso amazonas 
load("ce2s16Am.sav")


setwd("/Users/gregory/Documents/pucp/muestreo")

load("ce2s16Cz.rdata")

ce2s16Cz=as.data.table(ce2s16Cz)

#clean data

ce2s16Cz = ce2s16Cz[!is.na(ce2s16Cz$M500_M),]

ce2s16Cz = ce2s16Cz[!is.na(ce2s16Cz$Sexo),]

ce2s16Cz = ce2s16Cz[!is.na(ce2s16Cz$Area),]

ce2s16Cz = ce2s16Cz[!is.na(ce2s16Cz$Gestion),]

ce2s16Cz[,Sexo:=droplevels(Sexo)]

# Crear estrato----
ce2s16Cz[,Estrato:=interaction(Area,Gestion)]
#Crear fpc----
ce2s16Cz[,fpc:=.N,by=Estrato]
#Tama?o de estratos
Nh=as.numeric(table(ce2s16Cz$Estrato))
#Muestra piloto
sigmah=numeric(4)
set.seed(65435)
sigmah[1] = sd(ce2s16Cz$M500_M[ce2s16Cz$Estrato==levels(ce2s16Cz$Estrato)[1]][sample(Nh[1],10)])
sigmah[2] = sd(ce2s16Cz$M500_M[ce2s16Cz$Estrato==levels(ce2s16Cz$Estrato)[2]][sample(Nh[2],10)])
sigmah[3] = sd(ce2s16Cz$M500_M[ce2s16Cz$Estrato==levels(ce2s16Cz$Estrato)[3]][sample(Nh[3],10)])
sigmah[4] = sd(ce2s16Cz$M500_M[ce2s16Cz$Estrato==levels(ce2s16Cz$Estrato)[4]][sample(Nh[4],10)])
#Hallar proporciones
ah = Nh*sigmah/sum(Nh*sigmah)
#Hallar tama?os de muestras
e=5
d = nrow(ce2s16Cz)*e/qnorm(0.975)
n = sum(((Nh*sigmah)^2)/ah)/(d^2 + sum(Nh*sigmah^2))
nh = pmax(round(ah*n),2)
#Ordenar Base seg?n estratos
ce2s16Cz=ce2s16Cz[order(ce2s16Cz$Estrato),]
#muestreo
set.seed(46512)
m=strata(data = ce2s16Cz,stratanames = c("Estrato"),size=nh,method="srswor")
samp = getdata(ce2s16Cz,m)
#Dise?o
disMAE = svydesign(id=~1,strata=~Estrato,fpc=~fpc,data=samp)
#Variables para la aplicaci?n de dominios (Sexo)----
N_d=table(ce2s16Cz$Sexo)
N_h=table(ce2s16Cz$Estrato)
n_h=nh
N_dh=table(ce2s16Cz$Sexo,ce2s16Cz$Estrato)
mu_dh=ce2s16Cz[,mean(M500_M),by=.(Sexo,Estrato)]
var_dh=ce2s16Cz[,var(M500_M),by=.(Sexo,Estrato)]
mu_d=ce2s16Cz[,mean(M500_M),by=.(Sexo)]
#Varianza para los dominios (Sexo)----
var_dom=numeric(2)
names(var_dom)=c('Hombre','Mujer')
dom='Hombre'
var_dom[dom]=(1/N_d[dom]^2)*sum((N_h^2/n_h)*(1-n_h/N_h)*(((N_dh[dom,]-1)/(N_h-1))*var_dh[Sexo==dom,V1]+(N_dh[dom,]/(N_h-1))*(1-N_dh[dom,]/N_h)*(mu_dh[Sexo==dom,V1]-mu_d[Sexo==dom,V1])^2))
dom='Mujer'
var_dom[dom]=(1/N_d[dom]^2)*sum((N_h^2/n_h)*(1-n_h/N_h)*(((N_dh[dom,]-1)/(N_h-1))*var_dh[Sexo==dom,V1]+(N_dh[dom,]/(N_h-1))*(1-N_dh[dom,]/N_h)*(mu_dh[Sexo==dom,V1]-mu_d[Sexo==dom,V1])^2))
#sd por svyby----
svyby(formula = ~M500_M,by = ~Sexo,design = disMAE,FUN = svymean)
sqrt(var_dom) #Para compararlo con el que hallamos por la f?rmula de dominios
#Lo que creo que el profesor hizo en el solpar... Creo que est? super mal----
aggregate(samp$M500_M,by=list(samp$Sexo),FUN=sd)


