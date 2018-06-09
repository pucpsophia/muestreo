library(data.table)
library(sampling)
library(survey)
setwd('D:/Archivos/Maestría/Ciclo 1/Técnicas de Muestreo/Lista2')
load("ce2s16Cz.rdata")
ce2s16Cz=as.data.table(ce2s16Cz)
#Limpiar----
ce2s16Cz=ce2s16Cz[!is.na(ce2s16Cz$M500_M),]
ce2s16Cz=ce2s16Cz[!is.na(ce2s16Cz$Sexo),]
ce2s16Cz=ce2s16Cz[!is.na(ce2s16Cz$Area),]
ce2s16Cz=ce2s16Cz[!is.na(ce2s16Cz$Gestion),]
ce2s16Cz[,Sexo:=droplevels(Sexo)]
#Crear estrato----
ce2s16Cz[,Estrato:=interaction(Area,Gestion)]
#Crear fpc----
ce2s16Cz[,fpc:=.N,by=Estrato]
#Tamaño de estratos
Nh=as.numeric(table(ce2s16Cz$Estrato))
#Muestra piloto
sigmah=numeric(4)
set.seed(351681)
sigmah[1] = sd(ce2s16Cz$M500_M[ce2s16Cz$Estrato==levels(ce2s16Cz$Estrato)[1]][sample(Nh[1],10)])
sigmah[2] = sd(ce2s16Cz$M500_M[ce2s16Cz$Estrato==levels(ce2s16Cz$Estrato)[2]][sample(Nh[2],10)])
sigmah[3] = sd(ce2s16Cz$M500_M[ce2s16Cz$Estrato==levels(ce2s16Cz$Estrato)[3]][sample(Nh[3],10)])
sigmah[4] = sd(ce2s16Cz$M500_M[ce2s16Cz$Estrato==levels(ce2s16Cz$Estrato)[4]][sample(Nh[4],10)])
#Hallar proporciones
ah = Nh*sigmah/sum(Nh*sigmah)
#Hallar tamaños de muestras
e=5
d = nrow(ce2s16Cz)*e/qnorm(0.975)
n = sum(((Nh*sigmah)^2)/ah)/(d^2 + sum(Nh*sigmah^2))
nh = pmax(round(ah*n),2)
#Ordenar Base según estratos
ce2s16Cz=ce2s16Cz[order(ce2s16Cz$Estrato),]
#muestreo
set.seed(65452)
m=sampling::strata(data = ce2s16Cz,stratanames = c("Estrato"),size=nh,method="srswor")
samp = getdata(ce2s16Cz,m)
#Diseño
disMAE = svydesign(id=~1,strata=~Estrato,fpc=~fpc,data=samp)
#Variables para la aplicación de dominios (Sexo)----
# N_d=table(ce2s16Cz$Sexo) #Estimar
N_h=table(ce2s16Cz$Estrato)
n_h=nh
# N_dh=table(ce2s16Cz$Sexo,ce2s16Cz$Estrato) #Estimar
# mu_dh=ce2s16Cz[,mean(M500_M),by=.(Sexo,Estrato)]
# var_dh=ce2s16Cz[,var(M500_M),by=.(Sexo,Estrato)]
# mu_d=ce2s16Cz[,mean(M500_M),by=.(Sexo)]
N_dh=table(samp$Sexo,samp$Estrato)*matrix(c(N_h,N_h),nrow = 2,byrow = T)/matrix(c(nh,nh),nrow = 2,byrow = T)
N_d=rowSums(N_dh)
mu_dh=samp[,mean(M500_M),by=.(Sexo,Estrato)]
var_dh=samp[,var(M500_M),by=.(Sexo,Estrato)]
#Esperanza de la media para los dominios----
mu_d=numeric(2)
names(mu_d)=c('Hombre','Mujer')
dom='Hombre'
mu_d[dom]=sum((N_h/n_h)*samp[Sexo==dom,sum(M500_M),by=Estrato]$V1)/sum((N_h/n_h)*samp[Sexo==dom,length(M500_M),by=Estrato]$V1)
dom='Mujer'
mu_d[dom]=sum((N_h/n_h)*samp[Sexo==dom,sum(M500_M),by=Estrato]$V1)/sum((N_h/n_h)*samp[Sexo==dom,length(M500_M),by=Estrato]$V1)
#Varianza para los dominios (Sexo)----
var_dom=numeric(2)
names(var_dom)=c('Hombre','Mujer')
dom='Hombre'
var_dom[dom]=(1/N_d[dom]^2)*sum((N_h^2/n_h)*(1-n_h/N_h)*(((N_dh[dom,]-1)/(N_h-1))*var_dh[Sexo==dom,V1]+(N_dh[dom,]/(N_h-1))*(1-N_dh[dom,]/N_h)*(mu_dh[Sexo==dom,V1]-mu_d[dom])^2))
dom='Mujer'
var_dom[dom]=(1/N_d[dom]^2)*sum((N_h^2/n_h)*(1-n_h/N_h)*(((N_dh[dom,]-1)/(N_h-1))*var_dh[Sexo==dom,V1]+(N_dh[dom,]/(N_h-1))*(1-N_dh[dom,]/N_h)*(mu_dh[Sexo==dom,V1]-mu_d[dom])^2))
#sd por svyby----
svyby(formula = ~M500_M,by = ~Sexo,design = disMAE,FUN = svymean)
data.frame(Sexo=c('Hombre','Mujer'),M500_M=mu_d,se=sqrt(var_dom))
#Lo que creo que el profesor hizo en el solpar... Creo que está super mal----
aggregate(samp$M500_M,by=list(samp$Sexo),FUN=sd)
#Pregunta b y c
#Colapsar Estratos
ce2s16Cz[,Estrato2:=as.factor(c('Estatal','Estatal','No estatal','No estatal')[match(Estrato,c('Urbana.Estatal','Rural.Estatal','Urbana.No estatal','Rural.No estatal'))])]
ce2s16Cz[,fpc2:=.N,by=Estrato2]
samp[,Estrato2:=ce2s16Cz$Estrato2[ID_unit]]
samp[,fpc2:=ce2s16Cz$fpc2[ID_unit]]

disMAE2 = svydesign(id=~1,strata=~Estrato2,fpc=~fpc2,data=samp)
#Variables para la aplicación de dominios (Sexo)----
# N_d=table(ce2s16Cz$Sexo) #Estimar
n_h=table(samp$Estrato2)
N_h=table(ce2s16Cz$Estrato2)
# N_dh=table(ce2s16Cz$Sexo,ce2s16Cz$Estrato) #Estimar
# mu_dh=ce2s16Cz[,mean(M500_M),by=.(Sexo,Estrato)]
# var_dh=ce2s16Cz[,var(M500_M),by=.(Sexo,Estrato)]
# mu_d=ce2s16Cz[,mean(M500_M),by=.(Sexo)]
N_dh=table(samp$Sexo,samp$Estrato2)*matrix(c(N_h,N_h),nrow = 2,byrow = T)/matrix(c(n_h,n_h),nrow = 2,byrow = T)
N_d=rowSums(N_dh)
mu_dh=samp[,mean(M500_M),by=.(Sexo,Estrato2)]
var_dh=samp[,var(M500_M),by=.(Sexo,Estrato2)]
#Esperanza de la media para los dominios----
mu_d=numeric(2)
names(mu_d)=c('Hombre','Mujer')
dom='Hombre'
mu_d[dom]=sum((N_h/n_h)*samp[Sexo==dom,sum(M500_M),by=Estrato2]$V1)/sum((N_h/n_h)*samp[Sexo==dom,length(M500_M),by=Estrato2]$V1)
dom='Mujer'
mu_d[dom]=sum((N_h/n_h)*samp[Sexo==dom,sum(M500_M),by=Estrato2]$V1)/sum((N_h/n_h)*samp[Sexo==dom,length(M500_M),by=Estrato2]$V1)
#Varianza para los dominios (Sexo)----
var_dom=numeric(2)
names(var_dom)=c('Hombre','Mujer')
dom='Hombre'
var_dom[dom]=(1/N_d[dom]^2)*sum((N_h^2/n_h)*(1-n_h/N_h)*(((N_dh[dom,]-1)/(N_h-1))*var_dh[Sexo==dom,V1]+(N_dh[dom,]/(N_h-1))*(1-N_dh[dom,]/N_h)*(mu_dh[Sexo==dom,V1]-mu_d[dom])^2))
dom='Mujer'
var_dom[dom]=(1/N_d[dom]^2)*sum((N_h^2/n_h)*(1-n_h/N_h)*(((N_dh[dom,]-1)/(N_h-1))*var_dh[Sexo==dom,V1]+(N_dh[dom,]/(N_h-1))*(1-N_dh[dom,]/N_h)*(mu_dh[Sexo==dom,V1]-mu_d[dom])^2))
#sd por svyby----
svyby(formula = ~M500_M,by = ~Sexo,design = disMAE2,FUN = svymean)
data.frame(Sexo=c('Hombre','Mujer'),M500_M=mu_d,se=sqrt(var_dom))
#Resta de medias----
mu_dh
var_dh


