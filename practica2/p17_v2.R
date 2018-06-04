library(Hmisc)
library(sampling)
library(survey)
library(data.table)
load(file="D:\\Archivos\\Maestría\\Ciclo 1\\Técnicas de Muestreo\\Lista2\\IMdb.rda")
database=as.data.table(database)
#Crear estrato
database$Estrato=cut2(database$Anho,cuts = c(1970,1980,1990))
#Crear variables
database$Mean_Score=apply(database,1,function(x) mean(as.numeric(rep(10:1,x[5:14]))))
database$Var_Score=apply(database,1,function(x) sd(as.numeric(rep(10:1,x[5:14]))))
database[,fpc:=.N,by=Estrato]
#Tamaño de estratos
Nh=as.numeric(table(database$Estrato))
#Muestra piloto
sigmah=numeric(4)
set.seed(65435)
sigmah[1] = sd(database$Var_Score[database$Estrato==levels(database$Estrato)[1]][sample(Nh[1],10)])
sigmah[2] = sd(database$Var_Score[database$Estrato==levels(database$Estrato)[2]][sample(Nh[2],10)])
sigmah[3] = sd(database$Var_Score[database$Estrato==levels(database$Estrato)[3]][sample(Nh[3],10)])
sigmah[4] = sd(database$Var_Score[database$Estrato==levels(database$Estrato)[4]][sample(Nh[4],10)])
#Hallar proporciones
ah = Nh*sigmah/sum(Nh*sigmah)
#Hallar tamaños de muestras
e=0.1
d = nrow(database)*e/qnorm(0.975)
n = sum(((Nh*sigmah)^2)/ah)/(d^2 + sum(Nh*sigmah^2))
nh = pmax(round(ah*n),2)
#Ordenar Base según estratos
database=database[order(database$Estrato),]
#muestreo
set.seed(46512)
m=strata(data = database,stratanames = c("Estrato"),size=nh,method="srswor")
samp = getdata(database,m)
#Diseño
disMAE = svydesign(id=~1,strata=~Estrato,fpc=~fpc,data=samp)
#Hallar media y se
tabla2=svymean(x = ~Var_Score,design = disMAE)
#Aproximación de Satterthwaite para los grados de libertad
ch=Nh*(Nh-nh)/nh
df=sum(ch*sigmah^2)^2/sum((ch*sigmah^2)^2/(nh-1))
#Cálculo del IC con la t
t0.975=qt(0.975,df)
tabla2+as.numeric(SE(tabla2))*t0.975*c(-1,1)
#IC con la Z
tabla2+as.numeric(SE(tabla2))*qnorm(0.975)*c(-1,1)
confint(tabla2)