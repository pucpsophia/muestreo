install.packages("arrangements")
install.packages("combinat")
library("combinat")
require("arrangements")
options(digits = 5)

movies <- read.csv(file="F:/muestreo/movies.csv", header=TRUE, sep=",")

head(movies)

indexes <-sample(x=nrow(movies),size=10,replace=FALSE)

sample_movie =  movies[indexes, ]

sample_mean =as.numeric( apply(sample_movie[ , 3:12],1, sd))

hist(sample_mean)

pob_var = 0.27

# muestra 

#a) Hallar el tamaño de la muestra
e=0.01
z=1.96

n<-((z**2) * (pob_var)*250)/( (z**2)*(pob_var) + ((e**2)*250))
print(n)

#muestra aleatoria


#sobre las puntuaciones de cada película
p1<-rep(c(1:10),c(3127,463,413,502,998,2110,7538,17547,22598,36493))

hist(p1)

p2<-rep(c(1:10),c(12307,4323,6004,1085,21228,53434,144929,300423,356058,389870))
p3<-rep(c(1:10),c(2593,769,1067,1778,4232,12031,37473,74680,62964,60119))
p4<-rep(c(1:10),c(15830,5649,7116,10665,21850,49119,140039,324975,511533,610427))
p5<-rep(c(1:10),c(2294,882,1165,1806,4366,11364,32032,56447,46498,47029))
p6<-rep(c(1:10),c(1648,368,335,425,856,1708,5276,9913,11990,24896))
p7<-rep(c(1:10),c(4986,1896,2555,4275,10271,29092,90005,175112,134488,79853))
p8<-rep(c(1:10),c(7406,1538,1785,2139,4198,8316,22499,47407,69776,103819))
p9<-rep(c(1:10),c(2481,627,787,1117,2424,4665,12441,23059,21916,22324))
p10<-rep(c(1:10),c(4972,1723,2326,4297,10479,28893,97892,206195,208905,187649))
p11<-rep(c(1:10),c(5553,3282,4888,8281,17241,38787,85987,140494,131566,143494))
p12<-rep(c(1:10),c(1795,551,614,1009,2407,6122,18944,34857,26936,24081))

#Cálculo desviación estándar de cada película de la muestra del piloto
#varianza poblacional
varp1<-c((length(p1)-1)/length(p1)*var(p1),(length(p2)-1)/length(p2)*var(p2),(length(p3)-1)/length(p3)*var(p3),(length(p4)-1)/length(p4)*var(p4),(length(p5)-1)/length(p5)*var(p5),(length(p6)-1)/length(p6)*var(p6),(length(p7)-1)/length(p7)*var(p7),(length(p8)-1)/length(p8)*var(p8),(length(p9)-1)/length(p9)*var(p9),(length(p10)-1)/length(p10)*var(p10),(length(p11)-1)/length(p11)*var(p11),(length(p12)-1)/length(p12)*var(p12))

print(varp1)

#Varianza del piloto
sd_p<-sqrt(varp1)
print(sd_p)

varf<-c((length(sd_p)-1)/length(sd_p)*var(sd_p))
print(varf)  


#Calcular tamaño de la muestra (n)
n<-(z^2*(varf)*250)/(z^2*(varf)+(e^2)*250)
print(n)


