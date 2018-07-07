# Un estudiante perteneciente a un internado desea estimar el promedio final medio que alcanzaron ́el 
# y sus compañeros en un curso de la institucio ́n. En lugar de obtener un listado de todos sus compan ̃eros y realizar un MASs,
# el se da cuenta que los alumnos de su institucio ́n esta ́n 
# distribuidos en 100 cuartos de 4 alumnos cada uno; por lo que decide seleccionar al azar 5 de estos cuartos y preguntarles a todos 
# los estudiantes en ellos, el puntaje promedio que obtuvieron en el curso. Los resultados son los siguientes

# alumno / cuarto
#   1 2 3 4 5  
# 1 15.4 11.8 10 15 13.4
# 2 13 15.2 12.8 14.4 9.6
# 3 17.2 16.4 12.6 17.2 16.4
# 4 15.2 13.4 9.4 18.2 16


# a) Obtenga la estimacio ́n buscada.
# b) Estime el error est ́andar de estimaci ́on en a).
# c) Obtenga una estimacio ́n de la correlacio ́n intraclase.

library(data.table)

p14=data.table(puntaje=c(15.4,11.8,10,15,13.4,13,15.2,12.8,14.4,9.6,17.2,16.4,12.6,
                         17.2,16.4,15.2,13.4,9.4,18.2,16),
               cuarto=rep(1:5,each=4))

#####a
K=400
N=100
M=rep(4,5)
n=length(p14[,unique(cuarto)])
m=p14[,.N,cuarto]$N

sum(N*M*p14$puntaje/(n*m))/K

vartot=sd(p14[,mean(puntaje),cuarto]$V1)**2
varest=(1-n/N)*vartot/n

sqrt(varest)

#####b

M=unique(M)
a_res=anova(object = lm(puntaje~as.factor(cuarto),data=p14))
scc=a_res$`Sum Sq`[1]
sct=sum(a_res$`Sum Sq`)
  
rho=1-(M/(M-1))*scc/sct
rho

#####c---deff

(N*M-1)*(1+rho*(M-1))/(M*(N-1))


