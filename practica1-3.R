install.packages("arrangements")
require("arrangements")
pob = c(13.9, 11.5, 16.7, 14.4, 14.6, 15.1)
combinations(x=pob, k =3, replace=TRUE)

p = permutations(x=pob, k =3, replace=TRUE)

m = matrix(p, nrow = 216, ncol = 3)

ncol(m)
nrow()

length(m[,1])

media = (matrix(m[,1]) + matrix(m[,2]) +matrix(m[,3]))/3

cbind(m, media[,1]) 


nrow((m[,1]))

 

media <- m$0 + m$1  + m$2  


choose(6,3)

mean(pob)

var(pob)


var(pob)*(length(pob)-1)/length(pob)

# con reemplazamiento

sample(pob, 3, replace=TRUE)






#ncol(combn(6,3))

combinations(5, 2, replace=TRUE)

t(combn(6,3))

rep(pob, 3)
expand.grid() 


