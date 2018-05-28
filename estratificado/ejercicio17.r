install.packages("arrangements")
install.packages("combinat")
install.packages("survey")
library("combinat")
require("arrangements")
options(digits = 5)
setwd("f:/muestreo")
movies <- read.csv(file= "movies.csv", header=TRUE, sep=",")
head(movies)

str(movies)

nrow(movies)

str(d)


mean_function <- function(vec){
  d <- rep(c(1:10), vec)
  return( mean(d))
}


movies[c(which(movies$year  < 1970)), 'estrato'] <- 1
movies[c(which(movies$year  >= 1970  & movies$year < 1980)), 'estrato'] <- 2
movies[c(which(movies$year  >= 1980  & movies$year < 1990)), 'estrato'] <- 3
movies[c(which(movies$year  >= 1990)), 'estrato'] <- 4


movies_d <- cbind( movies, mean = c(apply(movies[,3:12] , 1, mean_function)))

str(movies_d)


unique(movies$estrato)

movies[sample(which(movies$estrato == 1), 10), ]


means = apply(array, margin, ...)
movies_d[55, ]
(movies [55, "v1"] * 1 + movies [55, "v2"] * 2 + movies [55, "v3"] * 3 + movies [55, "v4"] * 4 +movies [55, "v5"] * 5 + movies [55, "v6"] * 6 + movies [55, "v7"] * 7 +movies [55, "v8"] * 8 + movies [55, "v9"] * 9 + movies [55, "v10"] * 10) / sum(movies[55, 3:12])

movies [55, ]
# muestra piloto 

piloto <-sample(x=nrow(movies),size=10,replace=FALSE)
sample_movie_test =  movies[piloto, ]


sigma_h_0070 = sd(Pop$M500_M[Pop$Estrato=="Urbana.Estatal"][sample(Nh[1],10)])
sigma_h_7080 = sd(Pop$M500_M[Pop$Estrato=="Urbana.Estatal"][sample(Nh[1],10)])
sigma_h_8090 = sd(Pop$M500_M[Pop$Estrato=="Urbana.Estatal"][sample(Nh[1],10)])
sigma_h_9000 = sd(Pop$M500_M[Pop$Estrato=="Urbana.Estatal"][sample(Nh[1],10)])


ah = Nh*sigmah/sum(Nh*sigmah)