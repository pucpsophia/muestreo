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


movies_d <- cbind(movies, mean = c(apply(movies[,3:12] , 1, mean_function)))

str(movies_d)

unique(movies$estrato)



# muestra piloto 

piloto_estrato_1 <- movies_d[sample(which(movies_d$estrato == 1), 10), ]
piloto_estrato_2 <- movies_d[sample(which(movies_d$estrato == 2), 10), ]
piloto_estrato_3 <- movies_d[sample(which(movies_d$estrato == 3), 10), ]
piloto_estrato_4 <- movies_d[sample(which(movies_d$estrato == 4), 10), ]


sigma_h_estrato_1 = sd(piloto_estrato_1$mean)
sigma_h_estrato_2 = sd(piloto_estrato_2$mean)
sigma_h_estrato_3 = sd(piloto_estrato_3$mean)
sigma_h_estrato_4 = sd(piloto_estrato_4$mean)



ah = Nh*sigmah/sum(Nh*sigmah)