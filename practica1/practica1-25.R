install.packages("arrangements")
install.packages("combinat")
library("combinat")
require("arrangements")
options(digits = 5)

setwd("f:/muestreo")

movies <- read.csv(file= "movies.csv", header=TRUE, sep=",")

head(movies)

indexes_test <-sample(x=nrow(movies),size=12,replace=FALSE)

sample_movie_test =  movies[indexes_test, ]

head(sample_movie_test)

sample_function <- function(vec){
  d <- rep(c(1:10), vec)
  m <- mean(d)
  s2 <- (1/ (length(d) -1) ) * sum ( (d - m) **2)
  res <- sqrt(s2)
  return ( res )
}

sample_test_sd_vec <- apply(sample_movie_test[, 3:12],1, sample_function)
sample_test_mean <- mean(sample_test_sd_vec)
sample_test_var <-  (1/length(sample_test_sd_vec)) * sum ( (sample_test_sd_vec - sample_test_mean) **2)

N <- 250
e <- 0.1
alpha = 0.05
z= qnorm(1-alpha/2)

n_muestra <- ( z ^ 2 * ( sample_test_var ) * N)/( z ^ 2 * ( sample_test_var ) + ( e ^ 2 ) * N )

print(n_muestra)

# ================= Part B ===============================

N <- 250
n <- 15 # n de parte a
alpha = 0.05
z= qnorm(1-alpha/2)
e = 0.1


indexes <-sample(x=nrow(movies),size=n,replace=FALSE)

sample_movie =  movies[indexes, ]
sample_movie

sample_sd_vec <- apply(sample_movie[, 3:12],1, sample_function)
sample_mean <- mean(sample_sd_vec)
sample_var <-  (1/length(sample_sd_vec)) * sum ( (sample_sd_vec - sample_mean) **2)



error  <- ( z * sqrt(sample_var)/ sqrt(n) ) * sqrt( 1 - n/N )

lim_inf <- sample_mean - error
lim_sup <- sample_mean + error

print(paste0( "mean ", sample_mean))
print(paste0( "lim inf ", lim_inf))
print(paste0( "lim sup ", lim_sup))


# ================= Part C ===============================

whiplash<-rep(c(1:10), as.numeric(movies[movies$name == 'Whiplash', 3:12]))

#  2989 1410 2029 3244 6710 18499 63151 148482 177511 120836

e_mean = mean(whiplash)

e_var = (1/length(whiplash)) * sum ( (whiplash - e_mean) **2)

e_sd = sqrt(e_var)

print(e_sd)



