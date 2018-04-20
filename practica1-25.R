install.packages("arrangements")
install.packages("combinat")
library("combinat")
require("arrangements")
options(digits = 5)
getwd()

movie_path <- file.path(getwd(), "Documents", "pucp" , "muestreo", "movies.csv")

movies <- read.csv(file=movie_path, header=TRUE, sep=",")
head(movies)

indexes <-sample(x=nrow(movies),size=12,replace=FALSE)

sample_movie =  movies[indexes, ]

sample_movie

sample_function <- function(vec){
    d <- rep(c(1:10), vec)
    m <- mean(d)
    s2 <- (1/ (length(d) -1) ) * sum ( (d - m) **2)
    res <- sqrt(s2)
    return ( res )
}

sample_sd_vec <- apply(sample_movie[, 3:12],1, sample_function)

sample_mean <- mean(sample_sd_vec)
sample_var <-  (1/length(sample_sd_vec)) * sum ( (sample_sd_vec - sample_mean) **2)
sample_sd <- sqrt(sample_var)

N <- 250
e <- 0.1
alpha = 0.05
z= qnorm(1-alpha/2)

# duda cual es esta formula 
n <- ( z ^ 2 * ( sample_var ) * N)/( z ^ 2 * ( sample_var ) + ( e ^ 2 ) * N )

print(n)


# ================= Part B ===============================

N <- 250
n <- 14
alpha = 0.05
z= qnorm(1-alpha/2)
e = 0.1

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



# sample_var = ((length(sample_vars)-1)/length(sample_vars))*var(sample_vars)


# n-1/n S2

mean_sample_function2 <- function(vec){
  d <- rep(c(1:10), vec)
  s2 = ((length(d)-1)/length(d))*var(d)
  res <- sqrt(s2)
  return ( res )
}

