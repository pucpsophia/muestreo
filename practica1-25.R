install.packages("arrangements")
install.packages("combinat")
library("combinat")
require("arrangements")
options(digits = 5)
getwd()

movie_path <- file.path(getwd(), "Documents", "studio" , "muestreo", "movies.csv")
movies <- read.csv(file=movie_path, header=TRUE, sep=",")
head(movies)

indexes <-sample(x=nrow(movies),size=12,replace=FALSE)

sample_movie =  movies[indexes, ]

mean_sample_function <- function(vec){
    d <- rep(c(1:10), vec)
    m <- mean(d)
    s2 <- (1/length(d)) * sum ( (d - m) **2)
    res <- sqrt(s2)
    return ( res )
}

mean_sample_function2 <- function(vec){
    d <- rep(c(1:10), vec)
    s2 = ((length(d)-1)/length(d))*var(d)
    res <- sqrt(s2)
    return ( res )
}

sample_sd <- apply(sample_movie[, 3:12],1, mean_sample_function)

# sample_var = ((length(sample_vars)-1)/length(sample_vars))*var(sample_vars)

sample_mean <- mean(sample_sd)
sample_var <-  (1/length(sample_sd)) * sum ( (sample_sd - sample_mean) **2)
sample_sd <- sqrt(sample_var)

N <- 250
e <- 0.1
alpha = 0.05
z= qnorm(1-alpha/2)

n <- ( z ^ 2 * ( sample_var ) * N)/( z ^ 2 * ( sample_var )+( e ^ 2 ) * N )

print(n)


# ================= Part B ===============================

# n = 14 
indexes <-sample(x=nrow(movies), size=14, replace=FALSE)

sample_movie =  movies[indexes, ]

head(sample_movie)

sample_sd <- apply(sample_movie[, 3:12],1, mean_sample_function)

sample_mean <- mean(sample_sd)

sample_var <-  (1/length(sample_sd)) * sum ( (sample_sd - sample_mean) **2)

N <- 250
n <- 14
alpha = 0.05
z= qnorm(1-alpha/2)
e = 0.1

error  <- ( z * sqrt(sample_var)/ sqrt(n) ) * sqrt( 1 - n/N )

lim_inf <- sample_mean - error

lim_sup <- sample_mean + error

result <- c( lim_inf, sample_mean, lim_sup)

print(result)

# ================= Part C ===============================

whiplash<-rep(c(1:10), as.numeric(movies[movies$name == 'Whiplash', 3:12]))
#  2989 1410 2029 3244 6710 18499 63151 148482 177511 120836

e_mean = mean(whiplash)

e_var = (1/length(whiplash)) * sum ( (whiplash - e_mean) **2)

e_sd = sqrt(e_var)

print(e_sd)



