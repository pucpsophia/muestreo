install.packages("arrangements")
install.packages("combinat")
library("combinat")
require("arrangements")

install.packages("rootSolve")
library(rootSolve)
options(digits = 5)


N = 500
n = 30
x = 2
alpha = 0.05
pbar = x/nß
z = qnorm(1-alpha/2)
e = 0.03

wald = ( ( z ^ 2 * pbar * ( 1 - pbar ) ) * N )/( ( z ^ 2 * pbar * ( 1 - pbar ) ) + ( e ^ 2 * ( N - 1 )))
wald


fun_wilson <- function (n) {
  4 * ( (z ^ 2) * (N - n) / (n * (N -1) ) ) + ((z ^ 2) * (N - n) / (n * (N -1) )) ^ 2 - 4 * ((z ^ 2) * (N - n) / (n * (N -1) )) * ( pbar ^ 2) - e  
} 

uniroot.all(fun_wilson, c(0, 500))


fun_wald <- function (n) (((((z ^ 2) * (N - n)) / (n * (N - 1))) ^ (1 / 2)) * ( - (4 * (p ^ 2)) + (4 * p)) ^ (1 / 2)) / 2 - e


uniroot.all(fun_wald, c(0, 500))
