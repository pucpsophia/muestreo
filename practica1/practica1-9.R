
install.packages("arrangements")
install.packages("combinat")
library("combinat")
require("arrangements")

install.packages("rootSolve")
library(rootSolve)
options(digits = 5)


N = 500
alpha = 0.05
pbar = 2/30
z = qnorm(1-alpha/2)
e = 0.03

wald = ( ( z ^ 2 * pbar * ( 1 - pbar ) ) * N )/( ( z ^ 2 * pbar * ( 1 - pbar ) ) + ( e ^ 2 * ( N - 1 )))
wald

#Función de Wilson 
fun_wilson <- function (n) {
  (4 * ( ((z ^ 2) * (N - n)) / (n * (N -1) ) )*pbar + ((z ^ 2) * (N - n) / (n * (N -1)))^ (2) 
   -4 * ((z ^ 2) * (N - n) / (n * (N -1) )) * ( pbar ^ 2))^(1/2)/(2*(1+(z^2*(N-n)/(n*(N-1))))) - e  
}
wilson = uniroot.all(fun_wilson, c(0, 500))

c(wald, wilson)

fun_wald <- function (n) (((((z ^ 2) * (N - n)) / (n * (N - 1))) ^ (1 / 2)) * ( - (4 * (p ^ 2)) + (4 * p)) ^ (1 / 2)) / 2 - e


uniroot.all(fun_wald, c(0, 500))




