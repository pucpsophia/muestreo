install.packages("arrangements")
install.packages("combinat")
library("combinat")
require("arrangements")
options(digits = 5)


N=500
n=30
x = 2
alpha = 0.05
pbar = x/n
z= qnorm(1-alpha/2)
e=0.03

wald= ((z^2)*pbar*(1-pbar))/(e^2)
wald

wilson =((z^2*pbar*(1-pbar))*N)/((z*pbar*(1-pbar))+(e^2*(N-1)))
wilson

