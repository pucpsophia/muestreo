install.packages("arrangements")
install.packages("combinat")
install.packages("survey")
library("combinat")
require("arrangements")
options(digits = 5)
library(survey)
set.seed(100)

data(api)
apipop[1:2, ]

N <- dim(apipop)[1]
n <- 500

print(N)
print(n)

index  <- sample(N, n)
sample <- apipop[index, ] 

disMASs <- svydesign( id = ~1, fpc=rep(N,n), data = sample)

means = svymean(~api00+api99 , disMASs)

contr = svycontrast(means, c( api00=1, api99=-1))

contr

# ======== b ======

confint(contr)

# ======= c ========

Dbar  = mean(sample$api00 - sample$api99)
Sx2 = var(sample$api99)
Sy2 = var(sample$api00)

Sxy = cov(sample$api99, sample$api00)

e = 1.96*sqrt( ( 1- n/ N ) / n ) * sqrt( Sx2 + Sy2 - 2*Sxy)

c(Dbar - e, Dbar + e)






 


