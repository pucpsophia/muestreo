install.packages("arrangements")
install.packages("combinat")
install.packages("survey")
install.packages("sampling")
install.packages("survey")

library(survey)
library(sampling)
library("combinat")
library("arrangements")



set.seed(12345)

options(digits = 5)
setwd("f:/muestreo")
movies <- read.csv(file= "movies.csv", header=TRUE, sep=",")
head(movies)
str(movies)
nrow(movies)
str(d)

movies[c(which(movies$year  < 1970)), 'estrato'] <- 1
movies[c(which(movies$year  >= 1970  & movies$year < 1980)), 'estrato'] <- 2
movies[c(which(movies$year  >= 1980  & movies$year < 1990)), 'estrato'] <- 3
movies[c(which(movies$year  >= 1990)), 'estrato'] <- 4


s_function <- function(vec){
  d <- rep(c(1:10), vec)
  m <- mean(d)
  s2 <- (1/ (length(d) -1) ) * sum ( (d - m) **2)
  res <- sqrt(s2)
  return ( res )
}

# generate the standard deviation for movies based on opinions 
movies_d <- cbind(movies, sd = c(apply(movies[,3:12] , 1, s_function)))
str(movies_d)
table(movies_d$estrato)
# 1   2   3   4 
# 68  20  30 132 

# muestra piloto

# piloto_estrato_1 <- movies_d[sample(which(movies_d$estrato == 1), 12), ]
# selected values by movie Id
piloto_estrato_1 = movies_d[is.element(movies_d$id, c(31381,19254,50986,59578,33870,33467,55031,15324,50976,21749,41959,53125)), ]
# piloto_estrato_2 <- movies_d[sample(which(movies_d$estrato == 2), 12), ]
piloto_estrato_2 = movies_d[is.element(movies_d$id, c(75314,73195,71853,75686,75148,66921,73486,79470,79944,70735,70510,77416)), ]
# piloto_estrato_3 <- movies_d[sample(which(movies_d$estrato == 3), 12), ]
piloto_estrato_3 = movies_d[is.element(movies_d$id, c(86250,93058,95327,81505,88247,86879,82096,83658,87544,80678,87843,88763)), ]
# piloto_estrato_4 <- movies_d[sample(which(movies_d$estrato == 4), 12), ]
piloto_estrato_4 = movies_d[is.element(movies_d$id, c(5074352,338013,2106476,1675434,266543,3783958,2267998,245712,198781,120586,1832382,986264)), ]

Nh_estrato = vector(length = 4)
Nh_estrato[1]  = nrow(movies_d[which(movies_d$estrato == 1),])
Nh_estrato[2]  = nrow(movies_d[which(movies_d$estrato == 2),])
Nh_estrato[3]  = nrow(movies_d[which(movies_d$estrato == 3),])
Nh_estrato[4]  = nrow(movies_d[which(movies_d$estrato == 4),])

# validation 
stopifnot( sum(Nh_estrato) == 250)

sigma_h = vector(length = 4)
sigma_h[1] = sd(piloto_estrato_1$sd)
sigma_h[2] = sd(piloto_estrato_2$sd)
sigma_h[3] = sd(piloto_estrato_3$sd)
sigma_h[4] = sd(piloto_estrato_4$sd)

sum_Nh = sum(Nh_estrato * sigma_h) 

ah = (Nh_estrato * sigma_h)/sum_Nh

N  = 250
e = 0.1
alpha = 0.05
z= qnorm(1-alpha/2)
d = N*e/z

sum_n_a =  sum( ( Nh_estrato * sigma_h ) ^ 2 / ah )
sum_n = sum( Nh_estrato * sigma_h ^ 2)

n =  ceiling(sum_n_a/(d ^ 2 + sum_n)) #18
nh = ceiling(ah * n) # 6 2 2 11 = 21

# sort by estrato
movies_d = movies_d[order(movies_d$estrato),]

m <- sampling::strata(movies_d, c("estrato"), size= nh, method="srswor")
sample_mae = getdata(movies_d,m)

table(is.na(sample_mae$sd))

aux = data.frame(fpc = c(rep(Nh_estrato[1],nh[1]),rep(Nh_estrato[2],nh[2]), rep(Nh_estrato[3],nh[3]), rep(Nh_estrato[4],nh[4])))
sample_mae_fpc = cbind(num = 1 :sum(nh) , sample_mae , aux )

design_mae = svydesign(id=~1,strata=~estrato,fpc=~fpc,data=sample_mae_fpc)

means1 = svymean(~sd,design_mae, deff=T)

means1
# mean    SE DEff
# sd 1.731 0.052 1.18
confint(means1)
# 2.5 % 97.5 %
# sd 1.6294 1.8334


# ================= Part C ===============================

whiplash<-rep(c(1:10), as.numeric(movies[movies$name == 'Whiplash', 3:12]))
#  2989 1410 2029 3244 6710 18499 63151 148482 177511 120836
e_mean = mean(whiplash)
sd =  sd(whiplash)
# 1.3882

