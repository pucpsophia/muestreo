varianza_obreros <- 36
varianza_tecnicos <- 25
varianza_administrativos <- 9 

sigma_obreros <- sqrt(varianza_obreros)
sigma_tecnicos <- sqrt(varianza_tecnicos)
sigma_administrativos = sqrt(varianza_administrativos)

N_obreros <- 132
N_tecnicos <- 92
N_administradores <- 27

N_total =  N_obreros + N_tecnicos + N_administradores

error <- 100
alpha <- 0.05
z <-  qnorm(p=(1-(alpha/2)),0,1)


###MASS con un error maximo de 100

n_obreros = ceiling( ( (z) ^2 * varianza_obreros * N_obreros)/( (z) ^2 * varianza_obreros + ( ( error) ^ 2 / N_obreros)))
n_tecnicos = ceiling( ( (z) ^2 * varianza_tecnicos * N_tecnicos)/( (z) ^2 * varianza_tecnicos + ( ( error) ^ 2 / N_tecnicos)))
n_administradores = ceiling( ( (z) ^2 * varianza_administrativos * N_administradores) / ( ( z ) ^ 2 * varianza_administrativos + ( ( error ) ^ 2 / N_administradores) ))

N_MASs = n_obreros + n_tecnicos + n_administradores # 86 + 44 + 3


e_MAE = z * sqrt( ( ( 1- n_obreros / N_obreros) *  ( (N_obreros / N_total) ^ 2 ) * ( varianza_obreros / n_obreros) ) +
                  ( ( 1- n_tecnicos / N_tecnicos) * ( (N_tecnicos / N_total) ^ 2) * ( varianza_tecnicos / n_tecnicos) ) +
                  ( ( 1- n_administradores / N_administradores) * ( ( N_administradores / N_total) ^ 2 ) * (varianza_administrativos/n_administradores)))  

max_error = N_total  * e_MAE # E = N * e

max_error # 163.92 
