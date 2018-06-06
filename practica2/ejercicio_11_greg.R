varianza_obreros <- 36
varianza_tecnicos <- 25
varianza_administrativos <- 9 

sigma_obreros <- sqrt(varianza_obreros)
sigma_tecnicos <- sqrt(varianza_tecnicos)
sigma_administrativos = sqrt(varianza_administrativos)

N_obreros <- 132
N_tecnicos <- 92
N_administradores <- 27

N =  N_obreros + N_tecnicos + N_administradores

error <- 100
alpha <- 0.05
z <-  qnorm(p=(1-(alpha/2)),0,1)

# asignacion de neyman 



###MASS con un error maximo de 100

n_obreros = ceiling(((z)^2*varianza_obreros * N_obreros)/((z)^2*varianza_obreros+((error)^2/N_obreros)))
n_tecnicos = ceiling(((z)^2*varianza_tecnicos * N_tecnicos)/((z)^2*varianza_tecnicos+((error)^2/N_tecnicos)))
n_administradores = ceiling(((z)^2*varianza_administrativos * N_administradores)/((z)^2*varianza_administrativos+((error)^2/N_administradores)))

N_MASs = n_obreros + n_tecnicos + n_administradores


e_MAE = z * sqrt(((1-(n_obreros/N_obreros)) * ((N_obreros/N_MASs) ^ 2 ) * (varianza_obreros/n_obreros) ) +
                    ( ( 1 - (n_tecnicos/N_tecnicos))*((N_tecnicos/N_MASs)^2)*(varianza_tecnicos/n_tecnicos)) +
                    ( ( 1 - (n_administradores/N_administradores)) * ((N_administradores/N_MASs) ^ 2 ) * (varianza_administrativos/n_administradores)))  

max_error = N_MASs  * e_MAE

max_error

error_obreros = (1 - n_obreros / N_obreros) * ( N_obreros / N )
ee = z * sqrt(  )



n1=ceiling(qnorm(0.975)^2*36*132/(qnorm(0.975)^2*36+(100/132)^2*132))
n2=ceiling(qnorm(0.975)^2*25*92/(qnorm(0.975)^2*25+(100/92)^2*92))
n3=ceiling(qnorm(0.975)^2*9*27/(qnorm(0.975)^2*9+(100/27)^2*27))

n=n1+n2+n3

d_error=qnorm(0.975)*sqrt(132^2*(1-n1/132)*36/(n1)+
                          92^2*(1-n2/92)*25/(n2)+
                          27^2*(1-n3/27)*9/(n3))
  
  