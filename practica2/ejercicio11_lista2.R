Nh_1=132
Var_1=36
sd_1=sqrt(Var_1)
Nh_2=92
Var_2=25
sd_2=sqrt(Var_2)
Nh_3=27
Var_3=9
sd_3=sqrt(Var_3)

e=100
alpha=0.05
Z_1_menos_alplha =  qnorm(p=(1-(alpha/2)),0,1)

N=Nh_1+Nh_2+Nh_3

###utilizando MASS con un error maximo de 100

n1_MASs=ceiling(((Z_1_menos_alplha)^2*Var_1*Nh_1)/((Z_1_menos_alplha)^2*Var_1+((e)^2/Nh_1)))
n2_MASs=ceiling(((Z_1_menos_alplha)^2*Var_2*Nh_2)/((Z_1_menos_alplha)^2*Var_2+((e)^2/Nh_2)))
n3_MASs=ceiling(((Z_1_menos_alplha)^2*Var_3*Nh_3)/((Z_1_menos_alplha)^2*Var_3+((e)^2/Nh_3)))

N_MASs=n1_MASs+n2_MASs+n3_MASs
  
e_MAE=Z_1_menos_alplha*sqrt(((1-(n1_MASs/Nh_1))*((Nh_1/N_MASs)^2)*(Var_1/n1_MASs))+
                              ((1-(n2_MASs/Nh_2))*((Nh_2/N_MASs)^2)*(Var_2/n2_MASs))+
                              ((1-(n3_MASs/Nh_3))*((Nh_3/N_MASs)^2)*(Var_3/n3_MASs)))  

E_maximo=N_MASs*e_MAE; E_maximo
