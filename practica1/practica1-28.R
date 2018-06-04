
# Población


# Población
N= 3000

z = 1.96

#Error
#e = 10^6
e=950000
#Confianza
c = 0.95
#Personas que tienen servicio de agua
#N_d= 2100
N_d=1950

#Promedio del consumo de agua de las 2100 personas
#u = 51*10^2
u = 12000

#Desviación estandar del consumo de agua de las 2100 personas

#ds = 380
ds = 1540
# Proción de N_d con respecto a la población
p = N_d/3000



# Tamaño de la muestra

n = (((N_d-1)*(ds^2)+(1-p)*(N_d)*u^2)*(z)^2*(N)^2)/(((N_d-1)*ds^2+(1-p)*N_d*u^2)*(z^2)*N+e^2*(N-1))
n

# Estimación del consumo total del agua en la zona será
Est = N*u
Est

# varianza

#var = (1/(N-1))*((N_d)*(ds^2)+(1-p)*N_d*u^2)
#var

# varainza del estimador

#var_est = (N^2)*(1-n/N)*(var/n)
#sqrt(var_est)

#N*u






