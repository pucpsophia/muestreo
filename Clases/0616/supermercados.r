# supermercados 

id = c( "A", "B", "C", "D", "E", "F")
tamanio = c(300, 200, 100, 1000, 150,500)
ventas = c(24,20,11,245,18,90)

total = sum(tamanio)

phi = tamanio / total
phiacum =  cumsum(phi)

pik = inclusionprobabilities(tamanio, 3)
pikacum = cumsum(pik)

pik2 = UPsystematicpi2(pik)

data = data.frame(id, tamanio, ventas, pik, phi, phiacum, pikacum)

# similar 
r =  runif(6)
pik > r
UPpoisson(pik)

# esquema sistematico ppt 

anchor =  0.6723  # runif(1) 

# elegir primer integrante que supera, en este caso row = 2, 0.8 

# despues elegir el integrante que supere anchor + 1 , en este caso row = 3 , 1.96
 
# despues elegir el siguiente que supere al nuevo anchor + 1 + 1, en este caso row = 6 , 3.0

# puede tener problemas con el orden inicial por depender de como se ordenan los datos , se puede calcular las probabilidades de segundo orden. para solucionar
# el problem UpRandomSystematic

index = UPsystematic(pik)
index = UPrandomsystematic(pik)

sample = getdata(data, index)

total_estimacion =  HTestimator(sample[, "ventas"], sample[, "pik"] ) # verdadero valor 408

sample_pik = pik2[ as.logical( index), as.logical( index)]

vtauh = sqrt(varHT(sample[, "ventas"], sample_pik))

















