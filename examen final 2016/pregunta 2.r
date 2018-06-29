# De una poblacio ́n de 4 personas se van a seleccionar sin reemplazamiento a 2 con prob bilidades no constantes.
# Se sabe que la probabilidad de que se seleccionen a las dos primeras personas es 0.2, 
# que se seleccionen a la primera y tercera es la misma que se seleccionen a la primera y cuarta, sienda esta de 0.1, 
# que se seleccionen a la segunda y cuarta es la misma que se seleccionen a la tercera y cuarta, siendo esta de 0.15 
# finalmente que se seleccionen a la segunda y tercera personas es de 0.3.

# a) Halle las probabilidades de inclusion de cada persona en la muestra. (2.0 puntos)
# b) Si la poblacion estadistica del numero de hermanos de estas cuatro personas es respectivamente P(y) = {2, 1, 5, 4}, 
# tome bajo este disenio una muestra de tamaño 2 y estimeel numero total de hermanos de la poblacion
# Obtenga tambien el error estadanr de estimacion correspondiente.


# http://matematicas.unex.es/~inmatorres/teaching/muestreo/assets/cap_2.pdf

# conjunto de muestras asociadas al procedimiento 
# S = {a,b} {a,c} {a,d} {b,c} {b, d} {c, d}
# probabilidades asociadas a cada muestra 
# P{a,b} = 0.2
# P{a,c} = 0.1
# P{a,d} = 0.1
# P{b,d} = 0.15
# P{c,d} = 0.15
# P{b,c} = 0.3

# phi_1 = P{a,b} + P{a,c} + P{a,d} => 0.2 + 0.1 + 0.1  => 0.4
# phi_2 = P{a,b} + P{b,c} + P{b,d} => 0.2 + 0.3 + 0.15 => 0.65
# phi_3 = P{a,c} + P{b,c} + P{c,d} => 0.1 + 0.3 + 0.15 => 0.55
# phi_4 = P{a,d} + P{c,d} + P{b,d} => 0.1 + 0.15 + 0.15 => 0.35

# b part

N = 4
n = 2



# hermanos respecitvamente (2, 1, 5, 4)

y = c(2,1,5,4)
pik = c(0.4, 0.65, 0.55, 0.35)

# select sample of 2 because has  the correct probabilities  
UPpoisson(pik)
UPsystematic(pik)
index = UPrandomsystematic(pik)
# we need to generate the second orden probabilities 
pik2 = UPsystematicpi2(pik)

pik
pik2

index
sample = getdata(y, index)
prob = getdata(pki, index)
total = HTestimator(sample[,2], prob[,2])
pik2_index = pik2[as.logical(index), as.logical(index)]

# stimated error sqrt of variance
sqrt(varHT(sample[,2], pik2_index, 1))

