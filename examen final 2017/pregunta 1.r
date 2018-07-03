# 1.- Justificando adecuadamente sus respuestas, responda como verdadera o falsa cada una de las
# dos siguientes afirmaciones. Cada parte vale dos puntos.
# a) En una poblacíon de 80 cuartos de un hospital, se desea aplicar alǵun tipo de muestreo
# sistem atico de tamãno 20 para estimar entre otras la proporcíon de cuartos en este hospital que
# no cuentan con un sistema de alerta. Entonces salvo se asuma un MASs sera imposible estimar
# la varianza de la proporcion estimada de cuartos en este hospital que no cuentan con un sistema
# de alerta.
# b) La f ́ormula n = ˆd × nMASs, donde n es el tama ̃no de muestra a tomarse en un muestreo
# complejo, ˆd su efecto de dise ̃no estimado y nMASs es el tama ̃no de muestra calculado para un
# MASs de esta misma poblaci ́on, podr ́ıa considerarse v ́alida siempre y cuando la poblaci ́on en
# estudio fuese lo suficientemente grande.


N = 80 
n = 20
# proporcion de cuantos con sistema de alerta

# part a 

# indicar que el muestreo sistematico es un caso particular del muestreo por conglomerados , donde cada fila de la matriz representa una posible 
# muestra sistematica de tamanio n , con su respectiva media. Considerando cada fila como un conglomerado del cual solo tomaremos uno. Asi el muestreo 
# sistematico se reduce a un muestreo por conglomerados de una etapa con k conglomerados cada uno de tamanio n. De donde por defecto solo se toma uno
# Un problema central con el muestreo sistematico es , como adelantamos que este no nos 
# permite obtener una estimacion directa de la varianza del estimador , ya que solo se base en una muestra de un unico conglomerado, Una solucion es usar la formuala
# de un MASs. Se puede utilizar Muestreo sistematico replicado Detalle pagina 95

