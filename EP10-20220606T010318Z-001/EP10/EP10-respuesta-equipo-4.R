library(dplyr)

basename <- "EP10 Datos.csv"
file <- file.path("C:/Users/adolf/Desktop/Cosas/IME/IME_equipo_4", basename)
datos <- read.csv2(file = file)

#====================
#==== Pregunta 1 ====
#====================

# ¿Existe diferencia entre la puntuacion obtenida por los envases disenados por DisenoColor 
# y LaKajita?

#Para analizar si hay diferencia entre las puntuaciones obtenidas por los envases de ambas empresas 
#se realizara una prueba de suma de rangos para muestras grandes de Wilcoxon.

#Para realizar esta prueba se deben cumplir las siguientes condiciones:
# 1. Las observaciones de ambas muestras deben ser independientes, lo que se cumple ya que cada
#    observacion representa a una persona distinta y se eligieron de manera aleatoria.
# 2. La escala de medicion empleada debe ser a lo menos ordinal, lo cual se cumple ya que los datos 
#    se obtuvieron usando una escala Likert.

#Con lo anterior se demuestra que se cumplen todas las condiciones para llevar a cabo la prueba 
#mencionada.

#Hipotesis:
#H1: no hay diferencia en la puntuacion obtenida para los envases disenados por DisenoColor y LaKajita.
#HA: si hay diferencia en la puntuacion obtenida para los envases disenados por DisenoColor y LaKajita.

#Se fija el nivel de significacion en 0.05.
alfa <- 0.05

#Se filtran los datos pertenecientes a cada empresa desde el dataframe.
disenoColor <- datos %>% filter(Diseno == "DisenoColor")
laKajita <- datos %>% filter(Diseno == "LaKajita")

#Se obtiene la columna que contiene los puntajes obtenidos por los envases de cada empresa.
puntaje_disenoColor <- disenoColor$Puntaje
puntaje_laKajita <- laKajita$Puntaje

#Se realiza la prueba de Wilcoxon usando la funcion de R wilcox.test.
prueba1 <- wilcox.test(puntaje_disenoColor, puntaje_laKajita, alternative = "two.sided", conf.level = 1 - alfa)

#Se imprime por consola el resultado de la prueba realizada.
print(prueba1)

#====================
#==== Respuesta 1 ===
#====================

#De la prueba anterior se obtiene un p-valor igual a 0.0106, el cual es menor al nivel de significación, 
#por lo que se rechaza la hipotesis nula en favor de la hipotesis alternativa, concluyendo con un 95% de 
#confianza que si existe diferencia en la puntuación obtenida por los envases diseñados por DisenoColor y
#LaKajita.

#----------------------------------------------------------------------------------------------------------------------

#====================
#==== Pregunta 2 ====
#====================

# �Existe diferencias en las puntuaciones obtenidas para el envase de cuchifli disenado por LaKajita segun 
# la edad de los evaluadores? De ser asi, �cual(es) grupo(s) de evaluador(es) se diferencia(n) de los demas?

#Para analizar si hay diferencias en las puntucaciones obtenidas por el envase de cuchufli diseñado por
#LaKajita segun la edad se usara una prueba de Kruskal-Wallis, la que permite realizar pruebas no parametricas
#para mas de dos muestras (en este caso son tres muestras: adulto, joven y niño).

#Las condiciones que se deben cumplir para poder realizar esta prueba son:
# 1. La variable independiente debe tener al menos dos niveles, lo que se cumple ya que la variable independiente
#    es la edad y tiene 3 niveles: adulto, joven y niño.
# 2. La escala de la variable independiente debe ser, a lo menos ordinal, lo que tambien se cumple ya que 
#    los datos se obtuvieron usando una escala de Likert.
# 3. Las observaciones son independientes entre si, esto se cumple porque cada observacion representa a una 
#    persona distinta y que fue seleccionada de manera aleatoria.

#Por lo anterior, se cumplen todas las condiciones necesarias para realizar la prueba de Kruskal-Wallis.

#Hipotesis:
#H0: la puntuacion obtenida por el envase de cuchufli disenado por LaKajita es igual para cada rango etario.
#HA: la puntuacion obtenida por el envase de cuchufli disenado por LaKajita es diferente para, por lo menos,
#    un rango etario.

#Se fija el nivel de significacion en 0.05.
alfa <- 0.05

#Se filtran las observaciones que corresponden al envase de cuchufli disenado por LaKajita desde
#los datos leidos.
laKajita <- datos %>% filter(Diseno == "LaKajita" & Producto == "Cuchufli")

#Se filtran las observaciones segun el rango etario, es decir, en adultos, jovenes y niños.
adultos_laKajita <- laKajita %>% filter(Edad == "Adulto")
ninos_laKajita <- laKajita %>% filter(Edad == "Nino")
joven_laKajita <- laKajita %>% filter(Edad == "Joven")

#Se obtiene la columna de puntajes de cada rango etario.
puntaje_adultos <- adultos_laKajita$Puntaje
puntaje_ninos <- ninos_laKajita$Puntaje
puntaje_jovenes <- joven_laKajita$Puntaje

#Se unen los puntajes de cada rango etario en un solo vector.
puntaje <- c(puntaje_adultos, puntaje_ninos, puntaje_jovenes)

#Se crea un nuevo vector que contenga los rangos etarios de cada puntaje.
edad <- c(rep("Adulto", length(puntaje_adultos)),
          rep("Nino", length(puntaje_ninos)),
          rep("Joven", length(puntaje_jovenes)))
edad <- factor(edad)

#Se crea un nuevo data frame con los vectores creados para el puntaje y la edad.
datos <- data.frame(puntajes, edad)

#Se realiza la prueba de Kruskal-Wallis usando la funcion kruskal.test de R.
prueba2 <- kruskal.test(puntaje ~ edad, data = datos)

#Se imprime por consola el resultado de la prueba.
print(prueba2)

#====================
#==== Respuesta 2 ===
#====================

#De la prueba realizada se obtiene un p-valor igual a 0.4003, el cual es mayor al nivel de significacion fijado,
#por lo que es falla al rechazar la hipotesis nula, pudiendo concluir con 95% de confianza que la puntuacion
#obtenida por el envase de cuchufli diseñado por LaKajita es igual para cada rango etario.

#Dado que se concluyo que los 3 grupos analizados tienen la misma puntuacion para el envase de cuchufli diseñado
#por LaKajita, no es necesario realizar una prueba post-hoc, ya que esta solo se realiza si por lo menos un grupo
#presenta diferencias.