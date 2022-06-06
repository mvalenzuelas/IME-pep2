library(ggpubr)
library(tidyverse)
library(DescTools)
library(WRS2)

#------------------------------Pregunta 1---------------------------------------
# En el trabajo de título de un estudiante del DIINF se reportan los siguientes 
# tiempos de ejecución (en milisegundos) medidos para dos versiones de un 
# algoritmo genético para resolver instancias del problema del vendedor viajero 
# disponibles en repositorios públicos. ¿Es uno de los algoritmos más rápido 
# que el otro? Aplique una transformación de datos adecuada para responder.
#-------------------------------------------------------------------------------

instancia_A <- c(102, 112, 151, 4, 55, 70, 98, 135, 7, 183)
instancia_B <- c(43, 189, 140, 51, 146, 25, 2, 179, 68, 182)

tiempo_A <- c(842079, 834565, 885722, 48705, 4428151, 62764, 8576989, 37449, 48667, 1510394)
tiempo_B <- c(6684497, 180141, 2830464, 1252837, 5743260, 6701654, 1174562, 120276, 994830, 48408)

tabla_A <- data.frame(instancia_A, tiempo_A)
tabla_B <- data.frame(instancia_B, tiempo_B)

alfa <- 0.05


tabla<-rbind(tiempo_A,tiempo_B)
g_A <- gghistogram(tabla_A,
                 x = "tiempo_A", 
                 color = "blue",fill="blue",
                 xlab = "tieempo",
                 ylab="frecuencia",
                 bins=5)
g_B <- gghistogram(tabla_B, 
                 x="tiempo_B",
                 bins = 5,
                 color = "red",fill="red",
                 xlab = "tiempo", 
                 ylab = "frecuencia")

print(g_A)
print(g_B)

# En esta situación, no se utilizará un método robusto ya que las distribuciones
# presentan una asimetría bastante desbalanceda, por lo tanto, aunque se 
# aplicaran métodos robustos, no se podría obtener una distribución 
# aproximadamente a la normal


# En este caso, la transformación utilizada es la transformación de Box Cox, ya 
# que, como se mencionó anteriormente, las distribuciones de ambos grupos 
# son muy asimétricas.

lambda<-BoxCoxLambda(tabla,lower=-4,upper=4)

transformacion1<-BoxCox(tabla_A$tiempo_A,lambda = lambda)
transformacion2<-BoxCox(tabla_B$tiempo_B,lambda = lambda)

qq_A_1<-ggqqplot(transformacion1,color="blue")
qq_B_1<-ggqqplot(transformacion2,color="red")

comparacion1<-ggarrange(qq_A_1,qq_B_1,ncol=2,nrow=1)

print(comparacion1)

# H0: Los algoritmos tienen un tiempo de ejecución medio iguales.
# Ha: Los algoritmos tienen un tiempo de ejecución medio diferentes.

# H0: mu_A == mu_B
# Ha: mu_A != mu_B

# Luego, se aplica una Prueba t de student para 2 muestras independientes.

prueba <- t.test(tiempo_A, tiempo_B)
print(prueba)
# p-valor = 0.5006 > alfa

# Por tanto, se falla en rechazar H0 en favor de Ha, es decir, se concluye, con 
# un 95% de confianza, que los algoritmos tienen un tiempo de ejecución medio
# iguales, por ende, ningún algoritmo es más rápido que el otro.


#------------------------------Pregunta 2---------------------------------------
# Analice la primera pregunta abordada en el ejercicio práctico 11, con los 
# mismos datos, utilizando un método robusto adecuado.
#-------------------------------------------------------------------------------

data <- read.csv2("C:\\Users\\danie\\Desktop\\Ramos 2022-1\\IME\\EP11 Datos.csv")

# Pregunta de investigación utilizada:
# El promedio de edades de personas adultas en la región metropolitana, que se 
# encuentran afiliados a un sistema previsional y aquellos que no se encuentran 
# afiliados, son iguales.

n = 333
set.seed(336)

muestra <- sample_n(data, n)

# H0:El promedio de edades de las personas afiliadas es igual al promedio de 
# edades de las personas no afiliadas en la Región Metropolitana

# Ha:El promedio de edades de las personas afiliadas es diferente al promedio de 
# edades de las personas no afiliadas en la Región Metropolitana

# H0: mu_A == mu_B
# Ha: mu_A != mu_B

afiliados <- muestra%>%filter(o28 == "Sí")

no_afiliados <- muestra%>%filter(o28 == "No")

edades_afiliados <- afiliados$edad
edades_afiliados <- sort(edades_afiliados)
edades_noAfiliados <- no_afiliados$edad
edades_noAfiliados <- sort(edades_noAfiliados)

edades <- c(edades_afiliados, edades_noAfiliados)
afiliacion <- c(rep("Afiliados", length(edades_afiliados)), rep("No Afiliados", length(edades_noAfiliados)))

datos <- data.frame(edades, afiliacion)

# Para esta situación, se utilizará el método robusto de la prueba de Yuen para 
# dos muestras independientes, debido a que el tamaño de las 2 muestras son muy dispares

gamma <- 0.2
n_A <- length(edades_afiliados)
n_B <- length(edades_noAfiliados)

poda_A <- n_A*gamma
poda_B <- n_B*gamma

afiliados_truncada <- edades_afiliados[poda_A:(n_A-poda_A)]
noAfiliados_truncada <- edades_noAfiliados[poda_B:(n_B-poda_B)]

edades <- c(afiliados_truncada, noAfiliados_truncada)
afiliacion <- c(rep("Afiliados", length(afiliados_truncada)), rep("No Afiliados", length(noAfiliados_truncada)))

datos_truncados <- data.frame(edades, afiliacion)

prueba_yuen <- yuen(edades ~ afiliacion, data = datos, tr = gamma)
print(prueba_yuen)

# En conclusión, con un nivel de significación de alfa 0.05, existe suficiente 
# evidencia para fallar en rechazar la H0 en favor de Ha, utilizando la prueba 
# de Yuen para 2 muestras independientes, lo que significa que el promedio de 
# edades de las personas afiliadas es igual al promedio de edades de las 
# personas no afiliadas en la Región Metropolitana.

# En base a lo anterior, se puede observar que las conclusiones obtenidas a 
# partir de las 2 pruebas, una con simulación de Monte Carlo y la otra con 
# prueba de Yuen, en la primera se rechaza la H0 debido a ala existencia de 
# datos atípicos, que interfieren con la distribución de la muestra, lo que no 
# sucede en la prueba de Yuen, ya que trata los valores atípicos de una forma 
# distinta al método anterior.

#------------------------------Pregunta 3---------------------------------------
# Analice la segunda pregunta abordada en el ejercicio práctico 11, con los 
# mismos datos, utilizando un método robusto adecuado.
#-------------------------------------------------------------------------------

# Pregunta de investigación utilizada:
# Determinar si existe relación entre las medias de las edades entre personas 
# solteras, casadas y divorciadas en la Región Metropolitana


set.seed(224)
n2 = 500

muestra2 = sample_n(data, n2)

# H0: Las medias de las edades entre personas solteras, casadas y divorciadas 
# en la Región Metropolitana, son iguales

# Ha: Las medias de las edades entre personas solteras, casadas y divorciadas 
# en la Región Metropolitana, son diferentes en al menos un estado civil.

#H0: mu_A == mu_B == mu_C
#Ha: mu_A != mu_B || mu_A != mu_C || mu_C != mu_B


# Para esta situación, se utilizará el método robusto de comparaciones de una 
# vía para múltiples grupos independientes, debido a que la prueba de Yuen está 
# contemplada para dos grupos, y en el contexto de los datos, estos se
# encuentran en más de 2 grupos independientes.

casado <- muestra2%>%filter(ecivil == "Casado(a)")
soltero <- muestra2%>%filter(ecivil == "Soltero(a)")
divorciado <- muestra2%>%filter(ecivil == "Divorciado (a)")

edades_casado <- casado$edad
edades_soltero <- soltero$edad
edades_divorciado <- divorciado$edad

n_1 = length(edades_casado)
n_2 = length(edades_soltero)
n_3 = length(edades_divorciado)
n_edades <- n_1 + n_2 + n_3

edades <- c(edades_casado, edades_soltero, edades_divorciado)
ecivil <- c(rep("Casado", n_1), rep("Soltero", n_2), rep("Divorciado", n_3))

datos3 <- data.frame(edades, ecivil)

medias_truncadas <- t1way(edades ~ ecivil, data = datos3, tr = gamma, alpha = alfa)
print(medias_truncadas)
# p-valor = 7e-05 < alfa

# Por tanto, se rechaza H0 en favor de Ha, es decir, se concluye, 
# con un 95% de confianza, que las medias de las edades entre personas 
# solteras, casadas y divorciadas en la Región Metropolitana, son diferentes, 
# utilizando comparaciones de una vía para múltiples grupos independientes.

posthoc <- lincon(edades ~ ecivil, data = datos3, tr = gamma, alpha = alfa)
print(posthoc)

# Utilizando el procedimiento post hoc lincon, se logra determinar, con un nivel
# de signifcancia de 0.05, que existe una diferencia significativa entre las 
# edades de las personas Casadas y Solteras, y entre las edades de las personas 
# Divorciadas y Solteras, por lo cual, la muestra de las edades de las personas 
# solteras es la que difiere en comparación de las otras 2 muestras.

# Para esta situación, las conclusiones difieren debido a los p-valor de ambas 
# pruebas, una aplicando bootstrapping y la otra aplicando comparaciones de una 
# vía para múltiples grupos independientes.