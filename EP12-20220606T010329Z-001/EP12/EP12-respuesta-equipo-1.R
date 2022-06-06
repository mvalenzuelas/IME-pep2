library(dplyr)
library(ggpubr)
library(rcompanion)

#====================
#==== Pregunta 1 ====
#====================

# En el trabajo de título de un estudiante del DIINF se reportan los siguientes tiempos de ejecución
# (en milisegundos) medidos para dos versiones de un algoritmo genético para resolver instancias del 
# problema del vendedor viajero disponibles en repositorios públicos. ¿Es uno de los algoritmos más 
# rápido que el otro? Aplique una transformación de datos adecuada para responder.

#Se fija el nivel de significación en 0.05.
alfa <- 0.05

#Se crean los vectores con los valores dados en la tabla del enunciado.
instanciaA <- c(129,109,28,178,74,16,87,108,149,78)
tiempoA <- c(1510394,402929,885722,4428151,48667,834565,70599,783108,210041,37449)
instanciaB <- c(134,193,10,88,142,86,36,190,163,33)
tiempoB <- c(1252837,21962277,120276,4629726,5743260,6701654,6568968,180141,6684497,35974)

#Se crea el dataframe con los vectores anteriores.
data <- data.frame(instanciaA,tiempoA,instanciaB,tiempoB)

#Se grafican los histogramas para ver la forma de la distribución de las muestras para comprobar si se acerca
#a la normal.
#Gráfico de tiempo A.
g1 <- gghistogram(data, x = "tiempoA", bins = 10,
                  xlab = "Tiempo [milisegundos]", ylab = "Frecuencia",
                  color = "red", fill = "red")

#Gráfico de tiempo B.
g2 <- gghistogram(data, x = "tiempoB", bins = 10,
                  xlab = "Tiempo [milisegundos]", ylab = "Frecuencia",
                  color = "blue", fill = "blue")

#Se imprimen los gráficos creados.
print(g1)
print(g2)

#Como se puede apreciar en los dos histogramas mostrados, la distribución de las muestras no se acerca a la 
#distribución normal, por lo que es necesario aplicar una transformación a los datos.
#En este caso se decidió aplicar una transformación de escalera de potencias de Tukey, para la cual es necesario
#obtener el valor lambda que se utilzará para calcular los valores transformados. Para obtener el mejor lambda
#se usa la función transformTukey, la cual realiza pruebas con valores dentro del intervalo dado para determinar cual
#es el que permite lograr una distribución más cercana a la normal.
lambda <- transformTukey(tiempoA, start = -4 , end = 4, int = 0.001, plotit = TRUE, statistic = 1, returnLambda = TRUE)

#De la función anterior se obtiene un valor de lambda de 0.11, que es mayor a 0, por lo que se realiza la transformación
#usando la fórmula X^ lambda (X elevado a lambda).
tiempoATrans <- tiempoA**lambda
tiempoBTrans <- tiempoB**lambda

#Usando la prueba de Shapiro-Wilk se comprueba que luego de la transformación ambas muestras tienen una distribución
#cercana a la normal, ya que los p-valores de tiempo A (0.6349) y tiempo B (0.1478) son mayores al nivel de
#significación fijado.
print(shapiro.test(tiempoATrans))
print(shapiro.test(tiempoBTrans))

#Con las muestras ya transformadas y con una distribución cercana a la normal se procede a realizar una prueba t
#de Student para dos muestras independientes para comprobar si el primer algoritmo es mas rápido que el segundo.
#Condiciones de la prueba:
# 1. Que las observaciones sean independientes entre sí, lo que se cumple ya que cada observación corresponde a
#    a una ejecución distinta.
# 2. Que las muestras sigan una distribución cercana a la normal, lo que se logra aplicando la transformación.

#Hipótesis:
#H0: en promedio, el algoritmo A es igual de rápido que el algoritmo B.
#HA: en promedio, el algoritmo A es más rápido que el algoritmo B.

#Fórmulas matemáticas:
#H0: mu.A = mu.B
#HA: mu.A > mu.B

#Se aplica la prueba t de Student para muestras independientes.
prueba1 <- t.test(x = tiempoATrans,
                  y = tiempoBTrans,
                  paired = FALSE,
                  alternative = "greater",
                  mu = 0,
                  conf.level = 1 - alfa)

#Se imprime el resultado de la prueba.
print(prueba1)

#====================
#==== Respuesta 1 ===
#====================

#De la prueba anterior se obtiene un p-valor igual a 0.966, el cual es mayor al nivel de significación fijado, por 
#lo que se falla al rechazar la hipótesis nula, pudiendo concluir con un 95% de confianza que, en promedio, el 
#algoritmo A es igual de rápido que el algoritmo B.

#---------------------------------------------------------------------------------------------------------------------

#====================
#==== Pregunta 2 ====
#====================
library(dplyr)
library(ggpubr)
library(WRS2)

# Analice la primera pregunta abordada en el ejercicio práctico 11, con los mismos datos, utilizando un método 
# robusto adecuado.

#La pregunta de investigacion propuesta para la primera parte del trabajo es la de analizar si, en promedio,
#la cantidad de hijos nacidos vivos es igual en las mujeres residentes en la region del Maule y las 
#mujeres residentes en la region de Los Lagos.

#Hipotesis:
#H0: En promedio, la cantidad de hijos nacidos vivos es igual en las mujeres residentes en la region del Maule y 
#    en las mujeres residentes en la region de Los Lagos.
#HA: En promedio, la cantidad de hijos nacidos vivos es distinta en las mujeres residentes en la region del Maule
#    y en las mujeres residentes en la region de Los Lagos.

#Formula matematica:
#H0: mu.maule - mu.losLagos = 0
#HA: mu.maule - mu.losLagos != 0

#Lectura de los datos desde el archivo csv entregado.
basename <- "EP11 Datos.csv"
file <- file.path("C:/Users/adolf/Desktop/Cosas/IME/IME_equipo1/EP12", basename)
datos <- read.csv2(file = file)

#Se fija el nivel de significacion para este analisis.
alfa <- 0.05

#Se fija la semilla seleccionada al azar para esta prueba
set.seed(420)

#Se obtienen las observaciones correspodientes a mujeres de las dos regiones a estudiar desde el dataframe
#original.
mujeresMaule <- datos %>% filter(region == "Región del Maule" & sexo == "Mujer")
mujeresLagos <- datos %>% filter(region == "Región de Los Lagos" & sexo == "Mujer")

#Se obtiene una muestra aleatoria de 150 observaciones para region.
muestraMaule <- sample_n(mujeresMaule, 200)
muestraLagos <- sample_n(mujeresLagos, 100)

#Se obtiene la columna que contiene la cantidad de hijos nacidos vivos de cada observacion.
hijosMaule <- muestraMaule$s4
hijosLagos <- muestraLagos$s4

#Se crea el dataframe con los datos.
hijos <- c(hijosMaule, hijosLagos)
region <- c(rep("Maule", length(hijosMaule)), rep("Los Lagos", length(hijosLagos)))
datos1 <- data.frame(hijos, region)

# cantidad de muestras a generar con bootstrapping.
R <- 9999

#Se realiza una prueba de yuen usando la media, ya que los tamaños de las muestras son muy dispares.
prueba_yuen_media <- pb2gen(hijos ~ region,
                       data = datos1,
                       est = "mean",
                       nboot = R)

#Se imprimen los resultados de la prueba por consola.
cat("\n\nResultado al usar la media como estimador\n\n")
print(prueba_yuen_media)

#====================
#==== Respuesta 2 ===
#====================

#De la prueba de Yuen se obtiene un p-valor igual a 0.88459, el cual es mayor al nivel de significación fijado,
#por lo tanto se falla al rechazar la hipótesis nula, concluyendo con un 95% de confianza que En promedio, 
#la cantidad de hijos nacidos vivos es igual en las mujeres residentes en la region del Maule y en las mujeres 
#residentes en la region de Los Lagos.

#-------------------------------------------------------------------------------

#====================
#==== Pregunta 3 ====
#====================

#Librerias
library(dplyr)
library(ggpubr)
library(WRS2)
library(tidyverse)

# Analice la segunda pregunta abordada en el ejercicio práctico 11, con los mismos datos, utilizando un método
# robusto adecuado.

# Una universidad de Chile necesita conocer la situacion económica de los hogares a los que pertenecen 
# sus estudiantes, esto con el fin de disponer de distintas becas que los apoyen durante su carrera. Para ello
# realizaron estudio tomando como ejemplo en 3 regiones correspondientes al sector norte, centro y sur del país respectivamente.
# Lo que se busca con esto es verificar si el ingreso total (variable ytotcorch) medio de los hogares es similar en
# las regiones norte(region Coquimbo), centro(region maule) y sur (region los lagos) del pais.

#Hipotesis:
#H0: El ingreso total medio de los hogares es igual para las tres regiones
#HA: El ingreso total medio de los hogares es diferente para al menos una región

set.seed(573)

#Lectura de los datos desde el archivo csv entregado.
basename <- "EP11 Datos.csv"
file <- file.path("C:/Users/adolf/Desktop/Cosas/IME/IME_equipo1/EP12", basename)
datos <- read.csv2(file = file)

# Fijar nivel de significacion.
alfa <- 0.05

# Limpiamos los datos de la columna región
datosRegion <- datos %>% filter(ytotcorh != "NA")

# Filtramos las regiones de interés
datosRegion <- filter(datos, region %in% c("Región de Coquimbo", "Región del Maule", "Región de Los Lagos"))

# Se toma una muestra de 600 personas pertenecientes a esas regiones
tam <- 600

# Obtenemos la mustra de las regiones junto a los ingresos correspondientes de cada hogas
muestra <- datosRegion[sample(nrow(datosRegion), tam),]
ingresos <- muestra[["ytotcorh"]]
regiones <- factor(muestra[["region"]])
datos2 <- data.frame(ingresos, regiones)

# Dividimos los datos en tres vectores Norte, Centro y Sur
ingreso_Norte <- sample_n(datos2 %>% filter(regiones == "Región de Coquimbo") %>% select(ingresos),100)
vectorNorte <- as.vector(t(ingreso_Norte))
ingreso_Centro <- sample_n(datos2 %>% filter(regiones == "Región del Maule") %>% select(ingresos),100)
vectorCentro <- as.vector(t(ingreso_Centro))
ingreso_Sur <- sample_n(datos2 %>% filter(regiones == "Región de Los Lagos") %>% select(ingresos),100)
vectorSur <- as.vector(t(ingreso_Sur))

# Adjuntamos los vectores en un mismo data frame, junto a ellos la id correspondiente
id <- 1:nrow(ingreso_Norte)
datos_Agrupados <- data.frame(id,vectorNorte,vectorCentro,vectorSur)

#Obtenemos los el largo de los agrupados
datos_Agrupados_largo <- datos_Agrupados %>% pivot_longer(c("vectorNorte", "vectorCentro", "vectorSur"), names_to = "region",
                                values_to = "ingresos")
#Agregamos factor region
datos_Agrupados_largo[["region"]] <- factor(datos_Agrupados_largo[["region"]])



# Aplicar alternativa robusta para ANOVA de una via con
# muestras correlacionadas.
gamma <- 0.2

# Aplicamos la prueba rmanova, ya que los datos se encuentras correlacionados, y poseen diferencias en su tamaño
prueba <- rmanova(y = datos_Agrupados_largo[["ingresos"]], groups = datos_Agrupados_largo[["region"]],
                  blocks = datos_Agrupados_largo[["id"]], tr = gamma)

# Mostramos la muestra por pantalla
print(prueba)

#====================
#==== Respuesta 3 ===
#====================

#De la prueba de Rmanova se obtiene un p-valor igual a 0.45248, el cual es mayor al nivel de significación fijado,
#por lo tanto se falla al rechazar la hipótesis nula, concluyendo con un 95% de confianza que El ingreso total medio 
#de los hogares es igual para las tres regiones.
