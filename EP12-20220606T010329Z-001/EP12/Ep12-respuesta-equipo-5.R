
library(DescTools)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(WRS2)

#-------------------------------Pregunta 1--------------------------------------

#Se trabaja con una significancia de 0.05.
alfa <- 0.05
#En el trabajo de título de un estudiante del DIINF se reportan los siguientes tiempos
#de ejecución (en milisegundos) medidos para dos versiones de un algoritmo genético 
#para resolver instancias del problema del vendedor viajero disponibles en repositorios públicos. 
#¿Es uno de los algoritmos más rápido que el otro? 
# Aplique una transformación de datos adecuada para responder.

#-------------------------------------------------------------------------------

#Se aplica una transformada de los datos para hacerlos normales y
#luego se aplica una prueba t-student para muestras independientes.

#Hipótesis prueba bilateral
#H0: Los tiempos de ejecución de ambos algoritmos para resolver problema del vendedor viajero son iguales.
#H0: mu = 0

#Ha: Los tiempos de ejecución de ambos algoritmos para resolver problema del vendedor viajero son diferentes.
#Ha: mu =/= 0

instancia_a <- c(171, 27, 162, 102, 89, 185, 158, 64, 149, 14)
tiempo_a <- c(70599, 783108, 4428151, 37449, 834565, 48705, 842079, 210041, 402929, 62764)

data_a <- data.frame(instancia = instancia_a, tiempo = tiempo_a)

instancia_b <- c(190, 95, 183, 198, 16, 151, 11, 169, 187, 135)
tiempo_b <- c(120276, 1174562, 2196277, 180141, 35974, 1252837, 92932, 6701654, 6568968, 4629726)

data_b <- data.frame(instancia = instancia_b, tiempo = tiempo_b)

#Gráfica de los datos originales:

g_a <- gghistogram(data_a, x = "tiempo", 
                   bins = 30, 
                   xlab = "Tiempo", 
                   ylab = "Instancia",
                   title =  "Original a")
print(g_a)

g_b <- gghistogram(data_b, x = "tiempo", 
                   bins = 30, 
                   xlab = "Tiempo", 
                   ylab = "Instancia", 
                   title =  "Original b")
print(g_b)

#Segun la gráfica de los datos originales y la distribución muy asimetrica que tienen se decide 
#aplicar la transformada Box Cox.

l_a <- BoxCoxLambda(tiempo_a, NULL, -5, 5)
transformada_a <- BoxCox(tiempo_a, l_a)
normalidad_a <- shapiro.test(transformada_a)

l_b <- BoxCoxLambda(tiempo_b, NULL,  -5, 5)
transformada_b <- BoxCox(tiempo_b, l_b)
normalidad_b <- shapiro.test(transformada_b)

data_ta <- data.frame(instancia = instancia_a, transformada = transformada_a)
data_tb <- data.frame(instancia = instancia_b, transformada = transformada_b)

g_ta <- gghistogram(data_ta, x = "transformada", 
                    bins = 30, xlab = "Tiempo", 
                    ylab = "Instancia", 
                    title = "Transformada a")
print(g_ta)

g_tb <- gghistogram(data_tb, x = "transformada",
                    bins = 30, xlab = "Tiempo",
                    ylab = "Instancia", 
                    title =  "Transformada b")
print(g_tb)

#Al realizar la transformada se logra encontrar normalidad en los datos, la prueba shapiro lo confirma, pues para ambos
#conjuntos de datos entrega un p-value mayor al alfa con el que se trabaja.

#Ahora se aplica la prueba t-student para muestras independientes.
#Se cumplen las condiciones de normalidad y los datos fueron obtenidos de manera aleatoria, 
#por lo tanto son independientes de la muestra.

prueba_t <- t.test(x = transformada_a,
                   y = transformada_b, 
                   paired = FALSE,
                   alternative = "two.sided",
                   mu = 0,
                   conf.level = 1 - alfa)$p.value
print(prueba_t)

#El P-value = 1.818e-6 < alfa, por lo tanto se rechaza la hipótesis nula a favor de la hipótesis alternativa.
#Por ende, existe prueba suficiente para afirmar que los tiempos de ejecucion de ambos algoritmos para resolver
#problema del vendedor viajero son diferentes y uno es mas rápido que el otro.



#-------------------------------Pregunta 2--------------------------------------

#Se desea estudiar el nivel de ingreso de los Chilenos dependiendo su nivel de 
#educación, en particular, se desea determinar si existe una diferencia entre 
#la media de los ingresos percapita de jefes de hogares que cuenten con un 
#título profesional y un título técnico completos

#-------------------------------------------------------------------------------


#Se realiza la lectura de los datos y se define la seed a utilizar para
#obtener una muestra de los datos
datos <- read.csv2("C:/Users/alena/Downloads/EP11 Datos.csv")
set.seed(1234)
muestra <- sample_n(datos,450)

#Se filtran las personas que poseen los títulos de educación a estudiar
tecnicos <- muestra %>% filter(educ ==  "Técnico Nivel Superior Completo")
profesional <- muestra %>% filter(educ ==  "Profesional Completo")
tecnicos <- tecnicos %>% select(id.vivienda,educ,ytotcorh)
profesional <- profesional %>% select(id.vivienda,educ,ytotcorh)

#Se calcula varianza de los datos para obtener la diferencia de estas
varTecnicos <- var(tecnicos$ytotcorh)
varProfesionales <- var(profesional$ytotcorh)
difVar <- varTecnicos - varProfesionales

#Se verifica normalidad de los datos
gTecnicos <- ggqqplot(tecnicos,
                      x= "ytotcorh",
                      color = "blue")
print(gTecnicos)
gProfesional <- ggqqplot(profesional,
                         x = "ytotcorh",
                         color = "red")
print(gProfesional)


#Se observa que, tanto los tecnicos, como los profesionales no siguen una distribución
#normal. Adicionalmente se tiene una enorme diferencia entre varianzas de las muestras.
#Por lo tanto, se aplicará la prueba de Yuen para dos muestras independientes, especificamente
#la función pb2gen que utiliza bootstrapping con la función mean como estadístico a emplear.


#Como hipótesis se tiene:
#Ho: La diferencia de medias entre los ingresos percápita de jefes de hogares que
#cuenten con un título profesional y un título técnico completos no existe (ut1 = ut2)

#Ha: La diferencia de medias entre los ingresos percápita de jefes de hogares que
#cuenten con un título profesional y un título técnico completos si existe (ut1 != ut2)

#se ultiliza un nivel de significancia y cantidad de muestras a generar con bootstrapping
alfa <- 0.05
boots <- 599

#Se juntan las tablas de datos
datosJuntos <- rbind(profesional,tecnicos)

#Se aplica la prueba de Yuen utilizando bootstrapping
set.seed(1234)
prueba <- pb2gen(ytotcorh ~ educ, data = datosJuntos, est = "mean", nboot = boots)

print(prueba)

#Obtenemos que el p_valor > alfa, por lo tanto, se falla en rechazar H0 en favor a Ha.
#Obteniendo que La diferencia de medias entre los ingresos per capita de jefes de hogares que
#cuenten con un título profesional y un título técnico completos




#-------------------------------Pregunta 3--------------------------------------

#Analice la segunda pregunta abordada en el ejercicio práctico 11, 
#con los mismos datos, utilizando un método robusto adecuado.

# Propongan una pregunta de investigación original, que involucre la comparaciÃ³n 
# de las medias de mÃ¡s de dos grupos independientes (mÃ¡s abajo se dan unos 
# ejemplos). Fijando una semilla distinta a la anterior, seleccionen una muestra
# aleatoria de hogares (400 < n < 600) y respondan la pregunta propuesta 
# utilizando bootstrapping. Solo por ejercicio acadÃ©mico, aplique un anÃ¡lisis 
# post-hoc con bootstrapping aunque este no sea necesario.

#-------------------------------------------------------------------------------

# Pregunta de investigaciÓn original:
# Determinar si existe relaciÓn entre las medias de las edades entre personas 
# solteras, casadas y divorciadas en la RegiÓn Metropolitana.

#Se necesita realizar una prueba bilateral para dos muestras independientes.

#Hipótesis, prueba bilateral: 
# H0: Las medias de las edades entre personas solteras, casadas y divorciadas 
# en la Región Metropolitana, son iguales

# Ha: Las medias de las edades entre personas solteras, casadas y divorciadas 
# en la Región Metropolitana, son diferentes en al menos un estado civil.

#Obtención de los datos:
set.seed(224)
n2 = 600

data <- read.csv2("C:/Users/alena/Downloads/EP11 Datos.csv")
data <-data%>%filter(region =="Región Metropolitana de Santiago" & edad >= 18)

muestra2 <- sample_n(data, n2)

casado <- muestra2%>%filter(ecivil == "Casado(a)")
soltero <- muestra2%>%filter(ecivil == "Soltero(a)")
divorciado <- muestra2%>%filter(ecivil == "Divorciado (a)")

edades_casado <- casado$edad
edades_soltero <- soltero$edad
edades_divorciado <- divorciado$edad

#Comprobar normalidad de los datos:
normalidad_s <- shapiro.test(edades_soltero)
print(normalidad_s)
normalidad_c <- shapiro.test(edades_casado)
print(normalidad_c)
normalidad_d <- shapiro.test(edades_divorciado)
print(normalidad_d)

#No todos los datos que usan en la prueba cumplen la condición de normalidad.
#Por lo tanto, se realiza una prueba con método robusto.

#Se comparan las diferentes edades usando medias truncadas.

n_c = length(edades_casado)
n_s = length(edades_soltero)
n_d = length(edades_divorciado)

edades <- c(edades_casado, edades_soltero, edades_divorciado)
e_civil <- c(rep("casado", n_c), rep("soltero", n_s), rep("divorciado", n_d))
datos <- data.frame(edades, e_civil)

medias_truncadas<- t1way(edades ~ e_civil,
                          data = datos,
                          tr = 0.3,
                          alfa = alfa)

print(medias_truncadas)

#El P-value = 0 < alfa, por lo tanto se rechaza la hipótesis nula a favor de la hipótesis alternativa.
#Por ende, existe prueba suficiente para afirmar que las medias de las edades entre personas solteras, 
#casadas y divorciadas en la Región Metropolitana, son diferentes en al menos un estado civil.

#Como se rechaza H0,se procede a realizar post-hoc

post_hoc <- lincon(edades ~ e_civil,
                   data = datos,
                   tr = 0.1,
                   alfa = alfa)
print(post_hoc)

