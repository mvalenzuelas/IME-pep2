library(ggpubr)
library(rcompanion)
library(ggpubr)
library(ez)
library(tidyverse)
library(DescTools)
library(WRS2)
# ******************************************** PREGUNTA 1 ********************************#
# En el trabajo de título de un estudiante del DIINF se reportan los siguientes 
# tiempos de ejecución (en milisegundos) medidos para dos versiones de un algoritmo 
# genético para resolver instancias del problema del vendedor viajero disponibles en 
# repositorios públicos. ¿Es uno de los algoritmos más rápido que el otro? 
# Aplique una transformación de datos adecuada para responder.
# ****************************************************************************************#

# Cargar datos
instanciaA <- c(167, 8, 65, 91, 125, 64, 196, 117, 41, 56)
tiempoA <- c(1510394, 251843, 834565, 37449, 48705, 402929, 885722, 8576989, 62764, 783108)
instanciaB <- c(197, 7, 21, 195, 191,149, 39, 139, 154, 137)
tiempoB <- c(48408, 35974, 5743260, 6684497, 1252837, 6701654, 6568968, 120276, 1174562, 2830464)

tabla <- data.frame(instanciaA, tiempoA, instanciaB, tiempoB )

# Al mirar la tabla, se puede observar que todos los tiempos para ambos algoritmos son de 
# instancias distintas, por lo que los tiempos para ambos algoritmos son independientes
# entre si. 

# Como se necesita comparar las medias de dos poblaciones independientes entre sí y se cuenta
# con una muestra pequeña para ambas medias (n = 10), se piensa en utilizar la Prueba t 
# para dos muestras independientes.

# Condiciones a cumplir para utilizar la prueba:
# 1) Las observaciones para ambas muestras son independientes entre sí.
# 2) Las muestras son independientes entre sí.
# 2) Las observaciones provienen de una distribución cercana a la normal.

# Verificando condiciones:
# 1) Se puede suponer que las observaciones para ambas muestras fueron elegidas al azar
#    al ser de distintas instancias, por lo que se puede suponer que las observaciones
#    son independientes entre sí.
# 2) Ambas muestras son independientes entre sí, pues son diferentes instancias y fueron
#    ejecutados por dos algoritmos distintos. 
# 3) Verificando normalidad con la prueba de Shapiro y grafico Q-Q:

shapiro.test(tabla[["tiempoA"]])
# p = 7.913e-06
shapiro.test(tabla[["tiempoB"]])
# p = 0.01748

# Corroborando con un grafico Q-Q
tiempo <- c(tiempoA, tiempoB)
algoritmo <- c(rep("A", length(tiempoA)),  rep("B", length(tiempoB)) )
datos <- data.frame(tiempo, algoritmo)
  
g <- ggqqplot(datos, x = "tiempo", facet.by = "algoritmo", 
            palette = c("blue", "red"), color = "algoritmo")
print(g)

# Se puede observar que los tiempos para la muestra B apenas se cumple la normalidad y para
# los tiempos de la muestra A no se cumplen debido a la presencia de un punto atípico en 
# la parte derecha del gráfico.

# Conclusión, como no se comprueba la normalidad entonces no es posible realizar una prueba
# T de student, por lo que se utilizara otro método.

# Se utilizara una transformación adecuada a los datos, como se señala en el enunciado.


# Buscar la mejor transformación de Tukey usando una función de R.
transformacionTukey <- transformTukey(tiempo, start = -3, end = 3,
                                 int = 0.001, returnLambda = TRUE)
# lambda = 0.107
tiempoTukey <- transformTukey(tiempo, start = -3, end = 3,
                                      int = 0.001, returnLambda = FALSE)
tiempoA_Tukey <- tiempoTukey[1:10]
tiempoB_Tukey <- tiempoTukey[11:20]

# Se estudia la normalidad con el test de shapiro
shapiro.test(tiempoA_Tukey)
# p = 0.4674
shapiro.test(tiempoB_Tukey)
# p = 0.03876


# grafico Q-Q para el tiempo con transformada de Tukey
tiempoTukey <- c(tiempoA_Tukey, tiempoB_Tukey)
algoritmoTukey <- c(rep("A", length(tiempoA_Tukey)),  rep("B", length(tiempoB_Tukey)) )
datosTukey <- data.frame(tiempoTukey, algoritmoTukey)

g_tukey <- ggqqplot(datosTukey, x = "tiempoTukey", facet.by = "algoritmoTukey", 
              palette = c("blue", "red"), color = "algoritmoTukey")
print(g_tukey)

# Se puede ver que con la transformacion de Tukey, los datos se comportan
# de manera mucho mas normal.

# Buscar la mejor transformación Box-Cox usando funciones de R.
lambda <- BoxCoxLambda(tiempo, lower = -4, upper = 4)
# lambda = -0.01864571
tiempoBoxCox <- BoxCox(tiempo, lambda)

# Se estudia la normalidad con el test de shapiro
tiempoA_BoxCox <- tiempoBoxCox[1:10]
tiempoB_BoxCox <- tiempoBoxCox[11:20]

shapiro.test(tiempoA_BoxCox)
# p-value = 0.5494
shapiro.test(tiempoB_BoxCox)
# p-value = 0.02344

# grafico Q-Q para el tiempo con transformada de BoxCox
algoritmoBoxCox <- c(rep("A", length(tiempoA_BoxCox)),  rep("B", length(tiempoB_BoxCox)) )
datosBoxCox <- data.frame(tiempoBoxCox, algoritmoBoxCox)

g_BoxCox <- ggqqplot(datosBoxCox, x = "tiempoBoxCox", facet.by = "algoritmoBoxCox", 
                    palette = c("blue", "red"), color = "algoritmoBoxCox")
print(g_BoxCox)

# Se puede ver que con la transformacion de BoxCox, los datos se comportan
# de manera mucho más normal.

# Como ambas transformaciones son cercanas a la normal, con p valores parecidos
# para las distintas pruebas de Shapiro, se usará la transformación de Tukey
# ya que esta prueba nos entrega más información, al mostrar 3 gráficos distintos.

# Se establecen las hipótesis:
# Sea A y B las medias para los tiempos de ambos algoritmos con la transformación de Tukey.
# H0: mediaA = mediaB  ; (No hay diferencia entre la velocidad media de ambos algoritmos)
# HA: mediaA != mediaB ; (Si hay diferencia entre la velocidad media de ambos algoritmos)

# Definiendo un alfa de (0.01), podemos decir que los datos transformados
# se comportan de manera normal, por lo que se cumple la tercera condición para poder 
# utilizar la prueba T de student para dos muestras independientes.

alfa = 0.01
# Se realiza la prueba utilizando t.test
prueba <- t.test(tiempoA_Tukey, tiempoB_Tukey , alternative = "two.sided", mu = 0, conf.level = 1- alfa)
print(prueba)
# p-value = 0.2302

# Dado que el valor de p > alfa , no existe evidencia suficiente para rechazar la hipótesis nula en favor de la 
# hipótesis alternativa. Finalmente, se puede asegurar con un 99% de confianza que no existe diferencia 
# entre la velocidad media de ambos algoritmos.

# ******************************************** PREGUNTA 2 ********************************#
# Analice la primera pregunta abordada en el ejercicio práctico 11, con los mismos datos, 
# utilizando un método robusto adecuado.
# ****************************************************************************************#

basename <- "EP11 Datos.csv"
dir = "C:/Users/Dell PC/Desktop/IME-2022/Actividades/act11"
file <- file.path(dir,basename)
datos2 <- read.csv2(file = file)

# Un estudio en Chile busca identificar las situaciones laborales en el país dependiendo del sexo 
# y si existen diferencias, para ello realizan un estudio para verificar si el promedio de horas 
# laborares(variable o10) que trabajan hombres y es el mismo que el promedio de hora laborales de las mujeres.

#Filtramos los datos necesarios
datos3 <- datos2 %>% filter(o1 == "Sí")
datos3 <- datos3 %>% filter(o10 != "NA")
datos3 <- datos3 %>% select(sexo,o10)

# Se establece una semilla
set.seed(341)

# Se obtiene la muestra de tamaño 400
tamano <- 400
datos3 <- sample_n(datos3,tamano)

# Separamos las muestras
datosHombre <- datos3 %>% filter(sexo == "Hombre")
vectorHombre <- as.vector(t(datosHombre %>% select(o10)))
datosMujer <- datos3 %>% filter(sexo == "Mujer")
vectorMujer <- as.vector(t(datosMujer %>% select(o10)))

# Estudiando normalidad de los datos:
gh <- ggqqplot(datosHombre$o10,title="Horas laborales hombres")
gm <- ggqqplot(datosMujer$o10,title="Horas laborales mujeres")

# Como se puede ver en los gráficos cuantil-cuantil las horas
# laborales no se distribuyen normal puesto a que hay muchos cuantiles
# que se alejan demasiado del cuantil teórico, lo que no deja
# las condiciones para hacer una prueba t de Student de diferencia medias. 
# Es por esto por lo que se usará una transformación ad-hoc a los datos con métodos robustos,
# antes de aplicar la prueba t de Student

# Se establecen las hipótesis:
# Ho: No existe diferencia entre las horas laborales de hombres y mujeres.
# uh-um = 0
# Ha: Existe diferencia entre las horas laborales de hombres y mujeres.
# uh-um != 0

# n hombres = 244
# n mujeres = 156

# Se estudia la varianza de los datos:
varHombres = var(datosHombre$o10)
varMujeres = var(datosMujer$o10)

# varianza hombres = 130
# varianza mujeres = 228

# Como la varianza y tamaños de ambas muestras son muy diferentes,
# y la prueba es bilateral, entonces se piensa aplicar una prueba de Yuen para
# muestras independientes, ya que es una buena alternativa a la prueba 
# T de student , cuando las varianzas de ambas muestras son muy diferentes 
#o los tamaños de las muestras son muy dispares.

# Definiendo significación
alfa = 0.05

# Juntando datos.
datos4 <- rbind(datosHombre,datosMujer)

# Definiendo poda
gamma = 0.2

# Aplicando prueba de Yuen
prueba2 <- yuen(o10~sexo,data=datos4,tr=gamma)
print(prueba2)

# Como p valor = 0.00567 < alfa, entonces se rechaza Ho a favor de Ha, por
# lo que se concluye con 95% de confianza que hay diferencia entre el horario
# laboral medio de hombres y mujeres.

# ******************************************** PREGUNTA 3 ********************************#
# Analice la primera pregunta abordada en el ejercicio práctico 11, con los mismos datos, 
# utilizando un método robusto adecuado.
# ****************************************************************************************#

# Se define una pregunta:
# ¿Existe una diferencia significativa entre el ingreso per cápita de las personas en 
# la Región de Antofagasta, Región de Atacama y la Región de Los Lagos?

datos5 <- datos2 %>% filter(region == "Región de Antofagasta" | 
                             region == "Región de Atacama" |
                             region == "Región de Los Lagos" ,numper != "NA")

# Se establece una semilla
set.seed(341)

# Se obtiene la muestra de tamaño 500
tamano <- 500
muestra <- datos5[sample(nrow(datos5), tamano),]

# Se construye el dataframe con los datos en investigación
ytotcorh <- as.integer(muestra[["ytotcorh"]])
integrantes <- as.integer(muestra[["numper"]])
ingreso <- as.integer(ytotcorh/integrantes)
region <- factor(muestra[["region"]])
instancia <- factor(1:tamano)

datos6 <- data.frame(instancia, region, ingreso)

# Se define un alfa:
alfa <- 0.05

# se obtiene los ingresos per cápita de la Región Atacama
datosRAT  <- datos6 %>% filter(region == "Región de Atacama")
datosRAT_ingreso <- (datosRAT[["ingreso"]])

# se obtiene los ingresos per cápita de la Región Antofagasta
datosRA  <- datos6 %>% filter(region == "Región de Antofagasta")
datosRA_ingreso <- datosRA[["ingreso"]]

# se obtiene los ingresos per cápita de la Región de Los Lagos
datosRL  <- datos6 %>% filter(region == "Región de Los Lagos")
datosRL_ingreso <- datosRL[["ingreso"]]

print(shapiro.test(datosRAT_ingreso))
# p = 5.339e-11
print(shapiro.test(datosRA_ingreso))
# p = 2.2e-16
print(shapiro.test(datosRL_ingreso))
# p = 2.2e-16

# Se grafican los datos con un grafico QQ para ver los puntos atipicos.
qq1 <- ggqqplot(datosRAT$ingreso,title="Ingreso per capita Region de Antofagasta")
qq2 <- ggqqplot(datosRA$ingreso,title="Ingreso per capita Region de Atacama")
qq3 <- ggqqplot(datosRL$ingreso,title="Ingreso per capita Region de Los Lagos")
print(qq1)
print(qq2)
print(qq3)
# No se cumple la normalidad para ninguna de las muestras

# Se estudia los tamaños de las muestras:
tañano_RAT <- nrow(datosRAT)
# tañano_RAT = 119
tañano_RA <- nrow(datosRA)
# tamaño_ra = 157
tañano_RL <- nrow(datosRL)
# tamaño_RL = 224

# Se estudia la homocedasticidad de las muestras con la función de ezANOVA
pruebaAnova <- ezANOVA(data = datos6, dv = ingreso, between = region, 
                       wid = instancia , return_aov = TRUE)

print(pruebaAnova[["Levene's Test for Homogeneity of Variance"]]['p'] )
# p = 0.02209169

# Como los tamaños de las muestras son dispares, no se cumple la condición de 
# homocedasticidad y normalidad, entonces no es posible utilizar la prueba ANOVA para muestras
# independientes. Por este motivo, se procederá a utilizar una prueba robusta.

# Definimos nuestras hipótesis
# H0: No existe una diferencia significativa entre el ingreso per cápita de las personas en 
#     la Región de Antofagasta, Valparaíso y Los Lagos 
# HA: Si existe una diferencia significativa entre el ingreso per cápita de las personas en 
#     al menos una de las Regiones de Antofagasta, Valparaíso y Los Lagos

# Comparar los diferentes ingresos per cápita por región utilizando
# medias truncadas:

gamma <- 0.2
medias_truncadas <- t1way(ingreso ~ region, data = datos6, tr = gamma,
                          alpha = alfa )
print(medias_truncadas)
# p-value: 0.00048

# Conclusión
# Como p valor = 0.00048 < alfa, entonces se rechaza Ho a favor de Ha, por
# lo que se concluye con 95% de confianza que si existe una diferencia significativa 
# entre el ingreso per cápita de las personas en al menos una de las Regiones 
# de Antofagasta, Valparaíso y Los Lagos

# Se realiza un procedimiento Post-Hoc:

if(medias_truncadas$p.value < alfa){
  set.seed(666)
  post_hoc <- lincon(ingreso ~ region, data = datos6, tr = gamma,
                     alpha = alfa)
  print(post_hoc)
}

# Región de Antofagasta vs. Región de Atacama, p.value = 0.12064
# Región de Antofagasta vs. Región de Los Lagos, p.value = 0.00104
# Región de Atacama vs. Región de Los Lagos, p.value = 0.03189

# Conclusión:
# A partir de la prueba Post-Hoc se concluye que existen diferencias
# significativas en el ingreso per cápita de personas en la región de
# Antofagasta-Los Lagos ( p-value = 0.00104) y Atacama-Los Lagos ( p-value = 0.03189).



