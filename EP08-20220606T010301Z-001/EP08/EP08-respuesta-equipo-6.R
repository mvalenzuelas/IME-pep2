library(dplyr)
library(tidyverse)
library(ggpubr)
library(ez)
#=========================
#==== Lectura de datos ===
#=========================
#cambiar dependiendo de la direccion de su archivo
dir <- "C:/Users/fabia/Desktop/IME-Unidad2/EP08"   
basename <- "EP08 Datos.csv"
file <- file.path(dir, basename)
datos <- read.csv2(file = file)

#----------------------------------------------------------------------------------------

#=========================
#==== Enunciado ===
#=========================

#En este momento, los investigadores buscan determinar si existen 
# diferencias en el tiempo que tardan los usuarios en formular una 
# consulta para un problema de dificultad difícil en las áreas de 
# economía, psicología y arquitectura.

#----------------------------------------------------------------------------------------

#Suponemos para este trabajo un alfa
alfa <- 0.05

#Filtramos los datos segun las areas indicadas
datos1 <- datos %>% filter(dificultad == "Alta" & area == "Economía")
datos2 <- datos %>% filter(dificultad == "Alta" & area == "Psicología")
datos3 <- datos %>% filter(dificultad == "Alta" & area == "Arquitectura")

#Juntamos lo datos
datosT <- rbind(datos1,datos2,datos3)

#Filtramos los datos en base al area y tiempo
datosT <- select(datosT, c("area","tiempo"))

#Creamos los factores
datosT[["area"]] <- factor(datosT[["area"]])
datosT[["instancia"]] <- factor(1:nrow(datosT))

#----------------------------------------------------------------------------------------

#=================================
#==== Formulacion de Hipotesis ===
#=================================
#H0 : el tiempo de consulta promedio para un problema dificil es igual en las distintas areas.
#HA : el tiempo de consulta promedio para un problema dificil es diferente para al menos una area.

# Forma matemática: Se considera delta como la diferencia entre los 
# promedios de tiempo (ut) para la resolución de un problema difícil de las distintas áreas.

#H0: delta ut = 0
#HA: delta ut != 0
#----------------------------------------------------------------------------------------

#====================
#==== Condiciones ===
#====================

#Como el tiempo es una escala de razon, se tienen intervalos iguales.

#Considerando los datos como un estudio se asume que son aleatorios e independientes.

#Comprobación Normalidad
g <- ggqqplot(datosT,
              x = "tiempo",
              y = "area",
              color = "area")
g <- g + facet_wrap(~ area)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)
#En base al grafico entregado, podemos concluir que la muestra es cercana a la normalidad

#Para saber si las varianzas son aproximadamente iguales
#Se utiliza ezAnova, que incluye la prueba de homocedasticidad de Levene
# con ello debemos tener en cuenta las siguientes hipotesis

#Hipotesis para la prueba de homocedasticidad de Levene
#H0: las varianzas de las k muestras son iguales
#HA: al menos una de las muestras tiene varianza diferente a alguna de las demás

#Realizamos la prueba de ezAnova
pruebaEz <- ezANOVA(
  data = datosT,
  dv = tiempo,
  between = area,
  wid = instancia,
  return_aov = TRUE)

#Mostramos por pantalla
print(pruebaEz)

#Al realizar la prueba, obtenemos los datos de homocedasticidad de Levene
# p = 0.7909918 
# Suponiendo un alfa de 0.05 
# Al ser p > alfa se falla en rechazar la hipotesis nula, por lo tanto podemos concluir con un 95% de confianza,
# que la varianza de las k muestras son iguales

#----------------------------------------------------------------------------------------

#====================
#==== Desarrollo ===
#====================
#En base a lo anterior podemos realizar la prueba a ANOVA
#En la que se tiene un resultado p = 3.115953e-13
#tomando en cuenta el alfa de 0.05
# p < alfa , por lo que se rechaza la hipotesis nula en favor de la alternativa, por lo tanto con un
# 95% de confianza, el tiempo de consulta promedio para un problema dificil es diferente para al menos una area.

#Realizamos la prueba nuevamente con aov, para utilizar el analisis post_hoc de Tukey
pruebaAOV <- aov(tiempo ~area, data = datosT)
post_hoc <- TukeyHSD(pruebaAOV,
                     "area",
                     ordered = TRUE,
                     conf.level = 1-alfa)
#Mostramos el analisis por pantalla
print(post_hoc)

#----------------------------------------------------------------------------------------

#====================
#==== Conclusion ===
#====================

#Teniendo en cuenta los resultados, al obtener un p valor muy bajo en Arquitectura-Economia y Arquitectura-Psicologia 
# al ser menores que el alfa indicado (p < alfa), se rechaza la hipotesis nula en favor de la alternativa por lo que 
# podemos decir con un 95% de confianza que el tiempo de consulta promedio para un problema dificil es diferente estas areas.
# por otro lado se encuentra Psicologia-Economia, el cual se obtiene un p ajustado de 0.2769561, valor mayor al alfa tomado
# por lo que podemos decir con un 95% de confianza que el tiempo de consulta promedio para un problema dificil en estas dos areas son iguales.

#----------------------------------------------------------------------------------------