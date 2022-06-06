library(dplyr)
library(tidyverse)
library(ggpubr)
library(ez)
library(nlme)
library(emmeans)
#=========================
#==== Lectura de datos ===
#=========================
#cambiar dependiendo de la direccion de su archivo
datos <- read.csv2("C:/Users/fabia/Desktop/IME-Unidad2/EP09/EP08 Datos.csv",stringsAsFactors = FALSE)

#----------------------------------------------------------------------------------------

#=========================
#==== Enunciado ===
#=========================

#En este momento, los investigadores buscan determinar 
#si existen diferencias en el tiempo que tardan los usuarios 
# en formular consultas para problemas con diferente nivel 
# de dificultad en el área de leyes.

#Teniendo en consideracion un alfa = 0.05, se piensa utiliza la prueba ezANOVA
# debido a que el problema corresponde a muestras correlacionadas. Y la variable
# categorica (dificultad) cuenta con 3 niveles.

#----------------------------------------------------------------------------------------

#Filtramos los datos segun el enunciado
leyes <- datos %>% filter(area == "Leyes")

#Creamos los factores
leyes[["dificultad"]] <- factor(leyes[["dificultad"]])
leyes[["instancia"]] <- factor(factor(leyes$id))

#----------------------------------------------------------------------------------------

#=================================
#==== Formulacion de Hipotesis ===
#=================================
#H0 : el tiempo de consulta promedio es igual para las distintas dificultades en el area de leyes.
#HA : el tiempo de consulta promedio es distinto para al menos una dificultad en el area de leyes.

# Forma matemática: Se considera delta como la diferencia entre los 
# promedios de tiempo (ut) para la formulacion de consultas de distinta dificultad 
# en el area de leyes.

#H0: delta ut = 0
#HA: delta ut != 0

#----------------------------------------------------------------------------------------

#====================
#==== Condiciones ===
#====================

#Como el tiempo es una escala de razon, se tienen intervalos iguales.

#Considerando los datos como un estudio se asume que son aleatorios e independientes.

#Comprobación Normalidad
g <- ggqqplot(leyes,
              x = "tiempo",
              y = "dificultad",
              color = "dificultad")
g <- g + facet_wrap(~ dificultad)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)
#En base al grafico entregado, podemos concluir que la muestra es cercana a la normalidad

#Para determinar si las varianzas entre los diferentes nieveles de las medias repetidas deben ser iguales

# Se utiliza la funcion ezANOVA, la cual contiene la prueba de Mauchly

prueba2 <- ezANOVA(data=leyes,
                   dv=tiempo,
                   within=dificultad,
                   wid=instancia,
                   return_aov=TRUE)
print(prueba2$`Mauchly's Test for Sphericity`)

#Obteniendo un valor p = 0.1224 para la prueba de esfericididad, mayor que el alfa de 0.05, de lo que se desprende que los datos del ejemplo si
# cumplen con la condicion de esfericidad (hipotesis nula de la prueba de Mauchly).

#----------------------------------------------------------------------------------------

#====================
#==== Desarrollo ===
#====================
#Por lo que podemos utlizar el p valor entregado en la prueba ezANOVA
print(prueba2$ANOVA)
#Obteniendo un p = 3.138e-26, muy inferior al alfa definido, por lo que podemos rechazar con un 95% de confianza
# la hipotesis nula en favor de la alternativa, por lo que el tiempo de consulta promedio es distinto para 
# al menos una dificultad en el area de leyes.

#Debido a que al menos una dificultad es distinta, se procede a determinar cuales son distintas, con un 
# procedimiento post-hoc HSD de tukey, sin embargo este no se encuentra adaptado para las muestras 
# correlacionadas, por ello se realiza un ajuste mixto.
mixto <- lme( tiempo ~ dificultad , data = leyes , random = ~1| instancia )
medias <- emmeans( mixto , "dificultad" )
tukey <- pairs( medias , adjust = "tukey" )

print(tukey)

#----------------------------------------------------------------------------------------

#====================
#==== Conclusion ===
#====================

#Tomando los resultados entregados por el ajuste post-hoc, obtenemos que todos los p valor son 
# menores al nivel de signficacion (alfa), por lo que se concluye con un 95% de confiaza que 
# el tiempo de consulta promedio es distinto para todas las dificultades en el area de leyes.

#----------------------------------------------------------------------------------------
