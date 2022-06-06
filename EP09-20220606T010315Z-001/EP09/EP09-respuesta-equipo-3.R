library(tidyverse)
library(dplyr)
library(ggpubr)
library(ez)

basename <- "EP08 Datos.csv"
file <- file.path("C:/Users/Dell PC/Desktop/IME-2022/Actividades/act9",basename)
datos <- read.csv2(file = file)

# ******************************************** ENUNCIADO ********************************#
# Un equipo de investigadores del área de interacción humano-información está estudiando 
# si el área temática y el nivel de dificultad del problema de información influyen en el 
# tiempo (en segundos) que toma un usuario en formular una consulta de búsqueda para 
# resolver dicho problema. Para ello, han reclutado a un grupo de participantes voluntarios, 
# asignados aleatoriamente a distintos grupos. Cada participante debe resolver tres problemas 
# de información con diferentes niveles de dificultad: baja, media y alta. 
# A su vez, cada grupo debe resolver problemas relacionados a una temática diferente. 
# Los datos recolectados contemplan las siguientes variables:

# En este momento, los investigadores buscan determinar si existen diferencias en el tiempo 
# que tardan los usuarios en formular consultas para problemas con diferente nivel de dificultad 
# en el área de química.
# ****************************************************************************************#

# Objetivo: Se necesita determinar si existen diferencias en la media de tiempos que tardan los 
# usuarios en formular consultas para problemas con diferente nivel de dificultad en el area de quimica.
# Como queremos averiguar si existen diferencais en las medias de mas de dos grupos para muestras
# correlacionadas, se puede utilizar la prueba ANOVA de una via para muestras correlacionadas.

# Condiciones:
# 1) La escala con que se mide la variable dependiente tiene las propiedades de una escala de intervalos
#    iguales.
# 2) Las mediciones son independientes al interior de cada grupo.
# 3) Se puede suponer razonablemente que la(s) población(es) de origen sigue(n) una distribución normal.
# 4) La matriz de varianzas-covarianzas es esférica. Como explica Horn (2008, p. 1), esta condición establece
#    que las varianzas entre los diferentes niveles de las medidas repetidas deben ser iguales.

# Se filtran los datos a utilizar;
datos <- datos %>% filter(area == "Química" & (dificultad == "Baja" | dificultad == "Media" | dificultad == "Alta"))
datos[["dificultad"]] <- factor(datos[["dificultad"]])
datos[["area"]] <- factor(datos[["area"]])
datos[["id"]] <- factor(datos[["id"]])

# Verificando condiciones:

# 1) Se cumple la condición anterior ya que la variable dependiente es el tiempo, considerada una escala de razón
#    (presenta 0 absoluto) y la distancia entre sus unidades de medida es uniforme al tratarse de segundos.

# 2) Podemos suponer que las muestras son aleatorias e independientes desde la población de origen 
#    según el enunciado, ya que se señala que un grupo de participantes voluntarios fueron repartidos aleatoriamente
#    a distintos grupos, por lo que la muestra es menor al 10% de la población.

# 3) Se estudia la normalidad de los grupos:
g <- ggqqplot(datos,
              x = "tiempo",
              y = "dificultad",
              color = "dificultad")
g <- g + facet_wrap(~ dificultad)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

# Se puede observar que todos los puntos están dentro del umbral aceptado, por lo que
# se cumple la condición de normalidad en los grupos.

alfa = 0.05

# 4) La matriz de varianzas-covarianzas es esférica.
#     Para la comprobación de esta condición, la función ezAnova() entrega el resultado de la 
#     Prueba de esfericidad de Mauchly.

# Se establecen las hipótesis para la prueba de esfericidad de Mauchly.
# H0: las varianzas-covarianzas de las muestras son iguales para los grupos.
# HA: al menos una de las muestras tiene varianza-covarianza diferente a alguna de los demás grupos.

cat("\n\nProcedimiento ANOVA usando ezANOVA\n\n")
prueba <- ezANOVA(data = datos,
                         dv = tiempo,
                         within = dificultad,
                         wid = id,
                         type = 2,
                         return_aov = TRUE)

cat ("El resultado de la prueba de esfericidad de Mauchly :\n\n")
print(prueba[["Mauchly's Test for Sphericity"]] )

# Gráfico del tamaño del efecto.
g2 <- ezPlot(data = datos, 
             dv = tiempo,
             wid = id, 
             within = dificultad,
             x_lab = "Dificultas",
             y_lab = "Tiempo promedio de las tareas [s]", 
             x = dificultad)
print(g2)

# Como el vapor p = 0.4150249 entregado por la prueba de esfericidad es mayor a alfa, entonces 
# no se rechaza la hipótesis nula en favor de la alternativa, por lo tanto, se puede asegurar 
# con un 95% de confianza que las varianzas-covarianzas de las muestras son iguales para 
# los grupos.

# Se formula la hipótesis principal:

# Hipótesis
# H0: el tiempo promedio que tardan los usuarios en formular una consulta en el área de química 
#     es igual para todas las dificultades.

# HA: el tiempo promedio que tardan los usuarios en formular una consulta en el área de química 
#     es diferente para al menos una dificultad.


print(summary(prueba$aov))
# p = 5.45e-09

# Según los resultados de la prueba anova, como p < alfa, se rechaza la hipótesis nula en favor de 
# la hipótesis alternativa, por lo que se concluye con un 95% de confianza que el tiempo
# promedio que tardan los usuarios en formular una consulta en el área de química es
# diferente para al menos una dificultad. Ahora se procederá a realizar el procedimiento
# post-hoc para estudiar cual o cuales de los grupos 
#presentan diferencias significativas respecto de los demás.

# En este caso se realiza el procedimiento de Bonferroni.

# Procedimiento post-hoc de Bonferroni.
bonferroni <- pairwise.t.test(datos[["tiempo"]],
                               datos[["dificultad"]],
                               p.adj = "bonferroni", 
                               paired = TRUE)

cat("Corrección de Bonferroni\n")
print(bonferroni)

# A partir de la prueba post-hoc se concluye que existen diferencias significativas entre
# el tiempo promedio que tardan los usuarios en formular una consulta en el area de
# quimica para las dificultades Baja-Alta y Baja-Media
# (como se observa en el gráfico de tamaño de efecto),
# pues sus valores p son menores a alfa, 3.6e-07 para Baja-Alta y 
# 1.7e-07 para Baja-Media.
