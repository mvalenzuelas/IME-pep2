library(tidyverse)
library(dplyr)
library(ggpubr)
library(ez)

basename <- "EP08 Datos.csv"
file <- file.path("C:/Users/Dell PC/Desktop/IME-2022/Actividades/act8",basename)
datos <- read.csv2(file = file)

# ******************************************** ENUNCIADO ********************************#
#Un equipo de investigadores del área de interacción humano-información está 
#estudiando si el área temática y el nivel de dificultad del problema de 
#información influyen en el tiempo (en segundos) que toma un usuario en formular 
#una consulta de búsqueda para resolver dicho problema. Para ello, han reclutado 
#a un grupo de participantes voluntarios, asignados aleatoriamente a distintos 
#grupos. Cada participante debe resolver tres problemas de información con 
#diferentes niveles de dificultad: baja, media y alta.

#En este momento, los investigadores buscan determinar si existen diferencias en
#el tiempo que tardan los usuarios en formular una consulta para un problema de 
#dificultad media en las áreas de economía, literatura y arquitectura.
# ****************************************************************************************#

# Objetivo: Se necesita determinar si existen diferencias en la media de tiempos para problemas de dificultad
# media en las áreas de economía, literatura y arquitectura. Como queremos averiguar si existen 
# diferencias en las medias de más de dos grupos para muestras independientes, se puede utilizar
# la prueba ANOVA para muestras independientes.

# Condiciones:
# 1) La escala con que se mide la variable dependiente tiene las propiedades de una escala de intervalos iguales.
# 2) Las k muestras son obtenidas de manera aleatoria e independiente desde la(s) población(es) de origen.
# 3) Se puede suponer razonable que la(s) población(es) de origen sigue(n) una distribución normal.
# 4) Las k muestras tienen varianzas aproximadamente iguales.

# Se filtran los datos a utilizar;
datos <- datos %>% filter(dificultad == "Media" & (area == "Economía" | area == "Literatura" | area == "Arquitectura"))
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
             y = "area",
             color = "area")
g <- g + facet_wrap(~ area)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")
print(g)

# Se puede observar una gran mayoría de puntos dentro del umbral aceptado, sin embargo,
# Se observan algunos puntos atípicos, por lo que se procede a utilizar un 
# nivel de significancia más estricto.

# 4) Se verifica si las muestras tienen varianzas aproximadamente iguales:
econo <- datos %>% filter(area == "Economía")
lit <- datos %>% filter(area == "Literatura")
arqui <- datos %>% filter(area == "Arquitectura")

varEcono <- var(pull(econo, tiempo))
varLit <- var(pull(lit, tiempo))
varArqui <- var(pull(arqui, tiempo))

maximo <- max(c(varEcono,varLit,varArqui))
minimo <- min(c(varEcono,varLit,varArqui))

homo <- maximo/minimo

# Es menor a 1,5 así que se comprueba homogeneidad

#Como se comprueban las condiciones, se procede con el método ANOVA de una vía 
#para muestras independientes, planteando las hipótesis del problema.


# Hipótesis
# H0: el tiempo promedio que tardan los usuarios en formular una consulta para un
# problema de dificultad media es igual en las 3 áreas.

# HA: el tiempo promedio que tardan los usuarios en formular una consulta para un
# problema de dificultad media es diferente en al menos un área.


#Procedimiento ANOVA
alfa <- 0.01

prueba <- aov(tiempo ~ area, data = datos)
print(summary(prueba))
# p = 8.03e-11

# Según los resultados de la prueba aov, como p < alfa, se rechaza la hipótesis nula en favor de 
# la hipótesis alternativa, por lo que se concluye con un 99% de confianza que el tiempo
# promedio que tardan los usuarios en formular una consulta para un problema de dificultad
# media es diferente en la menos un área. Ahora se procederá a realizar el procedimiento
# POST-HOC HSD de Tukey, debido a que es más poderosa que los factores de corrección
# de Bonferromi y Holm.

# Grafico del tamaño de efecto:
g2 <- ezPlot(
  data = datos,
  dv = tiempo,
  wid = id,
  between = area,
  y_lab = "Tiempo promedio en formular consulta [s]",
  x = area
)
print(g2)

# Se puede apreciar una fuerte diferencia en el tiempo promedio en las áreas de
# Economía-Arquitectura y Economía-Literatura.

post_hoc <- TukeyHSD(prueba,
                     "area",
                     ordered = TRUE,
                     conf.level = 1 - alfa)
print(post_hoc)

# A partir de la prueba post-hoc se concluye que existen diferencias significativas entre
# el tiempo promedio que tardan los usuarios en formular una consulta para un
# problema de dificultad media en las áreas de Economía-Literatura y Economía-Arquitectura
# (como se observa en el gráfico de tamaño de efecto),
# pues sus valores p son menores a alfa (p = 0 en ambos casos).


