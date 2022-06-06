library (dplyr)
library(ggpubr)
library(ggplot2)
library(ez)

#====================
#==== Pregunta 1 ====
#====================

# En este momento, los investigadores buscan determinar si existen diferencias en el 
# tiempo que tardan los usuarios en formular una consulta para un problema de dificultad 
# media en las áreas de leyes, música y matemáticas.

#Hipotesis:
#H0: El promedio del tiempo que tardan los usuarios en formular una consulta para un problema de dificultad media
#    es igual en las areas de leyes, musica y matematicas.
#HA: El promedio del tiempo que tardan los usuarios en formular una consulta para un problema de dificultad media
#    es distinto en por lo menos un area de las 3 estudiadas.

#Se leen los datos desde el archivo EP08 Datos.csv.
basename <- "EP08 Datos.csv"
file <- file.path("C:/Users/adolf/Desktop/Cosas/IME/IME_equipo_4", basename)
datos <- read.csv2(file = file)

#Se filtran las observaciones que tienen una dificultad media.
dificultadMedia <- datos %>% filter(dificultad=="Media")
#Se filtran las observaciones para cada area: leyes, musica y matematicas.
Leyes <- dificultadMedia %>% filter(area=="Leyes" )
Musica <- dificultadMedia %>% filter(area=="Música")
Matematicas <- dificultadMedia %>% filter(area=="Matemáticas")

#Se crea un nuevo dataframe con los tres obtenidos al filtrar por area.
datos <- rbind(Leyes, Musica, Matematicas)
datos[["area"]] <- factor(datos[["area"]])
datos[["id"]] <- factor(1:nrow(datos))

#Se genera un gráfico Q-Q para ver si las muestras siguen una distribución normal.
g <- ggqqplot(datos,
              x="tiempo",
              y="area",
              color="area")
g <- g + facet_wrap(~ area)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")

#Se imprime el grafico.
print(g)

#Se fija el nivel de significación a usar en 0.05.
alfa = 0.05

#Se realiza la prueba ANOVA usando la función ezANOVA.
pruebaAnova <- ezANOVA(
  data = datos,
  dv = tiempo,
  between = area,
  wid = id,
  return_aov = TRUE)

#Se imprime el resultado de la prueba.
print(pruebaAnova)

#De la prueba de ANOVA realizada se obtiene un p-valor igual a 1.16x10^-12, el cual es claramente
#muy menor a nivel de significación fijado en 0.05, por lo que se rechaza la hipotesis nula en favor
#de la hipotesis alternativa, concluyendo con un 95% de confianza que la media del tiempo que tardan
#los usuarios en formular una consulta para un problema de dificultad media es diferente en por lo
#menos una de las areas estudiadas.

#Para determinar cuales de las areas son las que presentan una media diferente a las otras se realiza 
#un analisis post-hoc usando la prueba HSD de Tuckey.

#Dado que la función TukeyHSD recibe como parametro el resultado de una prueba de anova realizada
#con la función aov, se realiza nuevamente la prueba usando dicha funcion.
pruebaAnova2 <- aov(tiempo~area, datos)

#Se realiza la prueba HSD de Tuckey para la prueba anterior.
post_hocTukey <- TukeyHSD(pruebaAnova2, "area", ordered=TRUE, conf.level=1-alfa)

#Se imprime el resultado de la prueba realizada.
print(post_hocTukey)

#Al analizar los resultados de la prueba, se observa que el p-valor ajustado obtenido para cada 
#comparacion entre las areas son menores al nivel de significacion fijado en 0.05, por lo que 
#se puede concluir que todas las areas presentan medias de tiempo distintas.