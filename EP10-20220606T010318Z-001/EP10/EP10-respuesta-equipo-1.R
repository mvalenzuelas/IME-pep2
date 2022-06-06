library(dplyr)
library(ggpubr)
library(tidyverse)
library(ez)
library(nlme)
library(emmeans)

#------------------------------Pregunta 1---------------------------------------
# ¿Existe diferencia en la puntuación obtenida por los envases diseñados por 
# PackPro según las evaluaciones realizadas por niños y jóvenes?
#-------------------------------------------------------------------------------

data <- read.csv2("C:\\Users\\danie\\Downloads\\EP10 Datos.csv")

packpro <- data %>% filter(Diseno == "PackPro")

ninos <- packpro %>% filter(Edad == "Nino")
jovenes <- packpro %>% filter(Edad == "Joven")

#Se trabajará con la Prueba de suma de rangos de Wilcoxon, para evaluar 
#independencia en muestras independientes.

alfa <- 0.05

#Las muestras son independientes, pues son mútuamente excluyentes

#La escala de las muestras es una escala Likert, pues son evaluaciones que van 
#de menor a mayor puntaje, por lo que sí implica una relación de orden.


#Se definen las hipótesis nula y alternativa
#H0: La media de las diferencias en la puntuación obtenida por los envases 
#diseñados por PackPro según las evaluaciones realizadas por niños y jóvenes, 
#son iguales

#Ha: La media de las diferencias en la puntuación obtenida por los envases 
#diseñados por PackPro según las evaluaciones realizadas por niños y jóvenes, 
#son diferentes

prueba <- wilcox.test(ninos$Puntaje, 
                      jovenes$Puntaje, 
                      alternative = "two.side", 
                      conf.level = 1-alfa)
print(prueba)
#p-valor < 2.2e-16

#Como p-valor<alfa, se rechaza H0 en favor de Ha, por ende, la media de las 
#diferencias en la puntuación obtenida por los envases diseñados por PackPro 
#según las evaluaciones realizadas por niños y jóvenes, son diferentes


#----------------------------Pregunta 2-----------------------------------------
# ¿Existen diferencias entre las puntuaciones obtenidas para los diferentes 
# envases de galletas? De ser así, ¿cuál(es) envase(s) se diferencia(n) de los 
# demás?
#-------------------------------------------------------------------------------

galletas <- data %>% filter(Producto == "Galletas")

gPackpro <- galletas%>% filter(Diseno == "PackPro")
gKool <- galletas%>% filter(Diseno == "KoolDesign")
gKajita <- galletas%>% filter(Diseno == "LaKajita")
gColor <- galletas%>% filter(Diseno == "DisenoColor")

#Se trabajará con la prueba de Friedman para más de 2 muestras correlacionadas,
#pues se busca determinar evaluaciones en diferentes envases que son evaluadas 
#por un mismo sujeto

#La variable independiente es Diseno, que es categórica y tiene 4 niveles

#La escala de las muestras es una escala Likert, pues son evaluaciones que van 
#de menor a mayor puntaje, por lo que sí implica una relación de orden.

#Las muestras son obtenidas de manera aleatoria, por lo tanto, son 
#independientes de la población.


#Se definen las hipótesis nula y alternativa

#H0: La diferencia de las medias en la puntuación obtenida para los diferentes 
#envases de galletas, son iguales

#Ha: La diferencia de las medias en la puntuación obtenida para los diferentes 
#envases de galletas, es diferente, para al menos, una.

puntaje <- c(gPackpro$Puntaje, gKool$Puntaje, gKajita$Puntaje, gColor$Puntaje)

largoPackPro <- length(gPackpro$Puntaje)
largoKool <- length(gKool$Puntaje)
largoKajita <- length(gKajita$Puntaje)
largoColor <- length(gColor$Puntaje)

envase <- c(rep("PackPro", largoPackPro),
              rep("Kool", largoKool),
              rep("Kajita", largoKajita),
              rep("Color", largoColor))

envase <- factor(envase)

Sujeto <- rep(1:largoColor, 4)

datos <- data.frame(Sujeto, puntaje, envase)

prueba2 <- friedman.test(puntaje ~ envase | Sujeto, data = datos)
print(prueba2)
#p-valor=0.3283 > alfa

#Por lo tanto, se falla en rechazar Ho en favor de Ha, es decir, existe 
#evidencia suficiente para asegurar, con un 95% de confianza, que la 
#diferencia de las medias en la puntuación obtenida para los diferentes 
#envases de galletas, son iguales. De esta forma, no es necesario.
