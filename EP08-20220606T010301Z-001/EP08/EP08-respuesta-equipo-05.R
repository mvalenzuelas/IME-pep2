library(tidyverse)
library(ggpubr)
library(dplyr)
library(patchwork)
library(ez)

"
En este momento, los investigadores buscan determinar si existen diferencias en el 
tiempo que tardan los usuarios en formular una consulta
para un problema de dificultad fácil en las áreas de biología, economía y computación.
"

datos <- read.csv2("C:/Users/osswa/OneDrive/Escritorio/IME/EP08 Datos.csv")

facil<- datos[datos$dificultad == "Baja",]

biologia    <- facil[facil$area == "Biología",] %>% select(area,tiempo)
economia    <- facil[facil$area == "Economía",] %>% select(area,tiempo)
computacion <- facil[facil$area == "Computación",] %>% select(area,tiempo)


"
--Se decide utilizar una prueba ANOVA de una vía para muestras independientes, debido a que las variables son independientes
entre sí, por lo que se procede a revisar las condiciones para realizar la prueba.

Condiciones:
  1) La escala en la que se evalúan las variables es la misma para todas (tiempo) y 
    todas poseen una muestra de 200 sujetos.
  2) La muestra de los datos fueron escogidos al azar.
  
"
graficoB <- ggqqplot(biologia$tiempo)
graficoE <- ggqqplot(economia$tiempo)
graficoC <- ggqqplot(computacion$tiempo)


graficoB + graficoE + graficoC

"
  3) Datos normales (comprobado con gráfico Q-Q)
"

varB <-  var(biologia$tiempo)
varE <-  var(economia$tiempo) 
varC <-  var(computacion$tiempo)

varianza_entre_grupos <- varB/varC

"
  4) Las varianzas dan en promedio 1,338, lo cual es menor que 1.5.
"


#Hipótesis

#H0 : El tiempo promedio de consultas entre biología, computación y economía es igual para las tres áreas
#H1 : El tiempo promedio de consultas entre biología, computación y economía es diferente para al menos un área


# alpha = 0.05

datosNuevos <- rbind(biologia,economia,computacion)
datosNuevos[["instancia"]] <- factor(1:nrow(datosNuevos))

#Se realiza la prueba EZ


pruebaEZ <- ezANOVA(data=datosNuevos, 
                    dv=tiempo, 
                    wid = instancia, 
                    between = area,
                    return_aov=TRUE)

cat("p_valor: ", pruebaEZ$ANOVA$p)


#Conclusión: Se obtuvo un p_valor>alpha, por lo tanto se falla en rechazar H0, y por lo tanto no existe diferencia significativa
#           en los tiempos promedios de las consultas de las áreas estudiadas. Como observación no se requiere la utilización de
#           una prueba post-hoc, dado que no es necesario revisar las diferencias entre los grupos porque son muy pequeñas.




