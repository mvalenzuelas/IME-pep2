library(tidyverse)
library(ggpubr)
library(dplyr)
library(patchwork)
library(ez)

"
En este momento, los investigadores buscan determinar si existen diferencias en el 
tiempo que tardan los usuarios en formular una consulta
para un problema de dificultad f�cil en las �reas de biolog�a, econom�a y computaci�n.
"

datos <- read.csv2("C:/Users/osswa/OneDrive/Escritorio/IME/EP08 Datos.csv")

facil<- datos[datos$dificultad == "Baja",]

biologia    <- facil[facil$area == "Biolog�a",] %>% select(area,tiempo)
economia    <- facil[facil$area == "Econom�a",] %>% select(area,tiempo)
computacion <- facil[facil$area == "Computaci�n",] %>% select(area,tiempo)


"
--Se decide utilizar una prueba ANOVA de una v�a para muestras independientes, debido a que las variables son independientes
entre s�, por lo que se procede a revisar las condiciones para realizar la prueba.

Condiciones:
  1) La escala en la que se eval�an las variables es la misma para todas (tiempo) y 
    todas poseen una muestra de 200 sujetos.
  2) La muestra de los datos fueron escogidos al azar.
  
"
graficoB <- ggqqplot(biologia$tiempo)
graficoE <- ggqqplot(economia$tiempo)
graficoC <- ggqqplot(computacion$tiempo)


graficoB + graficoE + graficoC

"
  3) Datos normales (comprobado con gr�fico Q-Q)
"

varB <-  var(biologia$tiempo)
varE <-  var(economia$tiempo) 
varC <-  var(computacion$tiempo)

varianza_entre_grupos <- varB/varC

"
  4) Las varianzas dan en promedio 1,338, lo cual es menor que 1.5.
"


#Hip�tesis

#H0 : El tiempo promedio de consultas entre biolog�a, computaci�n y econom�a es igual para las tres �reas
#H1 : El tiempo promedio de consultas entre biolog�a, computaci�n y econom�a es diferente para al menos un �rea


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


#Conclusi�n: Se obtuvo un p_valor>alpha, por lo tanto se falla en rechazar H0, y por lo tanto no existe diferencia significativa
#           en los tiempos promedios de las consultas de las �reas estudiadas. Como observaci�n no se requiere la utilizaci�n de
#           una prueba post-hoc, dado que no es necesario revisar las diferencias entre los grupos porque son muy peque�as.




