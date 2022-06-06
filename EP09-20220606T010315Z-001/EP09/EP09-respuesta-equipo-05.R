library(tidyverse)
library(ggpubr)
library(dplyr)
library(patchwork)
library(ez)

"
En este momento, los investigadores buscan determinar si existen diferencias en el 
tiempo que tardan los usuarios en formular consultas para problemas con diferente 
nivel de dificultad en el área de economía.
"

datos <- read.csv2("C:/Users/osswa/OneDrive/Escritorio/IME/EP08 Datos.csv")

economia<- datos[datos$area == "Economía",]
economia <- economia %>% select(dificultad,tiempo)

baja<- economia[economia$dificultad == "Baja",]
baja[["instancia"]] <- factor(1:nrow(baja))

media<- economia[economia$dificultad == "Media",]
media[["instancia"]] <- factor(1:nrow(media))

alta<- economia[economia$dificultad == "Alta",]
alta[["instancia"]] <- factor(1:nrow(alta))


"
--Se decide utilizar una prueba ANOVA de una vía para muestras correlacionadas, debido a que se debe evaluar si existe
diferencia entre las variables a estudiar
por lo que se procede a revisar las condiciones para realizar la prueba.

Condiciones:
  1) La escala en la que se evalúan las variables es la misma para todas (tiempo) y 
    todas poseen una muestra de 600 sujetos.
  2) La muestra de los datos fueron escogidos al azar.
  
"

grafico1 <- ggqqplot(baja$tiempo)
grafico2 <- ggqqplot(media$tiempo)
grafico3 <- ggqqplot(alta$tiempo)


grafico1 + grafico2 + grafico3

"
  3) Datos normales (comprobado con gráfico Q-Q)
"

# Prueba ANOVA

#Se considera un alpha = 0.05 para la evaluación de resultados.


#Hipótesis

#H0 : El tiempo promedio de consultas entre dificultades del área de economía es igual para los tres valores
#H1 : El tiempo promedio de consultas entre dificultades del área de economía es diferente para los tres valores


#En primer lugar se debe verificar la prueba de esfericidad para revisar si deben corregir los datos

datosNuevos <- rbind(baja,media,alta)

datosNuevos[["dificultad"]] <- factor(datosNuevos[["dificultad"]])
  


pruebaEZ <- ezANOVA(data=datosNuevos, 
                    dv=tiempo, 
                    wid = instancia, 
                    within = dificultad,
                    return_aov=TRUE)

#----Prueba de esfericidad
print(pruebaEZ[["Mauchly's Test for Sphericity"]])

#H0: Las varianzas-covarianzas de los grupos son iguales.
#HA: Las varianzas-covarianzas de los grupos no son iguales.

#Se obtiene p_valor>0.05, por lo tanto se falla en rechazar la hipótesis nula, por lo que los datos
#son esféricos, ya que las varianzas son iguales. 
#-Observación: Como la prueba cumple la condición de esfericidad no se requiere corregir los datos
# y por lo tanto se puede realizar la prueba anova.


#----Se procede a revisar el valor obtenido por la prueba ezANOVA

print(summary(pruebaEZ$aov))
cat("p_valor: ", pruebaEZ$ANOVA$p)


#Conclusión: Se obtuvo un p_valor>alpha, por lo tanto se falla en rechazar H0, y por lo tanto no existe diferencia significativa
#           en los tiempos promedios de las consultas realizadas entre las dificultades de los datos del área de economía. Como observación 
#           no se requiere la utilización de una prueba post-hoc, dado que no es necesario revisar las diferencias entre los grupos porque 
#           son muy pequeñas (no son significativas).




