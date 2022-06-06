require(dplyr)
require(ggpubr)
require(tidyverse)
#En este momento, los investigadores buscan determinar si existen diferencias 
#en el tiempo que tardan los usuarios en formular una consulta para un 
#problema de dificultad fácil en las áreas de biología, leyes y psicología.
datos <- read.csv2("EP08 Datos.csv")

diFacil <- datos %>% filter(dificultad == "Baja" & (area == "Biología" | area == "Leyes" | area == "Psicología"))

leyes <- diFacil%>%filter(area=="Leyes")
biologia<- diFacil%>%filter(area=="Biología")
psicologia <-diFacil%>%filter(area=="Psicología")

t1<-shapiro.test(leyes$tiempo)
t2<-shapiro.test(biologia$tiempo)
t3<-shapiro.test(psicologia$tiempo)
# Con un nivel de significancia de alfa==0.05 al realizar la prueba de shapiro 
# se puede determinar que los 3 grupos siguen una distribución normal.
varianzaLeyes <- var(leyes$tiempo)
varianzaBio <- var(biologia$tiempo)
varianzaPsico <- var(psicologia$tiempo)

homogeneidad<- max(c(varianzaLeyes,varianzaBio,varianzaPsico))/min(c(varianzaLeyes,varianzaBio,varianzaPsico))

#Como la homogeneidad es menor a 1.5 entonces la muestra presenta  varianzas
#aproximadamente iguales

datosLoger <- diFacil%>% select("area","tiempo")

#Definir hipotesis

#H0: El tiempo de formular una consulta para pregunta facil en las areas de 
#biologia, leyes y psicologia es igual
#H1: El tiempo de formular una consulta para pregunta facil en las areas de 
#biologia, leyes y psicologia es diferente para al  menos uno

# Se utiliza una prueba ANOVA de una via para muestras indepedientes
prueba <-aov(tiempo ~ area, data = datosLoger)
resumen_prueba<-summary(prueba)
#Con un nivel de confianza del 95% existe suficiente evidencia para rechazar la
#hipotesis nula en factor de la alternativa. Por ende si existe diferencia del 
#tiempo para realizar una consulta para un problema de dificultad facil


#Coo existe una diferencia en los tiempo promedio de consulta se realiza una 
#prueba de post-hoc de HSD Tukey para determinar en donde se genera
post_hoc<- TukeyHSD(prueba,
                    "area",
                    ordered = TRUE,
                    conf.level = 0.95)
print(post_hoc)

#La variabilidad de los tiempo para realizar una consulta de un problema de 
#dificultad facil en las areas de biologia,, psicologia y leyes se genera entre
#biologia-Leyes y Biologia-Psicologia

