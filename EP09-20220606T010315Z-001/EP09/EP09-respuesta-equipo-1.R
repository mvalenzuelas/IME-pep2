library(dplyr)
library(ggpubr)
library(tidyverse)
library(ez)
library(nlme)
library(emmeans)

data <- read.csv2("C:\\Users\\danie\\Downloads\\EP08 Datos.csv")

fisica <- data%>%filter(area == "Física")

#Se trabajará con un nivel significación igual 0.05
alfa <- 0.05

#Se trabajará con la prueba ANOVA para muestras correlacionadas

facil <- fisica%>%filter(dificultad == "Baja")
facil <- facil[["tiempo"]]
media <- fisica%>%filter(dificultad == "Media")
media <- media[["tiempo"]]
dificil <- fisica%>%filter(dificultad == "Alta")
dificil <- dificil[["tiempo"]]

fisica[["id"]] <- factor(fisica[["id"]])
fisica[["dificultad"]] <- factor(fisica[["dificultad"]])

#La variable dependiente es el tiempo y cumple con las propiedades de una escala de intervalos iguales

#Los datos, al interior de cada grupo, fueron obtenidos de manera aleatoria, por lo tantos son independientes

#Comprobación de normalidad

shap_facil <- shapiro.test(facil)$p.value
#p-valor = 0.75

shap_media <- shapiro.test(media)$p.value
#p-valor = 0.80

shap_dificil <- shapiro.test(dificil)$p.value
#p-valor = 0.74

#Como los 3 p-valores son mayores alfa, entonces se cumple con la normalidad

#Se definen las hipótesis nula y alternativa
#H0: El tiempo que tardan los usuarios  en formular consultas para un problema 
#    del área de física con dificultad alta, media y baja, es igual.
#HA: El tiempo que tardan los usuarios  en formular consultas para un problema 
#    del área de física con dificultad alta, media y baja, es diferente, para al menos una dificultad.

prueba <- ezANOVA(
  data = fisica,
  dv = tiempo,
  within = dificultad,
  wid = id,
  return_aov = TRUE
)

print(prueba)
#p-valor = 5.28e-27 < alfa

#Prueba de esfericidad de Mauchly
print(prueba[["Mauchly's Test for Sphericity"]])
#p-valor = 0.93 > alfa
#Por tanto, si se cumple la condición de esfericidad, por lo cual, se utiliza el p-valor
#presentado en la tabla ANOVA p-valor = 5.28e-27

#Por tanto, se rechaza la hipótesis nula en favor de la hipótesis alternativa, por ende,
#con un 95% de confianza, se concluye que el tiempo que tardan los usuarios  en formular consultas para un problema 
#del área de física con dificultad alta, media y baja, es diferente, para al menos una dificultad.

mixto <- lme(tiempo ~ dificultad, data = fisica, random = ~1|id)
medias <- emmeans(mixto, "dificultad")
tukey <- pairs(medias, adjust = "tukey")

print(tukey)

#Gracias a la prueba post hoc HSD de Tukey, es posible determinar que las 
#diferencias más significativas del área de física están entre las dificultades Alta-Baja (p-valor<.0001)
#y las dificultades Alta-Media (p-valor<.0001).

#En conclusión, el tiempo en que tardan los usuarios en formular consultas para un problema 
#del área de física con dificultad alta, media y baja, es diferente, para al menos una dificultad,
#específicamente, entre las dificultades Alta-Baja y Alta-Media.