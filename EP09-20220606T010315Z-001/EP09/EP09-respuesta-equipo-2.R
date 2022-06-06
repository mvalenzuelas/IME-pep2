library(dplyr)
library(tidyverse)
library(ez)
library(nlme)
library(emmeans)
# En este momento, los investigadores buscan determinar si existen diferencias 
# en el tiempo que tardan los usuarios en formular consultas para problemas con 
# diferente nivel de dificultad en el área de computación.

# Hipótesis:
# H_0: No hay diferencias en el tiempo que tardan los usuarios en formular
# consultas para problemas con diferente nivel de dificultad, en el área de computación.

# H_A: Hay diferencias en el tiempo que tardan los usuarios en formular consultas
# para problemas con diferente nivel de dificultad en el área de computación.

# Tomando alpha = 0.05.

datos <- read.csv2("C:\\Users\\camil\\Desktop\\ime\\ep-09\\EP08 Datos.csv")

datosComp <- datos %>% filter(area == 'Computación') %>% select(dificultad, tiempo)

# Separar x dificultad:

datosBaja <- datosComp %>% filter(dificultad == 'Baja')
datosBaja <- datosBaja$tiempo

datosMedia <- datosComp %>% filter(dificultad == 'Media')
datosMedia <- datosMedia$tiempo

datosAlta <- datosComp %>% filter(dificultad == 'Alta')
datosAlta <- datosAlta$tiempo

# Comprobamos las condiciones:
# 1. La variable tiempo tiene escala de intervalos iguales, por lo tanto
# se cumple la primera condición.

# 2. Las mediciones son independientes al interior de cada grupo, ya que son 
# pruebas distintas para cada nivel de dificultad.

# 3. Para verificar la normaliad, se realiza un test de Shapiro
# para cada nivel de dificultad:

p1_baja <- shapiro.test(datosBaja)
p1_media <- shapiro.test(datosMedia)
p1_alta <- shapiro.test(datosAlta)

print(p1_baja)
print(p1_media)
print(p1_alta)

# Ya que todos los p-valores son mayores a 0.05,
# se verifica la condición número 3.

# Esfericidad de la matriz de varianzas-covarianzas:

# Formamos el dataframe para la prueba:

instancia <- factor(1:nrow(datosComp))
data_dificultad <- data.frame(instancia, Baja = datosBaja, Media = datosMedia, Alta = datosAlta)

data <- data_dificultad %>% pivot_longer(c("Baja", "Media", "Alta"),
                                 names_to = "dificultad", 
                                 values_to = "tiempo") 

data[['dificultad']] <- factor(data[['dificultad']])

prueba <- ezANOVA(data = data,
                  dv = tiempo,
                  within = dificultad,
                  wid = instancia,
                  return_aov = TRUE)

print("***")


print(prueba$`Mauchly's Test for Sphericity`)

print("Prueba ANOVA")
print(prueba$ANOVA)
#  p = 0.187 > alpha, por lo tanto la matriz cumple la condición de esfericidad,
# y cumplimos la condición (4), por tanto, se utiliza el p-valor presentado en
# la tabla ANOVA P=3.877e-24 < alfa, por lo que con un 95% de confianza se rechaza
# la hipótesis nula en favor de la hipótesis alternativa. 

# En conclusión , si hay diferencias en el tiempo que tardan los usuarios en formular consultas
# para problemas con diferente nivel de dificultad en el área de computación.

#Se procede a realizar post hoc de los datos obtenidos

mixto <- lme(tiempo ~ dificultad, data = data, random = ~1|instancia)
medias <- emmeans(mixto, "dificultad")
tukey <- pairs(medias, adjust = "tukey")

print("Post hoc HSD Tukey")
print(tukey)
# Debido a que todos los p-valor< alfa en el post hoc HSD tukey, se puede concluir
# con un 95% de confianza que los 3 resultados son distintos entre si.