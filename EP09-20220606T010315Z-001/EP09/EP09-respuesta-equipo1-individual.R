
# Nombre: Cristián Inzulza

# Equipo 1-individual:
  
# En este momento, los investigadores buscan determinar si existen diferencias en el 
# tiempo que tardan los usuarios en formular consultas para problemas con diferente nivel
# de dificultad en el área de física.

install.packages('emmeans')
install.packages('tidyverse')
install.packages('broom')
install.packages('nlme')
install.packages('ez')
install.packages('ggpubr')

library(readxl)
library(nlme)
library(emmeans)
library(ggpubr)
library(tidyverse)
library(broom)
library(ez)
library(dplyr)



# Obtención de los datos

Datos <- read_excel("C:/Users/crist/Desktop/EjercicioInferenciaSemana7/EP09 procedimiento ANOVA para muestras correlacionadas/Datos.xlsx")
Datos

datos_fisica <- Datos[Datos$area == "Física",]
datos_fisica

fisica_baja<- datos_fisica[datos_fisica$dificultad=="Baja",]

fisica_Media <- datos_fisica[datos_fisica$dificultad=="Media",]

fisica_Alta <- datos_fisica[datos_fisica$dificultad=="Alta",]

fisica_baja <- fisica_baja$tiempo

fisica_Media <- fisica_Media$tiempo

fisica_Alta <- fisica_Alta$tiempo


# Comprobando la normalidad

shapiro.test(fisica_baja)

shapiro.test(fisica_Media)

shapiro.test(fisica_Alta)

# Histogramas de los datos

hist(fisica_baja)

hist(fisica_Media)

hist(fisica_Alta)

qqnorm(fisica_baja)
qqline(fisica_baja)

qqnorm(fisica_Media)
qqline(fisica_Media)

qqnorm(fisica_Alta)
qqline(fisica_Alta)

datos_fisica$id <- factor(datos_fisica$id)
datos_fisica$dificultad <- factor(datos_fisica$dificultad)

# Hipotesis
# H0: No existe diferencias significativas entre las medias de los tiempos de respuesta para el curso de física entre los grupos baja - media - alta
# Ha: Existen diferencias significativas entre las medias de los tiempos de respuesta para el curso de física entre los grupos baja - media - alta


# Pruebas Omnibus

anova <- aov(datos_fisica$tiempo ~ datos_fisica$dificultad, data = datos_fisica)
summary(anova)

plot(anova$residuals)

# El valor-p es <2e-16 por lo que se rechaza  hipótesis y por lo tanto existesn diferencias significativas entre los tiempo de los grupos
# alta, baja y media

# Muestra el resumen de los residuos de anova
summary(anova$residuals)

#Graficando el gráfico de caja de los residuos
boxplot(anova$residuals)

# Test ezAnova
# El valor-p es de la esfericidad es de  0.9339161 por lo que es factible utilizar el test de esfericidad para probar la hipótesis

ezanova <- ezANOVA(
  data = datos_fisica,
  dv = tiempo,
  within = dificultad,
  wid = id,
  return_aov = TRUE
)


print(ezanova)

# Graficos de los residuos que indica la normalidad descriptivamente

# Gráficos qqnorm de los residuos
qqnorm(ezanova$residuals) 
qqline(ezanova$residuals)

# Muestra el resumen de los residuos de anova
summary(anova$residuals)

#Graficando el gráfico de caja de los residuos
boxplot(anova$residuals)

#Histograma de los residuos
hist(anova$residuals)

# Gráficos qqnorm de los residuos
qqnorm(anova$residuals) 
qqline(anova$residuals)

# Los gráficos y descriptivos nos informan si se verifica la igualdad de varianzas 
# en los grupos descritos:

boxplot(anova$residuals~datos4$area, col = c("yellow", "blue","green")) 

# Procedimiento post-hoc HSD Tukey

TukeyHSD(anova)


plot(TukeyHSD(anova))

# Vemos descriptivamente los grupos que se diferencian que son (Alta - Media) y  (Alta- Baja)

# Pruebas post-hoc co el test de tukey

modelo <- lme(tiempo ~ dificultad, data = datos_fisica, random = ~1|id)
means <- emmeans(modelo, "dificultad")
prueba_tukey <- pairs(means, adjust = "tukey")


print(prueba_tukey)

plot(prueba_tukey)

# Conclusiones

# Se rechaza la hipótesis de igualdad de medias entre los grupos baja, alta y media del curso de física

# Según las pruebas de Anova confirmadas por la esfericidad,existe diferencia entre los grupos y (Alta - Media) y  (Alta- Baja)
