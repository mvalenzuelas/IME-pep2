#Nombre: Cristián Inzulza

#Actividad: ANOVA para muestras independientes
 
#Ya conocemos bien la prueba T de Student que nos permite, entre otras cosas, 
#comparar las medias de una variable aleatoria en dos grupos independientes. 
#Ahora estamos conociendo que existe un procedimiento conocido como análisis de 
#varianza, o ANOVA, en corto, que nos permite comparar las medias de más de dos 
#grupos independientes. Esta prueba está compuesta de una etapa omnibus, más un 
#análisis post hoc si corresponde. Este ejercicio apunta a que practiquemos este 
#procedimiento.

#Equipo 1:
#En este momento, los investigadores buscan determinar si existen 
#diferencias en el tiempo que tardan los usuarios en formular una 
#consulta para un problema de dificultad fácil en las áreas de computación, 
#literatura y química.


install.packages('nortest')
install.packages('nlme')

library(readxl)

library(tidyverse)
library(ggpubr)
library(ez)
library(broom)
library(nlme)

#2. Obtención de los datos

datos <- read_excel("C:/Users/crist/Desktop/EjercicioInferenciaSemana7/Datos.xlsx")

datos2 <- datos[datos$dificultad == "Baja",]

literatura <- datos2[datos2$area == "Literatura",]
computacion <- datos2[datos2$area == "Computación",]
quimica <- datos2[datos2$area == "Química",]

# Gráficos de normalidad de los datos utilizados para cada muestra

qqplotLiteratura <- ggqqplot(literatura$tiempo)
qqplotComputacion <- ggqqplot(computacion$tiempo)
qqplotQuimica <- ggqqplot(quimica$tiempo)

qqplotLiteratura
qqplotComputacion
qqplotQuimica

# De los gráficos anteriores se ve descriptivamentes que las muestras distribuyen normal

# Test de normalidad de Shapiro-Wilk
shapiro.test(literatura$tiempo)
shapiro.test(computacion$tiempo)
shapiro.test(quimica$tiempo)

# Los test de Shapiro indican que las muestras son normales.

# Verificando Homogeneidad

varianza_literatura <- var(literatura$tiempo)
varianza_computacion<- var(computacion$tiempo)
varianza_quimica <- var(quimica$tiempo)

varianza_literatura
varianza_computacion
varianza_quimica

homogeneidad <- varianza_literatura/varianza_computacion
homogeneidad

# El valor anterior es dde 1.325 que es menos a 1.5, por lo tanto, las varianzas 
# de las muestras son similares o iguales


datos3 <- datos2[datos2$area == "Literatura" | datos2$area == "Computación" | datos2$area == "Química",]

area <- datos3$area

tiempo <- datos3$tiempo

table(datos3)

datos4 <- data.frame(area = area, tiempo=tiempo)

table(datos4$area)

ggplot(data = datos4, aes(x = area, y = tiempo, color = area)) +
  geom_boxplot() +
  theme_bw()

require(nortest)
by(data = datos4,INDICES = datos4$area,FUN = function(x){ lillie.test(x$tiempo)})

# Test Fligner de homogeneidad de varianzas
#Ho = Las varianzas son homogeneas
#Ha = Las varianzas no son homogeneas

fligner.test(tiempo ~ area,datos4)

# El valor-p es de 0.0646 por lo que no se rechaza la hipotesis donde las varianzas son homogeneas


#2.Formulación de la hipótesis
# Análisis de varianza ANOVA

#Ho: las medias son iguales en los tiempos esperados para las asignaturas de Literatura, Computación y Química
#Ha: hay alguna media distinta en los tiempos esperados para todas las asignaturas de Literatura, Computación y Química

anova <- aov(datos4$tiempo ~ datos4$area, data = datos4)
summary(anova)

# La columna Pr(>F) es el valor p de la estadística F. Esto muestra qué tan 
# probable es que el valor F calculado a partir de la prueba hubiera ocurrido 
# si la hipótesis nula de que no hay diferencia entre las medias de los grupos 
# fuera cierta.


post_hoc<- TukeyHSD(anova,
                    ordered = TRUE,
                    conf.level = 0.95)
print(post_hoc)


#Comparaciones múltiples Bonferroni

pairwise.t.test(x = datos4$area=="Computación", g = datos4$area=="Literatura", p.adjust.method = "bonferroni",
                pool.sd = TRUE, paired = FALSE, alternative = "two.sided")

#El valor-p es 0.00024

pairwise.t.test(x = datos4$tiempo, g = datos4$area=="Computación", p.adjust.method = "bonferroni",
                pool.sd = TRUE, paired = FALSE, alternative = "two.sided")

#El valor-p es 0.16

pairwise.t.test(x = datos4$tiempo, g = datos4$area=="Química", p.adjust.method = "bonferroni",
                pool.sd = TRUE, paired = FALSE, alternative = "two.sided")

#El valor-p es 2.9e-07


#Comparaciones múltiples Holm

pairwise.t.test(x = datos4$tiempo, g = datos4$area=="Computación", p.adjust.method = "holm",
                pool.sd = TRUE, paired = FALSE, alternative = "two.sided")

# valor-p= 0.16

pairwise.t.test(x = datos4$tiempo, g = datos4$area=="Literatura", p.adjust.method = "holm",
                pool.sd = TRUE, paired = FALSE, alternative = "two.sided")

# valor-p= 0.00024

pairwise.t.test(x = datos4$tiempo, g = datos4$area=="Química", p.adjust.method = "holm",
                pool.sd = TRUE, paired = FALSE, alternative = "two.sided")

# valor-p= 2.9e-07


# Procedimiento post-hoc HSD Tukey

TukeyHSD(anova)


plot(TukeyHSD(anova))

# Las asigaturas de computación - quimica y Literatura - Quimica son 
# las asignaturas que presentan diferencias en los tiempos de respuesta


# Validacion del modelo

plot(anova$residuals)

#normalidad

# Muestra el resumen de los residuos de anova
summary(anova$residuals)

#Graficando el gráfico de caja de los residuos
boxplot(anova$residuals)

#Histograma de los residuos
hist(anova$residuals)

# Gráficos qqnorm de los residuos
qqnorm(anova$residuals) 
qqline(anova$residuals)

# El test de Shapiro-Wilk indica que no tenemos evidencia suficiente para rechazar 
# la hipótesis nula (normalidad de los residuos), ya que el valor p 
# Por lo tanto los residuos distribuyen normal

shapiro.test(anova$residuals)


# homocedasticidad

# Los gráficos y descriptivos nos informan si se verifica la igualdad de varianzas 
# en los grupos descritos:

boxplot(anova$residuals~datos4$area, col = c("yellow", "blue","green")) 

desviaciones <- tapply(anova$residuals, datos4$area, sd)

desviaciones


# Conclusión las pruebas anova y demás son válidas e indican que las medias entre los 3 grupos son diferentes.

# Las asigaturas de computación - quimica y Literatura - Quimica son 
# las asignaturas que presentan diferencias en los tiempos de respuesta