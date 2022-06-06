library(dplyr)

# ******************************************** ENUNCIADO ********************************#
# Ñami-Ñam, compañía dedicada a la elaboración y comercialización de golosinas, se prepara 
# para lanzar una nueva línea de productos al mercado. Para asegurar el éxito comercial, 
# ha solicitado a varias empresas de diseño la creación de un empaque para cada uno de 
# los nuevos productos. A fin de decidir qué envase es mejor para cada producto y evaluar 
# un contrato permanente con una de las empresas de diseño, Ñami-Ñam ha reclutado a 2.000 
# voluntarios de todo el país, seleccionados aleatoriamente entre los participantes de un 
# concurso efectuado por Ñami-Ñam el año anterior. Cada participante debe puntuar las 
# distintas alternativas de envase para un producto (seleccionado al azar) mediante una 
# escala Likert de 7 puntos, donde el valor 1 indica que el envase le es muy poco atractivo, 
# mientras que el valor 7 señala que el envase le es muy atractivo. Los datos recolectados 
# contemplan las siguientes variables:
# ****************************************************************************************#

basename <- "EP10 Datos.csv"
file <- file.path("C:/Users/Dell PC/Desktop/IME-2022/Actividades/act10",basename)
datos <- read.csv2(file = file)

# ******************************************** PREGUNTA 1 ********************************#
# ¿Existe diferencia en la puntuación obtenida por los envases diseñados por LaKajita según 
# las evaluaciones realizadas por niños y jóvenes?
# ****************************************************************************************#

# Hipótesis
# H0: No hay diferencia en la puntiación obtenida por los envases diseñados por LaKajita según
#     las evaluaciones realizadas por niños y jóvenes.

# HA: Si hay diferencia en la puntiación obtenida por los envases diseñados por LaKajita según
#     las evaluaciones realizadas por niños y jóvenes.

# Seleccionamos los datos que utilizaremos:
datos <- datos %>% filter(Diseno == "LaKajita" & (Edad == "Nino" | Edad == "Joven") )

ninos <- datos %>% filter(Edad == "Nino" )
ninos <- datos[["Puntaje"]]

ninos <- datos %>% filter(Edad == "Nino" )
ninos <- datos[["Puntaje"]]
# length(ninos) = 8000

joven <- datos %>% filter(Edad == "Joven" )
joven <- datos[["Puntaje"]]
# length(joven) = 1342

normalidad_ninos <- shapiro.test(ninos)
print(normalidad_ninos)
#p-value < 2.2e-16

normalidad_joven <- shapiro.test(joven)
print(normalidad_joven)
#p-value < 2.2e-16

# Como se puede observar en las pruebas de Shapiro, las muestras no se
# comportan de manera normal, por lo que no es adecuado utilizar
# la prueba T de Student. Es por este motivo que se piensa utilizar la
# prueba de suma de rangos de Wilcoxon para muestras grandes (n > 5).

# Condiciones:
# 1) Las observaciones de ambas muestras son independientes
# 2) La escala de medición empleada debe ser a lo menos ordinal, de modo que 
#    tenga sentido hablar de relaciones de orden ("igual que", "menor que",
#    "mayor o igual que")     


# Se verifican las condiciones:

# 1) Se puede decir que ambas muestras son independientes ya que en el enunciado se especifica que los
#    participantes son escogidos aleatoriamente y no dependen uno del otro.

# 2) La escala de medición empleada es ordinal ya que se habla de una escala Likert, la cual se mide 
#    desde 1 a 7, donde el valor 1 indica que el envase le es muy poco atractivo, 
#    mientras que el valor 7 señala que el envase le es muy atractivo. Por lo que se puede hablar de una
#    relación de orden.

# Establecer nivel de significación.
alfa  <- 0.05

# Hacer la prueba suma de rangos de Wilcoxon para muestras grandes e independientes.
# El parámetro alternative toma el valor de "two.sided" ya que se 
# pregunta si existe diferencias en la puntuación obtenida por los envases diseñados por LaKajita según 
# las evaluaciones realizadas por niños y jóvenes.
prueba  <- wilcox.test(ninos, joven, alternative = "two.sided", paired = FALSE , conf.level = 1 - alfa)
print(prueba)
# p = 0.6936

# En conclusión, podemos decir que Considerando un nivel de significación alfa= 0,05 < 0.6936, 
# se falla al rechazar la hipótesis nula en favor de la hipótesis alternativa. 
# En consecuencia, concluimos con 95 % de confianza que no hay diferencia en la puntiación obtenida por los envases diseñados por LaKajita según
# las evaluaciones realizadas por niños y jóvenes.

# ******************************************** PREGUNTA 2 ********************************#
# ¿Existen diferencias entre las puntuaciones obtenidas para los diferentes envases de 
# chocolate? De ser así, ¿cuál(es) envase(s) se diferencia(n) de los demás?
# ****************************************************************************************#
library(tidyverse)
library(dplyr)

# Dado que se tienen 4 muestras correlacionadas (Envases de chocolate de 4 empresas distintas) 
# y se pide determinar si existe un algoritmo mejor o peor que los otros, se piensa utilizar 
# la prueba de Friedman.

# Condiciones para utilizar la prueba de Friedman:
# 1) La variable independiente debe ser categórica y tener a lo menos tres niveles.
# 2) La escala de la variable dependiente debe ser, a lo menos, ordinal.
# 3) Los sujetos son una muestra aleatoria e independiente de la población.

# La primera condición se cumple puesto que la variable independiente es el diseño 
# del envase, la cual efectivamente es una variable categórica y posee 4 niveles.
# La segunda condición se cumple puesto que la escala de la variable dependiente
# (puntaje) es una escala Likert, la cual es ordinal.
# La tercera condición se cumple puesto que en el enunciado se especifica que los
# participantes son escogidos aleatoriamente y no dependen uno del otro.

# Definimos nuestras hipótesis

# H0: no existen diferencias entre las puntuaciones para los diferentes envases
#     de chocolate
# H1: al menos un envase de chocolate presenta una diferencia entre las puntuaciones

#Deinimos el valor de alfa
alfa <- 0.05

# Seleccionamos los datos que utilizaremos y procedemos a realizar la prueba de
# Friedman

datos <- datos %>% filter(Producto == "Chocolate")
datos <- datos %>% select(Id, Diseno, Puntaje)

prueba <- friedman.test(Puntaje~Diseno | Id, data = datos)
print(prueba)
# p.valor = 0.4008

# En conclusión considerando que el p.valor > alfa=0.05, se puede afirmar con un 95%
# de confianza que no se rechaza la hipótesis nula, por lo tanto no existe diferencia
# entre las puntuaciones para los diferentes envases de chocolate.

if (prueba$p.value < alfa){
  post_hoc <- pairwise.wilcox.test(datos$Puntaje,
                                   datos$Diseno,
                                   p.adjust.method = "holm",
                                   paired = TRUE)
  print(post_hoc)
}

# En este caso, no se realiza la prueba post-hoc ya que de la prueba de friedman
# se puede concluir que no existen diferencias entre las puntuaciones. De haber
# obtenido un p.valor < alfa, se realiza la prueba de holm para saber en cuál(es)