

## Integrante: Cristián Inzulza

#Pregunta 1

#En el trabajo de título de un estudiante del DIINF se reportan los siguientes tiempos de ejecución 
#(en milisegundos) medidos para dos versiones de un algoritmo genético para resolver instancias del 
#problema del vendedor viajero disponibles en repositorios públicos. ¿Es uno de los algoritmos más 
#rápido que el otro? Aplique una transformación de datos adecuada para responder.

install.packages('ggplot2')
install.packages('ggpubr')
#install.packages('ggplot2')
install.packages('rcompanion')
install.packages('DescTools')

library(ggpubr)
library(rcompanion)
library(DescTools)
library(WRS2)

# 1.1 Obtención de la data

instaciaA <- c(129, 109, 28, 178, 74, 16, 87, 108, 149, 78)

tiempoA <- c(1510394, 402929, 885722, 4428151, 48667, 834565, 70599, 783108, 210041, 37449)

instanciaB <- c(134, 193, 10, 88, 142, 86, 36, 190, 163, 33)

tiempoB <- c(1252837, 2196277, 120276, 4629726, 5743260, 6701654, 6568968, 180141, 6684497, 35974)

datosA <- data.frame(instaciaA, tiempoA)

# Ordenando la data

datosA_ordenado <- datosA[order(datosA$instaciaA), ]

datosA_ordenado

datosB <- data.frame(instanciaB, tiempoB)

datosB_ordenado <- datosB[order(datosB$instanciaB), ]

datosB_ordenado

hist(datosA_ordenado$tiempoA)

hist(datosB_ordenado$tiempoB)

# 1.3 Buscando la mejor transformación sobre la data

transformacionA <- transformTukey(datosA_ordenado$tiempoA, start = -5, end = 5, 
                                  int = 0.001, returnLambda = TRUE)

transformacionB <- transformTukey(datosB_ordenado$tiempoB, start = -5, end = 5, 
                                  int = 0.001, returnLambda = TRUE)

transformacion_datosA <- datosA_ordenado$tiempoA^(0.11)

transformacion_datosB <- datosB_ordenado$tiempoB^(0.449)

hist(transformacion_datosA)

hist(transformacion_datosB)

shapiro.test(transformacion_datosA)

shapiro.test(transformacion_datosB)

# Con 99% de confianza ambas muestras son normales.

tiempo <- c(transformacion_datosA,transformacion_datosB)

algoritmo <- c(rep("transformacion_datosA", length(transformacion_datosA)), rep("transformacion_datosB", length(transformacion_datosB)))

datos <- data.frame(tiempo, algoritmo)

g <- ggqqplot(datos, x = "tiempo", facet.by = "algoritmo", palette = c("blue", "red"), color = "algoritmo")

print(g)


# 1.2 Condiciones

var(transformacion_datosA)
var(transformacion_datosB)

# Las varianzas de los datos transformados es muy diferente por lo que se aplicará la prueba de Yuen
# para dos muestras independientes.


# 1.4 Formulación de la hipótesis
#Ho: Las medias de las muestras del tiempo de ejecución del algoritmo son iguales
#Ha: Las medias de las muestras del tiempo de ejecución del algoritmo son diferentes


# Podando al 20%

alfa <- 0.05

gamma <- 0.2

n_a <- length(transformacion_datosA)
n_b <- length(transformacion_datosB)

poda_a <- n_a*gamma
poda_b <- n_b*gamma 

a_truncada <- transformacion_datosA[poda_a:(n_a - poda_a)]
b_truncada <- transformacion_datosB[poda_b:(n_b - poda_b)]

tiempo <- c(a_truncada, b_truncada)
algoritmo <- c(rep("A", length(a_truncada)), rep("B", length(b_truncada)))
datos_truncados <- data.frame(tiempo, algoritmo)

g <- ggqqplot(datos_truncados, x = "tiempo", facet.by = "algoritmo",
              palette= c("blue", "red"), color = "algoritmo")

print(g)


# 1.5 Prueba de Yuen

prueba <- yuen(tiempo ~ algoritmo, data = datos, tr = gamma)

print(prueba)

# El valor-p = 0.01773 << 0.05 Por lo tanto se rechaza la hipótesis, por lo que las medias son diferentes


# 1.6 Conclusión

# Se han aplicado transformaciones que normalizan los datos del tiempo para cada algoritmo. 
# El valor-p = 0.01773 << 0.05 Por lo tanto se rechaza la hipótesis, por lo que las medias son diferentes





# Pregunta 2

#Analice la primera pregunta abordada en el ejercicio práctico 11, con los mismos datos, 
#utilizando un método robusto adecuado.

# 1.1 Pregunta 1 Existen diferencias significativas entre el ingreso del hogar promedio para los profesionales titulados
# del área de la educación y la Salud.



# 2.1 Pregunta de investigación

# Existen diferencias significativas entre el ingreso del hogar promedio para los profesionales titulados
# del área de la educación y la Salud.


# 2.2 Obtención de los datos

library(readxl)
datos <- read_excel("C:/Users/crist/OneDrive/Escritorio/Ejercicio11InferenciaEstadística/datos.xlsx")

ingresos_ProfesionalCompleto <- datos[datos$educ == "Profesional Completo",]

ingresos_Educacion <- ingresos_ProfesionalCompleto[ingresos_ProfesionalCompleto$e7.cod.area=="Educación",]

ingresos_AdministracionDerecho <- ingresos_ProfesionalCompleto[ingresos_ProfesionalCompleto$e7.cod.area=="Administración de Empresas y Derecho",]

ingresos_TIC <- ingresos_ProfesionalCompleto[ingresos_ProfesionalCompleto$e7.cod.area=="Tecnología de la Información y la Comunicación (TIC)",]

ingresos_Ingenieria <- ingresos_ProfesionalCompleto[ingresos_ProfesionalCompleto$e7.cod.area=="Ingeniería, Industria y Construcción",]

ingresos_Salud <- ingresos_ProfesionalCompleto[ingresos_ProfesionalCompleto$e7.cod.area=="Salud y Bienestar",]


ingresos_Educacion <- ingresos_Educacion$ytotcorh
ingresos_AdministracionDerecho <- ingresos_AdministracionDerecho$ytotcorh
ingresos_TIC <- ingresos_TIC$ytotcorh
ingresos_Ingenieria <- ingresos_Ingenieria$ytotcorh
ingresos_Salud <- ingresos_Salud$ytotcorh


mediaEducacion <- mean(ingresos_Educacion)
mediaEducacion

mediaAdministracionDerecho <- mean(ingresos_AdministracionDerecho)
mediaAdministracionDerecho

mediaTIC <- mean(ingresos_TIC)
mediaTIC

mediaIngenieria <- mean(ingresos_Ingenieria)
mediaIngenieria

mediaSalud <- mean(ingresos_Salud)
mediaSalud


# Varianzas 

varEducacion <- var(ingresos_Educacion)
varEducacion

longitudEducacion <- length(ingresos_Educacion)
longitudEducacion

varSalud <- var(ingresos_Salud)
varSalud

longitudSalud <- length(ingresos_Salud)
longitudSalud

# Formulación de la hipótesis

# Ho: No Existe diferencia entre las medias de los ingresos de los profesionales del sector Salud vs Educación
# Ha: Existe diferencia entre las medias de los ingresos de los profesionales del sector Salud vs Educación


# Condiciones para aplicar la prueba robusta

# La longitud de las dos muestras son 1659 y 720 son dispares 
# Se utilizará la prueba de Yuan para dos muestras independientes

install.packages('WRS2')
install.packages('ggpubr')

library(WRS2)
library(ggpubr)

ingresos <- c(ingresos_Educacion,ingresos_Salud)

algoritmo <- c(rep("ingresos_Educacion", length(ingresos_Educacion)), rep("ingresos_Salud", length(ingresos_Salud)))

datos <- data.frame(ingresos, algoritmo)

g <- ggplot(datos, x = "ingresos", facet.by = "algoritmo",
            palette = c("blue", "red"), color="algoritmo")

print(g)

# Aplicando poda al 20%

alfa <- 0.05

gamma <- 0.2
n_a <- length(ingresos_Educacion)
n_b <- length(ingresos_Salud)

poda_a <- n_a*gamma
poda_b <- n_b*gamma

a_truncada <- ingresos_Educacion[poda_a:(n_a - poda_a)]
b_truncada <- ingresos_Salud[poda_b:(n_b - poda_b)]

ingresos <- c(a_truncada, b_truncada)
algoritmo <- c(rep("A",length(a_truncada)), rep("B", length(b_truncada)))

datos_truncado <- data.frame(ingresos, algoritmo)

g <- ggplot(datos_truncado, x = "ingresos", facet.by = "algoritmo", 
            pallete = c("blue", "red"), color="algoritmo")
print(g)

# Prueba aplicada

# Prueba de Yuan

prueba <- yuen(ingresos ~ algoritmo, data = datos, tr = gamma)
print(prueba)

#valor-p << 0.05 Por lo tanto se rechaza la hipótesis

alfa <- 0.05

bootstrap <- 500

# Aplicando prueba con la media y bootstraping

set.seed(135)


prueba_media <- pb2gen(ingresos ~ algoritmo, data = datos, est = "mean", nboot = bootstrap)

print(prueba_media)

# Valor-p <<0.05 Por lo tanto se rechaza la hipotesis de que las medias del sector de educación y salud son iguales.
# Existe diferencias significativas entre los ingresos entre Educación y Salud


# Conclusión 

# Existe diferencias significativas en los ingresos en los profesionales del sectores de Salud y Educación, dado que el valor-p 
# de las dos pruebas anteriores es < 0.05 Por esto se rechaza la hipótesis de igualdad entre las medias.






# Pregunta 3

# Analice la segunda pregunta abordada en el ejercicio práctico 11, con los mismos datos, 
# utilizando un método robusto adecuado.

# 3.1 Pregunta de investigación: Existe diferencia del ingreso entre los grupos de profesionales con estudios completos del 
# área de Administración, TIC e Ingeniería
  
  

library(readxl)

# 3.2 Obtención de los Datos
datos <- read_excel("C:/Users/crist/OneDrive/Escritorio/Ejercicio11InferenciaEstadística/datos.xlsx")

ingresos_ProfesionalCompleto <- datos[datos$educ == "Profesional Completo",]


ingresos_AdministracionDerecho <- ingresos_ProfesionalCompleto[ingresos_ProfesionalCompleto$e7.cod.area=="Administración de Empresas y Derecho",]

ingresos_TIC <- ingresos_ProfesionalCompleto[ingresos_ProfesionalCompleto$e7.cod.area=="Tecnología de la Información y la Comunicación (TIC)",]

ingresos_Ingenieria <- ingresos_ProfesionalCompleto[ingresos_ProfesionalCompleto$e7.cod.area=="Ingeniería, Industria y Construcción",]


ingresos_AdministracionDerecho <- ingresos_AdministracionDerecho$ytotcorh
ingresos_TIC <- ingresos_TIC$ytotcorh
ingresos_Ingenieria <- ingresos_Ingenieria$ytotcorh

length(ingresos_AdministracionDerecho)
length(ingresos_TIC)
length(ingresos_Ingenieria)

ingresos <- c(ingresos_AdministracionDerecho, ingresos_TIC, ingresos_Ingenieria)

algoritmo <- c(rep("A",length(ingresos_AdministracionDerecho)), rep("B", length(ingresos_TIC)), rep("C", length(ingresos_Ingenieria)))

datos <- data.frame(ingresos, algoritmo)
datos

# Hipótesis
# Ho: No Existe diferencia entre las medias de los ingresos de los profesionales del sector Administración - TIC e Ingeniería
# Ha: Existe diferencia entre las medias de los ingresos de los profesionales sector Administración - TIC e Ingeniería

# Transformando los datos

# Utilizando medias truncadas y bootstraping 

alfa <- 0.05
cat("Comparación entre grupos usando medias truncadas \n\n") 
bootstrap <- 500
gamma <- 0.2
set.seed(666)

# Prueba: Comparación de una vía para múltiples grupos independientes

# Se utilizo esta prueba porque las muestras son independientes.
# Ademas los tamaños de muestras son diferentes sobre todo con el grupo de las TIC

length(ingresos_AdministracionDerecho)
length(ingresos_TIC)
length(ingresos_Ingenieria)

# prueba de una vía para múltiples grupos independientes

medias_truncadas <- t1way(ingresos ~ algoritmo, data = datos, tr = gamma, 
                          alpha = alfa)
print(medias_truncadas)

# valor-p << 0.05 por lo que existe diferencia entre las medias de ingresos en los grupos Administración, TIC e Ingeniería

# Pruebas post_hoc

if(medias_truncadas$p.value < alfa){
  
  cat("\n Procesamiento post-hoc\n\n")
  
  set.seed(666)
  
  post_hoc <- lincon(ingresos ~ algoritmo, data = datos, tr = gamma,
                     alpha = alfa)
  print(post_hoc)

}


#psihat  ci.lower  ci.upper p.value
#A vs. B   51179.05 -200153.8 302511.91 0.62608
#A vs. C -179575.46 -316804.8 -42346.15 0.00539
#B vs. C -230754.50 -479237.0  17727.95 0.05479

# Se presenta diferencia entre las medias entre los grupos de Administración e Ingeniería con valor-p = 0.00539



# Conclusión 

# Existe diferencias significativas en los ingresos en los sectores de Administración e Ingeniería con valor-p = 0.00539 
# Para el caso de Administración vs TIC y TIC vs Ingeniería, no existe evidencia suficiente para afirmar que 
# que existe existe diferencia entre estos grupos

