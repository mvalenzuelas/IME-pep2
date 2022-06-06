
#install.packages('boot')
#install.packages('bootES')
#install.packages('simpleboot')

library(readxl)
datos <- read_excel("C:/Users/crist/OneDrive/Escritorio/Ejercicio11InferenciaEstadística/datos.xlsx")

#install.packages('ggpubr')
#install.packages('ez')
#install.packages('tidyverse')

library(ggpubr)
library(ez)
library(tidyverse)

# Comparación de medias de dos poblaciones con Simulación de Monte Carlo

# 1.1 Pregunta 1 Existen diferencias significativas entre el ingreso del hogar promedio para los profesionales titulados
# del área de la educación y la Salud.

# 1.2 Obtención de los datos
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

ingresos_Educacion
ingresos_AdministracionDerecho
ingresos_TIC
ingresos_Ingenieria
ingresos_Salud

# Normalidad de los datos

shapiro.test(ingresos_Educacion)
shapiro.test(ingresos_AdministracionDerecho)
shapiro.test(ingresos_TIC)
shapiro.test(ingresos_Ingenieria)
shapiro.test(ingresos_Salud)

boxplot(ingresos_Educacion,ingresos_Salud)

hist(ingresos_Educacion)
hist(ingresos_AdministracionDerecho)
hist(ingresos_TIC)
hist(ingresos_Ingenieria)
hist(ingresos_Salud)

  # Todas las muestras no distribuyen normal

# 3. Hipótesis
#H0: No existe diferencia entre las medias de los ingresos del hogar de los profesionales del área de la educación y Salud
#H1: Existe diferencia entre las medias de los ingresos del hogar de los profesionales del área de la educación y Salud

#Construyendo los remuestreos con Bootsting

library(boot)
library(bootES)


#Las muestras utilizadas son

ingresos_Educacion
ingresos_Salud

#Aplicando Bootstraping para dos muestras independientes

set.seed(432)

longitud_ingresos_Educacion <- length(ingresos_Educacion)
longitud_ingresos_Salud <- length(ingresos_Salud)



#Establecer semilla y cantidad de repeticiones

R = 300
set.seed(432)

#Las siguientes funciones se obtuvieron de los apuntes del curso
#Se obtienes las permutaciones
#Calculo de las diferenciaes entre las muestras
#Obtención del valor-p
# Graficando las distribuciones

obtiene_permutaciones <- function(i, ingresos_Educacion, ingresos_Salud){
  n_1 <- length(ingresos_Educacion)
  combinada <- c(ingresos_Educacion, ingresos_Salud)
  n <- length(combinada)
  permutacion <- sample(combinada, n, replace=FALSE)
  nueva_1 <- permutacion[1:n_1]
  nueva_2 <- permutacion[(n_1+1):n]
  return(list(nueva_1, nueva_2))
}

# Calculando las diferencias

calcular_diferencia <- function(muestras, FUN){
  muestra_1 <- muestras[[1]]
  muestra_2 <- muestras[[2]]
  diferencia <- FUN(muestra_1)-FUN(muestra_2)
  return(diferencia)
}


calcular_valor_p <- function(distribucion, valor_observado, repeticiones, alternative){
  if(alternative == "two.sided"){
    numerador <- sum(abs(distribucion)>abs(valor_observado))+1
    denominador <- repeticiones + 1
    valor_p <- numerador/denominador
  }
  else if(alternative == "greater"){
    numerador <- sum(distribucion > valor_observado) + 1
    denominador <- repeticiones + 1
    valor <- numerador/denominador
  }
  return(valor_p)
}


graficar_distribucion <- function(distribucion, ...){
  observaciones <- data.frame(distribucion)
  
  histograma <- gghistogram(observaciones, x = "distribucion",
                            xlab="Estadistico de interés",
                            ylab="Frecuencia", bins=30, ...)
  
  qq <- ggqqplot(observaciones, x = "distribucion", ...)
  
  figura <- ggarrange(histograma, qq, ncol=2, nrow=1)
  print(figura)
}


# Contraste de hipotesis

contrastar_hipotesis_permutaciones <- function(muestra_1, muestra_2, repeticiones, FUN, alternative, plot, ...){
  cat("Prueba de permiutaciones\n\n")
  cat("Hipótesis alternativa:", alternative, "\n")
  observado <- calcular_diferencia(list(muestra_1, muestra_2), FUN)
  cat("Valor observado:", observado, "\n")
  n_1 <- length(muestra_1)
  
  
  #Generar las permutaciones
  
  permutaciones <- lapply(1:repeticiones, obtiene_permutaciones, muestra_1, muestra_2)
  
  #Generar distribución
  distribucion <- sapply(permutaciones, calcular_diferencia, FUN)
  
  #Graficar la distribución
  if(plot){
    graficar_distribucion(distribucion, ...)
  }
  
  #Calcular el valor p
  valor_p <- calcular_valor_p(distribucion, observado, repeticiones, alternative)
  
  cat("Valor p:", valor_p, "\n\n")
}

# Contraste de las medias
contrastar_hipotesis_permutaciones(ingresos_Educacion, ingresos_Salud, repeticiones = R, FUN = mean, alternative="two.sided", 
                                   plot = TRUE, color="blue", fill="blue")

contrastar_hipotesis_permutaciones(ingresos_Educacion, ingresos_Salud, repeticiones = R, FUN = var, alternative="two.sided", 
                                   plot = TRUE, color="blue", fill="blue")

#Respuesta: El valor p de la prueba es de 0.003 por lo que se rechaza la hipórtesis de que las medias son iguales
            #Por lo tanto las medias son diferentes para los profesionales de la "Educación" y "Salud"


#Pregunta 2

install.packages("ez")
install.packages("devtools")
install.packages('tidyr')
install.packages("ggplot2")

#Pregunta de investigación: Existe diferencia entre los grupos de profesionales con estudios completos del 
#área de Administración, Educación e Ingeniería
library(readxl)
# Datos
datos <- read_excel("C:/Users/crist/OneDrive/Escritorio/Ejercicio11InferenciaEstadística/datos.xlsx")

ingresos_ProfesionalCompleto <- datos[datos$educ == "Profesional Completo",]

ingresos_Educacion <- ingresos_ProfesionalCompleto[ingresos_ProfesionalCompleto$e7.cod.area=="Educación",]

ingresos_AdministracionDerecho <- ingresos_ProfesionalCompleto[ingresos_ProfesionalCompleto$e7.cod.area=="Administración de Empresas y Derecho",]

ingresos_TIC <- ingresos_ProfesionalCompleto[ingresos_ProfesionalCompleto$e7.cod.area=="Tecnología de la Información y la Comunicación (TIC)",]

ingresos_Ingenieria <- ingresos_ProfesionalCompleto[ingresos_ProfesionalCompleto$e7.cod.area=="Ingeniería, Industria y Construcción",]

ingresos_Salud <- ingresos_ProfesionalCompleto[ingresos_ProfesionalCompleto$e7.cod.area=="Salud y Bienestar",]

ingresos_AdministracionDerecho <- ingresos_AdministracionDerecho$ytotcorh
ingresos_TIC <- ingresos_TIC$ytotcorh
ingresos_Ingenieria <- ingresos_Ingenieria$ytotcorh
ingresos_Educacion <- ingresos_Educacion$ytotcorh

ingresos_AdministracionDerecho
ingresos_TIC
ingresos_Ingenieria

# Normalidad de los datos

shapiro.test(ingresos_AdministracionDerecho)
shapiro.test(ingresos_TIC)
shapiro.test(ingresos_Ingenieria)

boxplot(ingresos_AdministracionDerecho,ingresos_TIC, ingresos_Ingenieria)

hist(ingresos_AdministracionDerecho)
hist(ingresos_TIC)
hist(ingresos_Ingenieria)

# Todas las muestras no distribuyen normal

#Creando un data frame

long1 <- length(ingresos_AdministracionDerecho)
long2 <- length(ingresos_Educacion)
long3 <- length(ingresos_Ingenieria)

long1
long2
long3

ingresos_AdministracionDerecho <- ingresos_AdministracionDerecho[1:1223]
ingresos_Educacion <- ingresos_Educacion[1:1223]
ingresos_Ingenieria <- ingresos_Ingenieria[1:1223]

mean(ingresos_AdministracionDerecho)
mean(ingresos_Educacion)
mean(ingresos_Ingenieria)


library(tidyr)

instancia <- factor(1:1223)
datos_anchos <- data.frame(instancia, ingresos_AdministracionDerecho, ingresos_Educacion, ingresos_Ingenieria)
datos_largos <- datos_anchos %>% pivot_longer(c("ingresos_AdministracionDerecho","ingresos_Educacion","ingresos_Ingenieria"), names_to = "Ingresos", values_to="Dinero")
datos_largos[["Ingresos"]]  <- factor(datos_largos[["Ingresos"]])

library(ggplot2)

#g <- ggqqplot(datos_largos, "Dinero", facet.by = "Dinero", color="Ingresos")
#print(g)

library(ez)
library("devtools")

alfa <- 0.01
anova <- ezANOVA(datos_largos, dv = Dinero, within=Ingresos, wid=instancia, return_aov=TRUE)
anova

valor_observado <- anova[["ANOVA"]][["F"]]

#Generar permutaciones

R = 500
set.seed(432)

#Función para obtener una permutación
#Devuelve una matriz de datos con formato ancho

obtiene_permutacion <- function(i, df_ancho) {
  df_ancho[, 2:4] <- t(apply(df_ancho[,2:4], 1, sample))
  return(df_ancho)
}

# Obtiene permutaciones
permutaciones <- lapply(1:R, obtiene_permutacion, datos_anchos)

#Función para obtener el estadistico F para una matriz de datos con formato ancho

obtiene_F <- function(df_ancho){
  df_largo <- df_ancho %>% pivot_longer(c("ingresos_AdministracionDerecho","ingresos_Educacion","ingresos_Ingenieria"), names_to = "Ingresos", values_to="Dinero")
  
  df_largo[["Ingresos"]]  <- factor(df_largo[["Ingresos"]])
  anova <- ezANOVA(df_largo, dv = Dinero, within=Ingresos, wid=instancia, return_aov=TRUE)
  return(anova[["ANOVA"]][["F"]])
}


# Genera distribución de estadisticos F con las permutaciones

distribucion <- sapply(permutaciones, obtiene_F)

# Obtener el valor p

p <- (sum(distribucion > valor_observado)+1)/(R+1)
cat("ANOVA de una vía para muestras pareadas con permutaciones")
cat("p=", p, "\n\n")


#Respuesta: Existen diferencias significativas entre el grupo de Administracion y Educación, y el grupo
#entre Ingeniería y Educación. El area de educación tiene en promedio menor ingreso comparado con Ingeniería y Administración



