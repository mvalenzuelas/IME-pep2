#Librerias
library(dplyr)
library(ggpubr)
library(ez)
library(tidyverse)
library(boot)

#=========================
#==== Lectura de datos ===
#=========================
#cambiar dependiendo de la direccion de su archivo
#datos <- read.csv2("C:/Users/fabia/Desktop/01-2022/IME/trabajos/IME-Unidad2/EP11/EP11 Datos.csv",stringsAsFactors = FALSE)
#datos <- read.csv2("C:/Users/dgrdo/Desktop/IME-Unidad2/EP11/EP11 Datos.csv",stringsAsFactors = FALSE)
datos <- read.csv2("EP11 Datos.csv",stringsAsFactors = FALSE)

#Definimos una semilla
set.seed(999)
R <- 5999

#=========================
#==== Funciones ===
#=========================

#Pregunta 1

#Funcion que entrega las permutaciones
obtiene_permutacion <- function(i, muestra_1, muestra_2) {
  n_1 <- length(muestra_1)
  combinada <- c(muestra_1, muestra_2)
  n <- length(combinada)
  permutacion <- sample(combinada, n, replace = FALSE)
  nueva_1 <- permutacion[1:n_1]
  nueva_2 <- permutacion[(n_1+1):n]
  return(list(nueva_1, nueva_2))
}

#Funcion que calcula la diferencia
calcular_diferencia <- function(muestras, FUN) {
  muestra_1 <- muestras[[1]]
  muestra_2 <- muestras[[2]]
  diferencia <- FUN(muestra_1) - FUN(muestra_2)
  return(diferencia)
}

#Funcion que calcula el valor p
calcular_valor_p <- function(distribucion, valor_observado,
                             repeticiones, alternative) {
  if(alternative == "two.sided") {
    numerador <- sum(abs(distribucion) > abs(valor_observado)) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  else if(alternative == "greater") {
    numerador <- sum(distribucion > valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  else {
    numerador <- sum(distribucion < valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  
  return(valor_p)
}

#Funcion para graficar la distribucion
graficar_distribucion <- function(distribucion, ...) {
  observaciones <- data.frame(distribucion)
  
  histograma <- gghistogram(observaciones, x = "distribucion",
                            xlab = "Estadistico de interes",
                            ylab = "Frecuencia", bins = 30, ...)
  
  qq <- ggqqplot(observaciones, x = "distribucion", ...)
  
  # Crear una unica figura con todos los graficos de dispersion.
  figura  <- ggarrange(histograma, qq, ncol = 2, nrow = 1)
  print(figura)
}

#Funcion para contrastar hipotesis
contrastar_hipotesis_permutaciones <- function(muestra_1, muestra_2,
                                               repeticiones, FUN,
                                               alternative, plot, ...) {
  cat("Prueba de permutaciones\n\n")
  cat("Hipotesis alternativa:", alternative, "\n")
  observado <- calcular_diferencia(list(muestra_1, muestra_2), FUN)
  cat("Valor observado:", observado, "\n")
  
  n_1 <- length(muestra_1)
  
  # Generar permutaciones.
  permutaciones <- lapply(1:repeticiones, obtiene_permutacion, muestra_1,
                          muestra_2)
  
  # Generar la distribucion.
  distribucion <- sapply(permutaciones, calcular_diferencia, FUN)
  
  # Graficar la distribucion.
  if(plot) {
    graficar_distribucion(distribucion, ...)
  }
  
  # Calcular el valor p.
  valor_p <- calcular_valor_p(distribucion, observado, repeticiones,
                              alternative)
  
  cat("Valor p:", valor_p, "\n\n")
}

#-------------------------------------------------------------------------------

#PREGUNTA 2 
bootstrap <- function(x){
  # Se toma una muestra con reemplazo para cada grupo
  i_norte <- sample(1:n_norte, replace = TRUE) 
  i_centro <- sample(1:n_centro, replace = TRUE)
  i_sur <- sample(1:n_sur, replace = TRUE)
  rbind(norte[i_norte,], centro[i_centro,], sur[i_sur,])
}

calcular_F <- function(frame){
  anova <- ezANOVA(frame, dv = ingresos, between = regiones, 
                   wid = id, return_aov = FALSE)
  invisible(anova$ANOVA$F)
}

calcular_diferencia2 <- function(muestra_1, muestra_2, FUN){
  diferencia <- FUN(muestra_1) - FUN(muestra_2)
  return(diferencia)
}

distribucion_dif <- function(permutaciones, columna_1, columna_2){
  R <- length(permutaciones)
  distribucion <- c()
  for(i in 1:R){
    datos <- as.data.frame(permutaciones[i])
    muestra_1 <- datos %>% filter(regiones == columna_1)
    muestra_2 <- datos %>% filter(regiones == columna_2)
    diferencia <- calcular_diferencia2(muestra_1[["ingresos"]], muestra_2[["ingresos"]], mean)
    distribucion <- c(distribucion, diferencia)
  }
  return(distribucion)
}


#=========================
#==== Enunciado 1 ===
#=========================
# Propongan una pregunta de investigacion original, que involucre la comparacion de las medias de dos grupos 
# independientes (mas abajo se dan unos ejemplos). Fijando una semilla propia, seleccionen una muestra aleatoria 
# de hogares (250 < n < 500) y respondan la pregunta propuesta utilizando una simulacion Monte Carlo.
# 
# PREGUNTA 1
# Un estudio en Chile busca identificar las situaciones laborales en el país dependiendo del sexo 
# y si existen difrencias, para ello realizan un estudio para verificar si  eL promedio de horas 
# labolares(variable o10) que trabajan hombres y es el mismo que el promedio de hora laborales de las mujeres.

# Definiendo significaciOn de 0.05
alfa = 0.05

# HIPOTESIS
# H0: No hay diferencia entre el promedio de horas laborales de hombres y mujeres.
# HA: Hay diferencia entre el promedio de horas laborales de hombres y mujeres.

# Matematicamente
# Sea "uh" el promedio de horas laborales de hombres y "um" el promedio de horas laborales de mujeres en Chile,
# entonces:
# H0: uh - um = 0
# HA: uh - um != 0

#Filtramos los datos necesarios
datos1 <- datos %>% filter(o1 == "Sí")
datos1 <- datos1 %>% filter(o10 != "NA")
datos1 <- datos1 %>% select(sexo,o10)
datos1 <- sample_n(datos1,400)

#Separamos las muestras
datosHombre <- datos1 %>% filter(sexo == "Hombre")
vectorHombre <- as.vector(t(datosHombre %>% select(o10)))
datosMujer <- datos1 %>% filter(sexo == "Mujer")
vectorMujer <- as.vector(t(datosMujer %>% select(o10)))

gh <- ggqqplot(datosHombre$o10,title="Horas laborales hombres")
gm <- ggqqplot(datosMujer$o10,title="Horas laborales mujeres")

# Como se puede ver en los graficos cuantil-cuantil, las horas
# laborales no se distribuyen normal puesto a que hay muchos cuantiles
# que se alejan demasiado del cuantil teorico. Es por esto
# que no se puede aplicar una prueba t de Student para muestras independientes,
# por lo que se optara por una prueba de permutaciones con Monte Carlo
# para inferir sobre las medias.

print(gh)
print(gm)
#=================================
#==== Desarrollo y Conclusion ===
#=================================

contrastar_hipotesis_permutaciones(vectorHombre, vectorMujer, repeticiones = R, FUN = mean,
                                   alternative = "two.sided", plot = TRUE,
                                   color = "blue", fill = "blue")
# Conclusion
# Los resultados muestran que p-valor = 0.00017 < alfa, por lo tanto hay suficiente evidencia para rechazar H0 en favor
# de HA, por lo que es posible concluir con un 95% de confianza que los promedios de horas laborales para hombres
# y mujeres son distintos.


#=========================
#==== Enunciado 2 ===
#=========================
# Propongan una pregunta de investigacion original, que involucre la comparacion de las medias de mas de dos grupos 
# independientes (mas abajo se dan unos ejemplos). Fijando una semilla distinta a la anterior, seleccionen una 
# muestra aleatoria de hogares (400 < n < 600) y respondan la pregunta propuesta utilizando bootstrapping. 
# Solo por ejercicio academico, aplique un analisis post-hoc con bootstrapping aunque este no sea necesario.

#PREGUNTA 2
 
# Una universidad de Chile necesita conocer la situacion economica de los hogares a los que pertenecen 
# sus estudiantes, esto con el fin de disponer de distintas becas que los apoyen durante su carrera. Para ello
# realizaron estudio tomando como ejemplo en 3 regiones correspondientes al sector norte, centro y sur del país respectivamente.
# Lo que se busca con esto es verificar si el ingreso total (variable ytotcorch) medio de los hogares es similar en
# las regiones norte(region Coquimbo), centro(region maule) y sur (region los lagos) del pais.

set.seed(573)

datosRegion <- datos %>% filter(ytotcorh != "NA")
datosRegion <- filter(datos, region %in% c("Región de Coquimbo", "Región del Maule", "Región de Los Lagos"))

# Se toma una muestra de 600 personas pertenecientes a esas regiones
tam <- 600
muestra <- datosRegion[sample(nrow(datosRegion), tam),]
ingresos <- muestra[["ytotcorh"]]
regiones <- factor(muestra[["region"]])
id <- factor(1:tam)
datos2 <- data.frame(id, ingresos, regiones)

norte <- datos2 %>% filter(regiones == "Región de Coquimbo")
n_norte <- nrow(norte)

centro <- datos2 %>% filter(regiones == "Región del Maule")
n_centro <- nrow(centro)

sur <- datos2 %>% filter(regiones == "Región de Los Lagos")
n_sur <- nrow(sur)


gn <- ggqqplot(norte$ingresos,title="Ingresos region de Coquimbo")
gc <- ggqqplot(centro$ingresos,title="Ingresos region del Maule")
gs <- ggqqplot(sur$ingresos,title="Ingresos region de Los Lagos")

# Como se puede ver en los gráficos cuantil cuantil para cada region,
# no cumplen la condicion de normalidad para el procedimiento ANOVA,
# puesto que se desvían bastante de los cuantiles teoricos. Por lo tanto, en
# este caso se piensa utilizar la tecnica de remuestreo de 
# bootstrapping para mas de dos muestras independientes, creando una funcion que haga esto,
# usando alfa = 0.05 y una cantidad de remuestreos R = 999.

# HIPOTESIS
# H0: El ingreso total medio de los hogares es el mismo paras las regiones de Coquimbo, Maule y Los Lagos.
# HA:  El ingreso total medio de los hogares es distinto paras las regiones de Coquimbo, Maule y Los Lagos.
#
# Matematicamente
# Sean uIC, uIM y uIL los ingresos totales medios por hogar para las regiones de Coquimbo, Maule y Los Lagos, respectivamente,
# entonces:
# H0: uIC - uIM - uIL = 0
# HA: uIC - uIM - uIL != 0

#=================================
#==== Desarrollo y Conclusion ===
#=================================

#Obtenemos el estadistico F
anova_1 <- ezANOVA(datos2, dv = ingresos, between = regiones,
                   wid = id, return_aov = FALSE)
print(anova_1)

# Conclusion
# Los resultados muestran que el p-valor (0.399) es significativamente mayor al nivel de significacion escogido (0.05),
# por lo tanto no existe evidencia suficiente para rechazar la hipotesis nula. Entonces, es posible decir con un 95% de
# confianza, que los ingresos totales medios por hogar en las regiones de Coquimbo, Maule y Los Lagos, son iguales.

#REMUESTREO

#Numero
R2 <- 999
set.seed(735)

distribucion2 <- lapply(1:R2, bootstrap)

suppressMessages(suppressWarnings(Fs <- sapply(distribucion2, calcular_F))) # evitar los warnings

p <- calcular_valor_p(Fs, anova_1$ANOVA$F, R2, "two.sided")
cat("Valor p (bootstrap): ", p)


# Analisis post-hoc
# Dado que el resultado de la prueba fue p-valor > alfa, no es necesario realizar un analisis post hoc,
# sin embargo, este se pide en el enunciado.

#Calculo de diferencias
dif_norte_sur <- calcular_diferencia2(norte[["ingresos"]], sur[["ingresos"]], mean)
dif_norte_centro <- calcular_diferencia2(norte[["ingresos"]], centro[["ingresos"]], mean)
dif_centro_sur <- calcular_diferencia2(centro[["ingresos"]], sur[["ingresos"]], mean)

dif_Coquimbo_Lagos <- distribucion_dif(distribucion2, "Región de Coquimbo", "Región de Los Lagos")
dif_Coquimbo_Maule <- distribucion_dif(distribucion2, "Región de Coquimbo", "Región del Maule")
dif_Maule_Lagos <- distribucion_dif(distribucion2, "Región del Maule", "Región de Los Lagos")

#Valores p
n1 <- sum(abs(dif_Coquimbo_Lagos) > abs(dif_norte_sur) + 1)
d1 <- R2 + 1

p_norte_sur <- n1/d1

n2 <- sum(abs(dif_Coquimbo_Maule) > abs(dif_norte_centro) + 1)
d2 <- R2 + 1

p_norte_centro <- n2/d2

n3 <- sum(abs(dif_Maule_Lagos) > abs(dif_centro_sur) + 1)
d3 <- R2 + 1

p_centro_sur <- n3/d3

#grafico
g2 <- ezPlot(data =datos2, dv = ingresos, wid = id, between = regiones, y_lab = "Media de los ingresos",
             x = regiones)
print(g2)
cat("p-valor norte-sur: ", p_norte_sur)
cat("p-valor norte-centro: ", p_norte_centro)
cat("p-valor centro-sur: ", p_centro_sur)

# Resultado analisis post hoc
# Se puede ver que todos los valores p obtenidos para los distintos grupos son mayores al alfa definido,
# lo cual es correcto dado que el p-valor obtenido por la prueba también concluye lo mismo.
# Con respecto al gráfico obtenido, se observa que las medias en los ingresos totales por hogar a pesar 
# de no ser iguales, no son significativamente diferentes. 
# También, se observa que la mayor diferencia está entre la región de Coquimbo y la región de 
# Los Lagos (norte y sur respectivamente), siendo esta última la que tiene un ingreso levemente
# mayor, esto se ve también con el p-valor, dado que es el menor entre todos los grupos.
# La mayor similitud está entre las region de Coquimbo y la región del Maule (norte y centro),
# y se evidencia al tener el mayor valor p de los grupos.




