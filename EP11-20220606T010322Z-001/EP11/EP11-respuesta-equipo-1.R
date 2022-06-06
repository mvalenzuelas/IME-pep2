library(dplyr)
library(ggpubr)
library(tidyverse)
library(ez)
library(nlme)
library(emmeans)
library(simpleboot)
library(boot)

data <- read.csv2("C:\\Users\\danie\\Desktop\\Ramos 2022-1\\IME\\EP11 Datos.csv")

#-------------------------------Pregunta 1--------------------------------------

# Propongan una pregunta de investigación original, que involucre la comparación 
# de las medias de dos grupos independientes (más abajo se dan unos ejemplos). 
# Fijando una semilla propia, seleccionen una muestra aleatoria de hogares 
# (250 < n < 500) y respondan la pregunta propuesta utilizando una 
# simulación Monte Carlo.

#-------------------------------------------------------------------------------

# Pregunta de investigación original: 
# El promedio de edades de personas adultas en la región metropolitana, que se 
# encuentran afiliados a un sistema previsional y aquellos que no se encuentran 
# afiliados, son iguales.

data <-data%>%filter(region =="Región Metropolitana de Santiago" & edad >= 18)


R = 399
n = 333
alfa = 0.05
set.seed(336)

muestra <- sample_n(data, n)

afiliados <- muestra%>%filter(o28 == "Sí")

no_afiliados <- muestra%>%filter(o28 == "No")

edades_afiliados <- afiliados$edad
edades_noAfiliados <- no_afiliados$edad

# H0:El promedio de edades de las personas afiliadas es igual al promedio de 
# edades de las personas no afiliadas en la Región Metropolitana

# Ha:El promedio de edades de las personas afiliadas es diferente al promedio de 
# edades de las personas no afiliadas en la Región Metropolitana


obtiene_permutacion <- function(i, muestra_1, muestra_2) {
  n_1 <- length(muestra_1)
  combinada <- c(muestra_1, muestra_2)
  n <- length(combinada)
  permutacion <- sample(combinada, n, replace = FALSE)
  nueva_1 <- permutacion[1:n_1]
  nueva_2 <- permutacion[(n_1+1):n]
  return(list(nueva_1, nueva_2))
}


calcular_diferencia <- function(muestras, FUN) {
  muestra_1 <- muestras[[1]]
  muestra_2 <- muestras[[2]]
  diferencia <- FUN(muestra_1) - FUN(muestra_2)
  return(diferencia)
}

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

graficar_distribucion <- function(distribucion, ...) {
  observaciones <- data.frame(distribucion)
  
  histograma <- gghistogram(observaciones, x = "distribucion",
                            xlab = "Estadístico de interés",
                            ylab = "Frecuencia", bins = 30, ...)
  
  qq <- ggqqplot(observaciones, x = "distribucion", ...)
  
  # Crear una única figura con todos los gráficos de dispersión.
  figura  <- ggarrange(histograma, qq, ncol = 2, nrow = 1)
  print(figura)
}

contrastar_hipotesis_permutaciones <- function(muestra_1, muestra_2,
                                               repeticiones, FUN,
                                               alternative, plot, ...) {
  cat("Prueba de permutaciones\n\n")
  cat("Hipótesis alternativa:", alternative, "\n")
  observado <- calcular_diferencia(list(muestra_1, muestra_2), FUN)
  cat("Valor observado:", observado, "\n")
  
  n_1 <- length(muestra_1)
  
  # Generar permutaciones.
  permutaciones <- lapply(1:repeticiones, obtiene_permutacion, muestra_1,
                          muestra_2)
  
  # Generar la distribución.
  distribucion <- sapply(permutaciones, calcular_diferencia, FUN)
  
  # Graficar la distribución.
  if(plot) {
    graficar_distribucion(distribucion, ...)
  }
  
  # Calcular el valor p.
  valor_p <- calcular_valor_p(distribucion, observado, repeticiones,
                              alternative)
  
  cat("Valor p:", valor_p, "\n\n")
}

contrastar_hipotesis_permutaciones(edades_afiliados, edades_noAfiliados, 
                                   repeticiones = R, FUN = mean,
                                   alternative = "two.sided", plot = FALSE,
                                   color = "blue", fill = "blue")
#p-valor = 0.0025 < alfa

# Por tanto, se rechaza la H0 en favor de Ha, es decir, que se concluye, con un 
# 95% de confianza, que el promedio de edades de las personas afiliadas es 
# diferente al promedio de edades de las personas no afiliadas 
# en la Región Metropolitana.


#-------------------------------Pregunta 2--------------------------------------

# Propongan una pregunta de investigación original, que involucre la comparación 
# de las medias de más de dos grupos independientes (más abajo se dan unos 
# ejemplos). Fijando una semilla distinta a la anterior, seleccionen una muestra
# aleatoria de hogares (400 < n < 600) y respondan la pregunta propuesta 
# utilizando bootstrapping. Solo por ejercicio académico, aplique un análisis 
# post-hoc con bootstrapping aunque este no sea necesario.

#-------------------------------------------------------------------------------

# Pregunta de investigación original:
# Determinar si existe relación entre las medias de las edades entre personas 
# solteras, casadas y divorciadas en la Región Metropolitana

set.seed(224)
n2 = 500
B = 499

muestra2 = sample_n(data, n2)


casado <- muestra2%>%filter(ecivil == "Casado(a)")
soltero <- muestra2%>%filter(ecivil == "Soltero(a)")
divorciado <- muestra2%>%filter(ecivil == "Divorciado (a)")

edades_casado <- casado$edad
edades_soltero <- soltero$edad
edades_divorciado <- divorciado$edad

n_1 = length(edades_casado)
n_2 = length(edades_soltero)
n_3 = length(edades_divorciado)
n_edades <- n_1 + n_2 + n_3

id <- factor(1:n_edades)

largo_origin <- rbind(data.frame(muestra = c(rep(1, n_1)), edad = edades_casado),
                      data.frame(muestra = c(rep(2, n_2)), edad = edades_soltero), 
                      data.frame(muestra = c(rep(3, n_3)), edad = edades_divorciado))

largo_origin <- cbind(id, largo_origin)


largo_origin[["muestra"]] = factor(largo_origin[["muestra"]])

anova <- ezANOVA(largo_origin, dv =edad, between = muestra, wid = id,
                 return_aov = TRUE)

valor_observado <- anova[["ANOVA"]][["F"]]

# H0: Las medias de las edades entre personas solteras, casadas y divorciadas 
# en la Región Metropolitana, son iguales

# Ha: Las medias de las edades entre personas solteras, casadas y divorciadas 
# en la Región Metropolitana, son diferentes en al menos un estado civil.

obtiene_bootstrap <- function(i, muestra_1, muestra_2, muestra_3) {
  n_1 <- length(muestra_1)
  n_2 <- length(muestra_2)
  n_3 <- length(muestra_3)
  n <- n_1+n_2+n_3
  id <- factor(1:n)
  combinacion_1 <- sample(muestra_1, n_1, replace = TRUE)
  combinacion_2 <- sample(muestra_2, n_2, replace = TRUE)
  combinacion_3 <- sample(muestra_3, n_3, replace = TRUE)
  ancho_1 <- data.frame(muestra = c(rep(1, n_1)), edad =  combinacion_1)
  ancho_2 <- data.frame(muestra = c(rep(2, n_2)), edad = combinacion_2)
  ancho_3 <- data.frame(muestra = c(rep(3, n_3)), edad = combinacion_3)
  largo <- rbind(ancho_1, ancho_2, ancho_3)
  largo <- cbind(id, largo)
  return(largo)
}

combinaciones <- lapply(1:B, 
                        obtiene_bootstrap, 
                        edades_casado, 
                        edades_soltero, 
                        edades_divorciado)

obtiene_F <- function(largo){
  largo[["muestra"]] = factor(largo[["muestra"]])
  
  anova <- ezANOVA(largo, 
                   dv = edad, 
                   between = muestra, 
                   wid = id,
                   return_aov = TRUE, 
                   type = 2)
  
  return(anova[["ANOVA"]][["F"]])
}

distribucion <- sapply(combinaciones, obtiene_F)

p <- (sum(distribucion > valor_observado)+1)/(B+1)
print(p)

# p-valor = 0.536 > alfa

# Por tanto, se falla en rechazar H0 en favor de Ha, es decir, se concluye, 
# con un 95% de confianza, que las medias de las edades entre personas 
# solteras, casadas y divorciadas en la Región Metropolitana, son iguales.

# Para este resultado un analisis post - hoc no es necesario.
# Sin embargo, igual se hará con fines de aprendizaje.

# Análisis post-hoc:

calcular_diferencia_2 <- function(d_largos, m1, m2) {
  muestra_1 <- d_largos %>% filter(muestra == m1)
  muestra_2 <- d_largos %>% filter(muestra == m2)
  diferencia <- mean(muestra_1$edad) - mean(muestra_2$edad)
  return(diferencia)
}

# Medias de las diferencias observadas:
dif_original_soltero_casado <- calcular_diferencia_2(largo_origin, 2, 1)
dif_original_soltero_divorciado <- calcular_diferencia_2(largo_origin, 2, 3)
dif_original_casado_divorciado <- calcular_diferencia_2(largo_origin, 1, 3)

# Media de las diferencias post:
dif_post_soltero_casado <- sapply(combinaciones, calcular_diferencia_2, 2, 1)
dif_post_soltero_divorciado <- sapply(combinaciones, calcular_diferencia_2, 2, 3)
dif_post_casado_divorciado <- sapply(combinaciones, calcular_diferencia_2, 1, 3)

# Valores p:
p1 <- (sum(abs(dif_post_soltero_casado) > abs(dif_original_soltero_casado))+1)/(B+1)
print(p1)
# p1 = 0.48 > alfa

p2 <- (sum(abs(dif_post_soltero_divorciado) > abs(dif_original_soltero_divorciado))+1)/(B+1)
print(p2)
# p2 = 0.5 > alfa 

p3 <- (sum(abs(dif_post_casado_divorciado) > abs(dif_original_casado_divorciado))+1)/(B+1)
print(p3)
# p3 = 0.986 > alfa

# Por lo tanto, se comprueba el resultado original, ya que ningún p-valor tuvo 
# una diferencia significativa.