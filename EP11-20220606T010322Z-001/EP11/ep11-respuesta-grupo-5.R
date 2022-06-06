library(tidyverse)
library(ggpubr)
library(dplyr)
library(patchwork)
library(ez)
library(simpleboot)
library(boot)

# -- Funciones para la permutación

# FunciÃ³n para obtener una permutaciÃ³n.
# Devuelve una matriz de datos con formato ancho.
obtiene_per <- function(i, df_ancho) {
  df_ancho[1:450,] <- t(apply(df_ancho[1:450,], 1, sample))
  return(df_ancho)
}

obtiene_F <- function(indigenas_muestra) {
pruebaAnova <- ezANOVA(indigenas_muestra,
                       dv=ytotcorh,
                       between = r3,
                       wid = instancia,
                       return_aov = TRUE)
f <- pruebaAnova[["ANOVA"]][["F"]]
}
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
  
  # Generar la distribuciÃ³n.
  distribucion <- sapply(permutaciones, calcular_diferencia, FUN)
  
  # Graficar la distribuciÃ³n.
  if(plot) {
    graficar_distribucion(distribucion, ...)
  }
  
  # Calcular el valor p.
  valor_p <- calcular_valor_p(distribucion, observado, repeticiones,
                              alternative)
  
  cat("Valor p:", valor_p, "\n\n")
}


#-------------------------------------------------------------#

datos <- read.csv2("C://Users//Monica Lopez//Desktop//ime//EP11 Datos.csv")
coquimbo <- datos %>% filter(datos$region=="Región de Coquimbo")

"
Propongan una pregunta de investigación original, que involucre la comparación de las medias de 
dos grupos independientes (más abajo se dan unos ejemplos). Fijando una semilla propia, 
seleccionen una muestra aleatoria de hogares (250 < n < 500) y respondan la pregunta propuesta 
utilizando una simulación Monte Carlo.
"

set.seed(4207)

#La cantidad promedio de persona que habitan un hogar en la región de coquimbo, depende
#del nivel educacional de enseñanza básica del jefe del hogar


muestra <- sample_n(coquimbo %>% select(numper,educ), 400, replace = FALSE)
muestra_basicaIncom <- muestra %>% filter(educ == "Básica Incom.") 
muestra_basicaComp <- muestra %>% filter(educ == "Básica Compl.")

basica_incompleta <- muestra_basicaIncom$numper
basica_completa <- muestra_basicaComp$numper


#Hipótesis:
#H0: La cantidad de personas que habitan un hogar depende de si el jefe de hogar finalizó la enseñanza básica
#HA: La cantidad de personas que habitan un hogar no depende de si el jefe de hogar finalizó la enseñanza básica

#Matemáticamente:

#H0: media_incom != media_compl
#H0: media_incom == media_compl

#Cantidad P de permutaciones

P = 2999
alpha = 0.05

#Distribución del estadístico


# Hacer pruebas de permutaciones para la media y la varianza.
contrastar_hipotesis_permutaciones(basica_completa, basica_incompleta, repeticiones = P, FUN = mean,
                                   alternative = "two.sided", plot = TRUE,
                                   color = "blue", fill = "blue")



#Conclusión: Se determina con un 95% de confianza que no existe diferencia entre la media de personas que habitan
#             un hogar donde el jefe de hogar tiene educación básica completa o incompleta. (p>alpha)
#            (Se falla en rechazar h0 en favor a ha)




"Propongan una pregunta de investigación original, que involucre la comparación de las medias de
más de dos grupos independientes (más abajo se dan unos ejemplos). Fijando una semilla distinta 
a la anterior, seleccionen una muestra aleatoria de hogares (400 < n < 600) y respondan la 
pregunta propuesta utilizando bootstrapping. Solo por ejercicio académico, aplique un análisis 
post-hoc con bootstrapping aunque este no sea necesario."

#La escala de la variable dependiente es una escala de intervalos iguales
#La muestra se consigue de forma aleatoria e independiente de la población de origen

#Nivel de significancia de 0.05

set.seed(10245)

#Repeticiones bootstrapping
rep <- 9999

#El ingreso total del hogar de personas con raices indigenas
#H0: El ingreso total del hogar de personas con raices indigenas es igual
#Ha: El ingreso total del hogar de personas con raices indigenas es diferente

#Matemáticamente:
#H0: mu_quechua = mu_diaguita = mu_aimara
#H1: mu_quechua != mu_diaguita != mu_aimara

#Valor nulo = 0 (se omite en los cálculos)


indigenas_tot <- datos %>% filter(r3 != "No pertenece a ningún pueblo indígena")

indigenas_tot <- indigenas_tot %>% select(ytotcorh,r3)

quechua  <- indigenas_tot %>% filter(r3 == "Quechua")
diaguita <- indigenas_tot %>% filter(r3 == "Diaguita")
aimara    <- indigenas_tot %>% filter(r3 == "Aimara")

indigenas <- rbind.data.frame(quechua,diaguita,aimara)

indigenas_muestra <- sample_n(indigenas, 450)

quechua  <- indigenas_muestra %>% filter(r3 == "Quechua")
diaguita <- indigenas_muestra %>% filter(r3 == "Diaguita")
aimara    <- indigenas_muestra %>% filter(r3 == "Aimara")

Ingreso_Quechua  <- quechua$ytotcorh
Ingreso_Diaguitas <- diaguita$ytotcorh
Ingreso_Aimara    <- aimara$ytotcorh


#La distribucion de origen sigue una distribucion normal
grafico <- ggqqplot(Ingreso_Quechua)

print(grafico)

grafico <- ggqqplot(Ingreso_Diaguitas)

print(grafico)

grafico <- ggqqplot(Ingreso_Aimara)

print(grafico)

indigenas_muestra[["instancia"]] <- factor(1:nrow(indigenas_muestra))

pruebaAnova <- ezANOVA(indigenas_muestra,
                       dv=ytotcorh,
                       between = r3,
                       wid = instancia,
                       return_aov = TRUE)
f <- pruebaAnova[["ANOVA"]][["F"]]

# Obtiene permutaciones
#permutaciones <- lapply(1:rep, obtiene_per, indigenas_muestra)
#distribucion <- sapply(permutaciones, obtiene_F)
#p <- calcular_valor_p(distribucion, f, rep, "two.sided")

#Como los datos de EZanova dan diferente se opta por el metodo de bootstrapping para 
#muestras independientes para la prueba post-hoc

#Cálculo de las diferencia de las medias
media_quechua <- mean(Ingreso_Quechua)
media_diaguita <- mean(Ingreso_Diaguitas)
media_aimara <- mean(Ingreso_Aimara)
dif_quechua_diaguita <- media_quechua - media_diaguita
dif_quechua_aimara <- media_quechua - media_aimara
dif_diaguita_aimara <- media_diaguita - media_aimara

#Distribución bootstrapping para las muestras
quechua_diaguita <- two.boot(Ingreso_Quechua,Ingreso_Diaguitas, FUN = mean, R = rep)
quechua_aimara <- two.boot(Ingreso_Quechua,Ingreso_Aimara, FUN = mean, R = rep)
diaguita_aimara <- two.boot(Ingreso_Diaguitas,Ingreso_Aimara, FUN = mean, R = rep)

#Examinando la distribución bootstrapping
valor1 <- quechua_diaguita[["t"]] - mean(quechua_diaguita[["t"]])
valor2 <- quechua_aimara[["t"]] - mean(quechua_aimara[["t"]])
valor3 <- diaguita_aimara[["t"]] - mean(diaguita_aimara[["t"]])

#Sacando el valor p para las muestras
p1 <- (sum(abs(valor1) > abs(dif_quechua_diaguita)) + 1) / (rep+1)
p2 <- (sum(abs(valor2) > abs(dif_quechua_aimara)) + 1) / (rep+1)
p3 <- (sum(abs(valor3) > abs(dif_diaguita_aimara)) + 1) / (rep+1)

cat("Valor p para las diferencias de medias entre quechua diaguita:",p1)
cat("Valor p para las diferencias de medias entre quechua aimara:",p2)
cat("Valor p para las diferencias de medias entre diaguita aimara:",p3)

#Se demuestra que el p-valor más bajo entre las muestras es mayor que el nivel de significancia (p-valor>alpha)
#Se determina con un 95% de confianza que no existe diferencia entre las medias de ingresos entre personas
#con raices indigenas. (Se falla en rechazar h0 en favor a ha)



