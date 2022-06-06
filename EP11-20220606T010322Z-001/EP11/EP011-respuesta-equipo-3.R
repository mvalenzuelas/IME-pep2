library("dplyr")   # Para el %>%
library ("ggpubr") # Histograma

basename <- "EP11 Datos.csv"
file <- file.path("C:/Users/Dell PC/Desktop/IME-2022/Actividades/act11",basename)
datos <- read.csv2(file = file)

# ******************************************** PREGUNTA 1 ********************************#
# Propongan una pregunta de investigación original, que involucre la comparación de 
# las medias de dos grupos independientes (más abajo se dan unos ejemplos). Fijando 
# una semilla propia, seleccionen una muestra aleatoria de hogares (250 < n < 500) y 
# respondan la pregunta propuesta utilizando una simulación Monte Carlo.
# ****************************************************************************************#

# Se define una semilla
set.seed(19)

# Función para calcular la diferencia entre dos estadísticos
# Argumentos:
# - muestra_1, muestra_2: vectores numéricos con las muestras a comparar
# - FUN: función del estadístico E para el que se calcula la diferencia
# Valor:
# - diferencia E_1 - E_2
calcular_diferencia <- function(muestra_1, muestra_2, FUN){
  diferencia <- FUN(muestra_1) - FUN(muestra_2)
  return(diferencia)
}

# Función para hacer una permutación y calcular el estadístico
# de interés
# Argumentos:
# - muestra_1, muestra_2: vectores numéricos con las muestras a comparar
# - FUN: función del estadístico E para el que se calcula la diferencia
# Valor:
# - diferencia E_1 - E_2
permutar <- function(muestra_1, muestra_2, FUN){
  n_1 <- length(muestra_1)
  n_2 <- length(muestra_2)
  
  # Hacer la permutación
  permutacion <- sample(c(muestra_1, muestra_2), size = n_1 + n_2,
                        replace = FALSE)
  
  # Asignar elementos a los dos grupos
  permutacion_1 <- permutacion[1 : n_1]
  permutacion_2 <- permutacion[n_1 + 1 : n_2]
  
  # Calcular y devolver la diferencia de medias
  return(calcular_diferencia(permutacion_1, permutacion_2, FUN))
}

# Función para calcular el valor p
# Argumentos:
# - distribución: distribución nula del estadístico de interés
# - valor observado: valor del estadístico de interés para las muestras originales
# - repeticiones: cantidad de permutaciones a realizar
# - alternative: tipo de hipótesis alternativa ("two.sided" para
#   hipótesis bilateral, "greater" o "less" para hipótesis unilaterales)
# Valor:
# - el valor p calculado
calcular_valor_p <- function(distribucion, valor_observado, repeticiones, alternative){
  if(alternative == "two.sided"){
    numerador <- sum(abs(distribucion) > abs(valor_observado)) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  else if(alternative == "greater"){
    numerador <- sum(distribucion > valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  else{
    numerador <- sum(distribucion < valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  return(valor_p)
}

# Función para graficar una distribución
# Argumentos:
# - distribución: distribución nula el estadístico de interés
# - ...: otros argumentos a ser entregados a gghistogram y ggqqplot
graficar_distribucion <- function(distribucion, ...){
  observaciones <- data.frame(distribucion)
  
  histograma <- gghistogram(observaciones, x = "distribucion",
                            xlab = "Estadistico de interes",
                            ylab = "Frecuencia", ...)
  qq <- ggqqplot(observaciones, x = "distribucion", ...)
  
  # Crear una unica figura con todos losgraficos de dispersion
  figura <- ggarrange(histograma, qq, ncol = 2, nrow = 1)
  print(figura)
}

# Función para hacer la prueba de permutaciones
# Argumentos :
# - muestra_1, muestra_2: vectores numéricos con las muestras a comparar
# - repeticiones: cantidad de permutaciones a realizar
# - FUN: función del estadístico E para el que se calcula la diferencia
# - alternative: tipo de hipótesis alternativa ("two.sided" para
#   hipótesis bilateral, "greater" o "less" para hipótesis unilaterales)
# - plot: si es TRUE, construye el grafico de la distribución generada
# - ...: otros argumentos a ser entregados a graficar_distribucion
contrastar_hipotesis_permutaciones <- function(muestra_1, muestra_2,
                                               repeticiones, FUN,
                                               alternative, plot, ...){
  cat("Prueba de permutaciones\n\n")
  cat("Hipotesis alternativa:", alternative, "\n")
  observado <- calcular_diferencia(muestra_1, muestra_2, FUN)
  cat("Valor observado:", observado, "\n")
  
  distribucion <- rep(NA, repeticiones)
  
  for(i in 1:repeticiones){
    distribucion[i] <- permutar(muestra_1, muestra_2, FUN)
  }
  
  if(plot){
    graficar_distribucion(distribucion, ...)
  }
  
  valor_p <- calcular_valor_p(distribucion, observado, repeticiones,
                              "two.sided")
  
  cat("Valor p:", valor_p, "\n\n")
}

# Se define una pregunta:

# ¿En promedio, existe una diferencia significativa en el ingreso per cápita de las personas de la 
# Región Metropolitana de Santiago y la Región de Tarapacá?

# Se formula la Hipótesis

# H0: No existe una diferencia significativa en el ingreso per cápita de los habitantes de la región
#     Metropolitana y Tarapacá

# HA: Si existe una diferencia significativa en el ingreso per cápita de los habitantes de la región
#     Metropolitana y Tarapacá

# Para realizar la prueba, se realizará una simulación de Monte Carlo con un nivel de
# significación de 0.05, utilizando 3999 permutaciones.

# Se obtienen los ingresos y el número de personas en el hogar de los habitantes
# en todas las regiones.

regiones <- datos%>% select(region,numper,ytotcorh)

# se obtiene los ingresos per cápita de la Región Metropolitana
datosRM  <- regiones %>% filter(region == "Región Metropolitana de Santiago",numper != "NA")
datosRM$ytotcorh <- datosRM$ytotcorh / as.integer(datosRM$numper)
datosRM <- datosRM[["ytotcorh"]]

#se obtiene los ingresos per cápita de la Región de Tarapacá
datosTarapaca  <- regiones %>% filter(region == "Región de Tarapacá",numper != "NA")
datosTarapaca$ytotcorh <- datosTarapaca$ytotcorh / as.integer(datosTarapaca$numper)
datosTarapaca <- datosTarapaca[["ytotcorh"]]

# Hacer pruebas de permutaciones para la media
R = 3999
alfa = 0.05


contrastar_hipotesis_permutaciones(datosRM, datosTarapaca, repeticiones = R, FUN = mean,
                                   alternative = "two.sided", plot = TRUE, color = "blue", fill = "blue")
# p value = 0.000990099 

# En conclusión, podemos decir que Considerando un nivel de significación alfa= 0,05 > 0.000990099, 
# se rechaza la hipótesis nula en favor de la hipótesis alternativa. 
# En consecuencia, concluimos con 95 % de confianza que si existe una diferencia significativa en el 
# ingreso per cápita de los habitantes de la región Metropolitana y Tarapacá

# ******************************************** PREGUNTA 2 ********************************#
# Propongan una pregunta de investigación original, que involucre la comparación de 
# las medias de más de dos grupos independientes (más abajo se dan unos ejemplos). 
# Fijando una semilla distinta a la anterior, seleccionen una muestra aleatoria de 
# hogares (400 < n < 600) y respondan la pregunta propuesta utilizando bootstrapping. 
# Solo por ejercicio académico, aplique un análisis post-hoc con bootstrapping aunque este 
# no sea necesario.
# ****************************************************************************************#

library(ez)
# Función que remuestreo para 3 muestras
# Argumentos:
# muestra: muestra a remuestrear
# Retorno:
# matriz con valores remuestreados
my_boot <- function(i, muestra1, muestra2, muestra3){
  #se toma una muestra con reemplazo para cada grupo
  muestra1_copia <- sample(1:nrow(muestra1), replace = TRUE) 
  muestra2_copia <- sample(1:nrow(muestra2), replace = TRUE)
  muestra3_copia <- sample(1:nrow(muestra3), replace = TRUE)
  rbind(muestra1[muestra1_copia,], muestra2[muestra2_copia,], muestra3[muestra3_copia,])
}

# Función que calcula el estadístico F
# Argumentos:
# f: dataframe
# Valor:
# estadístico F
my_F <- function(frame){
  # Obtener valor observado, correspondiente al estadístico F entregado
  # por ANOVA para la muestra original
  anova <- ezANOVA(frame, dv = ingreso, between = region, 
                   wid = instancia, return_aov = FALSE)
  invisible(anova$ANOVA$F)
}


# Se define una pregunta:
# ¿Existe una diferencia significativa entre el ingreso per cápita de las personas en la Región de Antofagasta,
# Región de Valparaíso y la Región de Los Lagos?

# Definimos nuestras hipótesis
# H0: No existe una diferencia significativa entre el ingreso per cápita de las personas en 
#     la Región de Antofagasta, Valparaíso y Los Lagos 
# HA: Si existe una diferencia significativa entre el ingreso per cápita de las personas en 
#     al menos una de las Regiones de Antofagasta, Valparaíso y Los Lagos

datos2 <- datos %>% filter(region == "Región de Antofagasta" | 
                             region == "Región de Atacama" |
                             region == "Región de Los Lagos" ,numper != "NA")

# Se establece una semilla
set.seed(341)

# Se obtiene la muestra de tamaño 500
tamano <- 500
muestra <- datos2[sample(nrow(datos2), tamano),]

# Se construye el dataframe con los datos en investigación
ytotcorh <- as.integer(muestra[["ytotcorh"]])
integrantes <- as.integer(muestra[["numper"]])
ingreso <- as.integer(ytotcorh/integrantes)
region <- factor(muestra[["region"]])
instancia <- factor(1:tamano)

datos3 <- data.frame(instancia, region, ingreso)

# se obtiene los ingresos per cápita de la Región Atacama
datosRAT  <- datos3 %>% filter(region == "Región de Atacama")
datosRAT_ingreso <- (datosRAT[["ingreso"]])

# se obtiene los ingresos per cápita de la Región Antofagasta
datosRA  <- datos3 %>% filter(region == "Región de Antofagasta")
datosRA_ingreso <- datosRA[["ingreso"]]

# se obtiene los ingresos per cápita de la Región de Los Lagos
datosRL  <- datos3 %>% filter(region == "Región de Los Lagos")
datosRL_ingreso <- datosRL[["ingreso"]]

print(shapiro.test(datosRAT_ingreso))
# p = 5.339e-11
print(shapiro.test(datosRA_ingreso))
# p = 2.2e-16
print(shapiro.test(datosRL_ingreso))
# p = 2.2e-16

# Como se puede ver en las pruebas de Shapiro, las muestras para las tres regiones no se 
# comportan de manera normal ya que el valor p es muy bajo, por lo que se 
# define un valor alfa muy exigente igual a 0.01 con el fin de ser muy cautelosos 
# al realizar la prueba. Para esta prueba, se procederá a utilizar 
# el método de Bootstrapping para evaluar tres muestras independientes.

# Se define el valor del alfa
alfa <- 0.01

# Se obtiene el estadístico F original
anova_original <- ezANOVA(datos3, dv = ingreso, between = region, 
                          wid = instancia, return_aov = FALSE)
print(anova_original)

# Se llama a la función my_boot
R <- 2999 
distribuciones  <- lapply(1:R, my_boot, datosRAT,  datosRA, datosRL)

# Se calculan los valores F suprimiendo los mensajes de Warning
suppressMessages(suppressWarnings(valores_F <- sapply(distribuciones, my_F)))

p <- calcular_valor_p(valores_F, anova_original$ANOVA$F, R, "two.sided")
cat("el valor p es: ", p )
# p = 0.201

# CONCLUSIÓN:
# Al finalizar la prueba, se obtiene un p igual a 0.201 > alfa,
# se falla al rechazar la hipótesis nula en favor de la hipótesis alternativa. 
# En consecuencia, concluimos con 95 % de confianza que no existe una diferencia 
# significativa entre el ingreso per cápita de las personas en 
# la Región de Antofagasta, Valparaíso y Los Lagos.

# Para verificar el resultado obtenido, se realiza una prueba post-hoc.

# Análisis post -hoc.

# Función para calcular la diferencia de medias para el ingreso per cápita de dos regiones
# Argumentos:
# datos: en formato largo
# reg1: ingreso per cápita región 1
# reg2: ingreso per cápita región 2
# Valor:
# Diferencia de medias
media_diferencias <- function(datos , reg1, reg2) {
  i1 <- datos[["region"]] == reg1
  i2 <- datos[["region"]] == reg2
  media <- mean(datos[["ingreso"]][i1]) - mean(datos[["ingreso"]][i2])
  return(media)
}

# Función para generar la diferencia de medias para las distribuciones encontradas
# Argumentos:
# dist: distribuciones encontradas
# ing1: ingreso per cápita región 1
# ing2: ingreso per cápita región 2
# Valor:
# Diferencia de medias
distribucion_diferencias <- function(dist, ing1, ing2){
  B <- length(dist)
  distribucion <- c()
  
  for (i in 1:B) {
    datos <- as.data.frame(dist[i])
    
    diferencia <- media_diferencias (datos , ing1, ing2)
    distribucion <- c( distribucion , diferencia )
  }
  
  return(distribucion)
}

# Se guardan las regiones en variables 
AT <- "Región de Atacama"
A <- "Región de Antofagasta"
L <- "Región de Los Lagos"

# Calcular diferencias observadas en la muestra original.
dif1 <- media_diferencias(datos3, AT , A)
dif2 <- media_diferencias(datos3, AT , L)
dif3 <- media_diferencias(datos3, A , L)

# Generar diferencias por cada una de las distribuciones encontradas
dif_dist_1 <- distribucion_diferencias(distribuciones , AT , A )
dif_dist_2 <- distribucion_diferencias(distribuciones , AT , L )
dif_dist_3 <- distribucion_diferencias(distribuciones , A , L )


# Obtener valores p
num1 <- sum(abs(dif_dist_1) > abs(dif1)) + 1
den1 <- R + 1
p_atacama_antofagasta <- num1 / den1
# p = 0.4836667

num2 <- sum(abs(dif_dist_2) > abs(dif2)) + 1  
den2 <- R + 1
p_atacama_lagos <- num2 / den2
# p = 0.8036667

num3 <- sum(abs(dif_dist_3) > abs(dif3)) + 1 
den3 <- R + 1
p_antofagasta_lagos <- num3 / den3
# p = 0.5006667

# En conclusión, como todos los valores p > alfa, no existe diferencia significativa para el ingreso
# per cápita entre las personas de la región de Atacama-Antofagasta (0.4836667), Atacama-Lagos (0.8036667)
# y Antofagasta-Lagos (0.5006667), tal como se concluyó anteriormente.