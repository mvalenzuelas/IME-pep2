library(dplyr)
library(ggpubr)

#Lectura de los datos desde el archivo csv entregado.
basename <- "EP11 Datos.csv"
file <- file.path("C:/Users/adolf/Desktop/Cosas/IME/IME_equipo_4", basename)
datos <- read.csv2(file = file)

#====================
#==== Pregunta 1 ====
#====================

#Propongan una pregunta de investigación original, que involucre la comparación de las medias de dos grupos 
#independientes (más abajo se dan unos ejemplos). Fijando una semilla propia, seleccionen una muestra 
#aleatoria de hogares (250 < n < 500) y respondan la pregunta propuesta utilizando una simulación Monte Carlo.

#La pregunta de investigacion propuesta para la primera parte del trabajo es la de analizar si, en promedio,
#la cantidad de hijos nacidos vivos es igual en las mujeres residentes en la region del Maule y las 
#mujeres residentes en la region de Los Lagos.

#Hipotesis:
#H0: En promedio, la cantidad de hijos nacidos vivos es igual en las mujeres residentes en la region del Maule y 
#    en las mujeres residentes en la region de Los Lagos.
#HA: En promedio, la cantidad de hijos nacidos vivos es distinta en las mujeres residentes en la region del Maule
#    y en las mujeres residentes en la region de Los Lagos.

#Formula matematica:
#H0: mu.maule - mu.losLagos = 0
#HA: mu.maule - mu.losLagos != 0

#Se fija el nivel de significacion para este analisis.
alfa <- 0.05

#Se fija la semilla seleccionada al azar para esta prueba
set.seed(420)

#Se obtienen las observaciones correspodientes a mujeres de las dos regiones a estudiar desde el dataframe
#original.
mujeresMaule <- datos %>% filter(region == "Región del Maule" & sexo == "Mujer")
mujeresLagos <- datos %>% filter(region == "Región de Los Lagos" & sexo == "Mujer")

#Se obtiene una muestra aleatoria de 150 observaciones para region.
muestraMaule <- sample_n(mujeresMaule, 150)
muestraLagos <- sample_n(mujeresLagos, 150)

#Se obtiene la columna que contiene la cantidad de hijos nacidos vivos de cada observacion.
hijosMaule <- muestraMaule$s4
hijosLagos <- muestraLagos$s4

#Se fija el numero de repeticiones en 6000.
R <- 6000

#A continuacion, se encuentran las funciones entregadas por el profesor en los scripts de la lectura 
#correspondiente. 

# Funcion para obtener una permutacion.
# Argumentos:
# - i: iterador (para llamadas posteriores).
# - muestra_1, muestra_2: muestras.
# Valor:
# - lista con las muestras resultantes tras la permutacion.
obtiene_permutacion <- function(i, muestra_1, muestra_2) {
  n_1 <- length(muestra_1)
  combinada <- c(muestra_1, muestra_2)
  n <- length(combinada)
  permutacion <- sample(combinada, n, replace = FALSE)
  nueva_1 <- permutacion[1:n_1]
  nueva_2 <- permutacion[(n_1+1):n]
  return(list(nueva_1, nueva_2))
}

# Funcion para calcular la diferencia de un estadistico de interes entre las
# dos muestras.
# Argumentos:
# - muestras: lista con las muestras.
# - FUN: nombre de la funcion que calcula el estadistico de interes.
# Valor:
# - diferencia de un estadistico para dos muestras.
calcular_diferencia <- function(muestras, FUN) {
  muestra_1 <- muestras[[1]]
  muestra_2 <- muestras[[2]]
  diferencia <- FUN(muestra_1) - FUN(muestra_2)
  return(diferencia)
}

# Funcion para calcular el valor p.
# Argumentos:
# - distribucion: distribucion nula del estadistico de interes.
# - valor_observado: valor del estadistico de interes para las muestras
#   originales.
# - repeticiones: cantidad de permutaciones a realizar.
# - alternative: tipo de hipotesis alternativa. "two.sided" para
#   hipotesis bilateral, "greater" o "less" para hipotesis unilaterales.
# Valor:
# - el valorp calculado.
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

# Funcion para graficar una distribucion.
# Argumentos:
# - distribucion: distribucion nula del estadistico de interes.
# - ...: otros argumentos a ser entregados a gghistogram y ggqqplot.
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

# Funcion para hacer la prueba de permutaciones.
# Argumentos:
# - muestra_1, muestra_2: vectores numericos con las muestras a comparar.
# - repeticiones: cantidad de permutaciones a realizar.
# - FUN: funcion del estadistico E para el que se calcula la diferencia.
# - alternative: tipo de hipotesis alternativa. "two.sided" para
#   hipotesis bilateral, "greater" o "less" para hipotesis unilaterales.
# - plot: si es TRUE, construye el grÃ¡fico de la distribucion generada.
# - ...: otros argumentos a ser entregados a graficar_distribucion.
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



#Se hace la prueba de permutaciones para comparar una variable continua en dos muestras independientes 
#usando la media.
contrastar_hipotesis_permutaciones(hijosMaule, hijosLagos, repeticiones = R, FUN = mean,
                                   alternative = "two.sided", plot = TRUE,
                                   color = "blue", fill = "blue")

#====================
#==== Respuesta 1 ===
#====================

#De la prueba realizada anteriormente se puede observar en el grafico que la muestra sigue una distribución normal,
#y no se observan datos atipicos, tambien se obtiene un p-valor igual a 0.6443926, el cual es mayor al nivel de
#significacion considerado para la prueba, por lo que se falla al rechazar la hipotesis nula, pudiendo concluir
#con 95% de confianza que, en promedio, la cantidad de hijos nacidos vivos es es igual en las mujeres residentes
#en la region del Maule y en las mujeres residentes en la region de Los Lagos.

#----------------------------------------------------------------------------------------------------------------------

#====================
#==== Pregunta 2 ====
#====================

# Propongan una pregunta de investigación original, que involucre la comparación de las medias de más de dos grupos 
# independientes (más abajo se dan unos ejemplos). Fijando una semilla distinta a la anterior, seleccionen una
# muestra aleatoria de hogares (400 < n < 600) y respondan la pregunta propuesta utilizando bootstrapping. Solo 
# por ejercicio académico, aplique un análisis post-hoc con bootstrapping aunque este no sea necesario.

#La pregunta de investigacion planteada es la de analizar si, en promedio, los hombres de las regiones de Atacama,
#Metropolitana y Los Lagos trabajan la misma cantidad de horas a la semana.

#Hipotesis:
#H0: en promedio, los hombres de las regiones de Atacama, Metropolitana y de Los Lagos trabajan la misma cantidad de 
#    horas a la semana.
#HA: en promedio, los hombres de al menos una de las regiones estudiadas trabajan una cantidad distinta de horas 
#    respecto a las demas regiones.

#Se fija la nueva semilla para la obtencion de las muestras.
set.seed(298)

#Se filtran las observaciones de interes para el estudio desde el archivo.
hombresAtacama <- datos %>% filter(region == "Región de Atacama" & sexo == "Hombre" & o10 != "N/A")
hombresMetropolitana <- datos %>% filter(region == "Región Metropolitana de Santiago" & sexo == "Hombre" & o10 != "N/A")
hombresLosLagos <- datos %>% filter(region == "Región de Los Lagos" & sexo == "Hombre" & o10 != "N/A")

#Se obtiene una muestra aleatoria de cada grupo de interes.
muestraAtacama <- sample_n(hombresAtacama, 150)
muestraMetropolitana <- sample_n(hombresMetropolitana, 250)
muestraLosLagos <- sample_n(hombresLosLagos, 200)

#Se realiza una prueba de Shapiro-Wilk para comprobar si las muestras siguen una distribucion cercana a la normal.
print(shapiro.test(muestraAtacama$o10))
print(shapiro.test(muestraMetropolitana$o10))
print(shapiro.test(muestraLosLagos$o10))

#De las pruebas de Shapiro-Wilk, se concluye que los 3 grupos no siguen una distribucin normal.
