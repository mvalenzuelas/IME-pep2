library(ggpubr)
library(ggplot2)
library(tidyverse)
library(ez)
library(simpleboot)

datos <- read.csv2("EP11 Datos.csv")

# Contexto: Se desea realizar una aplicaci?n de citas y se 
# requiere evaluar los grupos a los que ir? enfocada la app.
# Por ello se desea estudiar el promedio de edad de hombres y mujeres 
# que est?n solteros (as) en el rango de edad entre 20 y 40 a?os.


adulto_joven <-  datos %>% filter(edad > 20 & edad < 40)

set.seed(198)
n <- 350

adulto_joven <- adulto_joven %>% sample_n(n) %>% select(sexo, ecivil, edad)

solteros <- adulto_joven %>% filter(ecivil == "Soltero(a)" & sexo == "Hombre")
solteros <- solteros$edad
solteras <- adulto_joven %>% filter(ecivil == "Soltero(a)" & sexo == "Mujer")
solteras <- solteras$edad

#En promedio la edad de los hombres solteros y mujeres solteras es igual.
#H_0: el promedio de la edad de los hombres solteros y mujeres solteras es igual.
#H_A: el promedio de la edad de los hombres solteros y mujeres solteras es distinto.

# Usando el mÃ©todo de Monte Carlo, establecemos primero las repeticiones
R <- 1999

# Definimos una funcion que obtenga una permutacion en cada repeticiÃ³n

obtiene_permutacion <- function(i, muestra_1 , muestra_2) {
  n_1 <- length( muestra_1)
  combinada <- c( muestra_1 , muestra_2)
  n <- length ( combinada )
  permutacion <- sample ( combinada , n , replace = FALSE )
  nueva_1 <- permutacion[1:n_1]
  nueva_2 <- permutacion[(n_1+1):n]
  return(list(nueva_1 , nueva_2))
}

# Definimos la función para tomar la diferencia entre los estadísticos a calcular
calcular_diferencia <- function(muestras , FUN) {
  muestra_1 <- muestras [[1]]
  muestra_2 <- muestras [[2]]
  diferencia <- FUN(muestra_1) - FUN(muestra_2)
  return(diferencia )
}

# Realizamos el procedimiento de Monte Carlo

observado <- calcular_diferencia(list(solteros, solteras), mean) # Queremos analizar la media
n_1 <- length(solteros)
permutaciones <- lapply(1:R, 
                        obtiene_permutacion,
                        solteros,
                        solteras)

graficar_distribucion <- function (distribucion) {
  observaciones <- data.frame(distribucion)
  histograma <- gghistogram(observaciones, x = "distribucion",
                            xlab = "EstadÃ­stico de interÃ©s",
                            ylab = "Frecuencia", 
                            bins = 30)
  
  qq <- ggqqplot(observaciones, x = "distribucion")
  
  # Crear una única figura con todos los gráficos de dispersiÃ³n .
  figura <- ggarrange(histograma, qq, ncol = 2, nrow = 1)
  print(figura )
}

distribucion <- sapply(permutaciones, calcular_diferencia, mean) # FUN = mean
graficar_distribucion(distribucion)

# Usamos la fórmula para obtener el p-value
numerador <- sum(distribucion > observado) + 1
denominador <- R + 1
p_val <- numerador/denominador
print(p_val)

# Con alpha = 0.05 (nivel de significaciÃ³n), la simulaciÃ³n 
# de Monte Carlo nos entrega un p-valor = 0.025, es decir,
# la mitad del nivel de significaciÃ³n, es decir,
# p-valor < alpha, y concluimos que, en promedio, 
# la edad de hombres y mujeres solteros es diferente.

# 2.
# Ej. Idea: dependiendo de cuanto ganan influye en si van a pie, en vehiculo o en transporte publico (2)

# H_0: El ingreso promedio de los hogares es igual para cada grupo que utiliza distintos 
# tipos de transporte para movilizarse.
# H_0: El ingreso promedio de los hogares es diferente para cada grupo que utiliza distintos 
# tipos de transporte para movilizarse.


set.seed(891)
n2 <- 450

datos2 <- datos %>% sample_n(n2) %>% select(region, ytotcorh, o25c)

# ezAnova type 2 o 3 
# aov usa tipo 1

motorizado <- datos2 %>% filter(o25c == "Vehículo motorizado particular (auto, camioneta, motocicleta") 
motorizado <- motorizado$ytotcorh

publico <- datos2 %>% filter(o25c == "Transporte público (bus, microbús, metro, taxi colectivo, et") 
publico <- publico$ytotcorh


pie <- datos2 %>% filter(o25c == "A pie") 
pie <- pie$ytotcorh

# Usaremos el procedimiento para bootstrapping como es pedido:

obtiene_permutacion_2 <- function(i, muestra){
  muestra1 <- sample(muestra, length(muestra), replace = TRUE)
}

calcular_media_muestra <- function(muestra){
  return(mean(muestra))
}

B <- 2000
motorizado_1 <- lapply(1:B, obtiene_permutacion_2, motorizado)
publico_1 <- lapply(1:B, obtiene_permutacion_2, publico)
pie_1 <- lapply(1:B, obtiene_permutacion_2, pie)

# Calculamos las distribuciones de las medias de cada una de las muestras
# generadas mediante bootstrapping

motorizado_boot <- sapply(motorizado_1, calcular_media_muestra)
publico_boot <- sapply(publico_1, calcular_media_muestra)
pie_boot <- sapply(pie_1, calcular_media_muestra)

# Ahora podemos trabajar con estas distribuciones bootstrap
# ya que cumplen con los requisitos para utilizar una
# prueba ANOVA para comprar 3 grupos independientes.

datos_formato <- data.frame(motorizado_boot, publico_boot, pie_boot)

datos_formato <- datos_formato %>% pivot_longer(c("motorizado_boot", "publico_boot", "pie_boot"),
                                                names_to = "transporte",
                                                values_to = "ingreso")

datos_formato[['transporte']] <- factor(datos_formato[['transporte']])
datos_formato[['instancia']] <- factor(1:nrow(datos_formato))


prueba_anova <- ezANOVA(
  data = datos_formato,
  dv = ingreso,
  between = transporte,
  wid = instancia,
  return_aov = TRUE,
)

print(prueba_anova$ANOVA)

post <- TukeyHSD(prueba_anova$aov)
print(post)

# Tras realizar ANOVA, observamos que el valor del p-value es
# directamente 0. Esto, debido a que el estadístico F
# es muy grande, y claramente la probabilidad en una dist. de
# Fisher con un valor tan grande, entregará un p-valor muy pequeño
# al estar en la cola superior con menos área.
# Además, mirando un box-plot de las distribuciones generadas
# con bootstrapping, las cajas no se sobreponen en ninguna distribución
# con esto, sumado al p-valor menor a alpha, se concluye que
# los ingresos de los diferentes grupos que utilizan distintos medios
# de transporte son distintos (es decir, se rechaza H_0 en favor de H_A). Es más, realizando el análisis post-hoc
# correspondiente, todos los ingresos promedio entre los grupos, difieren.

# Ahora, es pertinente realizar una prueba que tenga un valor nulo
# definido (distinto de 0) para entregar datos útiles a la ciudadanía.


g_grupos <- ggboxplot(datos_formato, x = "transporte", y = "ingreso")
print(g_grupos)
