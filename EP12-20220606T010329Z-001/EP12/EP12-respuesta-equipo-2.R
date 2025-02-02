library(ggpubr)
library(rcompanion)
library(WRS2)
library(tidyverse)

# 1. En el trabajo de t�tulo de un estudiante del DIINF se reportan los siguientes tiempos de ejecuci�n (en milisegundos)
#medidos para dos versiones de un algoritmo gen�tico para resolver instancias del problema del vendedor viajero disponibles en repositorios p�blicos.
#�Es uno de los algoritmos m�s r�pido que el otro? Aplique una transformaci�n de datos adecuada para responder.



# Datos para preguntas '2 y '3

datos_anterior <- read.csv2("EP11 Datos.csv")


# Transformar tabla a data.frame para que pueda ser procesada en R.

instanciaA <- c(129, 109, 28, 178, 74, 16, 87, 108, 149, 78)
tiempoA <- c(1510394, 402929, 885722, 4428151,48667,834565, 70599, 783108,210041, 37449)
algoritmoA <- data.frame(instanciaA, tiempoA)

instanciaB <- c(134, 193, 10, 88, 142, 86, 36, 190, 163, 33)
tiempoB <- c(1252837, 2196277, 120276, 4629726, 5743260, 6701654, 6568968, 180141, 6684497, 35974)
algoritmoB <- data.frame(instanciaB, tiempoB)

algoritmos <- data.frame(tiempoA, tiempoB)

g <- gghistogram(algoritmoA, x = "instanciaA")
print(g)

# Transformar datos seg�n Tukey
transformarDatos <- function(datos){
  lambda <- transformTukey(datos, start = -1, end = 1, 
                             int = 0.1, plotit = TRUE, 
                             returnLambda = TRUE)
  
  datosTukey <- transformTukey(datos, start = -1, end = 1, 
                                int = 0.1, plotit = TRUE, 
                                returnLambda = FALSE)
  
  return(list(lambda, datosTukey))
}

# Obtener datos transformados
transfA <- transformarDatos(algoritmoA$tiempoA)
lambda_A <- transfA[1]
datosTukeyA <- transfA[2]

transfB <- transformarDatos(algoritmoB$tiempoB)
lambda_B <- transfB[1]
datosTukeyB <- transfB[2]

# Unificar datos transformados en una sola tabla.
tiemposTukey <- data.frame(datosTukeyA, datosTukeyB)

# Realizar la prueba T de Student
# Se aplica con un nivel de significaci�n de 0.05

# Formulaci�n de hip�tesis
# H0 = Los tiempos de ejecuci�n del algoritmo A, son iguales a los tiempos de ejecuci�n del algoritmo B (ninguno es mas r�pido que otro)
# H1 = Los tiempos de ejecuci�n del algoritmo A, son diferentes a los tiempos de ejecuci�n del algoritmo B (Uno es m�s r�pido que el otro)

# H0 = tA = tB
# h1 = tA != tB

# Se corrobora supuesto de normalidad mediante Shapiro Wilk
normalidadA = shapiro.test(datosTukeyA[[1]])
normalidadB = shapiro.test(datosTukeyB[[1]])
# Las observaciones provienen de una distribuci�n cercana a la normal (p>alfa)

# Como las muestras son independientes y aleatorias, se puede proceder a aplicar la prueba.

alfa <- 0.05
prueba <- t.test(x = datosTukeyA[[1]], 
                 y = datosTukeyB[[1]],
                 paired = FALSE,
                 alternative = "greater",
                 mu = 0,
                 conf.level = 1 - alfa)

print(prueba$p.value)

# Como p = 0.9996, y p > alfa, se falla al rechazar la hip�tesis nula, por lo que, con un 
# 95% de confianza, se puede asegurar que ning�n algoritmo es m�s rapido que otro (los tiempos de ejecuci�n son iguales)

#####################
####################

# 2.

# Contexto: Se desea realizar una aplicaci�n de citas y se 
# requiere evaluar los grupos a los que ir� enfocada la app.
# Por ello se desea estudiar el promedio de edad de hombres y mujeres 
# que est�n solteros(as) en el rango de edad entre 20 y 40 a�os.

# Hip�tesis:

# H_0: En promedio, la edad de las mujeres y hombres solteros es la misma.
# H_A: La edad promedio de hombres y mujeres solteras difieren o son distintas.

# H_0: u_M = u_F, donde M : male, F: female
# H_A: u_M != u_F

adulto_joven <-  datos_anterior %>% filter(edad > 20 & edad < 40)

set.seed(198) # Semilla para repetibilidad de estudio
n <- 350

adulto_joven <- adulto_joven %>% sample_n(n) %>% select(sexo, ecivil, edad)
solteros <- adulto_joven %>% filter(ecivil == "Soltero(a)" & sexo == "Hombre")
solteros_edad <- solteros$edad
solteras <- adulto_joven %>% filter(ecivil == "Soltero(a)" & sexo == "Mujer")
solteras_edad <- solteras$edad

# Ajustamos datos para la prueba a realizar...

edad <- c(solteros_edad, solteras_edad)
genero <- c(rep("M", length(solteros_edad)), rep("F", length(solteras_edad)))
datos_df <- data.frame(edad, genero)

# Veamos la normalidad de los datos, para saber qu� m�todo debemos utilizar
# o si directamente podemos utilizar una prueba param�trica t�pica:

# Comprobar normalidad
g_mf <- ggqqplot(datos_df, x = "edad", facet.by = "genero" ,
               palette = c("red", "blue") , color = "genero")
print(g_mf)

# Como en el gr�fico Q-Q notamos que muchos puntos escapan
# del margen de normalidad (generado por la funci�n) por lo
# que debemos trabajar los datos problem�ticos, al no poder
# utilizar pruebas t�picas.

# Como tenemos grupos independientes (mujeres y hombres) con muestras
# de datos problem�ticos, utilizaremos la prueba
# de Yuen para dos muestras independientes.

alfa2 <- 0.05

# Vemos c�mo son los gr�ficos si realiz�ramos una poda:

gamma <- 0.2
n_a <- length(solteros_edad)
n_b <- length (solteras_edad)
poda_a <- n_a*gamma
poda_b <- n_b*gamma
a_truncada <- solteros_edad[poda_a:(n_a-poda_a)]
b_truncada <- solteras_edad[poda_b:(n_b-poda_b)]
edad <- c(a_truncada, b_truncada)
genero <- c(rep("M", length(a_truncada)), rep("F", length(b_truncada)))
datos_truncados <- data.frame(edad, genero)
g <- ggqqplot(datos_truncados, x="edad",facet.by = "genero",
              palette = c("red", "blue") , color = "genero" )
print(g) 

# Vemos que al realizar la poda, el margen de normalidad para los puntos
# se hace m�s grande y tambi�n los datos calzan mejor a esta franja, lo que nos dice
# que un procedimiento con la prueba de Yuen entregar� buenos resultados
# ya que internamente utiliza la media truncada y winsorizada.

# Como funciona la poda, utilizamos la prueba de Yuen, con m�s seguridad.
# Aplicar prueba de Yuen:
prueba_yuen <- yuen(edad ~ genero, data=datos_df, tr=gamma)
print(prueba_yuen)


# Tras realizar la prueba, tenemos que nuestro p = 0.089 y es mayor a alfa.
# Por ello, diremos, con un 95% de confianza, que la edad promedio de
# hombres y mujeres solteras es igual, o bien, fallamos en rechazar
# la hip�tesis nula.

# Con esta informaci�n, claramente los desarrolladores de la supuesta
# aplicaci�n de citas pueden saber correctamente a qu� usuarios dirigir 
# su publicidad.

###############
###############

# 3

# Contexto e hip�tesis:

# Dependiendo de cu�nto ganan (personas de chile) influye en si van a pie, en vehiculo 
# o en transporte publico en su movilizaci�n en el d�a a d�a.

# H_0: El ingreso promedio de los hogares es igual para cada grupo que utiliza distintos 
# tipos de transporte para movilizarse.
# H_A: El ingreso promedio de los hogares es diferente para cada grupo que utiliza distintos 
# tipos de transporte para movilizarse.

# Obtenci�n de datos para cada grupo - es el mismo c�digo que para el EP 11.

set.seed(891)
n2 <- 450

datos2 <- datos_anterior %>% sample_n(n2) %>% select(region, ytotcorh, o25c)


motorizado <- datos2 %>% filter(o25c == "Veh�culo motorizado particular (auto, camioneta, motocicleta") 
motorizado <- motorizado$ytotcorh

publico <- datos2 %>% filter(o25c == "Transporte p�blico (bus, microb�s, metro, taxi colectivo, et") 
publico <- publico$ytotcorh

pie <- datos2 %>% filter(o25c == "A pie") 
pie <- pie$ytotcorh

# Veamos la distribuci�n de los ingresos, en forma de histogramas:

g_01 <- gghistogram(motorizado, bins = 15, xlab = "Ingreso (grupo motorizado)")
g_02 <- gghistogram(publico, bins = 15, xlab = "Ingreso (grupo trans. p�b.)")
g_03 <- gghistogram(pie, bins = 20, xlab = "Ingreso (grupo 'a pie')")

histogramas_ingresos <- ggarrange(g_01, g_02, g_03,
                                  nrow = 1, ncol = 3)

# Vemos que la distribuci�n en la cola derecha tiene una larga separaci�n
# aumentando incre�blemente el ingreso, y con una frecuencia m�nima (entre 0 y 5)
# Por otro lado, vemos en la cola izquierda el mismo caso, solo que mejor agrupado
# a las barras con m�s frecuencia, sin embargo, son datos muy alejados de la
# distribuci�n que tiene la mayor�a de la muestra cerca de la media.
# Por ello, utilizaremos un m�todo robusto, pues estamos en presencia
# de datos problem�ticos y no podr�amos realizar m�todos convencionales como ANOVA
# ya que existen ciertos datos muy grandes o muy peque�os que arruinan nuestros 
# supuestos.

print(histogramas_ingresos)


# Construimos el data frame necesario con los ingresos agrupados 

ingreso <- c(motorizado, publico, pie)
tipo_transporte <- c(rep("motorizado", length(motorizado)),
                     rep("publico", length(publico)),
                     rep("pie", length(pie))
                     )
  
datos_transporte <- data.frame(ingreso, tipo_transporte)

# Ahora, como se ha explicado, usaremos una prueba robusta para los datos, ya que, como siempre se
# explica, los datos de "ingresos" son la mayor�a de las veces problem�ticos.

# En nuestro caso, esto se ve en los histogramas presentados para mirar la forma de los datos 
# de los ingresos (figura histogramas_ingresos).

# Con esto en mente, se utiliza el m�todo t1way, que utiliza una medida robusta como
# la media truncada, justo lo que necesitamos en este caso, para eliminar
# cierto porcentaje de datos en ambas colas de la distribuci�n, que son los menos
# probables y tienen valores extremos (demasiado altos y demasiado peque�os)
# evitando un gran salto de datos m�nimos y m�ximos.


set.seed(565) # Semilla para reproducibilidad de datos

alfa <- 0.05 # Fijamos el nivel de significaci�n
gamma <- 0.15 # Usamos una poda de "X" % para la media

# Realizamos la prueba como tal:

prueba_media_truncada <- t1way(ingreso ~ tipo_transporte, 
                               data = datos_transporte,
                               tr = gamma,
                               alpha = alfa)


print(prueba_media_truncada)

# La prueba nos da un p-value = 0.00015. Dado que el valor es menor a nuestro nivel
# de significaci�n, diremos que, con un 95% de confianza, existen diferencias
# en los ingresos de los usuarios de diferentes tipos de transportes.
# Como tenemos tres grupos, queremos realizar un an�lisis post-hoc para
# identificar en qu� par de grupos se encuentra la diferencia significitiva,
# o bien, si todos los grupos tiene ingresos definitivamente distintos.

# Realizamos el an�lisis post_hoc con la funci�n 'lincon', especialmente hecha
# para ser utilizada tras haber aplicado la funci�n t1way:

post_hoc <- lincon(ingreso ~ tipo_transporte, 
                   data = datos_transporte, 
                   tr = gamma, 
                   alpha = alfa)

print(post_hoc)

# Conclusiones finales:

# Dados los resultados del an�lisis post-hoc, podemos ver que entre grupo de personas que utilizan 
# tipos de transporte motorizados (autos, motos) y aquellos que solo van 'a pie'
# existe una gran diferencia entre los ingresos (p = 0.00008, muy menor a alpha).
# Por otro lado, aquellos que van en transporte p�blico y los mismos usuarios con 
# transporte motorizado propio, se diferencian en menor forma, sin embargo a�n existe
# diferencia significativa estad�sticamente (p = 0.003 < alpha).
# Finalmente, aquellos que se transportan 'a pie' y aquellas personas que utilizan
# transporte p�blico no poseen diferencias significativas entre sus ingresos (p = 0.0621 > alfa)
# es decir, en promedio estos dos grupos tienen los mismos ingresos, sin embargo, 
# el margen para esta prueba es bastante estrecho (de 0.1), por lo que faltar�a corroborar
# con otro estudio.
