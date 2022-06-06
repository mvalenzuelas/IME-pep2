library(ggpubr)
library(dplyr)
library(WRS2)

#En el trabajo de título de un estudiante del DIINF se reportan 
#los siguientes tiempos de ejecución (en milisegundos) medidos para dos 
#versiones de un algoritmo genético para resolver instancias del problema 
#del vendedor viajero disponibles en repositorios públicos. 
#¿Es uno de los algoritmos más rápido que el otro? 
#Aplique una transformación de datos adecuada para responder.

tiempoA <- c(48705,251843,210041,8576989,842079,1510394,
             4428151,834565,37449,885722)
tiempoB <- c(6701654,1252837,6568968,6684497,92932,994830,
             120276,180141,5743260,1174562)

datos <- data.frame(tiempoA, tiempoB)

#las muestras no siguen una distribucion normal
#hacemos una transformacion logaritmica dado que los datos son valores muy grandes así que suavizamos los datos

tlogA <- log(tiempoA)
tlogB <- log(tiempoB)
datosLog <- data.frame(tlogA, tlogB)

shapiro.test(tlogA)
shapiro.test(tlogB)

#luego de la transformacion ambas muestras se aproximan a
# distribucion normal

# usamos la prueba t de Student para muestras no relacionadas 
# H0 = el tiempo medio de los algoritmos A y B son iguales 
# Ha = el tiempo medio de los algortimos A y B son distintos 

#H0 :  muA = muB
#Ha : muA != muB

# parametro estimado 
# diferencia de medias muestrales

# test 2 colas

# nivel de significación 
alfa <- 0.05

# condiciones
# Independencia: Se trata de un muestreo aleatorio donde el tamaño de las muestras no supera el 10% de la población. Se puede afirmar que los eventos son independientes.
# Normalidad: los datos transformados siguen una distribución normal

t.test(
  x           = tlogA,
  y           = tlogB,
  alternative = "two.sided",
  mu          = 0,
  var.equal   = TRUE,
  conf.level  = 0.95
)

# conclusión
#Dado que p-value (0.3636) es mayor que alfa (0.05) , no se dispone de evidencia suficiente para 
#considerar que existe una diferencia entre el tiempo promedio de ejecución del algoritmo A
# y el tiempo promedio de ejcución del algoritmo B

# pregunta 2

basename <- "EP11 Datos.csv"
file <- file.path("C:/Users/mauri-Pc/Desktop/Inferencia y modelos estadisticos/1-2022/IME-TEAM-7/EP12",basename)
datos <- read.csv2(file = file)

# ¿En promedio, existe una diferencia significativa en el ingreso per capita de las personas de la 
# Región Metropolitana de Santiago y la Región de Tarapaca?

# Se formula la Hipótesis

# H0: No existe una diferencia significativa en el promedio de ingresos per capita de los habitantes de la región
#     Metropolitana y Tarapaca
  # prom_RM = prom_TA

# HA: Si existe una diferencia significativa en el promedio de ingresos per capita de los habitantes de la región
#     Metropolitana y Tarapaca¡
  # prom_RM != prom_TA

# Para realizar la prueba, se utilizara un nivel de significación de 0.05.
alfa <- 0.05
# Se obtienen los ingresos y el número de personas en el hogar de los habitantes
# en todas las regiones.

regiones <- datos%>% select(region,numper,ytotcorh)

# se obtiene los ingresos per capita de la Región Metropolitana
datosRM  <- regiones %>% filter(region == "Región Metropolitana de Santiago",numper != "NA")
datosRM$ytotcorh <- datosRM$ytotcorh / as.integer(datosRM$numper)
datosRM <- datosRM[["ytotcorh"]]

#se obtiene los ingresos per capita de la Región de Tarapaca¡
datosTarapaca  <- regiones %>% filter(region == "Región de Tarapaca¡",numper != "NA")
datosTarapaca$ytotcorh <- datosTarapaca$ytotcorh / as.integer(datosTarapaca$numper)
datosTarapaca <- datosTarapaca[["ytotcorh"]]

gamma <- 0.4
n_TA <- length(datosTarapaca)
n_RM <- length(datosRM)

poda_TA <- n_TA * gamma
poda_RM <- n_RM * gamma

TA_truncada <- datosTarapaca[poda_TA:(n_TA - poda_TA)]
RM_truncada <- datosRM[poda_RM:(n_RM - poda_RM)]


datosIngresos <- c(RM_truncada,TA_truncada)
zonas <- c(rep("RM", length(RM_truncada)),rep("TA", length(TA_truncada)))
datosP2 <- data.frame(zonas,datosIngresos)

shapiro.test(RM_truncada)
shapiro.test(TA_truncada)
#en ambos casos p<<alfa por lo que se necesita una 
# prueba con bootstrapping



ingresos <- c(datosRM,datosTarapaca)
zonas <- c(rep("RM", length(datosRM)),rep("TA", length(datosTarapaca)))
datosP2 <- data.frame(zonas,ingresos)

R <- 3999
set.seed(243)
prueba <- pb2gen(ingresos~zonas,
                 data= datosP2,
                 est = "mean",
                 nboot = R)

#p <<alfa por lo que se rechaza H0 y se concluye con una certeza del 95%
#que si existe una diferencia significativa en el promedio de ingresos
#per capita de los habitantes de la región Metropolitana y Tarapaca

#Pregunta 3
#Analice la segunda pregunta abordada en el ejercicio practico 11, 
#con los mismos datos, utilizando un método robusto adecuado.

basename <- "EP11 Datos.csv"
dir <- "D:\\Documentos\\Universidad\\SEMESTRE 2 - 2022\\IME\\EP12"
file <- file.path(dir, basename)
datos <- read.csv2(file = file)

#La pregunta de investigacion planteada es la de analizar si, en promedio, los hombres de las regiones de Atacama,
#Metropolitana y Los Lagos trabajan la misma cantidad de horas a la semana.

#Hipotesis:
#H0: en promedio, los hombres de las regiones de Atacama, Metropolitana y de Los Lagos trabajan la misma cantidad de 
#    horas a la semana.
#HA: en promedio, los hombres de al menos una de las regiones estudiadas trabajan una cantidad distinta de horas 
#    respecto a las demas regiones.

#Se fija un seed.
set.seed(674)

#Se filtran las observaciones de interes para el estudio desde el archivo.
hombresAtacama <- datos %>% filter(region == "Región de Atacama" & sexo == "Hombre" & o10 != "N/A")
hombresMetropolitana <- datos %>% filter(region == "Región Metropolitana de Santiago" & sexo == "Hombre" & o10 != "N/A")
hombresLosLagos <- datos %>% filter(region == "Región de Los Lagos" & sexo == "Hombre" & o10 != "N/A")

#Se obtiene una muestra aleatoria de cada grupo de interes.
muestraAtacama <- sample_n(hombresAtacama, 150)
muestraMetropolitana <- sample_n(hombresMetropolitana, 250)
muestraLosLagos <- sample_n(hombresLosLagos, 200)

#se obtiene el vector de la columna de cantidad horas para la muestra de las 3 regiones.
mA <- pull(muestraAtacama, o10)
mM <- pull(muestraMetropolitana, o10)
mL <- pull(muestraLosLagos, o10)

horas <- c(mA, mM, mL)
regiones <- c(rep("Atacama", length(mA)), rep("Metropolitana", length(mM)), rep("Los Lagos", length(mL)))

dataframe <- data.frame(horas, regiones)

#fijamos el nivel de significacion
alfa <- 0.05

cat("Comparacion entre grupos usando medias truncadas\n\n")
gamma <- 0.2

medias_truncadas <- t1way(horas ~ regiones, data=datos, tr=gamma, alpha = alfa)
print(medias_truncadas)

if(medias_truncadas$p.value<alfa){
  cat("\nProcedimiento post-hoc\n\n")
  set.seed(123)
  post_hoc <- lincon(horas~regiones, data=datos, tr=gamma, alpha=alfa)
  print(post_hoc)
}

#p-value=0.15895 
#alfa<p-value

#Método bootstrap

cat("Comparacion entre grupos usando bootstrap\n\n")
muestras <- 999
set.seed(123)
bootstrap <- t1waybt(horas~regiones, data = datos, tr=gamma, nboot=muestras)
print(bootstrap)
if(bootstrap$p.value<alfa){
  cat("\nProcedimiento post-hoc\n\n")
  set.seed(123)
  post_hoc1 <- mcppb20(horas~regiones, data=datos, tr=gamma, nboot = muestras)
  print(post_hoc1)
}

#Conclusion
#Al ser p-value mayor que el se puede afirmar con un 95% de confianza que en promedio,
#los hombres de las regiones de Atacama, Metropolitana y de Los Lagos trabajan la misma cantidad de 
#horas a la semana. 
