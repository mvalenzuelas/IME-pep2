library(patchwork)
library(ggpubr)
library(dplyr)
library(WRS2)
library(ggplot2)
library(tidyverse)
library(ez)
library(simpleboot)
library(WRS2)
library(DescTools)


#----------Pregunta 1----------

#¿Es uno de los algoritmos más rápido que el otro? 

#Para responder a esta pregunta se necesita comparar las medias de los algoritmos. En este caso se podría utilizar una prueba
#t, por lo que se procede a revisar las condiciones.

#Datos de la tabla (Se interpreta por el enunciado que los datos son independientes porque todos provienen de distintas instancias)

tiempoA <- c(48667,783108,4428151,842079,8576989,882722,834565,210041,48705,251843)
tiempoB <- c(6684497,5743260,92932,48408,994830,2196277,35974,4629726,180141,1174562)


tiempos <- c(tiempoA,tiempoB)
algoritmo <- c(rep("A", length(tiempoA)), rep("B", length(tiempoB)))
datos <- data.frame(tiempos, algoritmo)


#Condiciones:
#-Independencia de datos: El enunciado asegura que los tiempos de ejecución son independientes entre si.

#-Normalidad: Se procede a utilizar un gráfico qq para verificar la normalidad de los datos.

grafico1 <- ggqqplot(datos, x = "tiempos", facet.by = "algoritmo", palette = c("blue","red"), color = "algoritmo")
print(grafico1)

shapiro.test(tiempoA)
shapiro.test(tiempoB)

#De acuerdo al grafico QQ, la tabla del tiempo A presenta datos atípicos, por lo que no cumple el supuesto de normalidad.
#Se procede a verificar estos resultados a través de un test de Shapiro-Wilk y se confirma que los datos no provienen de poblaciones
#con distribución normal.

#En consecuencia, para poder evaluar las medias de los grupos se procede a evaluar mediante el método de Yuen (método 
#Robusto) para dos muestras independientes.


#----------------------------#
#Método de Yuen para dos muestras independientes

set.seed(127)

#Significación
alpha <- 0.05

#Hipótesis
#H0: Las medias de ambos algoritmos son iguales.
#H1: Las media de ambos algoritmos son distintas.

#H0 : media_A == media_B
#H1 : media_A != media_B

#Se realiza la prueba a las medias de los datos con un bootstrapping de 5999.

media <- pb2gen(tiempos ~ algoritmo, 
                data = datos,
                est = "mean",
                nboot = 5999)

print(media[["p.value"]])


#-Conclusión:  
#Se obtiene un p-valor mayor al alpha establecido, por lo que se puede asegurar con un 
#95% de confianza que no se rechaza la hipótesis nula, y por lo tanto ningún algoritmo 
#es más rápido que el otro.


#-------- Pregunta 2 -----------

# Lectura de datos
datos <- read.csv2("C:/Users/osswa/OneDrive/Escritorio/IME/EP11/EP11 Datos.csv",stringsAsFactors = FALSE)
# Se filtran los datos
coquimbo <- datos %>% filter(datos$region=="Región de Coquimbo")
# Semilla para sample
set.seed(4207)

# Pregunta EP 11
# La cantidad promedio de persona que habitan un hogar en la región de coquimbo, depende
# del nivel educacional de enseñanza básica del jefe del hogar

# Se toma una muestra
muestra <- sample_n(coquimbo %>% select(numper,educ), 500, replace = FALSE)

# Se filtran los datos
muestra_basicaIncom <- muestra %>% filter(educ == "Básica Incom.") 
muestra_basicaComp <- muestra %>% filter(educ == "Básica Compl.")
muestra <- rbind(muestra_basicaIncom,muestra_basicaComp)

basica_incompleta <- muestra_basicaIncom$numper
basica_completa <- muestra_basicaComp$numper

# Grafico de cantidad de personas jefes de hogar dependiendo del tipo de educacion
gh <- ggqqplot(basica_incompleta,title="N° de personas con jefe hogar con Ed. basica incompleta")
gm <- ggqqplot(basica_completa,title="N° de personas con jefe hogar con Ed. basica completa")

print(gh)
print(gm)

# Se observa que la muestra presenta datos que no estan dentro del rango necesario para considerar normalidad,
# es por esto, sumado al hecho de que las muestras tienen tamaños diferentes, que se utilizaran metodos
# robustos que permitan trabajar con estos datos problematicos.

# Hipotesis
# H0: La media de la cantidad de personas que habitan un hogar es igual para hogares con jefes de hogar
# con ed. basica completa e incompleta.
# HA: La media de la cantidad de personas que habitan un hogar es distinta para hogares con jefes de hogar
# con ed. basica completa e incompleta.

# Matematicamente:
# Sea ubc la cantidad de personas en un hogar con jefes de hogar con eduacion basica completa y ubi 
# la cantidad de personas en un hogar con jefes de hogar con eduacion basica incompleta, entonces:
# H0: ubc = ubi
# HA: ubc != ubi

# Dado que se busca comparar las medias de muestras independientes con diferentes tamanos, se utilizara la
# prueba de Yuen para dos muestras independientes. Tambien se aplicara la transformacion de datos
# Box Cox para que los datos se ajusten de mejor forma a la distribucion normal.

# Definiendo significaciOn de 0.05 y cantidad de muestras a generar con bootstraping
alfa = 0.05
bootstrap <- 5999

# Transformacion Box-cox
lambda <- BoxCoxLambda(muestra$numper, lower = -4, upper = 4)
cat("Lambda optimo: ", lambda)
transformacionDatos <- BoxCox(muestra$numper,lambda)
datos1 <- data.frame(muestra,transformacionDatos)

# Grafica de datos transformacion
g1 <- ggqqplot(transformacionDatos, color = "purple")
print(g1)

# Se aplica la prueba de yuen para dos muestras independientes (con Bootstrap)
prueba_media <- yuenbt(transformacionDatos ~ educ,
                       data = datos1,
                       tr = 0.1,
                       nboot = bootstrap,
                       side = TRUE)

cat("\n Resultado al usar la media como estimador\n")
print(prueba_media)

cat("\n Valor p obtenido:", prueba_media[["p.value"]])

# Conclusion
# Dado que el valor p obtenido es mayor al nivel de significacion, se puede afirmar con un 95% de confianza que
# no es posible rechazar la hipotesis nula en favor de la hipotesis alternativa, por lo tanto,
# la cantidad de personas que habitan un hogar es igual para hogares con jefes de hogar con ed. basica completa
# e incompleta.



#------ Pregunta 3 --------


#Usamos los datos y hacemos un estudio en base al estudio planteado en la experiencia anterior pero con un metodo mas robusto
#A continuación, un bloque con el problema referenciado
###problema referenciado###
###################################################
###################################################
###################################################
cat("PROBLEMA BASE")
# Dependiendo del ingreso de los hogares, influye en si van a pie, en vehiculo o en transporte publico

# H_0: El ingreso promedio de los hogares es igual para cada grupo que utiliza distintos 
# tipos de transporte para movilizarse.
# H_0: El ingreso promedio de los hogares es diferente para cada grupo que utiliza distintos 
# tipos de transporte para movilizarse.

#semilla usada
set.seed(891)
#tamaño de la muestra
n2 <- 450
#separando y seleccionando los datos a usar
datos2 <- datos %>% sample_n(n2) %>% select(region, ytotcorh, o25c)


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
# de transporte son distintos. Es más, realizando el análisis post-hoc
# correspondiente, todos los ingresos promedio entre los grupos, difieren.

# Ahora, es pertinente realizar una prueba que tenga un valor nulo
# definido (distinto de 0) para entregar datos útiles a la ciudadanía.


g_grupos <- ggboxplot(datos_formato, x = "transporte", y = "ingreso")
print(g_grupos)

cat("FIN DEL PROBLEMA BASE")
###################################################
###################################################
###################################################
#Ya expresado el problema y viendo las complicaciones que hubo
#a la hora de sacar conclusiones, procederemos a utilizar el metodo robusto
#correspondiente con el boostraping usado

#Siendo nuestras hipotesis y nuestro problema el mismo planteado antes
##Se necesita saber si dentro de  un hogar y las personas que trabajan en el, 
#si influye el medio de transporte dependiendo de el ingreso que percibe el hogar

#tenemos entonces las hipotesis
#H0: El ingreso neto familiar influye en el medio de transporte utilizado para ir a trabajar
#Ha: En al menos uno de los grupos no existe relacion entre el medio de transporte y el ingreso familiar

#Teniedo las hipotesis procedemos a la prueba usando los datos anteriormente mencionados 
cat ( "Comparación entre grupos usando bootstrap \n" )
muestras <- 2000 
set.seed(891)
gama <- 0.2
columnas <- c(pie,motorizado,publico)
algoritmo <- c(rep("pie",length(pie)),
               rep("motorizado",length(motorizado)),
               rep("publico",length(publico)))

cuadro <- data.frame(columnas,algoritmo)
#Prueba con Bootstrap
bootstrap <- t1waybt ( columnas ~ algoritmo, 
                       data = cuadro,
                       tr = gama,
                       nboot = muestras)

print(bootstrap)

#con un p valor menor al alfa = 0.05 deberiamos rechazar la hipotesis nula en favor de la alternativa, sin embargo, verificaremos con un analisis post-hoc
cat("Como p valor =0.001<alfa, realizamos analisis post-hoc para verificar")

post_hoc <- mcppb20(columnas ~ algoritmo, 
                    data = cuadro,
                    tr = gama,
                    nboot = muestras)
print (post_hoc)
#Vemos que el p valor de  personas que usan vehiculo motorizado vs las que usan transporte publico presentan un p valor ligeramente mayor a 0.05, lo que verifica un grupo diferente en las medias, porlo tanto confirmamos nuestra conclusion anterior
cat("Luego del post hoc, confirmamos con un 95% de confianza que en al menos un medio de transporte, no se ve relacion o preferencia con respecto al sueldo recibido")
#en este caso la prueba robusta y su correspondiente post hoc ayudaron a entregar una respuesta mas completa y correcta
#al roblema, por lo que un metodo robusto fue mejor y mas completo para la resolucion del mismo.



