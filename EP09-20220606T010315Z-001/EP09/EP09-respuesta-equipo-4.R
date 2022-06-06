library (dplyr)
library(tidyverse)
library(ez)
library(ggpubr)

basename <- "EP08 Datos.csv"
file <- file.path("C:/Users/adolf/Desktop/Cosas/Progra/IME/IME_Equipo_4", basename)
datos <- read.csv2(file = file)

# En este momento, los investigadores buscan determinar si existen diferencias en el tiempo que tardan 
# los usuarios en formular consultas para problemas con diferente nivel de dificultad en el área de biología.

#Para determinar si los tiempos son iguales o distintos para las distintas dificultades se usara una prueba
#ANOVA para muestras correlacionadas.

#Las condiciones para realizar esta prueba son:
# 1. La escala con que se mide la varible dependiente tiene las propiedades de una escala de intervalos
#    iguales, lo que se cumple ya que esta variable se mide en segundos, unidad en la que cada intervalo es igual.
# 2. Las mediciones son independiente al interior de cada grupo, lo que se cumple porque cada observacion en los
#    grupos representa a una persona distinta que se selecciono de manera aleatoria.
# 3. La poblacion sigue una distribucion normal, lo que tambien se cumple, lo que se puede comprobar observando el 
#    grafico Q-Q que se crea mas adelante en el codigo.
# 4. La matriz de varianzas-covarianzas es esferica, lo que se comprueba en los datos entregados por la funcion
#    ezANOVA, la que contiene el resultado de la prueba de esfericidad de Mauchly, que entrega un p-valor igual a
#    0.5329, el cual es mayor al nivel de significacion que se uso (0.05), por lo que se puede concluir que la
#    matriz de varianzas-covarianzas es esferica.

#Hipotesis:
#H0: El tiempo que tardan los usuarios en formular consultas para problemas es el mismo para cada dificultad.
#HA: El tiempo que tardan los usuarios en formular consultas para problemas es distinto para por lo menos
#    una dificultad.

#Se fija el nivel de significacion en 0.05.
alfa <- 0.05

#Se filtran las observaciones que corresponden a biologia segun su dificultad.
biologiaBaja <- datos %>% filter(area=="Biología" & dificultad=="Baja")
biologiaMedia <- datos %>% filter(area=="Biología" & dificultad=="Media")
biologiaAlta <- datos %>% filter(area=="Biología" & dificultad=="Alta")

#Se obtienen los tiempos de cada dificultad para el nuevo data frame.
Baja <- pull(biologiaBaja, tiempo)
Media <- pull(biologiaMedia, tiempo)
Alta <- pull(biologiaAlta, tiempo)

#Se crea un vector para representar la id de las observaciones.
id <- factor(1:length(Baja)) 

#Se crea el data frame con los tiempos de cada dificultad.
dataframePrueba <- data.frame(id, Baja, Media, Alta)

#Se pivotea el nuevo data frame.
pivot <- dataframePrueba%>%pivot_longer(c("Baja", "Media", "Alta"), names_to="Dificultad", values_to="Tiempo")
pivot[["Dificultad"]] <- factor(pivot[["Dificultad"]])

#Se crea un grafico Q-Q para ver la distribucion de las muestras, si se acercan a la normal.
g <- ggqqplot(pivot,
              x="Tiempo",
              y="Dificultad",
              color="Dificultad")
g <- g + facet_wrap(~ Dificultad)
g <- g + rremove("x.ticks") + rremove("x.text")
g <- g + rremove("y.ticks") + rremove("y.text")
g <- g + rremove("axis.title")

#Se imprime el grafico
print(g)

#Se realiza la prueba de ANOVA para muestras correlacionadas utilizando la funcion ezANOVA.
pruebaEz <- ezANOVA(
  data=pivot,
  dv=Tiempo,
  within = Dificultad,
  wid = id,
  return_aov=TRUE)

#Se imprime el resultado de la prueba por consola.
print(pruebaEz)

#De la prueba de ANOVA realizada se obtiene un p-valor igual a 3.177e-23, el cual es menor al nivel de signficacion
#fijado, por lo que se rechaza la hipotesis nula en favor de la hipotesis alternativa, pudiendo concluir con un
#95% de confianza que el tiempo que tardan los usuarios en formular consultas para problemas es distinto para por 
#lo menos una dificultad.

#Para determinar cual(es) dificultad(es) son distintas se realizara una prueba post-hoc, la que sera la prueba 
#HSD de Tukey.

#Se realiza la prueba de ANOVA usando la funcion aov, ya que este es el formato que se necesita para la prueba
#de Tukey.
prueba <- aov(Tiempo ~ Dificultad, data=pivot)

#Se realiza la prueba de Tuckey usando la funcion TukeyHSD de R.
post_hocTukey <- TukeyHSD(prueba, "Dificultad", ordered=TRUE, conf.level=1-alfa)

#Se imprime por consola el resultado de la prueba.
print(post_hocTukey)

#De la prueba HSD de Tukey se obtienen los p-valores de la comparacion de cada par de dificultades siendo estos:
# 1. Baja-media = 0.000593.
# 2. Alta-media = 0.000000.
# 3. Alta-baja = 0.000000.
#Cada uno de los p-valores obtenidos es menor al nivel de significacion fijado, por lo que se concluye que todas
#las difcultades presentan tiempos distintos.
