#Librerias
library(dplyr)

#=========================
#==== Lectura de datos ===
#=========================
#cambiar dependiendo de la direccion de su archivo
datos <- read.csv2("C:/Users/adolf/Desktop/Cosas/IME/EP10 Datos.csv",stringsAsFactors = FALSE)

#=========================
#==== Enunciado ===
#=========================
# 1)¿Existe diferencia entre la puntuación obtenida por los envases diseñados por DisenoColor y PackPro?
# 2)¿Existe diferencias en las puntuaciones obtenidas para el envase de queque diseñado por KoolDesign 
#   según la edad de los evaluadores? De ser así, ¿cuál(es) grupo(s) de evaluador(es) se diferencia(n) de los demás?


#----------------------------------------------------------------------------------------   

#=========================
#==== Pregunta 1 ===
#=========================
# ¿Existe diferencia entre la puntuación obtenida por los envases diseñados por DisenoColor y PackPro?

#Filtramos los datos

#Datos de DisenoColor
dataDiseno <- datos %>% filter(Diseno == "DisenoColor") %>% select(Puntaje)
VectorDiseno <- as.vector(t(dataDiseno))
#Datos de PackPro
dataPack <- datos %>%  filter(Diseno == "PackPro") %>% select(Puntaje)
VectorPack <- as.vector(t(dataPack))

#=========================
#==== Condiciones ===
#=========================

# Se tienen observaciones independientes, ya que son marcas diferentes.
# La variable es ordinal ya que son puntajes, a los que se le asigna una escala
#  de valores.

# Dado que se cumplen las condiciones anteriores, es posible aplicar la prueba de suma de rangos
# de Wilcoxon.

#=========================
#==== Desarrollo ===
#=========================

#Hipotesis
#H0 : No existe diferencia entre las puntuaciones obtenida para cada diseno
#H1 : Si hay diferencia entre las puntuaciones obtenida para cada diseno

# Forma matemática: No existe parametro a comparar

#Definimos un alfa 
alfa <- 0.05

# Hacer la prueba de Mann-Whitney.
prueba1 <- wilcox.test(VectorDiseno, VectorPack, alternative = "two.sided", conf.level = 1 - alfa)
print(prueba1)

#===============================
#==== Analisis y conclusion ====
#===============================
# El p obtenido fue 0.04253, que es menor al alfa que definimos, por lo tanto se rechaza la hipotesis nula
# en favor de la hipotesis alternativa, por lo que se puede afirmar con un 95% de confianza que
# Si hay diferencia entre las puntuaciones obtenida para cada diseno. Sin embargo, dado que la diferencia
# entre el p y el alfa es pequeña (aprox 0.01) se podría considerar utilizar una muestra mas grande.

#----------------------------------------------------------------------------------------   

#=========================
#==== Pregunta 2 ===
#=========================
#¿Existe diferencias en las puntuaciones obtenidas para el envase de queque disenado por KoolDesign 
# según la edad de los evaluadores? De ser así, ¿cuál(es) grupo(s) de evaluador(es) se diferencia(n) de los demás?


#Filtramos los datos

# Datos de DisenoColor
dataDiseno2 <- datos %>% filter(Diseno == "KoolDesign" & Producto == "Queque") 
dataDiseno2 <- datos %>% select(Edad,Producto,Puntaje)

# Datos por edad
dataNino <- dataDiseno2 %>%filter(Edad == "Nino") %>% select(Puntaje)
VectorNino <- as.vector(t(dataNino))
dataJoven <- dataDiseno2 %>%filter(Edad == "Joven") %>% select(Puntaje)
VectorJoven <- as.vector(t(dataJoven))
dataAdulto <- dataDiseno2 %>%filter(Edad == "Adulto") %>% select(Puntaje)
VectorAdulto <- as.vector(t(dataAdulto))

#=========================
#==== Condiciones ===
#=========================

# La variable independiente (Edad), tiene 3 niveles
# es ordinal la variabel dependiente (Puntaje)
# y las observaciones son independientes entre si, dado
# que son personas de edades distintas.

# Dado que se cumplen las condiciones anteriores, es posible aplicar la prueba Kruskal-Wallis

#=========================
#==== Desarrollo ===
#=========================

#Hipotesis
#H0: la puntuacion obtenida por el envase de queque disenado por KoolDesign es igual para cada rango de edad
#HA: la puntuacion obtenida por el envase de queque disenado por KoolDesign es diferente para, por lo menos,
#    un rango de edad.

# Forma matemática: No existe parametro a comparar

puntajes <- c(VectorNino, VectorJoven, VectorAdulto)

edades <- c(rep("Nino",length(VectorNino)),
            rep("Joven",length(VectorJoven)),
            rep("Adulto",length(VectorAdulto)))
edades <- factor(edades)

dataKruskal <- data.frame(puntajes, edades)

#En base al alfa definido en la pregunta anterior

pruebaKruskal <- kruskal.test(puntajes ~ edades, data = dataKruskal)

print(pruebaKruskal)
#=========================
#==== Analisis y conclusion ===
#=========================

#En base a la prueba anterior se obtiene un p valor igual a 0.3393
# el cual es menor a alfa designado (0.05), por lo que se falla en rechazar
# la hipotesis nula, por lo que con un 95% de confianza, la puntuacion obtenida 
# por el envase de queque disenado por KoolDesign es igual para cada rango de edad.
# y no existe diferencia entre los puntajes.