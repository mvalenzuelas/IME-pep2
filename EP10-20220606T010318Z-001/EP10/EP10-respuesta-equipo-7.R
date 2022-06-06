require(dplyr)
datos<-read.csv2("EP10 Datos.csv")

#1.-¿Existe diferencia en la puntuación obtenida por los envases diseñados por 
# PackPro según las evaluaciones realizadas por niños y adultos?
filtrado<-datos%>%filter(Diseno=="PackPro" & (Edad=="Nino" | Edad=="Adulto"))

#H0: No hay diferencia en la puntuación obtenida por los envase diseñados por
# PackPro
#HA: Hay diferencia en la puntuación obtenida por los envases diseñados por 
# PackPro

#Se pretende utilizar una prueba de rangos Wilconson, ya que las variables son 
# independientes y almenos ordinal
alfa=0.05

ninos<-filtrado%>%filter(Edad=="Nino")
adultos<-filtrado%>%filter(Edad=="Adulto")
prueba1<-wilcox.test(ninos$Puntaje,adultos$Puntaje,alternative = "two.sided",conf.level = 1-alfa)
print(prueba1)

#Con un nivel de significancia de alfa=0.05 no existe suficiente evidencia para
# rechazar la hipótesis nula, por ende no existe una diferencia en los puntajes
# obtenidos por los envases diseñados por PackPro entre niños y adultos

#2.-¿Existen diferencias entre las puntuaciones obtenidas para los diferentes 
# envases de caramelos? De ser así, ¿cuál(es) envase(s) se diferencia(n) de los
# demás?

#H0: No existe diferencia entre las puntuaciones obtenidas para los diferentes
# envases de caramelo
#HA: Existe diferencia entre las puntuaciones obtenidas para los diferentes 
# envases de caramelo

#Se realizará la prueba de kruskal, ya que la variable independiente tiene más de
# 2 niveles, la escala de la variable dependiente es almenos ordinal y las 
# observaciones son independientes entre sí.

caramelo<-datos%>%filter(Producto=="Caramelos")
prueba2<-kruskal.test(Puntaje~Diseno,data=caramelo)
print(prueba2)

#Con un nivel de significancia de alfa=0.05 no existe suficiente evidencia para
# rechazar la hipótesis nula por ende se puede determina que no existe diferencia
# entre las puntuaciones de los envases de caramelo.

