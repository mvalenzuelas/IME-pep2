
# Nombre: Cristi·n Inzulza

#Equipo 1_individual:
  
#Pregunta 1: 
  
#  øExiste diferencia en la puntuaci√≥n obtenida por los envases dise√±ados por PackPro seg√∫n las evaluaciones realizadas por ni√±os y j√≥venes?



install.packages('dplyr')

library(dplyr)
library(readxl)

library(readxl)
datos <- read_excel("C:/Users/crist/Desktop/Ejercicio10InferenciaEstadistica/datos.xlsx")

datos_PackPro <- datos[datos$Diseno=="PackPro",]
datos_PackPro

datos_ninos <- datos_PackPro[datos_PackPro$Edad=='Nino',]

datos_jovenes <- datos_PackPro[datos_PackPro$Edad=='Joven',]

ninos <- datos_ninos$Puntaje

jovenes <- datos_jovenes$Puntaje

#Pruebas de normalidad

shapiro.test(ninos)

shapiro.test(jovenes)

# Los datos no son normales por lo que se utilizar·n pruebas no paramÈtricas

# Los valores no son normales ambos valores-p son < 2.2e-16
# Por lo que las distribuciones de puntajes de los grupos de ni√±os y j√≥venes no son normales. 

#Utilizando un alpha igual a 0.05
alfa <- 0.05

#H0: No existe hay diferencias en el puntaje de PackPro entre Ni√±os y J√≥venes
#Ha: Hay diferencias en el puntaje de PackPro entre Ni√±os y J√≥venes


pruebaWilcox <- wilcox.test(datos_ninos$Puntaje,
                            datos_jovenes$Puntaje,
                       alternative = "two.sided",
                       conf.level = 1-alfa)


print(pruebaWilcox)

# El valor-p es < 2.2e-16. Por lo tanto no se puede rechazar la hipÛtesis nula H0.

# Por lo tanto, no hay diferencias en el diseÒo de PackPro entre NiÒos y Jovenes


#Pregunta 2:
  
#  øExisten diferencias entre las puntuaciones obtenidas para los diferentes envases de galletas? De ser as√≠, ¬øcu√°l(es) 
#  envase(s) se diferencia(n) de los dem√°s?

datosGalletas <- datos[datos$Producto=="Galletas",]

#H0: No hay diferencias en los diseÒos de las galletas segun su puntaje
#H1: Hay diferencias en al menos un tipo de diseÒo de galletas seg˙n su puntaje

#pruebaFriedman <- friedman.test(Puntaje ~ Producto | Id, data = datosGalletas)

# ObtenciÛn de los datos

caja <- c(rep("PackPro", length(galletas_PackPro$Puntaje)), rep("Kool", length(galletas_KoolDesign$Puntaje)), rep("Kajita", length(galletas_LaKajita$Puntaje)), rep("Color", length(galletas_DisenoColor$Puntaje)))

factor <- factor(caja)

galletas_LaKajita <- datosGalletas[datosGalletas$Diseno=="LaKajita",]
galletas_DisenoColor <- datosGalletas[datosGalletas$Diseno=="DisenoColor",]
galletas_PackPro <- datosGalletas[datosGalletas$Diseno=="PackPro",]
galletas_KoolDesign <- datosGalletas[datosGalletas$Diseno=="KoolDesign",]

mean(galletas_LaKajita$Puntaje)
mean(galletas_DisenoColor$Puntaje)
mean(galletas_PackPro$Puntaje)
mean(galletas_KoolDesign$Puntaje)


#Pruebas de normalidad

shapiro.test(datos$Puntaje[1:4500])

shapiro.test(galletas_LaKajita$Puntaje)
shapiro.test(galletas_DisenoColor$Puntaje)
shapiro.test(galletas_PackPro$Puntaje)
shapiro.test(galletas_KoolDesign$Puntaje)
   
hist(datos$Puntaje[1:4500])
hist(galletas_LaKajita$Puntaje)
hist(galletas_DisenoColor$Puntaje)
hist(galletas_PackPro$Puntaje)
hist(galletas_KoolDesign$Puntaje) 

# Los datos no son normales por lo que se utilizar·n pruebas no paramÈtricas


puntajeGalletas <- c( galletas_LaKajita$Puntaje, galletas_DisenoColor$Puntaje,galletas_PackPro$Puntaje, galletas_KoolDesign$Puntaje)

id <- rep(1:length(galletas_DisenoColor$Puntaje), 4)

datos <- data.frame(id, puntajeGalletas, factor)

#Para utilizar la prueba de Friedman se debe culplir las siguiente condiciones:
#1. La variable independiente debe ser categÛrica y tener a lo menos 3 niveles
#2. La escala de la variable dependiente debe ser, a lo menos, 
#3. Los sujetos son una muestra aleatoria e independiente de la poblaciÛn


Friedman <- friedman.test(puntajeGalletas ~ factor | id, data = datos)
print(Friedman)

# El valor-p es de 0,3283. Por lo tanto, se rechaza la hipÛtesis nula 
# en favor de la hipÛtesis alternativa. 

# Conclusiones:
  
# 1. No hay diferencias en el diseÒo de PackPro entre NiÒos y Jovenes

# 2.Existe diferencia en los diseÒos de las cajas de galletas 



  

  