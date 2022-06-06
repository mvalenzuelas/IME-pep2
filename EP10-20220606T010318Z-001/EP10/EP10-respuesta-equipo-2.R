library(ggpubr)
library(tidyverse)


# 1. ¿Existe diferencia entre la puntuación obtenida por los envases diseñados por DisenoColor y 
# KoolDesign?

# 2 ¿Existe diferencias en las puntuaciones obtenidas para el envase de alfajores 
# diseñado por PackPro según la edad de los evaluadores? 


# De ser así, ¿cuál(es) grupo(s) de evaluador(es) se diferencia(n) de los demás?


datos <- read.csv2("EP10 Datos.csv")

# 1.

# Trabajamos los datos

diseno <- datos %>% filter(Diseno == "DisenoColor")
diseno <- diseno$Puntaje
  
kool <- datos %>% filter(Diseno == "KoolDesign")
kool <- kool$Puntaje


# Vemos si se cumple la condición de normalidad para las muestras.
p1 <- shapiro.test(kool)
p2 <- shapiro.test(diseno)

print(p1)
print(p2)

# Ambos p-valores son de un orden extremadamente pequeño, por lo que 
# se concluye que las muestras no siguen la condición de normalidad.

# Por ello, haremos una prueba no paramétrica que se ajuste a
# la información que tenemos.

# Como hay dos muestras, y los mismos id's están presentes en 
# ambas, se utilizará la prueba de rangos con signo de Wilcoxon.

#Verificando las condiciones para hacer esta prueba, tenemos las siguientes conclusiones:
#1) Se asume independencia de las muestras ya que se presume que evaluaron a cada envase sin
#tomar de referencia los otros
#2) tanto la continuidad, como la ordinalidad de la escala se verifican por el tip de escala que
#se utilizó para evaluar los envases, la escala de likert cumple ambas condiciones (intrínsecamente).

# Finalmente, señalamos las hipótesis a contrastar;
# H_0: Existe una diferencia entre el puntaje asignado al envase de DisenoColor y KoolDesign.
# H_A: No hay una diferencia entre las puntuaciones para los envases de DisenoColor y KoolDesign.

# Establecer nivel de significación .
alpha <- 0.05 

# Hacer la prueba de rangos con signo de Wilcoxon:
prueba <- wilcox.test(diseno, 
                      kool, 
                      alternative = "two.sided",
                      paired = TRUE,
                      conf.level = 1 - alpha)
print(prueba)

# Se observa que existe una diferencia entre las puntuaciones para cada envase
# ya que, de la prueba, se obtiene un p-valor = 0.106 que es mayor a nuestro
# alpha definido anteriormente, es decir, se rechaza H_0 en favor de H_A, con un
# 95% de confianza.

# 2.

alfajores <- datos %>% filter(Producto == "Alfajor" & Diseno == "PackPro")

ninos <- alfajores %>% filter(Edad == "Nino")
joven <- alfajores %>% filter(Edad == "Joven")
adulto <- alfajores %>% filter(Edad == "Adulto")

# En este caso, utilizaremos la prueba de Kruskal-Wallis
# ya que tenemos más de 2 grupos para la pregunta 2,
# siedo una alternativa no paramétrica para ANOVA en muestras independientes.

# Verificamos las condiciones para la prueba nombrada:
# - La var. independiente tiene a lo menos dos niveles: en este caso
# tenemos 3 niveles para la variable Edad: Niño, Joven y Adulto.
# - La escala de la var. dependiente es a lo menos ordinal:
# En este caso, el puntaje está ordenado de 1 a 7, por lo
# que cumple la condición "a lo menos".
# - Por último, las obs. son independientes entre sí, porque
# cada grupo coloca una puntuación de manera independiente
# a los demás productos.

# Ahora, podemos enunciar las hipótesis a contrastar:
# H_0: Todos los grupos etarios calificaron de igual manera el envase de Alfajor
# con diseño PackPro.
# H_A: Al menos un grupo etarios califica de forma distinta el envase de Alfajor
# con diseño PackPro.

ninos <- ninos$Puntaje
joven <- joven$Puntaje
adulto <- adulto$Puntaje
puntaje <- c(ninos, joven, adulto)


edades <- c(rep("ninos" , length(ninos)),
            rep("joven", length(joven)),
            rep("adulto", length(adulto))
            )


edades <- factor(edades)
datos_df <- data.frame(puntaje, edades)


prueba2 <- kruskal.test(puntaje ~ edades, datos_df)
print(prueba2)

if ( prueba2$p.value < alpha ) {
  post_hoc <- pairwise.wilcox.test(datos_df$puntaje, 
                                    datos_df$edades, 
                                    p.adjust.method = "holm", 
                                    paired = FALSE)
  print(post_hoc)
}

# Observamos que p < 2.2e-16 < alpha, or lo que existen diferencias
# muy acentuadas en los puntajes dados por los distintos grupos.
# Es decir, se rechaza la hipótesis nula en favor de la hipótesis alternativa,
# con un 95% de confianza (alpha = 0.05).

# Por ello, se realizó un análisis post-hoc con la corrección de Holm,
# mostrando nuevamente valores pequeños para cada par de grupos 
# A raíz de esto, se decide realizar un gráfico de cajas para 
# comparar las distribuciones, y vemos que, efectivamente,
# son muy distintas, siendo casi imposible que realicen
# la misma puntuación, corroborando el análisis hecho anteriormente.

g <- ggboxplot(datos_df, x = "edades", y = "puntaje")
print(g)
