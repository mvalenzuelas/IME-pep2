library(tidyverse)
library(ggpubr)
library(ggplot2)
library(ez)
"
Un equipo de investigadores del �rea de interacci�n humano-informaci�n est� estudiando
si el �rea tem�tica y el nivel de dificultad del problema de informaci�n influyen en el
tiempo (en segundos) que toma un usuario en formular una consulta de b�squeda para resolver
dicho problema. Para ello, han reclutado a un grupo de participantes voluntarios, asignados
aleatoriamente a distintos grupos. Cada participante debe resolver tres problemas 
de informaci�n con diferentes niveles de dificultad: baja, media y alta. A su vez, 
cada grupo debe resolver problemas relacionados a una tem�tica diferente.
Los datos recolectados contemplan las siguientes variables:
"

datos <- read.csv2("EP08 Datos.csv")

# Agregamos las hip�tesis:
#H_0:El promedio del tiempo que tardan los usuarios en formular una consulta de dif�cultad dif�cil en las �reas
#de psicolog�a, m�sica y matem�ticas, es igual entre si.
#H_A:El promedio del tiempo que tardan los usuarios en formular una consulta de dificultad dif�cil en las �reas
#de psicolog�a, m�sica y matem�ticas, es distinto para al menos uno de ellos.

datos_d_alta <- datos %>% filter(dificultad == "Alta")

datosPsi <- datos_d_alta %>% filter(area == "Psicolog�a")
datosPsi <- datosPsi$tiempo

datosMus <- datos_d_alta %>% filter(area == "M�sica")
datosMus <- datosMus$tiempo

datosMat <- datos_d_alta %>% filter(area == "Matem�ticas")
datosMat <- datosMat$tiempo

alpha <- 0.05

"Debemos revisar si la muestra cumple con las condiciones necesarias
    1) Como la variable a considerar son los tiempos, se asume que posee las propiedades
    de una escala de intervalos iguales\n
    2) La muestra fue seleccionada de forma aleatoria por el equipo de investigadores.
    3) Verificamos la normalidad con el test de Shapiro."

shapiro_p1 <- shapiro.test(datosPsi)
shapiro_p2 <- shapiro.test(datosMus)
shapiro_p3 <- shapiro.test(datosMat)

# Con un alpha = 0.05, podemos ver que todos los valores son mayores a este nivel de significaci�n
# y se verifica que fueron tomados de una poblaci�n con distribuci�n normal.
cat("Psicologia = ", shapiro_p1$p.value, "Musica = ", shapiro_p2$p.value, "Matematica = ", shapiro_p3$p.value, "\n")

# Finalmente, debemos verificar la condici�n n�mero 4)
# Que nos dice que debemos verificar que las varianzas sean distintas a cero.

vars <- c(var(datosPsi), var(datosMus), var(datosMat))
coef <- max(vars) / min(vars)

# Como coef = 1.17 < 1.5 se comprueba la homogeneidad de las varianzas (condici�n 4).


# Ahora podemos realizar la prueba ANOVA sin problemas:

# Llevar data frame a formato largo.

data_frame_datos <- data.frame(Psicologia = datosPsi, Musica = datosMus, Matematicas = datosMat)

datos_preguntas <- data_frame_datos %>% pivot_longer(c("Psicologia", "Musica", "Matematicas"),
                                                     names_to = "Area",
                                                     values_to = "Tiempo")


datos_preguntas[["Area"]] <- factor(datos_preguntas[["Area"]])
datos_preguntas[["Instancia"]] <- factor(1:nrow(datos_preguntas))

# variable_dependiente ~ variable_independiente

prueba_anova <- aov(Tiempo ~ Area, data = datos_preguntas)
print(summary(prueba_anova))

# En este caso tenemos el valor del estad�stico F: 29.83
# tambi�n obtenemos el 'p-value' que es 4.49e-13.

# Vemos que el p-valor=4.49e-13 es claramente muy peque�o en comparaci�n a alpha=0.05, por lo que 
#se rechaza la hipotesis nula en favor de la hipotesis alternativa, concluyendo con un 95% de 
#confianza que la media del tiempo que tardan los usuarios en formular una consulta para un problema 
#de dificultad dif�cil es diferente en por lo menos en una de las areas estudiadas.

# Buscamos la mayor diferencia haciendo un an�lisis POST-HOC, utilizando
# la funci�n TukeyHSD ya que entrega la informaci�n tabulada y se
# analiza cada caso directamente.

tuck <- TukeyHSD(prueba_anova,
              'Area',
              conf.level = 1 - alpha)

print(tuck)

# Como vemos, Musica-Matematica son los �nicos grupos que muestran una diferencia significativa
# en las medias, seg�n el m�todo post-hoc de Tukey;
# descartamos las combinaciones Psicologia-Matematicas y Psicolog�a-Musica
# ya que debemos recordar  que las columnas lwr y upr representan el intervalo
# de confianza dado para las diferencias entre las medias,
# y se muestra que para estas �ltimas dos combinaciones, los intervalos consideran
# n�meros negativos, lo que no tiene sentido para considerar un intervalo de confianza.

# Finalmente, concluimos de manera global:
# Existe una diferencia significativa en el tiempo que toma un usuario
# en formular una pregunta dependiendo del nivel de dificultad y el �rea
# tem�tica de esta pregunta (usando el test ANOVA).
# Ahora, sabemos cu�l es la diferencia, gracias al an�lisis POST-HOC utilizando el m�todo de Tukey,
# que nos dice que esta diferencia en tiempos promedio se encuentra en las �reas de m�sica y matem�tica.
