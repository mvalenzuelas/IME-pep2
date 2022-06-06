library(dplyr)
library(ggpubr)
library(boot)
library(ez)
set.seed(1234)
datos <- read.csv2("EP11 Datos.csv")
muestra <- sample_n(datos,450)
#Se desea estudiar el nivel de ingreso de los Chilenos dependiendo su nivel de 
# educación, en particular, se desea determinar si existe una diferencia entre 
# la media de los ingresos percapita de jefes de hogares que cuenten con un 
# titulo profesional y un titulo tecnico completos


#H0: La diferencia entre los promedios de personas con titulo profesional completo
#    y las personas con titulo tecnico superior completo es igual a 0

#Ha: La diferencia entre los promedios de personas con titulo profesional completo
#    y las personas con titulo tecnico superior completo es distinta a 0

#H0: MUp-MUt  =  0
#H1: MUp-MUt !=  0

tecnicos <- muestra %>% filter(educ ==  "Técnico Nivel Superior Completo")
profesional <- muestra %>% filter(educ ==  "Profesional Completo")

tecnicos <- tecnicos %>% select(id.vivienda,ytotcorh)
profesional <- profesional %>% select(id.vivienda,ytotcorh)


obtener_permu <- function(i, muestra1, muestra2){
  largoM1 <- length(muestra1)
  combinada <- c(muestra1,muestra2)
  n <- length(combinada)
  permutacion <- sample(combinada, n , replace = FALSE)
  nueva1 <- permutacion[1:largoM1]
  nueva2 <- permutacion[(largoM1+1) : n]
  return (list(nueva1,nueva2))
}


obtener_diferencias <- function(muestras, funcion){
  muestra1 <- muestras[[1]]
  muestra2 <- muestras[[2]]
  diferencia <- funcion(muestra1) - funcion(muestra2)
  return(diferencia)
}

calcularPValor <- function(distribucion, cantPermu, alternative, valorObservado){
  if (alternative == "two.sided"){
    numerador <- sum(abs(distribucion) > abs(valorObservado)) + 1
    denominador <- cantPermu + 1
    valorP <- numerador/denominador
  }
  else if (alternative == "greater"){
    numerador <- sum(distribucion > valorObservado) + 1
    denominador <- cantPermu + 1
    valorP <- numerador/denominador
  }
  else {
    numerador <- sum(distribucion < valorObservado) + 1
    denominador <- cantPermu + 1
    valorP <- numerador/denominador
  }
  
  return(valorP)
}



n_repeticiones <- 499
nPermutaciones <- lapply(1:n_repeticiones,
                         obtener_permu,
                         tecnicos$ytotcorh,
                         profesional$ytotcorh)

distribucionesPermu <- sapply(nPermutaciones,obtener_diferencias,
                              funcion = mean)

observado <- obtener_diferencias(list(tecnicos$ytotcorh,profesional$ytotcorh),
                                 funcion = mean)

#Considerando alfa de valor 0.05
shapiro1 <- shapiro.test(distribucionesPermu)

#La muestra sigue una distribución normal

p_valor <- calcularPValor(distribucionesPermu,
                          n_repeticiones,
                          "two.sided",
                          observado)



distribucionesPermuV <- sapply(nPermutaciones,
                               obtener_diferencias,
                               funcion = var)


observadoV <- obtener_diferencias(list(tecnicos$ytotcorh,profesional$ytotcorh),
                                 funcion = var)

#Considerando alfa de valor 0.05
shapiro2 <- shapiro.test(distribucionesPermuV)

#La muestra sigue una distribución normal

p_valorV <- calcularPValor(distribucionesPermuV,
                          n_repeticiones,
                          "two.sided",
                          observadoV)

#Con un nivel de significancia del 0.05 existe suficiente evidencia para 
# rechazar la hipotesis alternativa, en favor de la hipotesis nula. Por ende
# se puede concluir que no existe una diferencia en la media de los ingresos
# de las personas con titulo profesional completo y las personas con titulo
# tecnico superior completo

#Pregunta 2: existe diferencia entre la media de los ingresos per capita entre 
#los 3 estados civiles (Separado(a), Viudo(a), Divorciado (a))

#Definir hipotesis
#H0: Los ingresos de las personas solteras, casadas y viudas son los mismos
#H1: Los ingresos de las personas solteras, casados y viudas difieren en al 
# menos un estado civil
#Cargar nuevos datos
datos <- read.csv2("EP11 Datos.csv")
set.seed(1122673)

#Seleccionar una muestra
datos <-sample_n(datos,500)

#Desde la muestra filtrar segun estado civil y seleccionar el id de la vivienda
#el estado civil y sus ingresos
Separados <- datos %>% filter(ecivil == "Separado(a)")
Viudos <- datos %>% filter(ecivil == "Viudo(a)")
Divorciados <- datos %>% filter(ecivil == "Divorciado (a)")
Separados <- Separados %>% select(id.vivienda,ecivil, ytotcorh)
Viudos <- Viudos %>% select(id.vivienda,ecivil, ytotcorh)
Divorciados <- Divorciados %>% select(id.vivienda,ecivil, ytotcorh)

#Crear un dataframe con los 3 grupos
df<-rbind(Separados,Viudos)
df<-rbind(df,Divorciados)
df[['ecivil']]<-factor(df[['ecivil']])

#Calcular anova del dataframe para determinar si las medias de los estados civiles
#de la muestras son iguales
anova<-ezANOVA(data = df,
        dv=ytotcorh,
        between = ecivil,
        wid = id.vivienda,
        type=3,
        return_aov = TRUE)


cat("p-value con anova:",anova$ANOVA$p)
#Existe suficiente evidencia para rechazar la hipotesis alternativa en favor de
# la hipotesis nula, con un nivel de significancia del 0.05 por ende se puede 
# establece que no hay diferencia entre los ingresos promedios de las muestras

#Implementar post-hoc como se solicita en el enunciado usando boostrap

#Función que permite generar la distribución boostrap apartir de 3 muestras
threeBoot<-function(i,muestra1,muestra2,muestra3){
  n1<-nrow(muestra1)
  n2<-nrow(muestra2)
  n3<-nrow(muestra3)
  sm1<-sample_n(muestra1,n1,replace = TRUE)
  sm2<-sample_n(muestra2,n2,replace = TRUE)
  sm3<-sample_n(muestra3,n3,replace = TRUE)
  muestra<-rbind(sm1,sm2)
  muestra<-rbind(muestra,sm3)
  muestra[['ecivil']]<-factor(muestra[['ecivil']])
  return(muestra)
}

#Funcion que permite calcula el estadistico F a una muestra usando anova
estadistico<-function(muestra){
  anova<-ezANOVA(data = muestra,
                 dv=ytotcorh,
                 between = ecivil,
                 wid = id.vivienda,
                 type=3,
                 return_aov = FALSE)
  return(anova[['ANOVA']][['F']])
  
}

#Funcion que permite calcular la diferencias de las medias de dos estados civiles
# para cada remuestreo generado
obtener_diferencia_boostrap <- function(boostrap, estadoCivil1, estadoCivil2){
  n <- length(boostrap)
  diferencias<- c()
  for(i in 1:n){
    df <- as.data.frame(boostrap[i])
    muestra_ec1 <- df %>% filter(ecivil==estadoCivil1)
    muestra_ec2 <- df %>% filter(ecivil==estadoCivil2)
    diferencias <- c(diferencias,mean(muestra_ec1[['ytotcorh']])-mean(muestra_ec2[['ytotcorh']]))
  }
  return(diferencias)
}

#Definir la cantidad de remuestreos de cada variable
R<-100

#Obtener F de Fisher del anova para utilizarlo como estadistico de prueba
fisher<-anova[['ANOVA']][['F']]

#Generar la distribución boostrap
boostrap<-lapply(1:R,threeBoot,Divorciados,Viudos,Separados)

#Calcular el estadistico F de cada una de los remuestreos
listaF<-sapply(boostrap, estadistico)


#Calcular p-valor de la distribución boostrap a partir del estadistico F
prueba<-calcularPValor(listaF,R,fisher,"two.sided")
cat("p-value con boostrap:",prueba)

#El p-valor obtenido es mayor al nivel de siginificancia 0.05 pero se calcula 
# se realiza el analicis post-hoc de todas formas

#Encontrar la diferencia entre los distintos estados civiles
dif_separado_divorciado<-mean(Separados$ytotcorh)-mean(Divorciados$ytotcorh)

dif_separado_divorciado_boostrap<-obtener_diferencia_boostrap(boostrap,"Separado(a)","Divorciado (a)")

p_separado_divorciado<-(sum(abs(dif_separado_divorciado_boostrap)>abs(dif_separado_divorciado))+1)/(R+1)



dif_separado_viudo<-mean(Separados$ytotcorh)-mean(Viudos$ytotcorh)

dif_separado_viudo_boostrap<-obtener_diferencia_boostrap(boostrap,"Viudo(a)","Separado(a)")

p_separado_viudo<-(sum(abs(dif_separado_viudo_boostrap)>abs(dif_separado_viudo))+1)/(R+1)


dif_viudo_divorciado<-mean(Viudos$ytotcorh)-mean(Divorciados$ytotcorh)

dif_viudo_divorciado_boostrap<-obtener_diferencia_boostrap(boostrap,"Viudo(a)","Divorciado (a)")

p_viudo_divorciado<-(sum(abs(dif_viudo_divorciado_boostrap)>abs(dif_viudo_divorciado))+1)/(R+1)

cat("p-value separados-divorciado: ",p_separado_divorciado)
cat("p-value separados-viudo: ",p_separado_viudo)
cat("p-value viudo-divorciado: ",p_viudo_divorciado)

#Como muestra el procedimiento post-hoc; los valores p de las diferencias entre
# las medias de los ingresos  de personas con estados civiles separado, viudos 
# y divorciados indican que no hay diferencia en entre estos. Tal como indico
# anova











