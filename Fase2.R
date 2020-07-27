library(markovchain)
library(Matrix)
library(expm)

##Par?metros
#Tiempos de procesamiento de cada una de las estaciones de la planta de procesamiento por lote (semanas)

grass<- function(tiemposRT, tiempoSAL, tiempoSO, tiempoAS ) {
  

#Tasas de procesamiento en lotes por semana
tasaRT = 1/tiemposRT
tasaSAL = 1/tiempoSAL
tasaSO = 1/tiempoSO
tasaAS = 1/tiempoAS

#Matriz de tasas Q en Ibague
QProcesamiento = matrix(c(-0.84*tasaRT-0.16*tasaRT,(0.84*tasaRT),0,0,(0.16*tasaRT),0,
               0,-tasaSAL,tasaSAL,0,0,0,
               0,0,-tasaSO,tasaSO,0,0,
               0,0,(0.46*tasaAS),-0.46*tasaAS-0.54*tasaAS,0,(0.54*tasaAS),
               0,0,0,0,0,0,
               0,0,0,0,0,0),nrow=6,ncol=6,byrow = TRUE)
estadosQ = c("RT","SAL","SO","AS","D","S")
dimnames(QProcesamiento) = list(estadosQ,estadosQ)

#U  es la submatriz de las tasas de transici?n entre estados transitorios
estadosAbsorbentes = c("D","S")
estadosTransitorios = c("RT","SAL","SO","AS")
matrizU = matrix(0, nrow=4,ncol=4)
dimnames(matrizU)=list(estadosTransitorios, estadosTransitorios)
matrizU = QProcesamiento[estadosTransitorios, estadosTransitorios]

#Tiempo en salir del procesamiento empezando en la estaci?n Remoci?n del tallo
matrizTiemposAbsorcion=-solve(matrizU)
TiempoAbsorcionSalidaSemanas= sum(matrizTiemposAbsorcion["RT",])
TiempoAbsorcionSalidaHoras=TiempoAbsorcionSalidaSemanas*5*8
TiempoLlegada = TiempoAbsorcionSalidaHoras + 4 #4 horas en transporte
T = TiempoLlegada
##Tasa de entrada al centro de acopio 
TasaEntradaBodegaHoras = 1/TiempoLlegada
TasaEntradaDias = TasaEntradaBodegaHoras*8

#Matriz P de probabilidades de transici?n
estadosB=c(0:25)
matrizP=matrix(0,nrow = 26,ncol= 26)
dimnames(matrizP)<-list(estadosB,estadosB)

#Se llena la matriz P utilizando la formulaci?n general
for (i in estadosB) 
{
  for (j in estadosB) 
  {
    if(i <= 15 & j == 25)
    {
      matrizP[i+1,j+1] = ppois(q = 25, lambda = TasaEntradaDias, lower.tail = FALSE) #P[x ??? X]
      
    }
    if(i <= 15 & j < 25)
    {
      matrizP[i+1,j+1] = dpois(x = j, lambda = TasaEntradaDias) #P[X = x]
    }
    #
    if(i > 15 & j == 25)
    {
      matrizP[i+1,j+1] = ppois(q = 40 - i, lambda = TasaEntradaDias, lower.tail = FALSE) #P[x ??? X]
      
    }
    if(i > 15 & j < 25 & j >= i-15)
    {
      matrizP[i+1,j+1] = dpois(x = j-i+15, lambda = TasaEntradaDias) #P[X = x]
    }
  }
}
matrizP
rowSums(matrizP)

#N?mero de veces en una semana laboral que el cami?n despacha menos de 15 lotes
#Sabiendo que la bodega inicia vac?a
diaslaborales = 5
veces = 0
matrizM = matrix(0, nrow=26,ncol=26)
dimnames(matrizM)<-list(estadosB,estadosB)
for(i in 0:diaslaborales){
  matrizM = matrizM + (matrizP%^%i)
}
for(j in 1:15){
  veces = veces + matrizM[1,j] 
}
veces = veces-1 #No se tiene en cuenta un d?a 0 de despacho.
veces
rowSums(matrizM) #Se comprueba que est? bien la matriz de ocupaci

#En promedio, cada cu?nto tiempo el centro de acopio llega a su capacidad m?xima
cmtd <- new(Class="markovchain",states=as.character(estadosB),byrow = TRUE, transitionMatrix=matrizP)
matriztiempomax = meanFirstPassageTime(cmtd)
tiempomax = matriztiempomax[1,26]
tiempomax = tiempomax/(20*12)
tiempomax #En a?os laborales

#Tiempo que tarda un lote en ser procesado hasta que es entregado al cliente final
##Trabajamos bajo el supuesto de que el cami?n (cliente) pasa por el centro de acopio 8 horas despu?s de que llega el lote.
##Sabemos que todo lote que llega un d?a puede ser despachado ese mismo d?a, dado que ning?n d?a de la semana el cami?n llena su capacidad m?xima.
TiempoFinal = TiempoLlegada + 8
TiempoFinal

#C?lculo de la utilidad por hora LABORAL
ingresosporhoralaboral = 37265*(1/TiempoFinal)
egresosporhoralaboral = 10570*(1/TiempoLlegada)
utilidadporhoralaboral = ingresosporhoralaboral - egresosporhoralaboral
utilidadporhoralaboral
utilidadpordialaboral = utilidadporhoralaboral * 8
utilidadporsemanalaboral = utilidadpordialaboral * 5
utilidadpormeslaboral = utilidadpordialaboral * 20
utilidadpormeslaboral
utilidadporaniolaboral = utilidadpormeslaboral * 12
utilidadporaniolaboral
res <- c("veces"=veces, "tiempoBase"=round(TiempoFinal, digits = 2), "tiempoMaximo"=round(tiempomax, digits = 2), "utilidad"=utilidadporhoralaboral)
return(res)
##Recordar que 1 d?a tiene 8 horas laborales, 1 semana tiene 5 d?as laborales y 1 mes tiene 20 d?as laborales.
}
