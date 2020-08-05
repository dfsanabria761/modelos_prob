#Importar datos de semanas entre siembras
library(markovchain)
library(Matrix)

grass_1<-function(lambda, miu, alpha, ingresos, costos, nombre, t, color){
  #Matriz de tasas de transici?n Q
  estadosS=c(0:24) #{0, 1, 2, 3, ..., 24}
  piCero <- c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) #??(0)
  piCero <- c(rep(0,8),1,rep(0,16))
  #inicializar la matriz de probabilidades de transici?n en cero
  Q=matrix(0,nrow = 25,ncol= 25)
  dimnames(Q)<-list(estadosS,estadosS)
  #Se recorren las filas y columnas de la matriz para llenarla de acuerdo a la formulaci?n general 
  
  #llenar la matriz utilizando la formulaci?n general
  for (i in estadosS) 
  {
    for (j in estadosS) 
    {
      if(j==i+1 & i<24)
      {
        Q[i+1,j+1] = lambda
      }
      if(j==i-1 & i>1)
      {
        Q[i+1,j+1] = miu
      }
      if(j==i-2 & i>1)
      {
        Q[i+1,j+1] = alpha
      }
      if(j==i-1 & i==1 ) 
      {
        Q[i+1,j+1] = miu + alpha
      }
    }
  }
  diag(Q) = -rowSums(Q) 
  rowSums(Q) #Verificamos que cada fila de la matriz sume 0

  #CONSTRUIR CADENA DE MARKOV
  CMTC <- new(Class="ctmc", states = as.character(estadosS),
                   byrow = TRUE, generator = Q)

  #Exponencial de la matriz Qt
  library(expm)
  
  #Estimaci?n del valor esperado y la varianza en 2 meses (8 semanas)
  pi8 <- piCero%*%expm(Q*8)
  esp8 <- sum(pi8*estadosS)
  espplantas8 = esp8 * 250
  var8 <- (sum(pi8*(esp8-estadosS)^2))/25
  varplantas8 = var8 * 250

  #An?lisis transitorio a las t semanas
  vectorEsperado <- rep(0, times= t)
  tiempo <- seq(from=1, to = t, by = 1 )
  for (i in (1:t)) 
  {
    #??(t)=??(0)exp(Qt)
    probs=piCero%*%expm(Q*(i)) #Vector de Probabilidades de la semana t (probabilidades de que dado el estado inicial en t tenga j cultivos)
    #Valor esperado de lotes
    lotesEsperados<-sum(probs*(estadosS)) #Valor esperado de cultivos viables en el tiempo t
    #lotesEsperados_CunCa
    vectorEsperado[i] = lotesEsperados 
  }
  vectorEsperado = vectorEsperado *250
  plot(tiempo,vectorEsperado,type = "b",col=color, main = nombre, ylab = "N?mero promedio de plantas sanas", xlab = "Tiempo (semanas)")
  
  #An?lisis en el largo plazo, es decir, alcanzado el estado estable en el tiempo.
  piES <- steadyStates(CMTC)
  espES <- sum(piES*estadosS)
  espplantasES = espES * 250
  varES <- (sum(piES*(espES-estadosS)^2))/25
  varplantasES = varES * 250
  ingresosFinales = espplantasES * ingresos
  utilidad = ingresosFinales - costos
  return(ingresosFinales/costos)
}
grass_2<-function(lambda, miu, alpha, ingresos, costos, nombre, t, color){
  #Matriz de tasas de transici?n Q
  estadosS=c(0:24) #{0, 1, 2, 3, ..., 24}
  piCero <- c(0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0) #??(0)
  piCero <- c(rep(0,8),1,rep(0,16))
  #inicializar la matriz de probabilidades de transici?n en cero
  Q=matrix(0,nrow = 25,ncol= 25)
  dimnames(Q)<-list(estadosS,estadosS)
  #Se recorren las filas y columnas de la matriz para llenarla de acuerdo a la formulaci?n general 
  
  #llenar la matriz utilizando la formulaci?n general
  for (i in estadosS) 
  {
    for (j in estadosS) 
    {
      if(j==i+1 & i<24)
      {
        Q[i+1,j+1] = lambda
      }
      if(j==i-1 & i>1)
      {
        Q[i+1,j+1] = miu
      }
      if(j==i-2 & i>1)
      {
        Q[i+1,j+1] = alpha
      }
      if(j==i-1 & i==1 ) 
      {
        Q[i+1,j+1] = miu + alpha
      }
    }
  }
  diag(Q) = -rowSums(Q) 
  rowSums(Q) #Verificamos que cada fila de la matriz sume 0
  
  #CONSTRUIR CADENA DE MARKOV
  CMTC <- new(Class="ctmc", states = as.character(estadosS),
              byrow = TRUE, generator = Q)
  
  #Exponencial de la matriz Qt
  library(expm)
  
  #Estimaci?n del valor esperado y la varianza en 2 meses (8 semanas)
  pi8 <- piCero%*%expm(Q*8)
  esp8 <- sum(pi8*estadosS)
  espplantas8 = esp8 * 250
  var8 <- (sum(pi8*(esp8-estadosS)^2))/25
  varplantas8 = var8 * 250
  
  #An?lisis transitorio a las t semanas
  vectorEsperado <- rep(0, times= t)
  tiempo <- seq(from=1, to = t, by = 1 )
  for (i in (1:t)) 
  {
    #??(t)=??(0)exp(Qt)
    probs=piCero%*%expm(Q*(i)) #Vector de Probabilidades de la semana t (probabilidades de que dado el estado inicial en t tenga j cultivos)
    #Valor esperado de lotes
    lotesEsperados<-sum(probs*(estadosS)) #Valor esperado de cultivos viables en el tiempo t
    #lotesEsperados_CunCa
    vectorEsperado[i] = lotesEsperados 
  }
  vectorEsperado = vectorEsperado *250
  
  #An?lisis en el largo plazo, es decir, alcanzado el estado estable en el tiempo.
  piES <- steadyStates(CMTC)
  espES <- sum(piES*estadosS)
  espplantasES = espES * 250
  varES <- (sum(piES*(espES-estadosS)^2))/25
  varplantasES = varES * 250
  ingresosFinales = espplantasES * ingresos
  utilidad = ingresosFinales - costos
  plot(x = utilidad, y = 1)
  return(ingresosFinales/costos)
}