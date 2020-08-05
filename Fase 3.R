#Importar datos de historico de Intensidad Lluvia y estado del cultivo
#install.packages("triangle")
#install.packages("reshape2")
#install.packages("ggplot2")
library(triangle)
library(EnvStats)
library(readxl)
library(markovchain)
library(dplyr)
library(reshape2)
library(ggplot2)

historicoLluvia <- read_excel("Historial_Grass.xlsx", sheet = 2)
colnames(historicoLluvia) <- c("Dia","Intensidad")
historicoCultivo <- read_excel("Historial_Grass.xlsx", sheet = 1)
colnames(historicoCultivo) <- c("Fecha","Estado")

discretizar <- historicoLluvia %>%
  mutate(Nivel = case_when(
    Intensidad > 350 ~"GG",
    Intensidad <= 350 & Intensidad > 250 ~ "TyG",
    Intensidad <= 250 & Intensidad > 100 ~ "T",
    Intensidad <= 100 & Intensidad > 40 ~ "MF",
    Intensidad <= 40 & Intensidad > 16 ~ "F",
    Intensidad <= 16 & Intensidad > 6.5 ~ "M",
    Intensidad <= 6.5 & Intensidad > 2.5 ~ "L",
    Intensidad <= 2.5 & Intensidad > 1 ~ "D",
    Intensidad <= 1 & Intensidad > 0.4 ~ "MD",
    Intensidad <= 0.4 & Intensidad >= 0 ~ "N",
  )
  )
#Probabilidades de transición entre estados de Nivel de precipitación según historico
matFrec <- markovchainFit(data = discretizar$Nivel, byrow = T)
PLluvia = matFrec$estimate@transitionMatrix
matrix(PLluvia, nrow=10, ncol=10)
estadosLluvia <- c("GG","TyG","T","MF","F","M","L","D","MD","N")
rownames(PLluvia) <- estadosLluvia
colnames(PLluvia) <- estadosLluvia
rowSums(PLluvia)
PLluvia

#Probabilidades de transición entre estados del cultivo según historico
matFrecEstado <- markovchainFit(data = historicoCultivo$'Estado', byrow = T)
PEstado = matFrecEstado$estimate@transitionMatrix
matrix(PEstado, nrow=5, ncol=5)
estadosCultivo <- c("1","2","3","4","5")
rownames(PEstado) <- estadosCultivo
colnames(PEstado) <- estadosCultivo
rowSums(PEstado)
PEstado

#
estados = expand.grid(estadosCultivo,estadosLluvia)
estados<-apply(estados, 1,paste, collapse= ";")

mConjunta=matrix(0,ncol = length(estados), nrow= length(estados))
dimnames(mConjunta)=list(estados, estados)
for(fila in estados)
{
  eF=as.numeric(strsplit(fila,";")[[1]][1])
  lF=as.character(strsplit(fila,";")[[1]][2])
  for(col in estados)
  {
    eC=as.numeric(strsplit(col,";")[[1]][1])
    lC=as.character(strsplit(col,";")[[1]][2])
    #Probabilidades conjuntas
    mConjunta[fila,col] = PEstado[eF,eC]*PLluvia[lF,lC]
  }
}
rowSums(mConjunta)

#
##¿Qué pasa si no se interviene?
#Estado del cultivo y de la lluvia se saca del histórico de los datos
#Se crean las matrices de 50x50

##¿Qué pasa si se interviene?
#Estado del cultivo se estima con una triangular y el de la lluvia del histórico de los datos
#Se crean las matrices 50x50

#240 matrices dado que es una para cada día con 4 alternativas de decisión

#Se necesita una función que cree las matrices de la lista

dias <- 1:60
A <- c(0,1,2,3)
names(A) <- c("N.I","Fumigar","Regar","Abonar")
nombreslista <- expand.grid(dias,A)
nombreslista <- apply(nombreslista,1,paste,collapse= ";")
lista <- vector("list",240)
names(lista) <- nombreslista

estadoinicial <- "5;MD"

for(k in nombreslista){
  dia = as.numeric(strsplit(k, ";")[[1]][1])
  alt = as.numeric(strsplit(k, ";")[[1]][2])
  lista[[k]] <- matrix(0,nrow=50,ncol=50)
  dimnames(lista[[k]]) <- list(estados,estados)
  for(i in estados){
    for(j in estados){
      culti = as.numeric(strsplit(i, ";")[[1]][1])
      cultj = as.numeric(strsplit(j, ";")[[1]][1])
      lluviai = as.character(strsplit(i, ";")[[1]][2])
      lluviaj = as.character(strsplit(j, ";")[[1]][2])
      if(alt == 0){
        lista[[k]][i,j] <- mConjunta[i,j]
      }
      else{
        moda <- min(5,(3-alt)*culti*dia/60)
        valor <- ptriangle(cultj,a=0,b=5,c=moda)-ptriangle(cultj-1,a=0,b=5,c=moda)
        lista[[k]][i,j] <- valor*PLluvia[lluviai,lluviaj]
      }
    }
  }
}

retornos <- matrix(0,nrow=50,ncol=61)
dimnames(retornos) <- list(estados,1:61)
for (estado in estados){
  cultivo = as.numeric(strsplit(estado, ";")[[1]][1])
  lluvia = as.character(strsplit(estado, ";")[[1]][2])
  
  if (cultivo == 1){
    retornos[estado,61] = 20
  }
  if (cultivo == 2){
    retornos[estado,61] = 40
  }
  if (cultivo == 3){
    retornos[estado,61] = 60
  }
  if (cultivo == 4){
    retornos[estado,61] = 80
  }
  if (cultivo == 5){
    retornos[estado,61] = 100
  }
}

Ft <- matrix(0,nrow=50,ncol=61) #Ecuaciones de Bellman
dimnames(Ft) <- list(estados,1:61)
Dt <- matrix(0,nrow=50,ncol=60) #Decisiones óptimas
dimnames(Dt) <- list(estados,dias)

Ft[,61] <- retornos[,61] #Retorno de la última época (época fantasma)

vaux = 0
ret = 0
for (dia in 60:1){ #Retornos para las demás épocas
  for (alt in A){
    for (est in estados){
      vaux <- sum(lista[[paste(dia,alt,sep=";")]][est,])
      ret <- 0 + vaux*Ft[est,dia+1]
      if (ret >= Ft[est,dia]){ #Se comprueba la optimalidad de la alternativa de decisión
        Ft[est,dia] = ret
        Dt[est,dia] = alt
      }
    }
  }
}

View(Ft)
View(Dt)

heatmap_dec = melt(Dt)
heatmap_dec$value = as.factor(heatmap_dec$value)

ggplot(data=heatmap_dec,aes(x=Var2,y=Var1,fill=value))+geom_tile()+xlab('Día')+ylab('Estado')
