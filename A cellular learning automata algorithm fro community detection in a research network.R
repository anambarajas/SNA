#1 Construccion de la red social

# Librerias a usar

library("readxl")
library("igraph")
library("dplyr")
library("openxlsx")


#Importar datos de excel y asignarlos a un data frame

Articulos_1161 <- read_excel("C:/Users/baraj/Dropbox/TG/Libro/Documento libro/TRABAJO DE GRADO/Apéndices/Apéndice B Articulos 1161.xlsx", col_names = FALSE)
autores<-data.frame(Articulos_1161)
lista_arcos<-data.frame(autor=character(),coautor=character())
m<-ncol(autores)


#Crear la lista de enlaces existentes en  la red

for(i in 1 :(m-1)) {
  for(j in (i+1):m) {
    lista_arcos<-rbind(lista_arcos,
                       data.frame(autor=autores[,i],coautor=autores[,j])) 
  } 
}

lista_arcos<-lista_arcos[!is.na(lista_arcos[,2]),]
lista_arcos<- lista_arcos[!duplicated(lista_arcos), ]

lista_arcos$autor <-as.character(lista_arcos$autor)
lista_arcos$coautor <-as.character(lista_arcos$coautor)

a<-nrow(lista_arcos)

for (i in 1:a) { lista_arcos[i,]<-sort(lista_arcos[i,]) }

lista_arcos<- lista_arcos[!duplicated(lista_arcos), ]

fila<-nrow(lista_arcos)

for(i in 1:fila){if ((lista_arcos[i,1]==lista_arcos[i,2])==TRUE){
  lista_arcos[i,1]<-0} }
lista_arcos <-lista_arcos[ !(lista_arcos$autor %in% c("0")), ]


grafoautores <- graph_from_data_frame(lista_arcos, directed = FALSE)


# Graficar red de enlaces y generar la matriz de adyacencia del grafo

plot(grafoautores, vertex.size = 0.01, edge.size = 20)
mat_a <-data.frame(get.adjacency(grafoautores, sparse=FALSE))
n<-nrow(mat_a)
r_i<-degree(grafoautores)
V<-(names(mat_a))
E<-lista_arcos


#2 Algoritmo CLA-Net

#INPUT

#mat_a = A_nxn
#Set of nodes=V
#Set of edges=E
#Numero de nodos en la red=n=|V|
#Aij=1 si existe un enlace y Aij=0 si los autores no estan conectados
#mat_a matriz de adyacencia
#Pij: matriz de probabilidad


Pij<-data.frame(get.adjacency(grafoautores, sparse=FALSE))
Qbest <- 0
colnames(Pij)<- 1:n
rownames(Pij)<- 1:n


# PARAMETERS

#a_recomp: parametro de recompensa, cuanto se aumenta la probabilidad de la accion optima
a_recomp <- 0.2
# establecer numero de iteraciones para la inicializacion
inicio <-5

# VARIABLES

# r_i: Numero de acciones para cada LA, grado del nodo i.
# Wij: Numero de veces que la accion j se ha recompensado por el LA i.
# Zij: Numero de veces que la accion j se ha elegido por i hasta el ciclo t.
# Dij: Estimador de la accion j para el LA i
# Bi: Variable de castigo o recompensa
# Q: Modularidad en el ciclo t
# Qbest: Mejor modularidad hasta el ciclo t


# Crear matriz de acciones para cada LA  

vecinos <- data.frame(numeric())

#Crear matriz de vecinos para la seleccion de acciones de cada LAi

matady<-mat_a==0

for (i in 1:n) {for (j in 1:n) { ifelse(matady[i,j], vecinos[i,j] <- NA, vecinos[i,j] <- j )  }}
colnames(vecinos)<- 1:n
rownames(vecinos)<- 1:n


#Crear vector de probabilidad uniforme para las primeras iteraciones

prob_unif <- function(Pij) {
Pij <- Pij/r_i
return(Pij)
}
Pij <- prob_unif(Pij)

#Seleccionar el LA de acuerdo a las probabilidades uniformes de la matriz una cantidad de veces(),calcular W,Z y Qbest

accion_i<-matrix(data=NA, nrow = n, ncol = 1, byrow = TRUE)
rownames(Pij)<- 1:n
Zij<-matrix(0,nrow=n,ncol=n)
Wij <- matrix(0,nrow=n,ncol=n)
t<-0

#semilla <- sample(1:1000, 1)
set.seed(1)

repeat { 
  t<-t+1
  print(t)
  
  #Seleccionar el LA de acuerdo a las probabilidades uniformes de la matriz
  
  for (i in 1:n) {
    
 
    accion_i[i] <- sample(vecinos[i,],1,replace=FALSE, Pij[i,])
  }
  
  #Guardar acciones y generar grafo de las comunidades
  
  nodo<-c(1:n)
  accion<-data.frame((accion_i))
  accion<-t(accion)
  accion_selec<-as.matrix(data.frame(accion,nodo))
  
  grafoacciones <- graph_from_edgelist(accion_selec, directed = FALSE)
  plot(grafoacciones, vertex.size = 2)
  caminos<-matrix(0,nrow=n,ncol=1)
  ct<-matrix(0,nrow=1,ncol=n)
  c<-1
  
  #Decodificar la solucion en vector comunidad usando Depth-First Search

  
  repeat {
    nodoi <- which(ct==0)[1]
    caminos<-matrix(dfs(grafoacciones, 411, unreachable = FALSE)$order)
    for(j in 1:n){
      if (is.na(caminos[j])==TRUE){
        ct[j]<-ct[j]}else {ct[caminos[j]]<-c}
    }
    c<-c+1
    
    if(length(which(ct==0))==0){break}  
  }
  
  #Calcular la modularidad de la estructura de comunidad obtenida
  
  Q <- double(length = 1L)
  Q<- modularity(grafoautores, ct, weights = NULL)
 
  
  cmax<-max(ct)
  ki_in<-matrix(0,nrow=1,ncol=n)
  ki_out<-matrix(0,nrow=1,ncol=n)
  Bi<-matrix(0,nrow=1,ncol=n)
  
  #Validar condiciones de Raghavan y mejor modularidad para determinar castigo o recompensa
  
  for (c in 1:cmax) {
    for(i in 1:n){
      for(j in 1:n){
        if(ct[i]==c){
          if(ct[j]==c){
            ki_in[i]<-ki_in[i]+mat_a[i,j]}
        }
      }
      ki_out[i]<- r_i[i] - ki_in[i]
      if  (ki_in[i]>=ki_out[i] & Q>=Qbest) {Bi[i]<-0} else {Bi[i]<-1}
    } 
  }
  
  #Determinar la mejor modularidad hasta el momento
  
  Qbest <- max(Qbest, Q)
  
  #Calcular Z y W para las acciones elegidar por los LAi
  
  for(i in 1:n){
    for(j in 1:n){
      if (accion[i]==j){ Zij[i,j]<-Zij[i,j]+1}
    }
  }
  for (i in 1:n) { 
    for (j in 1:n) { if (accion[i] == j)
      {Wij[i,j] <- Wij[i,j] + (1- Bi[i])}
    }
  }
  if (t==inicio) { break }
}


Comunidad_Total<-data.frame(numeric())
converg <- 0
min_conv<-5

#Funcion de probabilidad para las acciones no-optimas

prob_nopt <- function(Pij) {
  
  Pij <- (1-a_recomp)*Pij
  return(Pij)
}


# Ciclos con un Qbest, Zij y Wij inicial

t<-0
Q_t <- data.frame(numeric())
Bi_t <- data.frame(numeric())
accion_t <- data.frame(numeric())
am_t <- data.frame(numeric())

repeat{
  t<-t+1
  print(t)
  
  
  #Elegir la accion actual de cada LAi a partir de las probabilidades
  
  for (i in 1:n) {
    accion_i[i] <- sample(vecinos[i,],1,replace=FALSE, Pij[i,])
  }
  
  #Guardar acciones y generar grafo de las comunidades
  
  accion<-data.frame(accion_i)
  accion_t<-rbind(accion_t,accion_i)
  colnames(accion_t)<- 1:n
  accion<-t(accion)
  nodo<-c(1:n)
  accion_selec<-as.matrix(data.frame(accion,nodo))
  rownames(accion_selec) <- 1:n
  grafoacciones <- graph_from_edgelist(accion_selec, directed = FALSE)
  plot(grafoacciones, vertex.size = 2)
  caminos<-matrix(0,nrow=n,ncol=1)
  ct<-matrix(0,nrow=1,ncol=n)
  c<-1
  
  #Decodificar la solucion en vector comunidad usando Depth-First Search
  
  repeat {
    nodoi <- which(ct==0)[1]
    caminos<-matrix(dfs(grafoacciones, nodoi, unreachable = FALSE)$order)
    for(j in 1:n){
      if ((is.na(caminos[j]))==TRUE){
        ct[j]<-ct[j]}else {ct[caminos[j]]<-c}
    }
    c<-c+1
    
    if(length(which(ct==0))==0){break}  
  }
  
  #Agregar vector de comunidades al vector de comunidades acumulativo para las iteraciones
  Comunidad_Total<-rbind(Comunidad_Total,ct)
  
  #Calcular la modularidad actual
  
  Q<- modularity(grafoautores, ct, weights = NULL)
  
  #Agregar modularidad actual al vector de modularidad acumulativa para las iteraciones
  Q_t <- rbind(Q_t, Q)
  
  #Verificar las condiciones para determinar el castigo o recompensa
  
  cmax<-max(ct)
  ki_in<-matrix(0,nrow=1,ncol=n)
  ki_out<-matrix(0,nrow=1,ncol=n)
  Bi<-matrix(0,nrow=1,ncol=n)

  
  for (c in 1:cmax) {
    for(i in 1:n){
      for(j in 1:n){
        if(ct[i]==c){
          if(ct[j]==c){
            ki_in[i]<-ki_in[i]+mat_a[i,j]}
        }
      }
      ki_out[i]<- r_i[i] - ki_in[i]
      if  (ki_in[i]>=ki_out[i] & Q>=Qbest) {Bi[i]<-0} else {Bi[i]<-1} }
    } 
  
    

#Agregar estados de castigo/recompensa actual al vector acumulativo para las iteraciones
#Calcular la mejor modularidad hasta el momento

  Bi_t <- rbind(Bi_t, Bi)
  Qbest <- max(Qbest, Q)
  
#Calcular Z y W para calcular el estimador D
  
  for(i in 1:n){
    for(j in 1:n){
      if (accion_i[i]==j){ Zij[i,j]<-Zij[i,j]+1}
    }
  }
  for (i in 1:n) { 
    for (j in 1:n) { if (accion_i[i] == j)
    {Wij[i,j] <- Wij[i,j] + (1- Bi[i])}
    }
  }
  
  
  # Estimar accion optima actual de cada LA con el estimador D
  
  Dij <- matrix(0,nrow=n,ncol=n)
  am <- matrix(0,nrow=n,ncol=1)

  
  for (i in 1:n) { 
    for (j in 1:n) {
      Dij[i,j]=(Wij[i,j])/ (Zij[i,j]) }
    am[i] <- which.max(Dij[i,])
  }
  
  am <- t(am)
  am_t <- rbind(am_t, am)
  
  #Actualizar Pij de cada LAi de acuerdo al castigo o recompensa
  
  
  Pij <- apply(Pij, 1:2, prob_nopt) 

  
  for (i in 1:n) { 
    for (j in 1:n) {
      if (j == am[i]) {
        Pij[i,j] <- ((Pij[i,j]/(1-a_recomp) + ((a_recomp)*(1 - (Pij[i,j]/(1-a_recomp))))))
      }
    }
    }
 

# Verificar si la solucion obtenida es igual a la anterior y detener cuando una solucion se obtenga 6 veces consecutivamente
 
   h<-0
  if(t>=2){
    for(j in 1:n) {
      if (Comunidad_Total[t,j]== Comunidad_Total[(t-1),j]){h<-h+1}else{h<-h}
    }}
  
  if(h==n){converg <- converg + 1} else {converg <- 0}
  print(h)
  print(converg)
  if (converg  == min_conv) { break }
}

#UNTIL Las comunidades en el vector comunidad no cambien por 6 ciclos


#Extraer los miembros de cada comunidad por filas

miembros <- matrix(data=NA, nrow = cmax, ncol = 966, byrow = TRUE)


for(i in 1:cmax) { jmax<- length(which(ct==i)) 
for (j in 1:jmax) {
  miembros[i,j] <- which(ct==i)[j] }  } 

#Extraer los miembros de cada comunidad con nombres por filas

nombres <- matrix(data=NA, nrow = cmax, ncol = ncol(ct), byrow = TRUE)

for(i in 1:cmax) { jmax<- length(which(ct==i)) 
for (j in 1:jmax) {
  nombres[i,j] <- V[miembros[i,j]] }  } 

#Guardando datos obtenidos de la corrida

write.xlsx(Comunidad_Total, "ct.xlsx")
write.xlsx(Q_t, "Q.xlsx")
write.xlsx(ct, "cmax.xlsx")
write.xlsx(t, "t.xlsx")
write.xlsx(Pij, "Pij.xlsx")
write.xlsx(accion_t, "accion.xlsx")
write.xlsx(Bi_t, "Bit.xlsx")
write.xlsx(am_t, "amt.xlsx")
write.xlsx(Dij, "D.xlsx")
write.xlsx(Zij, "Z.xlsx")
write.xlsx(Wij, "W.xlsx")
write.xlsx(miembros, "miembros.xlsx")
write.xlsx(nombres, "nombres.xlsx")
write.xlsx(vecinos, "vecinos.xlsx")
write.xlsx(ki_in, "k_in.xlsx")
write.xlsx(ki_out, "k_out.xlsx")


#3 Analisis y visualizacion de la red social


#Preparar datos para exportar a Gephi

lista_arcos_num <- read_excel("C:/Users/baraj/Dropbox/TG/Libro/Conjuntos de datos/Red de 966/lista_arcos_num.xlsx", sheet = "Arcos")
grafoautores_num <- graph_from_data_frame(lista_arcos_numeros, directed = FALSE)
mat_a_num <- data.frame(get.adjacency(grafoautores_num, sparse=FALSE))
write.graph(grafoautores_num, "Grafo966.gml", format = c("gml"))  

#Asignar comunidad como atributo de los nodos para visualizar comunidades en Gephi


for(i in 1:cmax) {
  
  set_vertex_attr(grafoautores_num, "Comunidad", index = na.omit(miembros[i,]), i)
}


#Exportar grafo a Gephi 

write.graph(grafoautores_num, "Grafo966.gml", format = c("gml"))  


#Encontrar lideres de cada comunidad, cargando pestaña lider del documento info pares

Datos_ARS <- read_excel("C:/Users/baraj/Dropbox/TG/Libro/Documento libro/TRABAJO DE GRADO/Apéndices/Apendice C Información Pares.xlsx", sheet = "Lider")
Datos_lideres <- data.frame(Datos_ARS)
Lideres <- Datos_lideres

Nodolider<-matrix(data = NA, nrow = 63, ncol = 2)


for (i in 1:63) {
  
  Lideres <- filter(Datos_lideres, Comunidad == i )
  Nodolider[i, 1] <- i
  Nodolider[i, 2] <- Lideres$V[which.max(Lideres$Grado.nodo)]
  
}

#Descargar la lista de nodos lideres de cada comunidad

write.xlsx(Nodolider, "Nodos_lideres.xlsx")

#Calcular medidas de centralidad

matriz_a <-as.matrix(mat_a)
Grafo1<-graph.adjacency(matriz_a,mode="Undirected",diag=FALSE)

plot(Grafo1)

conectividad<-vertex.connectivity(Grafo1)

Gradonorm<-data.frame(degree(Grafo1,mode="all",normalized=TRUE))

Grado<-data.frame(degree(Grafo1,mode="all",normalized=FALSE))

Intermediacion<-betweenness(Grafo1,directed=FALSE,normalized=FALSE)

Intermediacion_Norm<-betweenness(Grafo1,directed=FALSE,normalized=TRUE)

Cercania<-closeness(Grafo1,mode="all",normalized = FALSE)

Cercania_Norm<-closeness(Grafo1,mode="all",normalized = TRUE)

Vect_Prop<-authority.score(Grafo1)


#Calcular propiedades de la red social


#Small world in the Colombian scientists network

small_world <- mean_distance(grafoautores)

#Power law degree distribution

ley_potencia_prueba <- fit_power_law(r_i)
pvalue <- ley_potencia_prueba[["KS.p"]]
alfa_exponencial <- ley_potencia_prueba[["alpha"]]

#Calculo de Transitividad

Clustering_coefficient <- transitivity(grafoautores, type =c("undirected"), weights = NULL)



#Seleccion de las comunidades para la validacion

comunidades_rand <- sample(1:63,5)

#Analisis de colaboracion cientifica

#copublicaciones promedio

copub <- read_excel("C:/Users/baraj/Dropbox/TG/Libro/Documento libro/TRABAJO DE GRADO/Apéndices/Apéndice D Info copublicaciones.xlsx")
dist_copublicaciones <- hist(copub$`Número de copublicaciones`, xlim = c(1, 110), xlab = "Numero de copublicaciones", ylab = "Frecuencia", main = "Distribucion de frecuencia de las co-publicaciones por autor", nclass = 10, labels = TRUE,col = "gray" )

#coautores promedio

dist_grado <- hist(r_i, xlim = c(1, 90), xlab = "Grado por autor", ylab = "Frecuencia", main = "Distribucion de frecuencia del grado por autor", nclass = 10, labels = TRUE,col = "gray" )

#Referral chains

a <- sample(1:966, 1)
b <- sample(1:966, 1)
referral <- shortest_paths(grafoautores, from = a, to = b, mode = c("all"), weights = NULL, output =c("vpath"))





