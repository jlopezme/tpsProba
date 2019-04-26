album <- c(FALSE, FALSE, TRUE, TRUE, FALSE, FALSE) # es un ejemplo de un album de cant_figuritas = 6 que tiene pegadas las figuritas 3 y 4
album[4]
album[2]

## Si quiero pegar la figurita "2"
album[2] = TRUE
album


# Que pasa si vuelvo a pegar la figu 2? 
album[2] = TRUE
album
#Respuesta: queda igual. Se entenderá que cuando se dice que se pega una figu, la figu queda pegada en el album, 
#independientemente de si ya estaba pegada o no.

## PASO 1 ##

cant_figuritas_1 = 6

album = rep(FALSE, cant_figuritas_1)
figurita_sorteada = sample(1:6,1,replace = TRUE)
album[figurita_sorteada[1]] = TRUE 
album

## PASO 2 ##

album_lleno = function(album){
  for (elem in album){
    if (elem == FALSE) return(FALSE) #si descubre que falta una figurita, ya devuelve FALSE
  }
  #si recorre todo el album y estaban todas pegadas devuelve TRUE
  return(TRUE)
}

## PASO 3 ##

generar_sobre = function(cant_figuritas, cant_sobre){ 
  sobre <- 1:cant_sobre
  for (figu in sobre){ 
    #a cada posicion del sobre, le digo que numero de figurita va a tener
    sobre[figu]<-sample(1:cant_figuritas, 1)
    #sample toma una numero aleatorio entre 1 el tamaño del album que 
    #representa a la figurita y la mete en el sobre
  }
  return(sobre)
}

generar_sobre_sinRep <- function(cant_figuritas_3, cant_sobre_3){ 
 
  return(sample(1:cant_figuritas, cant_sobre))
}

album_3 = rep(FALSE, 6)
sobre = generar_sobre(6, 3)
sobre
for(i in 1:3){
  album[sobre[i]] = TRUE 

}
album

## PASO 4 ##

pegar_sobre = function(album,  sobre){

  for (figu in sobre) { #revisa todas las figuritas del sobre, y las pega en el álbum
    album[figu]<-TRUE
  }
  return(album)
}

album_vacio = rep(FALSE,6)
sobre1 = generar_sobre(6,3)
album_vacio = pegar_sobre(album_vacio,sobre1)
sobre1
album_vacio
sobre2 = generar_sobre(6,3)
album_vacio = pegar_sobre(album_vacio,sobre2)
sobre2
album_vacio

## PASO 5 ##

cuantas_figuritas = function(cant_figuritas, cant_sobre){
  #despues de ejecutar esta funcion con los tamaño de album=6 sobre de tam=1 tres veces, los resultados fueron 16, 17, 7 
 al<- 1:cant_figuritas
  for(i in 1:cant_figuritas){
    al[i]<-FALSE
  }
  sobresusados<-0
  while(album_lleno(al) ==FALSE){
    al<-pegar_sobre(al, generar_sobre(cant_figuritas, cant_sobre))
    sobresusados = sobresusados+1
  }
  return(sobresusados)
}

cuantas_figuritas_sinRep = function(cant_figuritas, cant_sobre){
  #despues de ejecutar esta funcion tres veces, los resultados fueron 16, 17, 7
  al<- 1:cant_figuritas
  for(i in 1:cant_figuritas){
    al[i]<-FALSE
  }
  sobresusados<-0
  while(album_lleno(al) ==FALSE){
    al<-pegar_sobre(al, generar_sobre_sinRep(cant_figuritas, cant_sobre))
    sobresusados = sobresusados+1
  }
  return(sobresusados)
}

## PASO 6 ##

cuantas_figuritas(6,1)
cuantas_figuritas(6,1)
cuantas_figuritas(6,1)

## PASO 7 ##

nRep = 1000
menos_de_800_sobres = 0
muestras = rep(0, nRep)
for(i in 1:nRep){
  muestras[i] = cuantas_figuritas(cant_figuritas_rusia, cant_sobre_rusia)
  if(muestras[i] <= 800){
    menos_de_800_sobres = menos_de_800_sobres + 1
  }
}

############

 proba800 <-function(n){
   #se fija cuantas veces se necesitaron 800 o menos paquetes para llenar el album, y a ese número
   #lo divide por la cantidad de repeticiones (Montecarlo). Despues de ejecutar esta funcion varias veces
   #descubrimos que P(canttidad de sobres usados <= 800) es 0.18 (entre 0.161 y 0.2 aprox)                      
   cant800<-0
   for (i in 1:n) {
     i<-(cuantas_figuritas(670, 5))
     if (i<=800) cant800 <-cant800+1
   }
   return(cant800/n)
 }
 
 proba800_sinRep <-function(n){
   #se fija cuantas veces se necesitaron 800 o menos paquetes para llenar el album, y a ese número
   #lo divide por la cantidad de repeticiones (Montecarlo). Despues de ejecutar esta funcion varias veces
   #descubrimos que P(canttidad de sobres usados <= 800) es 0.18 (entre 0.161 y 0.2 aprox)                      
   i<-0
   cant800<-0
   for (i in 1:n) {
     i<-(cuantas_figuritas_sinRep(670, 5))
     if (i<=800) cant800 <-cant800+1
   }
   return(cant800/n)
 }

 cuantos_sobres <- function(cant_sobres){
   al<- 1:670
   llenos<-0
   for (cantrep in 1:1000){
     for(i in 1:670){ #creo album vacio
       al[i]<-FALSE
     }
     for(i in 1:cant_sobres){
       al<-pegar_sobre(al, generar_sobre(670, 5))
     }
     if ( album_lleno(al) == TRUE ) llenos = llenos + 1
   
   }
   return(llenos/1000)
 }
 
 #Despues de realizar varias experimentaciones, llegamos a la conclusion de que para completar el album
 #con proba mayor o igual a 0.9, se necesitan como minimo 1179 sobres de figuritas.
 
 
 cuantos_sobres_sin_repetidos <- function(cant_sobres){
   al<- 1:670
   llenos<-0
   for (cantrep in 1:1000){
     for(i in 1:670){ #creo album vacio
       al[i]<-FALSE
     }
     for(i in 1:cant_sobres){
       al<-pegar_sobre(al, generar_sobre_sinRep(670, 5))
     }
     if ( album_lleno(al) == TRUE ) llenos = llenos + 1
     
   }
   return(llenos/1000)
 }
   

## AUXILIARES ##

pertenece<-function(n, v){
  for (i in v) {
    if (i ==n) {
      return(TRUE)
    }
  }
  return(FALSE)
}
