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




generar_sobre = function(album_size, sobre_size){ 
  sobre <- 1:sobre_size
  for (figu in sobre){ 
    #a cada posicion del sobre le digo que numero de figurita va a tener
    sobre[figu]<-sample(1:album_size, 1)
    #sample toma una numero aleatorio entre 1 y el size del album que 
    #representa a la figurita y la mete en el sobre
  }
  return(sobre)
}

generar_sobre_sinRep = function(album_size, sobre_size){ 
  
  return(sample(1:album_size, sobre_size))
  #se puede usar mas facilmente la funcion sample para generar un sobre tambien.
  #En este caso como no pasamos como parametro que queriamos que haya repetidos, se generan sobres sin repetidos.(es asi por default)
}


pegar_sobre = function(album,  sobre){
  
  for (figu in sobre) { 
    #revisa todas las figuritas del sobre, y las pega en el álbum
    album[figu]<-TRUE
  }
  return(album)
}


album_lleno = function(album){
  #la funcion te avisa si el album esta completo o no
  #para eso recorro cada posicion del album y me fijo si esta cada figurita pegada
  for (figu in album){
    if (figu == FALSE) return(FALSE) #si descubre que falta una figurita, ya devuelve FALSE (no hace falta ver el resto)
  }
  #si en ningun momento encontro una posicion vacia (en FALSE), sale del loop y devuelvo TRUE (que el album estaba lleno)
  return(TRUE)
}

cuantas_figuritas = function(album_size, sobre_size){
  #despues de ejecutar esta funcion con album_size=6 y sobre_size=1 tres veces, los resultados fueron 16, 17, 7 
  album<- 1:album_size
  for(i in 1:album_size){
    album[i]<-FALSE
  }
  #acabo de crear un album nuevo vacio
  sobres_usados<-0
  #abro sobres y pego sus figuritas en el album hasta que se llene el album
  while(album_lleno(album) ==FALSE){
    album<-pegar_sobre(album, generar_sobre(album_size, sobre_size))
    sobres_usados = sobres_usados+1
  }
  #una vez que llene el album, se va a cortar el while-loop y voy a tener registrado cuantos sobres se tuvieron que abrir para completarlo
  return(sobres_usados)
}

cuantas_figuritas_sinRep = function(album_size, sobre_size){
  #despues de ejecutar esta funcion tres veces, los resultados fueron 16, 17, 7
  #esta funcion hace exactamente lo mismo que la anterior, solo que al generar los sobres
  #que se utilizan para llenar el album estos vienen sin figuritas repetidas
  album<- 1:album_size
  for(i in 1:album_size){
    album[i]<-FALSE
  }
  sobres_usados<-0
  while(album_lleno(album) ==FALSE){
    album<-pegar_sobre(album, generar_sobre_sinRep(album_size, sobre_size))
    #aca esta la diferencia con el anterior, se generan sobres sin figuritas repetidas
    sobres_usados = sobres_usados+1
  }
  return(sobres_usados)
}

llenadoAlbumRusia = function(n){
  #te devuelve un vector en el cual cada posicion te indica cuantos sobres se necesitaron 
  #para llenar el album de rusia (album_size=670 y sobre_size=5).
  #n = numero de albums de rusia llenados
  res <- 1:n
  for(i in 1:n){
    res[i] <- (cuantas_figuritas(670,5))
  }
  return(res)
}

llenadoAlbumRusia_sinRep = function(n){
  #lo mismo que la funcion anterior pero se usan sobres sin rep
  res <- 1:n
  for(i in 1:n){
    res[i] <- (cuantas_figuritas_sinRep(670,5))
  }
  return(res)
}


#creo dos vectores con los resultados de haber completado 1000 albums de Rusia
#un vector tiene los 1000 albums llenados con sobres que pueden tener figus repetidas y el otro sin repetidas

albumRusia1000 <- llenadoAlbumRusia(1000)

albumRusia1000_sinRep <- llenadoAlbumRusia_sinRep(1000)

probaDeLlenarAlbumRusiaConMenosDe800sobres = function(){
  #la funcion se fija cuantas veces se necesitaron 800 o menos sobres para llenar el album de Rusia
  #A ese numero se lo divide por la cantidad de repeticiones (Montecarlo) que en este caso es el
  #tamaño del vector, osea los 1000 albums de rusia llenados
  #Luego de probar esta funcion varias veces, concluimos que P(cant de sobres usados <= 800) es 0.197
  
  cantidadMenorIgual800 <- 0
  for(i in albumRusia1000){
    #parseo el vector entero y registro la cantidad de veces que se lleno con 800 o menos paquetes
    if(i<= 800){
      cantidadMenorIgual800 <- cantidadMenorIgual800 + 1
    }
  }
  return(cantidadMenorIgual800 / length(albumRusia1000))
  
}

probaDeLlenarAlbumRusiaConMenosDe800sobres_sinRep = function(){
  #hace lo mismo que la anterior solo que utiliza sobres sin repetidos.
  #Luego de ejecutar la funcion un par de veces, 
  #se concluyo que P(cantidad de sobre usados sin rep <= 800) es 0.22
  
  cantidadMenorIgual800 <- 0
  for(i in albumRusia1000_sinRep){
    #parseo el vector entero y registro la cantidad de veces que se lleno con 800 o menos paquetes
    if(i<= 800){
      cantidadMenorIgual800 <- cantidadMenorIgual800 + 1
    }
  }
  return(cantidadMenorIgual800 / length(albumRusia1000_sinRep))
  
}

cuantos_sobres <- function(cant_sobres){
  #vamos a simular el intento de llenado de 1000 albums de Rusia
  #Sin embargo, esta vez vamos a tener un limite en la cantidad de sobres que se pueden utilizar
  #para el intento de llenado de cada album de rusia (cant_sobres)
  #La funcion se fija para cada intento si se logro llenar el album o no con esa cant_sobres.
  #al final se devuelve la probabilidad de llenar un album de rusia con cierta cantidad de sobres
  #disponibles (cant_sobres) que pueden tener figuritas repetidas.
  album<- 1:670
  llenos<-0
  for(cantRep in 1:1000){ 
    
    for (i in 1:670){
      #creo album vacio
      album[i]<-FALSE
    }
    #ahora pego todas las figuritas de la cant_sobres que me pasan como parametro a la funcion
    for(i in 1:cant_sobres){
      album<-pegar_sobre(album, generar_sobre(670, 5))
    }
    #una vez que pegue todas las figuritas de los sobres que me permitian tener, me
    #fijo si logre llenar el album
    if ( album_lleno(album) == TRUE ){
      llenos = llenos + 1
    } 
    
    
  }
  return(llenos/1000)
}

cuantos_sobres_sinRep = function(cant_sobres){
  #hace lo mismo que la anterior (como vengo diciendo siempre), pero esta vez se
  #utilizan sobres que no vienen con figuritas repetidas
  
  album<- 1:670
  llenos<-0
  for(cantRep in 1:1000){ 
    
    for (i in 1:670){
      #creo album vacio
      album[i]<-FALSE
    }
    #ahora pego todas las figuritas de la cant_sobres que me pasan como parametro a la funcion
    for(i in 1:cant_sobres){
      album<-pegar_sobre(album, generar_sobre_sinRep(670, 5))
    }
    #una vez que pegue todas las figuritas de los sobres que me permitian tener, me
    #fijo si logre llenar el album
    if ( album_lleno(album) == TRUE ){
      llenos = llenos + 1
    } 
    
    
  }
  return(llenos/1000)
}

#Luego de completar unas cuantas experimentaciones, se concluye que para poder completar el album con
#probabilidad mayor o igual que 0.9, se necesitan como minimo 1174 sobres de figuritas

esperanza_AlbumRusia = function(){
  #sumo los resultados (cantidad de sobres que se necesitaron para llenar el album de Rusia)
  #de los 1000 llenados del album de rusia y lo divido por esas 1000 pruebas para sacar la esperanza
  totalSum <- 0
  for(i in albumRusia1000){
    totalSum <- totalSum + i
  }
  return(totalSum/1000)
}
#por la ley de Grandes Numeros, sabemos que el promedio de realizar n experimentos
#tiende a E(X). El resultado que nos da con estas 1000 repeticiones es 951.383

esperanza_AlbumRusia_sinRep = function(){
  totalSum <- 0
  for(i in albumRusia1000_sinRep){
    totalSum <- totalSum + i
  }
  return(totalSum/1000)
}
#se deduce lo mismo que antes y ademas el resultado que nos dio para este caso
#particular es 945.196

desvioStandard_AlbumRusia = function(){
  res <- 0
  #encuentro E(X^2)
  for(i in albumRusia1000){
    res <- res + i^2
  }
  res <- res/1000
  #ahora resto E(X)^2 para obtener la varianza en res
  res <- res - (esperanza_AlbumRusia())^2
  #devuelvo el desvio
  return (sqrt(res))
}
#Sabemos que la varianza esta definida como V(X) = E(X^2) - (E(X))^2
#tambien sabemos que el desvio muestral es la raiz de la varianza.
#Habiendo aplicado estas formulas en el algoritmo, llegamos al resultado de 175.6693

desvioStandard_AlbumRusia_sinRep = function(){
  #lo mismo que antes solo que con sobres sin repe
  res <- 0
  for(i in albumRusia1000_sinRep){
    res <- res + i^2
  }
  
  res <- res/1000
  res <- res - (esperanza_AlbumRusia_sinRep())^2
  return (sqrt(res))
}
#El desvio estandard que se obtiene aqui es de 177.9827

