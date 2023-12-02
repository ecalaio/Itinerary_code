

library(opencage)
key <- "COLOCAR API KEY"
ciudad <- "Ciudad de Mendoza, Argentina"

# Hacer la consulta a la API de OpenCage
resultado <- opencage_forward(placename = ciudad, key = key)

# Extraer las coordenadas
if (nrow(resultado$results) > 0) {
  lat_lon <- resultado$results[1, c("geometry.lat", "geometry.lng")]
  print(paste("Latitud:", lat_lon["geometry.lat"], "Longitud:", lat_lon["geometry.lng"]))
}


# Lista de ciudades
ciudades <- c("Ciudad de Mendoza,Mendoza,Argentina", "Cañon del Atuel,Mendoza,Argentina","Potrerillos,Mendoza,Argentina", "San Rafael,Mendoza,Argentina", "Cacheuta,Mendoza,Argentina","Malargüe,Mendoza,Argentina","Puente del Inca,Mendoza,Argentina","las leñas,Mendoza,Argentina")

# Crear un data frame vacío para almacenar los resultados
resultados <- data.frame(Ciudad = character(), Latitud = numeric(), Longitud = numeric(), stringsAsFactors = FALSE)

# Bucle para obtener las coordenadas de cada ciudad
for (ciudad in ciudades) {
  resultado <- opencage_forward(placename = ciudad, key = key)
  if (nrow(resultado$results) > 0) {
    lat_lon <- resultado$results[1, c("geometry.lat", "geometry.lng")]
    resultados <- rbind(resultados, data.frame(Ciudad = ciudad, Latitud = lat_lon["geometry.lat"], Longitud = lat_lon["geometry.lng"]))
  }
}

# Ver los resultados
print(resultados)

#Cambio nombre de columnas
names(resultados)[names(resultados)=="geometry.lat"] <-"Latitud"
names(resultados)[names(resultados)=="geometry.lng"] <-"Longitud"

#armamos recorrido optimo

library(GA)

distancias <- as.matrix(dist(resultados[, c("Latitud", "Longitud")]))

# Definir la función de fitness
fitness <- function(tour) -sum(distancias[cbind(tour, c(tour[-1], tour[1]))])

# Crear un algoritmo genético
ga <- ga(type = "permutation", fitness = fitness,min=1,max=nrow(resultados), popSize = 50, maxiter = 100, run = 50)

# Imprimir el mejor tour
mejor_tour <- ga@solution
print(resultados$Ciudad[mejor_tour])

#garficamos en un mapa

library(ggplot2)

# Ordenar los resultados según el mejor tour
resultados_ordenados <- resultados[mejor_tour, ]

# Agregar una columna con el orden del recorrido
resultados_ordenados$Orden <- 1:nrow(resultados_ordenados)

# Crear el mapa
ggplot() +
  geom_point(aes(x = Longitud, y = Latitud), data = resultados_ordenados, color = "red") +
  geom_text(aes(x = Longitud, y = Latitud, label = paste(Orden, Ciudad,sep=". ")), data = resultados_ordenados, check_overlap = TRUE, hjust = -0.1, vjust = -0.1) +
  geom_path(aes(x = Longitud, y = Latitud), data = resultados_ordenados, color = "blue", lineend = "round") +
  theme_minimal()
