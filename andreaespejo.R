#cargar las librería a ser usadas
library(dplyr) #cargar librería que permite agrupar data y resumir estadísticos

faculty <- read.csv("faculty.csv") #importar base de datos

set.seed(280851373) #fijar set.seed para fijar la aleatorización de la muestra

muestra_n250 <- dplyr::slice_sample( #generar muestra de 250 filas
  faculty,
  n = 250,
  replace = TRUE #definir que sea posible repetir una misma observación en el muestreo
)

