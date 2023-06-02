#cargar las librería a ser usadas
library(dplyr) #cargar librería que permite agrupar data y resumir estadísticos

faculty <- read.csv("faculty.csv") #importar base de datos

set.seed(280851373) #fijar set.seed para fijar la aleatorización de la muestra

muestra_n250 <- dplyr::slice_sample( #generar muestra de 250 filas
  faculty,
  n = 250,
  replace = TRUE #definir que sea posible repetir una misma observación en el muestreo
)

muestra_stats <- muestra_n250 %>% #obtener estadísticos de tendencia central y dispersión de salarios por género
  group_by(male) %>%
  summarise(promedio = mean(salary),
            mediana = median(salary),
            desvest = sd(salary),
            rango =  max(salary) - min(salary))
