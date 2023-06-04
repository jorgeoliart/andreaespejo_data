#cargar las librería a ser usadas
library(dplyr) #cargar librería que permite agrupar data y resumir estadísticos
library(car) #cargar librería que permite el test de Levene

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

#chequear los supuestos previos al uso del test t
#generar submuestras de hombres y mujeres, previo al test t de welch
hombres <- muestra_n250[muestra_n250$male == 1, "salary"]
mujeres <- muestra_n250[muestra_n250$male == 0, "salary"]

#chequear el supuesto de normalidad de la distribución de salarios
shapiro.test(hombres)
shapiro.test(mujeres)

t_result <- t.test (hombres, mujeres, var.equal = FALSE) #Calcular t de welch para salarios entre hombres y mujeres

difsalariopromedio <- t_result$estimate[1] - t_result$estimate[2] #calcular la diferencia promedio en salario

