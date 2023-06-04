set.seed(280851373) #fijar set.seed para fijar la aleatorización de la muestra

#asegurarse de usar las versiones de R y de las librerías instaladas para poder reproducir el muestreo
#versión de R: 4.3.0

#cargar las librerías a ser usadas
#librería: dplyr, Versión: 1.1.2, permite agrupar y resumir estadísticos
library(dplyr) 
#librería: car, Versión: 3.1.2, permite el test de Levene
library(car) 

faculty <- read.csv("faculty.csv") #importar base de datos

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

#chequear el supuesto de igualdad de varianzas de la distribución de salarios
muestra_n250$male <- as.factor(muestra_n250$male) #transformar la variable de género a categórica
levene <- leveneTest(muestra_n250$salary ~ muestra_n250$male) #correr el test de Levene

t_result <- t.test (hombres, mujeres, var.equal = FALSE) #Calcular t de welch para salarios entre hombres y mujeres

difsalariopromedio <- t_result$estimate[1] - t_result$estimate[2] #calcular la diferencia promedio en salario

