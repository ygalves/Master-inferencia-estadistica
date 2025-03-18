# -------------------------------
# UNIVERSIDAD AUTONOMA DE OCCIDENTE
## MAESTRIA EN INTELIGENCIA ARTIFICIAL Y CIENCIA DE DATOS
## INFERENCIA ESTADISTICA
## DESAFÍO 2
## Profesor: Cristian E García.
## Alumno: Yoniliman Galvis Aguirre
## Codigo: 22500214
## Repositorio: https://github.com/ygalves/Master-inferencia-estadistica.git
## CALI - COLOMBIA
### 03/17/2025
# -------------------------------

###############################################################################
# Preparemos el Sistema instalando ó reinstalando librerias si es necesario
###############################################################################

# Variable de control para habilitar o deshabilitar la eliminación de archivos, si tiene problemas para instalar un paqute puede que ayude borrar estos archivos , primero ejecuta este Chunk en FALSE, si hay problemas llevalo a TRUE y ejecuta este Chunk de nuevo. ES probable que tenga que instalar las siguientes dependencias uasndo los siguientes comandos en uan terminal: 
# sudo apt-get update
# sudo apt-get install libharfbuzz-dev libfribidi-dev libcurl4-openssl-dev libssl-dev libxml2-dev libfontconfig1-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev

eliminar_archivos_habilitado <- FALSE

# Definir los archivos a eliminar
archivos <- c(".Rhistory", ".RData", ".Rprofile")

# Función para eliminar los archivos si existen
eliminar_archivos <- function(archivos) {
  for (archivo in archivos) {
    if (file.exists(archivo)) {
      file.remove(archivo)
      cat("Archivo eliminado:", archivo, "\n")
    } else {
      cat("Archivo no encontrado:", archivo, "\n")
    }
  }
}

# Eliminar los archivos si se habilitó la opción
if (eliminar_archivos_habilitado) {
  eliminar_archivos(archivos)
} else {
  cat("La eliminación de archivos está deshabilitada.\n")
}

# Instalar y cargar el paquete para manejo de conflictos
install.packages("conflicted")
library(conflicted)

# Preferir funciones específicas de paquetes que estan en conflicto en la librería tidyverse purrr y tidyr las cuales tienen funciones con nombres iguales "stats" y "caret". así que vamos adefinir cual de estas funciones vamos a preferir

conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("lift", "purrr")

# Para hacer isntalacion de varios paquetes creamos un vector que contenga los nombre de los paquetes que queremos instalar
paquetes <- c("dplyr", "ggplot2", "caret", "ModelMetrics", "stats4", "tidyverse","rlang","tidyr","gridExtra","progress","stats4","knitr","reshape2","boot")

# hacemos una Función que nos permite instalar paquetes si no están ya instalados en el sistema
instalar_paquetes <- function(paquetes) {
  paquetes_instalados <- paquetes[paquetes %in% installed.packages()[,"Package"]]
  nuevos_paquetes <- paquetes[!(paquetes %in% installed.packages()[,"Package"])]
  
  if(length(nuevos_paquetes)) {
    install.packages(nuevos_paquetes, quiet = TRUE)
    cat("Se instalaron los siguientes paquetes:", nuevos_paquetes, "\n")
  } else {
    cat("Todos los paquetes ya están instalados.\n")
  }
  
  if(length(paquetes_instalados)) {
    cat("Los siguientes paquetes ya estaban instalados:", paquetes_instalados, "\n")
  }
}

# Instala los paquetes que son necesarios
instalar_paquetes(paquetes)

# Cargar los paquetes
library(dplyr)
library(ggplot2)
library(caret)
library(ModelMetrics)
library(stats4)
library(tidyverse)
library(rlang)
library(progress)
### grid Extra esta deshabilitada ya que causa problemas con pivot_longer
### library(gridExtra)
library(tidyr)
library(stats4)
library(knitr)
library(reshape2)
library(boot)

###############################################################################
# SITUACION 1
# experimento de estudiantes para investigar si el uso del teléfono celular 
# afecta los tiempos de reacción de los conductores
###############################################################################
# creamos la semilla para que los resultados sean reproducibles
set.seed(123)

# establecemos el número de participantes en el experimento
n <- 8

# Vamos a simular los tiempos de reacción en la condición de control (sin teléfono)
# Vamos a suponer una media en y una desviación estándar en ms
control <- rnorm(n, mean = 350, sd = 20)

# Hacemos la simulacion de los tiempos de reacción en la condición con teléfono
# Vamos a sumponer que, en promedio, el uso del teléfono aumenta el tiempo de reacción en 15 ms para determinar algo de diferencia, solo por la prueba que vamos a realizar
phone <- control + rnorm(n, mean = 15, sd = 5)

# Tomamos un data frame que contenga los datos que simulamos
datos <- data.frame(
  Sujeto = 1:n,
  Control = control,
  Phone = phone
)

# Esta es la muestra los datos simulados
print("Datos simulados:")
print(datos)

# vamos a calcular la diferencia para cada participante: d = Phone - Control
datos$Diff <- datos$Phone - datos$Control

# miremos las diferencias
print("Diferencias (Phone - Control):")
print(datos$Diff)

# Realizamos la prueba t para muestras apareadas
resultado <- t.test(datos$Phone, datos$Control, paired = TRUE)

# vemos los resultado de la prueba t que realizamos
print("Resultado de la prueba t para muestras apareadas:")
print(resultado)

# Convertir los datos al formato largo para facilitar la graficación con ggplot2
datos_long <- melt(datos, id.vars = "Sujeto", 
                   measure.vars = c("Control", "Phone"),
                   variable.name = "Condicion", value.name = "Tiempo")

# Graficar las líneas pareadas para cada sujeto
ggplot(datos_long, aes(x = Condicion, y = Tiempo, group = Sujeto, color = factor(Sujeto))) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  labs(title = "Tiempos de reacción por condición",
       x = "Condición",
       y = "Tiempo de reacción (ms)",
       color = "Sujeto") +
  theme_minimal()

###############################################################################
# SITUACION 2
# Genera $5000$ muestras aleatorias de tamaño $n = 10$ de una población normal 
# con media $\mu = 5$ y Varianza 1.
###############################################################################
# cargamos los parámetros de la simulación según la situación 2
sample_sizes <- c(10, 30, 50, 100)  # tamaños de muestra
n_sim <- 5000                       # número de simulaciones
B <- 1000                           # número de re-muestras bootstrap

# usamos un data frame para almacenar los resultados
results <- data.frame(SampleSize = integer(), Method = character(), 
                      Coverage = numeric(), AvgWidth = numeric(), 
                      stringsAsFactors = FALSE)

# calculemos ahora el total de iteraciones para el progress bar y saber donde es que esta el script
total_iterations <- length(sample_sizes) * n_sim
pb <- txtProgressBar(min = 0, max = total_iterations, style = 3)
counter <- 0

# ahora  hacemos el loop para cada tamaño de muestra
for (n in sample_sizes) {
  coverage_t <- numeric(n_sim)
  width_t <- numeric(n_sim)
  
  coverage_boot <- numeric(n_sim)
  width_boot <- numeric(n_sim)
  
  # hagamos la simulación n_sim veces
  for (i in 1:n_sim) {
    # generemos ahora una muestra de tamaño n de N(5,1)
    x <- rnorm(n, mean = 5, sd = 1)
    xbar <- mean(x)
    s <- sd(x)
    se <- s / sqrt(n)
    
    ## apliquemos el metodo t-student
    t_crit <- qt(0.975, df = n - 1)
    lower_t <- xbar - t_crit * se
    upper_t <- xbar + t_crit * se
    width_t[i] <- upper_t - lower_t
    coverage_t[i] <- as.numeric((lower_t <= 5) & (5 <= upper_t))
    
    ## apliquemos el metodo Bootstrap (percentil)
    boot_means <- replicate(B, mean(sample(x, size = n, replace = TRUE)))
    lower_boot <- quantile(boot_means, 0.025)
    upper_boot <- quantile(boot_means, 0.975)
    width_boot[i] <- upper_boot - lower_boot
    coverage_boot[i] <- as.numeric((lower_boot <= 5) & (5 <= upper_boot))
    
    # vamos actualizando la barra de progreso
    counter <- counter + 1
    setTxtProgressBar(pb, counter)
  }
  
  # calculemos los resultados para el método t-student
  cov_t <- mean(coverage_t) * 100  # porcentaje de cobertura
  avg_width_t <- mean(width_t)
  
  # calculemos los resultados para el método bootstrap
  cov_boot <- mean(coverage_boot) * 100
  avg_width_boot <- mean(width_boot)
  
  # gaurdemos los resultados
  results <- rbind(results, data.frame(SampleSize = n, Method = "T-based", 
                                       Coverage = cov_t, AvgWidth = avg_width_t))
  results <- rbind(results, data.frame(SampleSize = n, Method = "Bootstrap", 
                                       Coverage = cov_boot, AvgWidth = avg_width_boot))
}

# hay que cerrar ahora la barra de progreso
close(pb)

# presentemos los resultados usando knitr::kable
library(knitr)
kable(results, caption = "Resultados: Porcentaje de Cobertura y Amplitud Promedio")

# mostremos el porcentaje de cobertura
ggplot(results, aes(x = factor(SampleSize), y = Coverage, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Porcentaje de Cobertura por Método y Tamaño de Muestra",
       x = "Tamaño de Muestra",
       y = "Cobertura (%)") +
  theme_minimal()

# y mostremos la amplitud promedio
ggplot(results, aes(x = factor(SampleSize), y = AvgWidth, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Amplitud Promedio de Intervalos por Método y Tamaño de Muestra",
       x = "Tamaño de Muestra",
       y = "Amplitud Promedio") +
  theme_minimal()

###############################################################################
# SITUACION 3
# Muestra aleatoria de $20$ proyectos que envió para ser evaluados, tanto a 
# consultores externos, como a su propio departamento de proyectos.
###############################################################################

# Datos proporcionados
X <- c(4, 2, 8, 10, 1, 3, 8, 3, 2, 2, 4, 4, 5, 6, 7, 2, 1, 3, 4, 9)
Y <- c(3, 1, 6, 8, 3, 2, 6, 2, 1, 1, 4, 4, 4, 7, 10, 3, 2, 4, 5, 10)
Z <- c(-1, -1, 0, 0, 0, 0, 1, 0, 0, 1, -1, -1, 0, 1, 1, -1, -1, 0, 1, -1)
W <- c(40, 30.5, 80.3, 68.5, 24.7, 40.5, 90.5, 38.5, 50.4, 50.2, 60.1, 60.8, 70.9, 80, 90, 30, 27, 40, 50, 40)

# Creación del data frame
datos <- data.frame(X, Y, Z, W)

# a: Intervalo de confianza del 90% para el costo medio
alpha <- 0.10
n <- length(W)
media_W <- mean(W)
desv_W <- sd(W)
t_critico <- qt(1 - alpha/2, df = n - 1)
margin_error <- t_critico * (desv_W / sqrt(n))
intervalo_W <- c(media_W - margin_error, media_W + margin_error)

print("a - Intervalo de confianza del 90% para el costo medio:")
print(intervalo_W)

# b: Estimación de proporción con IC del 90%
subset_datos <- datos[Y <= 6 & X > 2, ]
n_cumplen <- sum(subset_datos$W < 50)
n_total <- nrow(subset_datos)
prop_cumplen <- n_cumplen / n_total
se_prop <- sqrt((prop_cumplen * (1 - prop_cumplen)) / n_total)
z_critico <- qnorm(1 - alpha/2)
intervalo_prop <- c(prop_cumplen - z_critico * se_prop, prop_cumplen + z_critico * se_prop)

print("b - Intervalo de confianza del 90% para la proporción de proyectos con ciertas condiciones:")
print(intervalo_prop)

# c: Prueba de hipótesis para diferencia de medias
W_externo <- W[Z != -1]
W_interno <- W[Z == -1]
var_test <- var.test(W_externo, W_interno)
if (var_test$p.value < 0.05) {
  test_result <- t.test(W_externo, W_interno, var.equal = FALSE)
} else {
  test_result <- t.test(W_externo, W_interno, var.equal = TRUE)
}

print("c - Prueba de hipótesis para diferencia de medias:")
print(test_result)

# d: Prueba de hipótesis para proporciones
p_tanaka <- sum(X[Z == 1] > 4) / sum(Z == 1)
p_robani <- sum(X[Z == 0] > 4) / sum(Z == 0)
n_tanaka <- sum(Z == 1)
n_robani <- sum(Z == 0)
p_pool <- (p_tanaka * n_tanaka + p_robani * n_robani) / (n_tanaka + n_robani)
se_pool <- sqrt(p_pool * (1 - p_pool) * (1/n_tanaka + 1/n_robani))
z_stat <- (p_tanaka - p_robani) / se_pool
p_valor_prop <- 2 * (1 - pnorm(abs(z_stat)))

print("d - Prueba de hipótesis para proporciones:")
print(paste("z =", z_stat, ", p-valor =", p_valor_prop))

#grafico de costo por consultor
ggplot(datos, aes(x = factor(Z), y = W, fill = factor(Z))) +
  geom_boxplot() +
  scale_x_discrete(labels = c("-1" = "Interno", "0" = "Robani", "1" = "Tanaka")) +
  labs(title = "Costo de Evaluación por Tipo de Consultor",
       x = "Tipo de Consultor",
       y = "Costo (UF)") +
  theme_minimal()

###############################################################################
# SITUACION 4
# 40 recuentos anuales del número de reclutas y reproductores en una población de salmones
###############################################################################
# cargamos gridextra ahora porque al inicio esta da problemas con pivot longer
library(gridExtra)

# creamos los vectores para R y S con los datos de la tabla y tenemos en cuenta que las unidades: miles de peces
R_vals <- c(68, 222, 311, 244, 77, 205, 166, 222, 299, 233, 248, 195, 220, 228, 161, 203, 
            142, 188, 226, 210, 287, 132, 67, 275, 276, 285, 201, 286, 115, 188, 267, 275, 
            64, 224, 121, 304, 206, 121, 301, 214)
S_vals <- c(56, 351, 412, 265, 62, 282, 176, 301, 445, 310, 313, 234, 279, 266, 162, 229, 
            138, 256, 368, 270, 428, 144, 54, 478, 319, 447, 214, 419, 102, 186, 429, 490, 
            51, 389, 115, 430, 289, 113, 407, 235)

datos <- data.frame(R = R_vals, S = S_vals)

# Transformamos las variables usando 1/R y 1/S para normalizarlos, linearizarlos para el uso en el modelo de Beverton-Holt
datos$inv_R <- 1 / datos$R
datos$inv_S <- 1 / datos$S

# Ajustamos el modelo de regresión lineal:
# ajustamos 1/R = beta1 + beta2*(1/S)
modelo <- lm(inv_R ~ inv_S, data = datos)
beta1_hat <- coef(modelo)[1]
beta2_hat <- coef(modelo)[2]

# Calculamos el nivel estable de la población (R = S)
# Fórmula: S_estable = (1 - beta2_hat) / beta1_hat
S_estable <- (1 - beta2_hat) / beta1_hat

cat("\n=== Modelo de Beverton-Holt ===\n")
cat("Beta1 estimado:", beta1_hat, "\n")
cat("Beta2 estimado:", beta2_hat, "\n")
cat("Nivel estable estimado (S_estable):", S_estable, "\n\n")

# Aplicacion del método BOOTSTRAP

B <- 1000  # Número de replicaciones bootstrap, usamos l numero de iteraciones habituales para estos casos

# aplicamos el primer método, el de remuestreo de residuales
boot_residuales <- function(data, indices) {
  data_boot <- data
  # vamos a remuestrear los residuales (con reemplazo) y los sumamos a los valores ajustados
  data_boot$inv_R <- fitted(modelo) + residuals(modelo)[indices]
  modelo_boot <- lm(inv_R ~ inv_S, data = data_boot)
  beta1_boot <- coef(modelo_boot)[1]
  beta2_boot <- coef(modelo_boot)[2]
  return( (1 - beta2_boot) / beta1_boot )
}

# la semilla para grantizar la repetitibilidad del código
set.seed(123)
boot_res <- boot(data = datos, statistic = boot_residuales, R = B)

# aplicamos el segundo método, el de remuestreo de casos
boot_casos <- function(data, indices) {
  data_boot <- data[indices, ]
  modelo_boot <- lm(inv_R ~ inv_S, data = data_boot)
  beta1_boot <- coef(modelo_boot)[1]
  beta2_boot <- coef(modelo_boot)[2]
  return( (1 - beta2_boot) / beta1_boot )
}

set.seed(123)
boot_cas <- boot(data = datos, statistic = boot_casos, R = B)

# vamo a calcular la media bootstrap (S_barra) y el sesgo
S_barra <- mean(boot_res$t)  # Media de las estimaciones bootstrap (usando residuales)
bias_boot <- S_barra - S_estable
S_corr <- S_estable - bias_boot

cat("=== Resultados Bootstrap (Residuales) ===\n")
cat("Media de las estimaciones bootstrap (S_barra):", S_barra, "\n")
cat("Sesgo estimado (bias_boot):", bias_boot, "\n")
cat("Estimador corregido por sesgo (S_corr):", S_corr, "\n\n")

# encontrar el intervalo de confianza al 95% (por percentiles) para el método residuales
IC_95_res <- quantile(boot_res$t, probs = c(0.025, 0.975))
cat("Intervalo de confianza (remuestreo residuales): (", IC_95_res[1], ",", IC_95_res[2], ")\n\n")

# calcular el intervalo de confianza para el método de casos
IC_95_cas <- quantile(boot_cas$t, probs = c(0.025, 0.975))
cat("=== Intervalo de confianza (remuestreo de casos) ===\n")
cat("Intervalo: (", IC_95_cas[1], ",", IC_95_cas[2], ")\n\n")

# crear los histogramas para los dos métodos
df_resid <- data.frame(S_estable = boot_res$t, Metodo = "Residuales")
df_casos <- data.frame(S_estable = boot_cas$t, Metodo = "Casos")
df_total <- rbind(df_resid, df_casos)

p <- ggplot(df_total, aes(x = S_estable, fill = Metodo)) +
  geom_histogram(position = "dodge", bins = 30, color = "black", alpha = 0.7) +
  facet_wrap(~Metodo, ncol = 2) +
  geom_vline(xintercept = S_estable, linetype = "dashed", color = "red") +
  labs(title = "Distribución Bootstrap de S_estable",
       x = "Nivel Estable (S*)",
       y = "Frecuencia") +
  theme_minimal()

print(p)

