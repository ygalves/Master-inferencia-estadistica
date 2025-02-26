# -------------------------------
# UNIVERSIDAD AUTONOMA DE OCCIDENTE
## MAESTRIA EN INTELIGENCIA ARTIFICIAL Y CIENCIA DE DATOS
## INFERENCIA ESTADISTICA
## DESAFÍO 1
## Profesor: Cristian E García.
## Alumno: Yoniliman Galvis Aguirre
## Codigo: 22500214
## CALI - COLOMBIA
### 02/24/2025
# -------------------------------

# -------------------------------
# PREPARAR EL SISTEMA Y CARGAR LIBRERIAS Y 
# -------------------------------

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
paquetes <- c("dplyr", "ggplot2", "caret", "ModelMetrics", "stats4", "tidyverse","tidyr")

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
library(tidyr)

# -------------------------------
# SITUACION 1
# -------------------------------

# Estimacion de la proporcion de peces en el oceano pacífico
## Definimos los parámetros para hacer la simulacion
set.seed(1234)

# pi_verdadero es el valor real del parámetro que queremos estimar, en este caso es la proporcion de la especie de peces en el oceano pacífico $\pi$, contra este valor es que vamos a comparar los estimadores en la simulación, sin este valor no podriamos hacer la validacion del ECM ó del sesgo de la simulación. Si no se conoce el valor pi_verdadero, no es posible usar ECM y tocaría evaluar el rendimiento con otras métricas como varianza, intervalos de confianza o por estudios previos de muestreo.
pi_verdadero <- 0.3  # Valor verdadero de π (0 < π ≤ 0.5)

# Definimos los 3 escenarios: n = número de barcos, k = número de capturas requeridas
Escenarios <- list("Escenario 1" = list(n = 10, k = 5),
                   "Escenario 2" = list(n = 20, k = 9),
                   "Escenario 3" = list(n = 50, k = 20))

B <- 500  # Número de simulaciones por escenario

# --- Función para el Estimador Bayesiano ó Prior---
# Calcula la media del posterior con prior uniforme en (0, 0.5)
media <- function(sum_x, n, k) {
  post_density <- function(pi) {
    2 * pi^(n * k) * (1 - pi)^(sum_x - n * k)
  }
  norm_const <- integrate(post_density, lower = 0, upper = 0.5)$value
  num_int <- integrate(function(pi) pi * post_density(pi),
                       lower = 0, upper = 0.5)$value
  return(num_int / norm_const)
}

# --- Definición de los Estimadores Propuestos ---

# 1. Estimador MLE (o basado en la media muestral)
estimador_mle <- function(data, k) { 
  k / mean(data) 
}

# 2. Estimador basado en transformación logarítmica
# Evitamos usar el nombre "log" para no colisionar con la función base.
estimador_log <- function(data, k) { 
  exp(mean(log(k / data))) 
}

# 3. Estimador Bayesiano
estimador_bayes <- function(data, k) { 
  media(sum(data), length(data), k) 
}

# 4. Estimador UMVUE (insesgado)
estimador_umvue <- function(data, k) {
  (length(data) * k - 1) / (sum(data) - 1)
}

# --- Simulaciones y Comparaciones ---
result <- list()           # Para almacenar el ECM de cada estimador por escenario
estimates_all <- data.frame()  # Para recolectar los valores estimados y graficarlos

for (esc in names(Escenarios)) {
  n <- Escenarios[[esc]]$n
  k <- Escenarios[[esc]]$k
  
  # Matriz para almacenar los valores de cada estimador en cada simulación
  est <- matrix(NA, nrow = B, ncol = 4)
  colnames(est) <- c("MLE", "Logaritmo", "Bayes", "UMVUE")
  
  for (i in 1:B) {
    # Generar datos para n barcos:
    # rnbinom(n, size = k, prob = pi_verdadero) genera el número de fracasos hasta k éxitos.
    # Sumamos k para obtener el total de capturas.
    data <- rnbinom(n, size = k, prob = pi_verdadero) + k
    
    # Calcular cada uno de los estimadores:
    est[i, "MLE"]       <- estimador_mle(data, k)
    est[i, "Logaritmo"] <- estimador_log(data, k)
    est[i, "Bayes"]     <- estimador_bayes(data, k)
    est[i, "UMVUE"]     <- estimador_umvue(data, k)
  }
  
  # Calcular el ECM para cada estimador en el escenario actual
  ECM <- colMeans((est - pi_verdadero)^2)
  result[[esc]] <- ECM
  
  # Convertir a formato largo para graficar
  df <- as.data.frame(est)  # Se usa "est" (no "estimaciones")
  df$Simulacion <- 1:B
  df_long <- pivot_longer(df, cols = c("MLE", "Logaritmo", "Bayes", "UMVUE"),
                          names_to = "Estimador", values_to = "Valor")
  df_long$Escenario <- esc
  estimates_all <- rbind(estimates_all, df_long)
}

# Mostrar los resultados (ECM) para cada escenario
for (esc in names(result)) {
  cat("\nEscenario", esc, "\n")
  print(result[[esc]])
}

# --- Graficar la PMF teórica de la distribución negativa binomial para cada escenario ---
# Esto ilustra la distribución de capturas en un solo barco (para cada k del escenario)
for (esc in names(Escenarios)) {
  k_vis <- Escenarios[[esc]]$k
  x_vals <- k_vis:(k_vis + 40)
  # dnbinom calcula la probabilidad para el número de fracasos, por ello se usa (x - k_vis)
  pmf_vals <- dnbinom(round(x_vals - k_vis), size = k_vis, prob = pi_verdadero)
  df_teorico <- data.frame(Total = x_vals, Probabilidad = pmf_vals)
  
  set.seed(123)  # Para reproducibilidad
  n_sim <- 10000
  simulados <- rnbinom(n_sim, size = k_vis, prob = pi_verdadero) + k_vis
  df_sim <- data.frame(Total = simulados)
  
  g <- ggplot(df_teorico, aes(x = Total, y = Probabilidad)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    stat_function(fun = function(x) dnbinom(round(x - k_vis), linewidth = k_vis, prob = pi_verdadero),
                  color = "red", linewidth = 1) +
    labs(title = paste("PMF Teórica - Escenario", esc, "(k =", k_vis, ", π =", pi_verdadero, ")"),
         x = "Número Total de Peces Capturados", y = "Probabilidad") +
    xlim(k_vis, max(simulados)) +
    theme_minimal()
  
  print(g)
}

# Construir la tabla ECM utilizando sapply y verificando que cada estimador exista
ECM_tabla <- data.frame(
  Escenario   = names(result),
  n           = sapply(names(result), function(x) Escenarios[[x]]$n),
  k           = sapply(names(result), function(x) Escenarios[[x]]$k),
  MLE         = sapply(result, function(x) { if("MLE" %in% names(x)) as.numeric(x["MLE"]) else NA }),
  Logaritmo   = sapply(result, function(x) { if("Logaritmo" %in% names(x)) as.numeric(x["Logaritmo"]) else NA }),
  Bayes       = sapply(result, function(x) { if("Bayes" %in% names(x)) as.numeric(x["Bayes"]) else NA }),
  UMVUE       = sapply(result, function(x) { if("UMVUE" %in% names(x)) as.numeric(x["UMVUE"]) else NA }),
  stringsAsFactors = FALSE
)


# Resumen de EMC de el desempeño de los estimadores en cada escenario planteado
if(requireNamespace("knitr", quietly = TRUE)){
  knitr::kable(ECM_tabla, caption = "ECM de cada estimador por escenario, donde n = número de barcos, k = número de capturas requeridas")
} else {
  print(ECM_tabla)
}



