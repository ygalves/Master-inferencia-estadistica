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

paquetes <- c("dplyr", "ggplot2", "caret", "ModelMetrics", "stats4", "tidyverse","tidyr","gridExtra")

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

B <- 5000  # Número de simulaciones por escenario

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

  # Redondeamos para asegurarnos de tener valores enteros en la función dnbinom

  pmf_vals <- dnbinom(round(x_vals - k_vis), size = k_vis, prob = pi_verdadero)
  df_teorico <- data.frame(Total = x_vals, Probabilidad = pmf_vals)
  
  set.seed(123)  # Para reproducibilidad
  n_sim <- 10000
  simulados <- rnbinom(n_sim, size = k_vis, prob = pi_verdadero) + k_vis
  df_sim <- data.frame(Total = simulados)
  
  g <- ggplot(df_teorico, aes(x = Total, y = Probabilidad)) +
    geom_bar(stat = "identity", fill = "skyblue", color = "black") +
    geom_line(data = df_teorico, aes(x = Total, y = Probabilidad),
              color = "red", size = 1) +
    labs(title = paste("PMF Teórica - Escenario", esc, "(k =", k_vis, ", π =", pi_verdadero, ")"),
         x = "Número Total de Peces Capturados", y = "Probabilidad") +
    xlim(c(k_vis, k_vis + 40)) +
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

# -------------------------------
## Conclusiones
# -------------------------------

# En el ejercicio propuse 4 etimadores: el de mediana muestral o momentos (MLE) y su version insesgada UMVUE o de varianza mínima, el de transformación Logaritmica y el Bayesiano.
# Se plantearon 3 escenarios diferentes y se realizáron las simulaciones respectivas.
# Obsevamos que segun el ECM:
  
# *   Los estimadores basados en la media muestral (MLE y UMVUE) son los de mejor desempeño.
# *   Los estimadores basados en la media muestral (MLE y UMVUE) se comportan de manera muy similar.
# *   Los estimadores basados en la media muestral (MLE y UMVUE) mejoran en consistencia a medida que las muesras $k$ aumentan.
# *   El estimador basado en transformación logarítmica mejora igual a medida que las muestras sonmas grandes pero mantiene un ECM superior y eso significa menor eficiencia.
# *   El estimador bayesiano no presenta resultados en todos los escenarios y puede signiicar que necesita mayor verificacion o una seleccion del pior nueva o que este estimador no es suficientemente robusto para este muestreo en particular.


# -------------------------------
# SITUACION 2
# -------------------------------

###############################################################################
# Estimación de la Media de una Población
# Comparación de dos estimadores:
#   X1 = (1/(2n)) * Σ x_i (usa 2n datos)
#   X2 = (1/n)  * Σ x_i (usa solo los primeros n datos)
#
# Se evidenciará que X1 es mejor ya que tiene menor varianza.
###############################################################################

# Cargar las librerías necesarias
library(ggplot2)
library(knitr)

# Parámetros de la simulación
set.seed(123)                # La semilla para grantizar la reproducibilidad
n <- 50                      # tamaño de la muestra
N <- 2 * n                   # Tamaño total de la muestra
mu <- 10                     # Media de la poblacion
sigma <- 2                   # Desviación estándarde la poblacion
replicaciones <- 1000        # Número de muestras para la simulacion

# Creamos los vectores para almacenar los estimadores
X1 <- numeric(replicaciones) # Estimador que usa 2n datos (todos)
X2 <- X1  # Estimador que solo usa el primer set de datos

# Simulación: en cada réplica se extrae una muestra de tamaño 2n de la población
for(i in 1:replicaciones){
  sample_data <- rnorm(N, mean = mu, sd = sigma)
  X1[i] <- mean(sample_data)        # Usa todos los 2n datos
  X2[i] <- mean(sample_data[1:n])     # Usa solo los primeros n datos
}

# Calcular las varianzas empíricas (ECM, pues son insesgados)
ecm_X1 <- var(X1)   # Teóricamente: sigma^2/(2n) = (4)/(100) = 0.04
ecm_X2 <- var(X2)   # Teóricamente: sigma^2/n   = (4)/(50)  = 0.08

cat("Varianza (ECM) de X1 (2n datos):", ecm_X1, "\n")
cat("Varianza (ECM) de X2 (n datos):", ecm_X2, "\n")

# Crear un data frame para graficar las densidades de ambos estimadores
df <- data.frame(
  Estimador = factor(c(rep("X1 (2n datos)", length(X1)), rep("X2 (n datos)", length(X2)))),
  Valor = c(X1, X2)
)

# Gráfica comparativa: densidades de los estimadores y línea vertical en mu
g <- ggplot(df, aes(x = Valor, fill = Estimador)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = mu, linetype = "dashed", color = "black", linewidth = 1) +
  labs(title = "Comparación de Estimadores de la Media",
       subtitle = "X1: usa 2n datos vs. X2: usa n datos",
       x = "Valor estimado de μ", y = "Densidad") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal()

print(g)

# Tabla de resumen usando knitr::kable
ECM_tabla <- data.frame(
  Estimador = c("X1 (2n datos)", "X2 (n datos)"),
  ECM = c(ecm_X1, ecm_X2)
)

# Imprimir la tabla
kable(ECM_tabla, caption = "ECM (Varianza empírica) de cada estimador")

# -------------------------------
## Conclusiones
# -------------------------------
# *   El análisis en la gráfica y la teoría nos demuestra que el mejor estimador de $\mu$ es $\overline{X}_1$ ya que este utiliza los $2n$ datos y presenta una varianza $\frac{\sigma²}{(2n)}$ y esa es menor que la varianza de $\overline{X}_2 = \frac{\sigma²}{(n)}$, porque esta solo utiliza la primera mitad de lso datos $n$.
# *   Se demuestra entonces de forma teórica y de forma gráfica que ql estimador de mejor desempeño será siempre aquel que utiliza toda la muestra de datos y no solo una parte de ella.


# -------------------------------
# SITUACION 3
# -------------------------------

###############################################################################
# Estimación de θ (número total de vehículos piratas)
#
# La población consiste en vehículos numerados consecutivamente: 1,2,...,θ.
# Se extrae una muestra aleatoria de tamaño n (sin reemplazo) y se registran los
# números observados: X1, X2, …, Xn.
#
# Se proponen los siguientes estimadores:
#
#   (1)   θ̂(1) = 2·X̄ - 1
#          (porque E(X) = (θ+1)/2 para X ~ Uniforme{1, …, θ})
#
#   (2)   θ̂(2) = X_(n) + X_(1) - 1
#
#   (3)   θ̂(3) = X_max + d̄, donde d̄ es la media de los saltos consecutivos.
#          Al ordenar la muestra: d̄ = (X_(n) - X_(1))/(n-1), de modo que
#          θ̂(3) = X_(n) + (X_(n) - X_(1))/(n-1)
#
#   (4)   θ̂(4) = 2·Med(X) - 1
#
#   (5)   θ̂(5) = X̄ + 3·S
#
# Se simula el comportamiento de estos estimadores para diferentes escenarios
# variando el valor real de θ y el tamaño de muestra n.
###############################################################################

# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)
library(knitr)
library(gridExtra)

# Parámetros de simulación
set.seed(1234)
theta_true <- 200    # Valor real de θ (número total de vehículos)
n <- 10              # Tamaño de la muestra (n vehículos observados)
B <- 5000            # Número de réplicas de la simulación

# Inicializamos vectores para almacenar los valores de cada estimador
est1 <- numeric(B)   # Estimador (1): 2*mean - 1
est2 <- numeric(B)   # Estimador (2): min + max - 1
est3 <- numeric(B)   # Estimador (3): max + (max - min)/(n-1)
est4 <- numeric(B)   # Estimador (4): 2*median - 1
est5 <- numeric(B)   # Estimador (5): mean + 3*sd

# Simulación: Para cada réplica se extrae una muestra aleatoria de tamaño n de {1,2,...,θ}
for (i in 1:B) {
  muestra <- sample(1:theta_true, size = n, replace = FALSE)
  
  # Calcular cada uno de los estimadores:
  est1[i] <- 2 * mean(muestra) - 1
  est2[i] <- min(muestra) + max(muestra) - 1
  est3[i] <- max(muestra) + (max(muestra) - min(muestra)) / (n - 1)
  est4[i] <- 2 * median(muestra) - 1
  est5[i] <- mean(muestra) + 3 * sd(muestra)
}

# Crear un data frame con los resultados de las simulaciones y convertir a formato largo
df <- data.frame(
  Replication = 1:B,
  `θ1` = est1,
  `θ2` = est2,
  `θ3` = est3,
  `θ4` = est4,
  `θ5` = est5
)


df_long <- pivot_longer(df, cols = -Replication, 
                        names_to = "Estimator", values_to = "Value")
# Convertir la variable Estimator a factor (usando los valores observados)
df_long$Estimator <- as.factor(df_long$Estimator)

# Graficar las densidades de cada estimador (facetas) con la línea vertical en θ_true
p_dens <- ggplot(df_long, aes(x = Value, fill = Estimator)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~ Estimator, scales = "free") +
  geom_vline(xintercept = theta_true, linetype = "dashed", color = "black", linewidth = 1) +
  labs(title = "Distribución de los Estimadores para θ",
       subtitle = paste("θ verdadero =", theta_true, "| Tamaño de muestra n =", n),
       x = "Valor estimado de θ", y = "Densidad") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")
print(p_dens)

# Calcular sesgo, varianza y ECM para cada estimador y construir una tabla resumen
summary_table <- df_long %>%
  group_by(Estimator) %>%
  summarise(
    Bias = round(mean(Value) - theta_true, 3),
    Variance = round(var(Value), 3),
    ECM = round((mean(Value) - theta_true)^2 + var(Value), 3)
  )

# Imprimir la tabla resumen con knitr::kable
kable(summary_table, digits = 3, caption = "Resumen de los Estimadores: Sesgo, Varianza y ECM")

# Graficar barras comparativas para Bias (valor absoluto), Varianza y ECM
p_bias <- ggplot(summary_table, aes(x = Estimator, y = abs(Bias), fill = Estimator)) +
  geom_bar(stat = "identity") +
  labs(title = "Bias Absoluto de cada Estimador", y = "Bias Absoluto", x = "Estimador") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

p_variance <- ggplot(summary_table, aes(x = Estimator, y = Variance, fill = Estimator)) +
  geom_bar(stat = "identity") +
  labs(title = "Varianza de cada Estimador", y = "Varianza", x = "Estimador") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

p_ecm <- ggplot(summary_table, aes(x = Estimator, y = ECM, fill = Estimator)) +
  geom_bar(stat = "identity") +
  labs(title = "ECM de cada Estimador", y = "ECM", x = "Estimador") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

# Combinar las tres gráficas en un panel vertical
grid.arrange(p_bias, p_variance, p_ecm, ncol = 1)

# -------------------------------
## Conclusiones
# -------------------------------

# *   El indicador $\theta_4$ presenta un sesgo cercano a cero $(0.04)$ lo cual es el ideal pero la varianza es la mas alta de todos $(2856.68)$ y nos indica que los valores estimados tienen variaciones altas entre una muestra y la otra.
# *   El indicador $\theta_5$ presenta el sesgo mas elevado $(71.79)$ y una varianza alta $(1064.25)$ aunque la varianza está entre la media de todos el alto sesgo nos indica que no es un estimador muy adecuado para la implementacion.
# *   Los indicadores $\theta_1$ y $\theta_2$ presentan los sesgos mas bájos $(0.22)$ y $(0.23)$ respectivamente, pero la varianza de $\theta_1$ es alta $(1234.53)$ mientras la varianza de $\theta_2$ es moderada pero no es la mas baja de entre todos los indicadores $(562.23)$
# *   El indicador $\theta_3$ presenta un sesgo bajo y aunque no es el menor de todos los estimadores si es cercano a 0: $(1.08)$ además si tiene el valor de varianza mas bajo de todo el set $(306.33)$.
# *   El ECM es la sumatoria del cuadrado del sesgo y la varianza y refleja el desempeño o presicion global del estimador, tomando en cuenta esto el ECM más bajo es de $\theta_3$ con $(307.49)$ lo cual nos indica que a pesar de que el sesgo no es el mas bájo la combinacion de este y la poca varianza hacen este indicador como el mejor en precision global.
# *   Los estimadores $\theta_1, \theta_4 y \theta_5$ tiene ECMs muy altos $(1234.58, 2856.68, 6212.32)$ respectivamente y esto indica que no sin importar el bajo sesgo, la alta varianza los convierte en inadecuados.
# *   El estimador $\theta_2$ tiene igualmente un ECM bajo $(562.28)$, tiene tambien bajo sesgo $(0.23)$ y una varianza moderada $(562.23)$ pero el ECM es mayor que el ECM de $\theta_3$ con $(307.45)$, con lo cual concluimos que $\theta_2$ es un buen indicador.
# La mejor estimación de $\theta$ se logra usando el estimador $\hat{\theta_3} = X_{max} + \frac{X_{max} -X_{min}}{n-1}$, esto a que este presenta la mejor precision global $(307.49)$ manteniendo un sesgo cercano a cero $(1.08)$ y la varianza mas baja $(306.33)$, ahora bien este desafio necesita estimar un valor desconocido de $\theta$ por tanto una baja varianza nos ayuda a estimar un valor que sea mas cercano al valor real de $\theta$.


# -------------------------------
# SITUACION 4
# -------------------------------

# Configuración inicial
set.seed(123)           # Para reproducibilidad
M <- 10000              # Número de simulaciones
n <- 10                 # Tamaño muestral (n > 2)
lambda_true <- 2        # Valor real de lambda

# Inicializar vectores para almacenar los estimadores
lambda_hat_prop <- numeric(M)  # Estimador propuesto: (n-1)/S_n
lambda_hat_MLE  <- numeric(M)  # Estimador MLE (y de momentos): n/S_n = 1/mean(X)

# Simulación de los experimentos
for(i in 1:M){
  # Generar n tiempos de atención de una exponencial con parámetro lambda_true
  X <- rexp(n, rate = lambda_true)
  S_n <- sum(X)
  
  # Estimador propuesto: (n-1)/S_n
  lambda_hat_prop[i] <- (n - 1) / S_n
  
  # Estimador MLE y de momentos: n/S_n = 1/mean(X)
  lambda_hat_MLE[i] <- n / S_n
}

# Cálculo de sesgo, varianza y ECM para cada estimador
bias_prop <- mean(lambda_hat_prop) - lambda_true
var_prop  <- var(lambda_hat_prop)
mse_prop  <- mean((lambda_hat_prop - lambda_true)^2)

bias_MLE <- mean(lambda_hat_MLE) - lambda_true
var_MLE  <- var(lambda_hat_MLE)
mse_MLE  <- mean((lambda_hat_MLE - lambda_true)^2)

# Crear una tabla con los resultados finales
resultados <- data.frame(
  Estimador = c("Propuesto ((n-1)/S_n)", "MLE (n/S_n = 1/mean(X))"),
  Sesgo     = c(round(bias_prop, 4), round(bias_MLE, 4)),
  Varianza  = c(round(var_prop, 6), round(var_MLE, 6)),
  ECM       = c(round(mse_prop, 6), round(mse_MLE, 6))
)

# Mostrar la tabla de resultados
print(resultados)

# Cargar librerías para gráficos
library(ggplot2)
library(reshape2)

# Crear un data frame con los estimadores para los gráficos
df <- data.frame(`(n-1)/S_n` = lambda_hat_prop, `n/S_n` = lambda_hat_MLE)
df_melt <- melt(df, variable.name = "Estimador", value.name = "Valor")

# Histograma superpuesto de los estimadores
p1 <- ggplot(df_melt, aes(x = Valor, fill = Estimador)) +
  geom_histogram(alpha = 0.5, bins = 50, position = "identity") +
  geom_vline(xintercept = lambda_true, linetype = "dashed", color = "black", size = 1) +
  labs(title = "Distribución de los Estimadores de λ",
       x = "Valor Estimado de λ", y = "Frecuencia") +
  theme_minimal()

# Boxplot comparativo de los estimadores
p2 <- ggplot(df_melt, aes(x = Estimador, y = Valor, fill = Estimador)) +
  geom_boxplot(alpha = 0.7) +
  geom_hline(yintercept = lambda_true, linetype = "dashed", color = "black", size = 1) +
  labs(title = "Boxplot de los Estimadores de λ",
       x = "Estimador", y = "Valor Estimado de λ") +
  theme_minimal()

# Mostrar los gráficos
print(p1)
print(p2)

# -------------------------------
## Conclusiones
# -------------------------------
# *   Tanto en el soporte matemático como el programa en R podemos observar que el estimador $\hat{\lambda}= \frac{n-1}{S_n}$ es **insesgado**.
# *   El ECM calculado es $ECM(\hat{\lambda}) = \frac{\lambda²}{n-2}$.
# *   Los estimadores calculados de momentos y máxima verosimilitud coinciden ya que $\hat{\lambda}=\frac{1}{\overline{X}}=\frac{n}{S_n}$, con lo cual podemos definir que no importa cual usamos vamos a obtener la misma estimación, en términos generales podemos decir que aplicar el método de momentos es mas fácil de aplicar en situaciones que necesiten rapidez y facilidad, mientras, aplicar el método de máxima verosimilitud cuando se necesite un resultado mas robusto.



