# -------------------------------
# UNIVERSIDAD AUTONOMA DE OCCIDENTE
## MAESTRIA EN INTELIGENCIA ARTIFICIAL Y CIENCIA DE DATOS
## INFERENCIA ESTADISTICA
## DESAFÍO 3
## Profesor: Cristian E García.
## Alumno: Yoniliman Galvis Aguirre
## Codigo: 22500214
## Repositorio: https://github.com/ygalves/Master-inferencia-estadistica.git
## CALI - COLOMBIA
### 03/30/2025
# -------------------------------

###############################################################################
# SITUACION 1
# Calidad del vino vs la presencia de lluvia en la cosecha
###############################################################################

library(ggplot2)
library(knitr)
library(broom)

# --- Definir los datos manualmente ---
datos <- data.frame(
  cosecha = c(1961:2004),
  calidad = c(5,4,1,3,1,4,3,2,2,4,3,1,2,1,4,3,2,4,3,1,3,5,3,1,4,4,1,4,4,5,3,1,3,3.5,4,5,4,3.5,3.5,5,3.5,4,5,4),
  dias = c(28,50,53,38,46,40,35,38,45,47,45,54,39,45,40,32,47,50,48,54,39,30,41,44,41,46,47,40,21,32,40,39,36,29,27,32,25,35,30,41,43,47,30,49),
  lluvia = c(0,0,1,0,1,0,1,1,1,0,1,1,1,1,1,0,0,0,0,1,1,0,0,1,0,0,1,0,0,0,1,1,1,1,0,0,0,1,0,0,0,0,0,0)
)

# Convertir la variable 'lluvia' en factor
datos$lluvia <- factor(datos$lluvia, levels = c(0,1), labels = c("Sin lluvia", "Con lluvia"))

# Parte (a): Presentar el gráfico de dispersión con área sombreada para la intersección y la línea de regresion para las dos condiciones,a si podemos identificar como se comportan los datos

ggplot(datos, aes(x = dias, y = calidad, color = lluvia, fill = lluvia)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_ribbon(aes(ymin = calidad - 0.5, ymax = calidad + 0.5), alpha = 0.2, color = NA) +
  labs(title = "Calidad de la cosecha vs. Días de finalización",
       x = "Días desde el 31 de agosto",
       y = "Calidad de la cosecha",
       color = "Condición de lluvia",
       fill = "Condición de lluvia") +
  theme_minimal()


formulas <- data.frame(
  Condición = c("Sin lluvia", "Con lluvia"),
  Fórmula = c("$$y = \\beta_0 + \\beta_1 \\times dias$$", 
              "$$y = (\\beta_0 + \\beta_2) + (\\beta_1 + \\beta_3) \\times dias$$")
)
kable(formulas, escape = FALSE, caption = "Fórmulas de regresión para cada condición de lluvia")

# Parte (b): Ajuste del modelo con interacción ---


modelo <- lm(calidad ~ dias * lluvia, data = datos)
summary(modelo)

# Extraer coeficientes
coeficientes <- coef(modelo)
beta1 <- coeficientes["dias"]
beta3 <- coeficientes["dias:lluviaCon lluvia"]

# Calcular la pendiente para el grupo con lluvia
pendiente_lluvia <- beta1 + beta3

# Calcular el número de días para que la calidad disminuya en 1 unidad en presencia de lluvia
d_est <- -1 / pendiente_lluvia

# --- Cálculo del error estándar para el intervalo de confianza ---
vcov_mat <- vcov(modelo)
var_beta1 <- vcov_mat["dias", "dias"]
var_beta3 <- vcov_mat["dias:lluviaCon lluvia", "dias:lluviaCon lluvia"]
cov_beta1_beta3 <- vcov_mat["dias", "dias:lluviaCon lluvia"]

# Varianza de (beta1+beta3)
var_t <- var_beta1 + var_beta3 + 2 * cov_beta1_beta3
se_t <- sqrt(var_t)

# Método delta para calcular el error estándar de d = -1/t
se_d <- se_t / (pendiente_lluvia^2)

# Intervalo de confianza del 95%
z <- qnorm(0.975)
LI <- d_est - z * se_d
LS <- d_est + z * se_d

# Presentar resultados con knitr

resultado <- data.frame(
  Estimación = d_est,
  `Límite Inferior` = LI,
  `Límite Superior` = LS
)
kable(resultado, caption = "Intervalo de confianza para la disminución de 1 unidad en calidad con lluvia")

###############################################################################
# SITUACION 2
# estimación de los parámetros Theta y Phi
###############################################################################

# hacer el test si el resultado es correcto, para ello calculamos las 2 estimaciones usando R, estimamos valores para Y1,Y2 y Y3
Y <- c(Y1 = 10, Y2 = 15, Y3 = 12)

# hagamos la matriz
X <- matrix(c(1, 0,   # Para Y1: theta + 0*phi
              2, -1,  # Para Y2: 2theta - phi
              1, 2),  # Para Y3: theta + 2phi
            nrow = 3, byrow = TRUE)

# Calculemos el estimador usando R
beta_hat <- solve(t(X) %*% X) %*% t(X) %*% Y

# calculemos las estimaciones de theta y de phi
theta_hat <- unname(beta_hat[1])
phi_hat   <- unname(beta_hat[2])

# comprobemos:
# Calculamos theta manualmente según la fórmula: theta=(Y1+2Y2+Y3)/6
theta_hat_manual <- (Y[1] + 2 * Y[2] + Y[3]) / 6
# Calculamos phi manualmente según la fórmula: (-Y2 + 2*Y3) / 5
phi_hat_manual <- (-Y[2] + 2 * Y[3]) / 5

# Crear una tabla de resultados:
resultados <- data.frame(
  Parámetro = c("θ", "φ"),
  Método = rep("Mínimos cuadrados", 2),
  `Est_R` = c(theta_hat, phi_hat),
  `Est_manual` = c(theta_hat_manual, phi_hat_manual)
)

# Asegurarse de que no existan nombres de fila
rownames(resultados) <- NULL

# Mostrar la tabla con knitr::kable
library(knitr)
kable(resultados, digits = 4, 
      caption = "Comparación de estimaciones de θ y φ")


###############################################################################
# SITUACION 3
# Casos de melanoma durante 1969-1991 en USA
###############################################################################

# creamos los vectores para los datos
datos <- data.frame(
  Región = rep(c("Norte", "Sur"), each = 6),
  Edad = rep(c("0-35", "35-44", "45-54", "55-64", "65-74", "75+"), times = 2),
  Casos = c(61, 76, 98, 104, 63, 80,
            64, 75, 68, 63, 45, 27),
  Población = c(2880262, 564535, 592983, 450740, 270908, 161850,
                1074246, 220407, 198119, 134084, 70708, 34233)
)

# Hagamos el cálculo de tasa y logaritmo
datos$Tasa <- datos$Casos / datos$Población
datos$logTasa <- log(datos$Tasa)

# creamos el factor de edad para conservar el orden, 
datos$Edad <- factor(datos$Edad, levels = c("0-35", "35-44", "45-54", "55-64", "65-74", "75+"))

# creamos la gGrafica para el logTasa vs Edad diferenciados por colores

library(ggplot2)
ggplot(datos, aes(x = Edad, y = logTasa, group = Región, color = Región)) +
  geom_point(size = 3) +
  geom_line(aes(group = Región)) +
  labs(title = "Log de la tasa observada de melanoma vs. Edad",
       x = "Grupo de edad",
       y = "log(Tasa de melanoma)") +
  theme_minimal()

# llevamos la region al factor y aseguramos un orden con el Norte como referencia
datos$Región <- factor(datos$Región, levels = c("Norte", "Sur"))

# asjustemos el modelo de poisson con un offset(poblacion)
modelo <- glm(Casos ~ Región + Edad, 
              offset = log(Población), 
              family = poisson(link = "log"), 
              data = datos)
summary(modelo)

# Calculo de la rata de la desviación residual a los grados de libertad
dispersion <- sum(residuals(modelo, type = "pearson")^2) / modelo$df.residual
cat("Ratio de dispersión =", dispersion, "\n")

###############################################################################
# SITUACION 4
# Estudio de alimento primario caimanes en lagos de la florida, US
###############################################################################

# vamos a usar VGAM, se necesita instalar el paquete en R-studio
library(VGAM)

# cramos los vectores para el análisis y configuramos las variables Lago y tamaño como factores, esto le indica a R que cuando lo use en un modelo la librería VGRAM con vglm() cree de forma automática variables dummy para cada uno de susu niveles, similar a un one-hot pero a diferencia dummy descarta una columna y de esta manera los coeficientes se interpretan en comparacion con ese nivel de referencia.

datos <- data.frame(
  Lago = rep(c("Hancock", "Ocklawaha", "Trafford", "George"), each = 2),
  Tamaño = rep(c("≤ 2.3", "> 2.3"), times = 4),
  Peces = c(23, 7, 5, 13, 5, 8, 16, 17),
  Invertebrados = c(4, 0, 11, 8, 11, 7, 19, 1),
  Reptiles = c(2, 1, 1, 6, 2, 6, 1, 0),
  Aves = c(2, 3, 0, 1, 1, 3, 2, 1),
  Otros = c(8, 5, 3, 0, 5, 5, 3, 3)
)

# Aseguremonos de reconfigurar el factor de Tamaño para que "≤ 2.3" sea el nivel de referencia como lo solicita el ejercicio
datos$Tamaño <- factor(datos$Tamaño, levels = c("≤ 2.3", "> 2.3"))

# A.
#Ajustamos el modelo utilizando VGAM vglm(), en la función multinomial() y fijamos la categoría de referencia para la respuesta solicitada y para ello usamos la primera columna cbind que corresponde a los Peces como se pide en el ejercicio.
# vamos a obtener los coeficientes para cada caegoria en función de las variables predictoras exceptuando a los Peces que es la referencia.

modelo <- vglm(cbind(Peces, Invertebrados, Reptiles, Aves, Otros) ~ Lago + Tamaño,
               family = multinomial(refLevel = 1),  # refLevel = 1 esto nos indica que "Peces" es la variable de referencia
               data = datos)

# Resumen del modelo creado
summary(modelo)

print('coeficientes del modelo')
str(coef(modelo))

# Extraer los coeficientes del modelo
coef_model <- coef(modelo)
# Ver la estructura para confirmar
str(coef_model)
# Los nombres tienen la forma "(Predictor):(Nivel)". 
# Por ejemplo: "(Intercept):1", "LagoHancock:1", "Tamaño≤ 2.3:1", etc.
coef_names <- names(coef_model)

# Dividir los nombres en dos partes utilizando ':' como separador
split_names <- strsplit(coef_names, split = ":")

# Extraer la primera parte (Predictor) y la segunda parte (Nivel de respuesta)
Predictor <- sapply(split_names, `[`, 1)
NivelRespuesta <- sapply(split_names, `[`, 2)

# Crear un data frame con la información extraída y los coeficientes
resultados <- data.frame(
  Predictor = Predictor,
  NivelRespuesta = NivelRespuesta,
  Estimate = coef_model,
  OR = exp(coef_model)
)

# Quitar nombres de fila para que no aparezcan etiquetas no deseadas
rownames(resultados) <- NULL

# Mostrar la tabla usando knitr::kable 
library(knitr)
kable(resultados, digits = 4, 
      caption = "Razones de chances (Odds Ratios) para cada categoría de alimento (referencia: Peces)")

# Verificamos el orden en el modelo

# hacemos una matriz de respuesta con cbind() para ver el orden
response_matrix <- cbind(Peces = datos$Peces, 
                         Invertebrados = datos$Invertebrados, 
                         Reptiles = datos$Reptiles, 
                         Aves = datos$Aves, 
                         Otros = datos$Otros)

# Extraemos los nombres de las columnas de la matriz de respuesta
response_names <- colnames(response_matrix)

# Imprimimimos los nombres para confirmar el orden
print(response_names)

# Creamos manualmente el data frame de nuevas combinaciones
newdata <- expand.grid(
  Tamaño = c("≤ 2.3", "> 2.3"),
  Lago = c("Hancock", "Ocklawaha", "Trafford", "George")
)

# Convertimos las variables a factor con el orden deseado
newdata$Tamaño <- factor(newdata$Tamaño, levels = c("≤ 2.3", "> 2.3"))
newdata$Lago   <- factor(newdata$Lago, levels = c("Hancock", "Ocklawaha", "Trafford", "George"))

# Comprobamos newdata
print(newdata)

# Vamos a usar newdata para hacer unas predecciones
pred <- predict(modelo, newdata = newdata, type = "response")

# La matriz 'pred' tiene 5 columnas correspondientes a las categorías de alimento
newdata$Peces <- pred[,1]
newdata$Invertebrados <- pred[,2]
newdata$Reptiles <- pred[,3]
newdata$Aves <- pred[,4]
newdata$Otros <- pred[,5]

# Convertimos a formato largo para graficar
library(tidyr)
newdata_long <- pivot_longer(newdata, 
                             cols = c("Peces", "Invertebrados", "Reptiles", "Aves", "Otros"),
                             names_to = "Alimento",
                             values_to = "Probabilidad")

# Ahora el Gráfico: Probabilidades predichas para cada categoría, comparando Tamaño y diferenciando por Lago
library(ggplot2)
ggplot(newdata_long, aes(x = Tamaño, y = Probabilidad, fill = Lago)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Alimento) +
  labs(title = "Probabilidades predichas de elección de alimento primario",
       subtitle = "Comparación por Tamaño y Lago (referencia en la respuesta: Peces)",
       x = "Tamaño del caimán",
       y = "Probabilidad predicha",
       fill = "Lago") +
  theme_minimal()

###############################################################################
# SITUACION 5
# Estimadores Minimos Cuadrados
###############################################################################

# Semilla para que sea reproducible
set.seed(123)

# Número de observaciones simuladas
n <- 100

# Simular x distribucion normal o cualquier otra
x <- rnorm(n, mean = 0, sd = 1)  # Si mean = 0, se cumple \bar{x}=0

# Parámetros verdaderos
beta0 <- 2
beta1 <- 3
sigma <- 1

# Simulamos los errores
epsilon <- rnorm(n, mean = 0, sd = sigma)

# Generarmos y
Y <- beta0 + beta1 * x + epsilon

# Calculamos las medias
x_bar <- mean(x)
Y_bar <- mean(Y)

# los estimadores de mínimos cuadrados
beta1_hat <- sum((x - x_bar) * (Y - Y_bar)) / sum((x - x_bar)^2)
beta0_hat <- Y_bar - beta1_hat * x_bar

cat("Estimación de beta0:", beta0_hat, "\n")
cat("Estimación de beta1:", beta1_hat, "\n")

# Calculamos la varianza de beta1_hat (la fórmula teórica)
var_beta1_hat <- sigma^2 / sum((x - x_bar)^2)
cat("Var(beta1_hat):", var_beta1_hat, "\n")

# Calculamos la covarianza entre beta0_hat y beta1_hat teóricamente
cov_beta0_beta1 <- -x_bar * var_beta1_hat
cat("Cov(beta0_hat, beta1_hat):", cov_beta0_beta1, "\n")

###############################################################################
# SITUACION 6
# datos de severidad (3 niveles) de los efectos colaterales de 4 tipos de operación en 4 hospitales
###############################################################################

# Cargar librerías necesarias
library(tidyr)
library(ggplot2)
library(dplyr)

# Crear un data frame con los datos (cada fila corresponde a una combinación de Operación y Hospital)
# Los datos se han extraído de la tabla proporcionada:
datos <- data.frame(
  Operacion = rep(1:4, each = 4),
  Hospital = rep(c("Hospital 1", "Hospital 2", "Hospital 3", "Hospital 4"), times = 4),
  S1 = c(23, 18, 8, 12,   23, 18, 12, 15,    20, 13, 11, 14,    24, 9, 7, 13),
  S2 = c(7, 6, 6, 9,       10, 6, 4, 3,   13, 13, 6, 8,    10, 15, 7, 6),
  S3 = c(2, 1, 3, 1,       5, 2, 4, 2,      5, 2, 2, 3,     6, 2, 4, 4)
)

# Visualizar los datos originales
print(datos)

# Convertir el data frame a formato "largo" para poder graficar con ggplot2.
# Esto crea columnas "Severidad" y "Count"
datos_long <- pivot_longer(datos, 
                           cols = c("S1", "S2", "S3"), 
                           names_to = "Severidad", 
                           values_to = "Count")

# Convertir la variable Severidad a factor con etiquetas significativas
datos_long$Severidad <- factor(datos_long$Severidad, 
                               levels = c("S1", "S2", "S3"), 
                               labels = c("Ninguno", "Leve", "Moderada"))
datos_long$Operacion <- factor(datos_long$Operacion)

# Crear un gráfico de barras apiladas:
ggplot(datos_long, aes(x = Operacion, y = Count, fill = Severidad)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~ Hospital) +
  labs(title = "Distribución de la Severidad de Efectos Colaterales",
       subtitle = "Por tipo de operación y hospital",
       x = "Tipo de Operación",
       y = "Número de Casos",
       fill = "Severidad") +
  theme_minimal() +
  theme(text = element_text(size = 12))

# Datos agregados por operación
datos_agg <- data.frame(
  Operacion = factor(1:4),
  S1 = c(61, 68, 58, 53),
  S2 = c(28, 23, 40, 38),
  S3 = c(7, 13, 12, 16)
)
datos_agg

library(nnet)

# La respuesta se construye con cbind() a partir de los conteos de severidad.
Y <- as.matrix(datos_agg[, c("S1", "S2", "S3")])

# Fijamos la operación 4 como referencia
datos_agg$Operacion <- relevel(datos_agg$Operacion, ref = "4")

# Ajustamos el modelo multinomial
modelo_mult <- multinom(Y ~ Operacion, data = datos_agg)
summary(modelo_mult)

# Extraer y mostrar la matriz de coeficientes del modelo
coef_modelo <- summary(modelo_mult)$coefficients
print(coef_modelo)

# Para la operación 1, sumamos:
eta2_op1 <- coef_modelo["S2", "(Intercept)"] + coef_modelo["S2", "Operacion1"]
eta3_op1 <- coef_modelo["S3", "(Intercept)"] + coef_modelo["S3", "Operacion1"]

cat("Para la operación 1:\n")
cat("P(S=1|O=1) = 1 / (1 + exp(", round(eta2_op1, 4), ") + exp(", round(eta3_op1, 4), "))\n")
cat("P(S=2|O=1) = exp(", round(eta2_op1, 4), ") / (1 + exp(", round(eta2_op1, 4), ") + exp(", round(eta3_op1, 4), "))\n")
cat("P(S=3|O=1) = exp(", round(eta3_op1, 4), ") / (1 + exp(", round(eta2_op1, 4), ") + exp(", round(eta3_op1, 4), "))\n")

library(reshape2)
coef_mult_long <- melt(modelo_sum$coefficients)
names(coef_mult_long) <- c("Severidad", "Operacion", "Estimate")
coef_mult_long$OR <- exp(coef_mult_long$Estimate)
library(knitr)
kable(coef_mult_long, digits = 4,
      caption = "Coeficientes y Razones de Chances (OR) del Modelo Multinomial")



