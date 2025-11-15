# Instalar paquetes necesarios (ejecutar solo una vez)
# install.packages(c("tidyverse", "ggplot2", "corrplot", "VIM", "naniar"))

# Cargar librerías
library(tidyverse)
library(ggplot2)
library(corrplot)
library(VIM)
library(naniar)

# 1. CARGAR Y EXPLORAR LOS DATOS
# Crear dataset de ejemplo con valores faltantes y atípicos
set.seed(123)
n <- 200

datos <- data.frame(
  edad = c(rnorm(n-10, mean = 35, sd = 10), runif(10, 80, 100)), # con atípicos
  ingresos = c(rnorm(n-15, mean = 50000, sd = 15000), runif(15, 150000, 200000)),
  horas_estudio = c(rnorm(n-8, mean = 15, sd = 5), runif(8, 40, 60)),
  calificacion = rnorm(n, mean = 75, sd = 10),
  genero = sample(c("M", "F"), n, replace = TRUE)
)

# Introducir valores faltantes aleatorios
datos[sample(1:n, 20), "ingresos"] <- NA
datos[sample(1:n, 15), "edad"] <- NA
datos[sample(1:n, 10), "horas_estudio"] <- NA


# 2. EXPLORACIÓN INICIAL
cat("=== EXPLORACIÓN INICIAL ===\n")
str(datos)
summary(datos)

# 3. DETECCIÓN DE VALORES FALTANTES
cat("\n=== VALORES FALTANTES ===\n")
print(miss_var_summary(datos))

# Visualizar valores faltantes
gg_miss_var(datos) + 
  labs(title = "Patrón de Valores Faltantes por Variable")

aggr(datos, numbers = TRUE, sortVars = TRUE, 
     labels = names(datos), cex.axis = 0.7, gap = 3)

# 4. TRATAMIENTO DE VALORES FALTANTES
# Opción 1: Eliminar filas con NA
datos_sin_na <- na.omit(datos)
cat("\nFilas originales:", nrow(datos))
cat("\nFilas después de eliminar NA:", nrow(datos_sin_na))

# Opción 2: Imputación con media
datos_imputados <- datos
datos_imputados$ingresos[is.na(datos_imputados$ingresos)] <- 
  mean(datos_imputados$ingresos, na.rm = TRUE)
datos_imputados$edad[is.na(datos_imputados$edad)] <- 
  mean(datos_imputados$edad, na.rm = TRUE)
datos_imputados$horas_estudio[is.na(datos_imputados$horas_estudio)] <- 
  mean(datos_imputados$horas_estudio, na.rm = TRUE)
summary(datos_imputados)


# 5. DETECCIÓN DE VALORES ATÍPICOS
boxplot(datos_imputados$edad)
detectar_atipicos <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  limite_inferior <- Q1 - 1.5 * IQR
  limite_superior <- Q3 + 1.5 * IQR
  return(which(x < limite_inferior | x > limite_superior))
}







cat("\n=== VALORES ATÍPICOS ===\n")
cat("Atípicos en edad:", length(detectar_atipicos(datos$edad)), "\n")
cat("Atípicos en ingresos:", length(detectar_atipicos(datos$ingresos)), "\n")
cat("Atípicos en horas_estudio:", length(detectar_atipicos(datos$horas_estudio)), "\n")

# 6. ANÁLISIS VISUAL - BOXPLOTS
par(mfrow = c(2, 2))
boxplot(datos$edad, main = "Boxplot - Edad", col = "lightblue")
boxplot(datos$ingresos, main = "Boxplot - Ingresos", col = "lightgreen")
boxplot(datos$horas_estudio, main = "Boxplot - Horas Estudio", col = "lightcoral")
boxplot(datos$calificacion, main = "Boxplot - Calificación", col = "lightyellow")

# Boxplots con ggplot2
ggplot(datos, aes(y = edad)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(title = "Distribución de Edad", y = "Edad") +
  theme_minimal()

# 7. HISTOGRAMAS
par(mfrow = c(2, 2))
hist(datos$edad, main = "Histograma - Edad", col = "lightblue", xlab = "Edad")
hist(datos$ingresos, main = "Histograma - Ingresos", col = "lightgreen", xlab = "Ingresos")
hist(datos$horas_estudio, main = "Histograma - Horas Estudio", col = "lightcoral", xlab = "Horas")
hist(datos$calificacion, main = "Histograma - Calificación", col = "lightyellow", xlab = "Calificación")

# Histogramas con ggplot2
ggplot(datos, aes(x = edad)) +
  geom_histogram(binwidth = 5, fill = "lightblue", color = "black", alpha = 0.7) +
  labs(title = "Distribución de Edad", x = "Edad", y = "Frecuencia") +
  theme_minimal()

# 8. DIAGRAMAS DE DISPERSIÓN
par(mfrow = c(2, 2))
plot(datos$edad, datos$ingresos, main = "Edad vs Ingresos", 
     xlab = "Edad", ylab = "Ingresos", col = "blue", pch = 16)
plot(datos$horas_estudio, datos$calificacion, main = "Horas Estudio vs Calificación",
     xlab = "Horas Estudio", ylab = "Calificación", col = "red", pch = 16)
plot(datos$edad, datos$calificacion, main = "Edad vs Calificación",
     xlab = "Edad", ylab = "Calificación", col = "green", pch = 16)

# Diagramas de dispersión con ggplot2
ggplot(datos, aes(x = horas_estudio, y = calificacion)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relación: Horas de Estudio vs Calificación",
       x = "Horas de Estudio", y = "Calificación") +
  theme_minimal()

# 9. ANÁLISIS DE CORRELACIÓN Y COVARIANZA
# Seleccionar solo variables numéricas
variables_numericas <- datos %>% select(edad, ingresos, horas_estudio, calificacion)

# Matriz de correlación
matriz_cor <- cor(variables_numericas, use = "complete.obs")
cat("\n=== MATRIZ DE CORRELACIÓN ===\n")
print(matriz_cor)

# Matriz de covarianza
matriz_cov <- cov(variables_numericas, use = "complete.obs")
cat("\n=== MATRIZ DE COVARIANZA ===\n")
print(matriz_cov)

# Visualización de correlaciones
corrplot(matriz_cor, method = "color", type = "upper", 
         order = "hclust", tl.cex = 0.8, tl.col = "black")

# 10. ANÁLISIS POR GRUPOS (genero)
ggplot(datos, aes(x = genero, y = calificacion, fill = genero)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Calificación por Género", x = "Género", y = "Calificación") +
  theme_minimal()

# Resumen estadístico por grupo
datos %>% 
  group_by(genero) %>% 
  summarise(
    n = n(),
    media_edad = mean(edad, na.rm = TRUE),
    media_ingresos = mean(ingresos, na.rm = TRUE),
    media_calificacion = mean(calificacion, na.rm = TRUE)
  )