install.packages("readxl")
library(readxl)


file_path <- normalizePath("D:/OneDrive - Universidad Iberoamericana, A.C/2024/Analisis Luis/Docentes_deciles/ID docentes.xlsx")

docentes_data <- read_excel(file_path)
head(docentes_data)

# Resumen estadístico para promedio global y desviación global
summary(docentes_data$`promedio global`)
summary(docentes_data$Desviacion_Global)


# Histograma del promedio global
hist(docentes_data$`promedio global`, 
     main = "Histograma del Promedio Global", 
     xlab = "Promedio Global", 
     col = "lightblue", 
     border = "black", 
     breaks = 10)

# Histograma de la desviación global
hist(docentes_data$Desviacion_Global, 
     main = "Histograma de la Desviación Global", 
     xlab = "Desviación Global", 
     col = "lightgreen", 
     border = "black", 
     breaks = 10)


# Boxplot del promedio global
boxplot(docentes_data$`promedio global`, 
        main = "Boxplot del Promedio Global", 
        ylab = "Promedio Global", 
        col = "lightblue")

# Boxplot de la desviación global
boxplot(docentes_data$Desviacion_Global, 
        main = "Boxplot de la Desviación Global", 
        ylab = "Desviación Global", 
        col = "lightgreen")

# Media y desviación estándar del promedio global
mean_promedio_global <- mean(docentes_data$`promedio global`, na.rm = TRUE)
sd_promedio_global <- sd(docentes_data$`promedio global`, na.rm = TRUE)

# Media y desviación estándar de la desviación global
mean_desviacion_global <- mean(docentes_data$Desviacion_Global, na.rm = TRUE)
sd_desviacion_global <- sd(docentes_data$Desviacion_Global, na.rm = TRUE)

# Mostrar resultados
mean_promedio_global
sd_promedio_global
mean_desviacion_global
sd_desviacion_global



# Cálculo de cuartiles para promedio global
cuartiles_promedio <- quantile(docentes_data$`promedio global`, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# Cálculo de cuartiles para desviación global
cuartiles_desviacion <- quantile(docentes_data$Desviacion_Global, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# Mostrar resultados
cuartiles_promedio
cuartiles_desviacion






# Cálculo de quintiles para promedio global
quintiles_promedio <- quantile(docentes_data$`promedio global`, probs = seq(0, 1, 0.2), na.rm = TRUE)

# Cálculo de quintiles para desviación global
quintiles_desviacion <- quantile(docentes_data$Desviacion_Global, probs = seq(0, 1, 0.2), na.rm = TRUE)

# Mostrar resultados
quintiles_promedio
quintiles_desviacion


# Cálculo de deciles para promedio global
deciles_promedio <- quantile(docentes_data$`promedio global`, probs = seq(0, 1, 0.1), na.rm = TRUE)

# Cálculo de deciles para desviación global
deciles_desviacion <- quantile(docentes_data$Desviacion_Global, probs = seq(0, 1, 0.1), na.rm = TRUE)

# Mostrar resultados
deciles_promedio
deciles_desviacion


# Agregar una columna para el grupo de cuartiles en promedio global
docentes_data$cuartil_promedio <- cut(docentes_data$`promedio global`, 
                                      breaks = quantile(docentes_data$`promedio global`, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE), 
                                      include.lowest = TRUE)

# Grafico de barras para cuartiles del promedio global
library(ggplot2)
ggplot(docentes_data, aes(x = cuartil_promedio)) + 
  geom_bar(fill = "lightblue") +
  labs(title = "Distribución por Cuartiles del Promedio Global", x = "Cuartiles", y = "Frecuencia")



# Agregar una columna para el grupo de deciles en promedio global
docentes_data$decil_promedio <- cut(docentes_data$`promedio global`, 
                                    breaks = quantile(docentes_data$`promedio global`, probs = seq(0, 1, 0.1), na.rm = TRUE), 
                                    include.lowest = TRUE)

# Grafico de barras para deciles del promedio global
ggplot(docentes_data, aes(x = decil_promedio)) + 
  geom_bar(fill = "lightgreen") +
  labs(title = "Distribución por Deciles del Promedio Global", x = "Deciles", y = "Frecuencia")


# Agregar una columna para el grupo de quintiles en promedio global
docentes_data$quintil_promedio <- cut(docentes_data$`promedio global`, 
                                      breaks = quantile(docentes_data$`promedio global`, probs = seq(0, 1, 0.2), na.rm = TRUE), 
                                      include.lowest = TRUE)

# Gráfico de barras para quintiles del promedio global
ggplot(docentes_data, aes(x = quintil_promedio)) + 
  geom_bar(fill = "lightblue") +
  labs(title = "Distribución por Quintiles del Promedio Global", x = "Quintiles", y = "Frecuencia")







install.packages("plotly")

install.packages("plotly")
library(plotly)

# Crear un gráfico 3D
plot_ly(docentes_data, 
        x = ~`promedio global`, 
        y = ~Desviacion_Global, 
        z = ~I(1),  # Esto es solo para permitir la representación en 3D
        type = 'scatter3d', 
        mode = 'markers', 
        marker = list(size = 3, color = 'blue', opacity = 0.6)) %>%
  layout(title = "Relación entre Promedio Global y Desviación Estándar",
         scene = list(
           xaxis = list(title = "Promedio Global"),
           yaxis = list(title = "Desviación Global"),
           zaxis = list(title = "N/A")  # Esta dimensión no se utiliza
         ))





# Crear una cuadrícula de datos para el gráfico de superficie
library(dplyr)
library(tidyr)

# Crear una cuadrícula para promedio global y desviación
grid_data <- expand.grid(
  `promedio global` = seq(min(docentes_data$`promedio global`, na.rm = TRUE), 
                          max(docentes_data$`promedio global`, na.rm = TRUE), 
                          length.out = 50),
  Desviacion_Global = seq(min(docentes_data$Desviacion_Global, na.rm = TRUE), 
                          max(docentes_data$Desviacion_Global, na.rm = TRUE), 
                          length.out = 50)
)

# Calcular el promedio global estimado
grid_data$Estimacion <- with(grid_data, 
                             lm(`promedio global` ~ Desviacion_Global)$fitted.values)

# Gráfico de superficie 3D
plot_ly(grid_data, 
        x = ~`promedio global`, 
        y = ~Desviacion_Global, 
        z = ~Estimacion, 
        type = 'surface') %>%
  layout(title = "Superficie de Estimación entre Promedio Global y Desviación Estándar",
         scene = list(
           xaxis = list(title = "Promedio Global"),
           yaxis = list(title = "Desviación Global"),
           zaxis = list(title = "Estimación")
         ))


