# Practica de graficas con beer.csv. # descripcion
# Todo el codigo esta comentado para fines didacticos. # descripcion

load_pkg <- function(pkg) { # helper para cargar paquetes opcionales
  if (!requireNamespace(pkg, quietly = TRUE)) { # valida disponibilidad
    message("Paquete no instalado: ", pkg) # mensaje si falta
    return(FALSE) # retorna falso
  } # fin validacion
  library(pkg, character.only = TRUE) # carga paquete
  TRUE # retorna verdadero
} # fin helper

# Cargar datos y colores. # seccion
beer <- read.csv("data/beer.csv", sep = ",", dec = ",", stringsAsFactors = FALSE) # lee datos
source("colores.R") # carga paletas

# Variables cualitativas. # seccion
beer$marca <- factor(beer$marca) # convierte a factor
beer$tipo <- factor(beer$tipo) # convierte a factor
beer$origen <- factor(beer$origen) # convierte a factor

# Nueva variable cualitativa ordinal (alcohol). # seccion
beer$alcohol_cat <- cut( # crea categorias
  beer$poralcoh, # variable base
  breaks = c(-Inf, 4, 5, Inf), # puntos de corte
  labels = c("Bajo", "Medio", "Alto"), # etiquetas
  right = TRUE # incluye limite derecho
) # fin cut

# NIVEL BASICO ----------------------------------------------------------------
# Variables cualitativas (pie, barplot). # seccion
pie(table(beer$alcohol_cat)) # grafico de torta
barplot(table(beer$origen)) # grafico de barras

# Variables cuantitativas (hist, boxplot, plot). # seccion
hist(beer$precio) # histograma
boxplot(beer$calorias) # caja y bigotes
plot(beer$precio, beer$poralcoh) # dispersion
plot(density(beer$poralcoh))


# NIVEL MEDIO ------------------------------------------------------------------

# Variables cualitativas (pie, barplot). # seccion
pie( # grafico de torta
  table(beer$alcohol_cat), # frecuencias
  col = c(c31, c21, c41), # colores
  main = "Nivel de alcohol (bajo, medio, alto)" # titulo
) # fin pie

barplot( # grafico de barras
  table(beer$origen), # frecuencias
  col = c6, # color
  main = "Frecuencia por origen", # titulo
  xlab = "Origen", # etiqueta x
  ylab = "Frecuencia" # etiqueta y
) # fin barplot

# Variables cuantitativas (hist, boxplot, plot). # seccion
hist( # histograma
  beer$precio, # variable
  col = c12, # color
  main = "Distribucion de precios", # titulo
  xlab = "Precio" # etiqueta x
) # fin hist
boxplot( # caja y bigotes
  beer$calorias, # variable
  col = c51, # color
  main = "Calorias por cerveza", # titulo
  ylab = "Calorias" # etiqueta y
) # fin boxplot
plot( # dispersion
  beer$precio, beer$poralcoh, # variables
  pch = 19, # simbolo
  col = c3, # color
  main = "Precio vs Alcohol", # titulo
  xlab = "Precio", # etiqueta x
  ylab = "Alcohol (%)" # etiqueta y
) # fin plot

# NIVEL AVANZADO (ggplot2 + plotly)--------------------------------------------
library(ggplot2) # carga ggplot2
has_plotly <- load_pkg("plotly") # carga plotly si existe

# Tablas para variables cualitativas. # seccion
tabla_alcohol <- as.data.frame(table(beer$alcohol_cat)) # tabla alcohol
names(tabla_alcohol) <- c("alcohol_cat", "freq") # renombra columnas

tabla_origen <- as.data.frame(table(beer$origen)) # tabla origen
names(tabla_origen) <- c("origen", "freq") # renombra columnas

# Variables cualitativas (pie, barplot). # seccion
p1 <- ggplot(tabla_alcohol, aes(x = "", y = freq, fill = alcohol_cat)) + # base
  geom_col(width = 1, color = "white") + # columnas
  coord_polar(theta = "y") + # polar
  theme_void() + # tema
  scale_fill_manual(values = c(c31, c21, c41)) + # colores
  labs(title = "Nivel de alcohol (bajo, medio, alto)", fill = "Alcohol") # titulos
p1 # imprime

p2 <- ggplot(tabla_origen, aes(x = origen, y = freq, fill = origen)) + # base
  geom_col() + # columnas
  scale_fill_manual(values = paleta4) + # colores
  labs(title = "Frecuencia por origen", x = "Origen", y = "Frecuencia") + # titulos
  theme_minimal() # tema
p2 # imprime

# Variables cuantitativas (hist, boxplot, plot). # seccion
p3 <- ggplot(beer, aes(x = precio)) + # base
  geom_histogram(binwidth = 0.5, fill = c12, color = "white") + # hist
  labs(title = "Distribucion de precios", x = "Precio", y = "Frecuencia") + # titulos
  theme_minimal() # tema
p3 # imprime

p4 <- ggplot(beer, aes(x = tipo, y = calorias, fill = tipo)) + # base
  geom_boxplot() + # boxplot
  scale_fill_manual(values = c(c11,c21, c31,c41,c51)) + # colores
  labs(title = "Calorias por tipo", x = "Tipo", y = "Calorias") + # titulos
  theme_minimal() # tema
p4 # imprime

p5 <- ggplot(beer, aes(x = precio, y = poralcoh, color = tipo)) + # base
  geom_point() + # puntos
  scale_color_manual(values = c(c11, c21,c31,c41,c51)) + # colores
  labs(title = "Precio vs Alcohol", x = "Precio", y = "Alcohol (%)") + # titulos
  theme_minimal() # tema
p5 # imprime


