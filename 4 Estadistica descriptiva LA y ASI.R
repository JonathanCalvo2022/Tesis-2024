#install.packages("e1071")
library(e1071)
library(gridExtra)
library(readxl)

#===============================================================================
# Mostrar el directorio de trabajo actual
datos <- read_excel(paste0(getwd(), "/Resultado_actual.xlsx"), sheet = "Tiempo")
attach(datos)
str(datos)
#===============================================================================
#                           ESTADISTICA DESCRIPTIVA TIEMPO
#===============================================================================
# HISTOGRAMA
par(mfrow = c(2, 2))
hist(datos$met_hclust, breaks = 30, col = "#FFD39B", border = "white", 
     main = "met_hclust", xlab = "Tiempo(Segundos)", ylab = "Frecuencia")

hist(datos$met_diana, breaks = 30, col = "cadetblue2", border = "white", 
     main = "met_diana", xlab = "Tiempo(Segundos)", ylab = "Frecuencia")

hist(datos$met_sim, breaks = 30, col = "aquamarine3", border = "white", 
     main = "met_similarity", xlab = "Tiempo(Segundos)", ylab = "Frecuencia")

hist(datos$met_hierchy, breaks = 30, col = "azure3", border = "white", 
     main = "met_hierarchy", xlab = "Tiempo(Segundos)", ylab = "Frecuencia")

#################################BoxPlot########################################
# BOXPLOT
par(mfrow = c(1, 1))
nombres_metodos <- c("hclust(LA)", "diana(LA)", "similarity(ASI)", "hierarchy(ASI)")
colores <- c("#FFD39B", "cadetblue2", "aquamarine3", "azure3")

# Aplicacion de estadistica descriptiva
boxplot(datos, col = colores, # Boxplot para cada método
        main = " ",
        xlab = "Métodos", ylab = "Tiempo (segundos)",
        names = nombres_metodos)

boxplot.stats(met_hclust)
boxplot.stats(met_diana)
boxplot.stats(met_sim)
boxplot.stats(met_hierchy)
################################################################################
# MEDIDAS DE TENDENCIA CENTRAL Y DISPERSION
medidas_tendencia_central <- data.frame(
  Media = apply(datos, 2, mean),
  Mediana = apply(datos, 2, median),
  Moda = apply(datos, 2, function(x) {
    moda <- table(x)
    as.numeric(names(moda)[which.max(moda)])
  })
)

nombres_dispersion <- c("Varianza", "Desviación Estándar", "C.V.", "Asimetría",
                        "Curtosis")
medidas_dispersion <- data.frame(
  Varianza = apply(datos, 2, var),
  Desv.Est = apply(datos, 2, sd),
  Coef.Var = apply(datos, 2, function(x) sd(x)/mean(x)),
  Asimetría = apply(datos, 2, skewness),
  Curtosis = apply(datos, 2, kurtosis)
)
################################################################################
# RESULTADOS
round(medidas_dispersion,3)
print(round(medidas_tendencia_central,3))
summary(round(datos,3))
#=================================   FIN TIEMPO ================================

#===============================   INICIO ESPACIO ==============================
# Mostrar el directorio de trabajo actual
datos <- read_excel(paste0(getwd(), "/Resultado_actual.xlsx"), sheet = "Memoria")
attach(datos)
str(datos)
#===============================================================================
#                           ESTADISTICA DESCRIPTIVA TIEMPO
#===============================================================================
# HISTOGRAMA
par(mfrow = c(2, 2))
hist(datos$met_hclust, breaks = 30, col = "#FFD39B", border = "white", 
     main = "met_hclust", xlab = "Memoria(Mb)", ylab = "Frecuencia")

hist(datos$met_diana, breaks = 30, col = "cadetblue2", border = "white", 
     main = "met_diana", xlab = "Memoria(Mb)", ylab = "Frecuencia")

hist(datos$met_sim, breaks = 30, col = "aquamarine3", border = "white", 
     main = "met_similarity", xlab = "Memoria(Mb)", ylab = "Frecuencia")

hist(datos$met_hierchy, breaks = 30, col = "azure3", border = "white", 
     main = "met_hierarchy", xlab = "Memoria(Mb)", ylab = "Frecuencia")

#################################BoxPlot########################################
# BOXPLOT
par(mfrow = c(1, 1))
nombres_metodos <- c("hclust(LA)", "diana(LA)", "similarity(ASI)", "hierarchy(ASI)")
colores <- c("#FFD39B", "cadetblue2", "aquamarine3", "azure3")

# Aplicacion de estadistica descriptiva
boxplot(datos, col = colores, # Boxplot para cada método
        main = " ",
        xlab = "Métodos", ylab = "Memoria(Mb)",
        names = nombres_metodos)

boxplot.stats(met_hclust)
boxplot.stats(met_diana)
boxplot.stats(met_sim)
boxplot.stats(met_hierchy)
################################################################################
# MEDIDAS DE TENDENCIA CENTRAL Y DISPERSION
medidas_tendencia_central <- data.frame(
  Media = apply(datos, 2, mean),
  Mediana = apply(datos, 2, median),
  Moda = apply(datos, 2, function(x) {
    moda <- table(x)
    as.numeric(names(moda)[which.max(moda)])
  })
)

nombres_dispersion <- c("Varianza", "Desviación Estándar", "C.V.", "Asimetría",
                        "Curtosis")
medidas_dispersion <- data.frame(
  Varianza = apply(datos, 2, var),
  Desv.Est = apply(datos, 2, sd),
  Coef.Var = apply(datos, 2, function(x) sd(x)/mean(x)),
  Asimetría = apply(datos, 2, skewness),
  Curtosis = apply(datos, 2, kurtosis)
)
################################################################################
# RESULTADOS
round(medidas_dispersion,3)
print(round(medidas_tendencia_central,3))
summary(round(datos,3))
#=======================   FIN ESPACIO =========================================
