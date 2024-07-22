if (!requireNamespace("MASS", quietly = TRUE)) {
  install.packages("MASS")
}
library(MASS)

# Instalar y cargar los paquetes necesarios
if (!requireNamespace("nortest", quietly = TRUE)) {
  install.packages("nortest")
}
library(nortest)

if (!requireNamespace("forecast", quietly = TRUE)) {
  install.packages("forecast")
}
library(forecast)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

if (!requireNamespace("ggpubr", quietly = TRUE)) {
  install.packages("ggpubr")
}
library(ggpubr)
#===============================================================================
#-----------------------------  TIEMPO DE LA Y ASI  ----------------------------
#===============================================================================
# Mostrar el directorio de trabajo actual
datos <- read_excel(paste0(getwd(), "/Resultado_actual.xlsx"), sheet = "Tiempo")
attach(datos)
str(datos)
#===============================================================================
# 9) TRANSFORMACION A NORMALIDAD (BOX COX)

# 9.1) Escoger el mejor landa automaticamente
lambda_1 <- BoxCox.lambda(datos$met_hclust)
lambda_2 <- BoxCox.lambda(datos$met_diana)
lambda_3 <- BoxCox.lambda(datos$met_sim)
lambda_4 <- BoxCox.lambda(datos$met_hierchy)

# 9.1) Transformacion BOX-COX
transformed_met_hclust <- BoxCox(datos$met_hclust, lambda_1)
transformed_met_diana <- BoxCox(datos$met_diana, lambda_2)
transformed_met_similarity <- BoxCox(datos$met_sim, lambda_3)
transformed_met_hierarchy <- BoxCox(datos$met_hierchy, lambda_4)

# Se vuelve a emplear las pruebas de los supuestos para comprobar si se
# aproxima a la normalidad

met_hclust <- (transformed_met_hclust) 
met_diana <- (transformed_met_diana)
met_sim <-(transformed_met_similarity)
met_hierchy <-(transformed_met_hierarchy) 

#===============================================================================
#-----------------------------  SUPUESTOS --------------------------------------
#===============================================================================
# 10) SUPUESTO DE NORMALIDAD
# 10.1) Anderson-Darling
# Instalar el paquete nortest si no está instalado
if (!requireNamespace("nortest", quietly = TRUE)) {
  install.packages("nortest")
}
library(nortest)
ad.test(datos$met_hclust)# Metodo 1 
ad.test(datos$met_diana)  # Metodo 2
ad.test(datos$met_sim) # Metodo 3
ad.test(datos$met_hierchy) # Metodo 4
# 6.2) KS-Lilliefors
#install.packages("nortest")
library(nortest)
lillie.test(datos$met_hclust) # Metodo 1 
lillie.test(datos$met_diana)  # Metodo 2
lillie.test(datos$met_sim) # Metodo 3
lillie.test(datos$met_hierchy)  # Metodo 4
# 6.3) Shapiro-Wilk 
shapiro.test(datos$met_hclust) # Metodo 1 
shapiro.test(datos$met_diana)  # Metodo 2
shapiro.test(datos$met_sim) # Metodo 3
shapiro.test(datos$met_hierchy)  # Metodo 4
#===============================================================================
# 7) SUPUESTO DE HETEROCEDASTICIDAD
#install.packages("dplyr")
library(tidyr)
library(dplyr)
library(car)
# 7.1) Levene
datos_modificados <- gather(datos, key = "metodo", value = "Tiempo")
datos_modificados$metodo <- as.factor(datos_modificados$metodo)
leveneTest(tiempo ~ metodo , data = datos_modificados, center = "median")
#7.1) Bartlett
# Crear un vector 'tiempo' que contenga todos los valores de tiempo
tiempo <- c(datos$met_hclust, datos$met_diana,
            datos$met_sim, datos$met_hierchy)

# Crear un factor 'metodo' que indique el método correspondiente al tiempo
metodo <- factor(rep(c("met_hclust", "met_diana", "met_similarity", "met_hierarchy"),
                     each = nrow(datos)))

bartlett.test(tiempo ~ metodo)
#===============================================================================
# 8) SUPUESTO DE INDEPENDENCIA
# 8.1) CHI- CUADRADO

#Variable X : Metodos
x1 <- datos$met_hclust
x2 <- datos$met_diana
x3 <- datos$met_sim
x4 <- datos$met_hierchy

#Calcular los cuartiles para cada variable
cuartiles_x1 <- quantile(x1, probs = c(0.25, 0.5, 0.75))
cuartiles_x2 <- quantile(x2, probs = c(0.25, 0.5, 0.75))
cuartiles_x3 <- quantile(x3, probs = c(0.25, 0.5, 0.75))
cuartiles_x4 <- quantile(x4, probs = c(0.25, 0.5, 0.75))

#Codifica los valores continuos a discretos por medio de los cuartiles(1,2,3,4)
codificados_x1 <- ifelse(x1 <= cuartiles_x1[1], 1,
                         ifelse(x1 <= cuartiles_x1[2], 2,
                                ifelse(x1 <= cuartiles_x1[3], 3, 4)))

codificados_x2 <- ifelse(x2 <= cuartiles_x2[1], 1,
                         ifelse(x2 <= cuartiles_x2[2], 2,
                                ifelse(x2 <= cuartiles_x2[3], 3, 4)))

codificados_x3 <- ifelse(x3 <= cuartiles_x3[1], 1,
                         ifelse(x3 <= cuartiles_x3[2], 2,
                                ifelse(x3 <= cuartiles_x3[3], 3, 4)))

codificados_x4 <- ifelse(x4 <= cuartiles_x4[1], 1,
                         ifelse(x4 <= cuartiles_x4[2], 2,
                                ifelse(x4 <= cuartiles_x4[3], 3, 4)))

variable_x <- data.frame(
  met_hclust = codificados_x1,
  met_diana = codificados_x2,
  met_similarity = codificados_x3,
  met_hierarchy = codificados_x4
)
Metodos_x <- c(variable_x$met_hclust,variable_x$met_diana,
               variable_x$met_similarity,variable_x$met_hierarchy)

#Variable Y: Tiempo de ejecución
valores_concatenadosy <- c(x1,x2,x3,x4)
variable_y <- data.frame(tiempo_total = valores_concatenadosy)
y <- (variable_y$tiempo_total)
cuartiles_y <- quantile(y, probs = c(0.25, 0.5, 0.75))

Tiempos_y <- ifelse(y <= cuartiles_y[1], 1,
                    ifelse(y <= cuartiles_y[2], 2,
                           ifelse(y <= cuartiles_y[3], 3, 4)))

# Union Y y X
tiempo_y_metodos <- data.frame(Tiempos_y ,Metodos_x)
tabla_contingencia <- table( y =  tiempo_y_metodos$Tiempos_y , x = tiempo_y_metodos$Metodos_x)

# Prueba de Chi-cuadrado de independencia gl = (4-1)(4-1) = 9
chisq.test(tabla_contingencia)
#===============================================================================
#==========================  FIN DEL TIEMPO DE LA Y ASI   ======================
#===============================================================================

#===============================================================================
#-----------------------------  MEMORIA DE LA Y ASI ----------------------------
#===============================================================================
# Mostrar el directorio de trabajo actual
datos <- read_excel(paste0(getwd(), "/Resultado_actual.xlsx"), sheet = "Memoria")
attach(datos)
str(datos)
#===============================================================================
# 11) TRANSFORMACION A NORMALIDAD (BOX COX)

# 11.1) Escoger el mejor landa automaticamente
lambda_1 <- BoxCox.lambda(datos$met_hclust)
lambda_2 <- BoxCox.lambda(datos$met_diana)
lambda_3 <- BoxCox.lambda(datos$met_sim)
lambda_4 <- BoxCox.lambda(datos$met_hierchy)

# 11.2) Transformacion BOX-COX
transformed_met_hclust <- BoxCox(datos$met_hclust, lambda_1)
transformed_met_diana <- BoxCox(datos$met_diana, lambda_2)
transformed_met_similarity <- BoxCox(datos$met_sim, lambda_3)
transformed_met_hierarchy <- BoxCox(datos$met_hierchy, lambda_4)

# Se vuelve a emplear las pruebas de los supuestos para comprobar si se
# aproxima a la normalidad

met_hclust <- (transformed_met_hclust) 
met_diana <- (transformed_met_diana)
met_sim <-(transformed_met_similarity)
met_hierchy <-(transformed_met_hierarchy) 

#===============================================================================
#-----------------------------  SUPUESTOS --------------------------------------
#===============================================================================
# 12) SUPUESTO DE NORMALIDAD
#12.1) Anderson-Darling
# Instalar el paquete nortest si no está instalado
if (!requireNamespace("nortest", quietly = TRUE)) {
  install.packages("nortest")
}
library(nortest)
ad.test(datos$met_hclust)# Metodo 1 
ad.test(datos$met_diana)  # Metodo 2
ad.test(datos$met_sim) # Metodo 3
ad.test(datos$met_hierchy) # Metodo 4
# 12.2) KS-Lilliefors
#install.packages("nortest")
library(nortest)
lillie.test(datos$met_hclust) # Metodo 1 
lillie.test(datos$met_diana)  # Metodo 2
lillie.test(datos$met_sim) # Metodo 3
lillie.test(datos$met_hierchy)  # Metodo 4
# 12.3) Shapiro-Wilk 
shapiro.test(datos$met_hclust) # Metodo 1 
shapiro.test(datos$met_diana)  # Metodo 2
shapiro.test(datos$met_sim) # Metodo 3
shapiro.test(datos$met_hierchy)  # Metodo 4
#===============================================================================
# 13) SUPUESTO DE HETEROCEDASTICIDAD
#install.packages("dplyr")
library(tidyr)
library(dplyr)
library(car)
# 7.1) Levene
datos_modificados <- gather(datos, key = "metodo", value = "Memoria")
datos_modificados$metodo <- as.factor(datos_modificados$metodo)
leveneTest(Memoria ~ metodo , data = datos_modificados, center = "median")
#13.1) Bartlett
# Crear un vector 'tiempo' que contenga todos los valores de Memoria
Memoria <- c(datos$met_hclust, datos$met_diana,
             datos$met_sim, datos$met_hierchy)

# Crear un factor 'metodo' que indique el método correspondiente al memoria
metodo <- factor(rep(c("met_hclust", "met_diana", "met_similarity", "met_hierarchy"),
                     each = nrow(datos)))

bartlett.test(Memoria ~ metodo)
#===============================================================================
# 14) SUPUESTO DE INDEPENDENCIA
# 14.1) CHI- CUADRADO

#Variable X : Metodos
x1 <- datos$met_hclust
x2 <- datos$met_diana
x3 <- datos$met_sim
x4 <- datos$met_hierchy

#Calcular los cuartiles para cada variable
cuartiles_x1 <- quantile(x1, probs = c(0.25, 0.5, 0.75))
cuartiles_x2 <- quantile(x2, probs = c(0.25, 0.5, 0.75))
cuartiles_x3 <- quantile(x3, probs = c(0.25, 0.5, 0.75))
cuartiles_x4 <- quantile(x4, probs = c(0.25, 0.5, 0.75))

#Codifica los valores continuos a discretos por medio de los cuartiles(1,2,3,4)
codificados_x1 <- ifelse(x1 <= cuartiles_x1[1], 1,
                         ifelse(x1 <= cuartiles_x1[2], 2,
                                ifelse(x1 <= cuartiles_x1[3], 3, 4)))

codificados_x2 <- ifelse(x2 <= cuartiles_x2[1], 1,
                         ifelse(x2 <= cuartiles_x2[2], 2,
                                ifelse(x2 <= cuartiles_x2[3], 3, 4)))

codificados_x3 <- ifelse(x3 <= cuartiles_x3[1], 1,
                         ifelse(x3 <= cuartiles_x3[2], 2,
                                ifelse(x3 <= cuartiles_x3[3], 3, 4)))

codificados_x4 <- ifelse(x4 <= cuartiles_x4[1], 1,
                         ifelse(x4 <= cuartiles_x4[2], 2,
                                ifelse(x4 <= cuartiles_x4[3], 3, 4)))

variable_x <- data.frame(
  met_hclust = codificados_x1,
  met_diana = codificados_x2,
  met_similarity = codificados_x3,
  met_hierarchy = codificados_x4
)
Metodos_x <- c(variable_x$met_hclust,variable_x$met_diana,
               variable_x$met_similarity,variable_x$met_hierarchy)

#Variable Y: Tiempo de ejecución
valores_concatenadosy <- c(x1,x2,x3,x4)
variable_y <- data.frame(memoria_total = valores_concatenadosy)
y <- (variable_y$memoria_total)
cuartiles_y <- quantile(y, probs = c(0.25, 0.5, 0.75))

Memoria_y <- ifelse(y <= cuartiles_y[1], 1,
                    ifelse(y <= cuartiles_y[2], 2,
                           ifelse(y <= cuartiles_y[3], 3, 4)))

# Union Y y X
Memoria_y_metodos <- data.frame(Memoria_y ,Metodos_x)
tabla_contingencia <- table( y =  Memoria_y_metodos$Memoria_y , x = Memoria_y_metodos$Metodos_x)

# Prueba de Chi-cuadrado de independencia gl = (4-1)(4-1) = 9
chisq.test(tabla_contingencia)
#===============================================================================
#==========================  FIN DE MEMORIA DE LA Y ASI   ======================
#===============================================================================

Tiempo_y_metodos