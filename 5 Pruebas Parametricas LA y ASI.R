#install.packages("e1071")
library(e1071)
library(gridExtra)
#===============================================================================
#-----------------------------  TIEMPO DE LA Y ASI  ----------------------------
#===============================================================================
# Mostrar el directorio de trabajo actual
datos <- read_excel(paste0(getwd(), "/Resultado_actual.xlsx"), sheet = "Tiempo")
attach(datos)
str(datos)
#===============================================================================
# ----------------------     REPRESENTACION GRAFICA ----------------------------
#===============================================================================
# 5.1) CURVA DE DENSIDAD NORMAL
hist_hclust <- ggplot(datos, aes(x = met_hclust)) +
  geom_histogram(aes(y = ..density..), bins = 100, color = "black", fill = "lightblue", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(datos$met_hclust), sd = sd(datos$met_hclust)), color = "red", size = 1) +
  labs(title = "Aproximación normal de met_hclust (LA)",
       x = "Tiempo(Segundos)",
       y = "Densidad") +
  theme_minimal()

hist_diana <- ggplot(datos, aes(x = met_diana)) +
  geom_histogram(aes(y = ..density..), bins = 100, color = "black", fill = "lightblue", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(datos$met_diana), sd = sd(datos$met_diana)), color = "red", size = 1) +
  labs(title = "Aproximación normal de met_diana (LA)",
       x = "Tiempo(Segundos)",
       y = "Densidad") +
  theme_minimal()

hist_similarity <- ggplot(datos, aes(x = met_sim)) +
  geom_histogram(aes(y = ..density..), bins = 100, color = "black", fill = "lightblue", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(datos$met_sim), sd = sd(datos$met_sim)), color = "red", size = 1) +
  labs(title = "Aproximación normal de met_similarity (ASI)",
       x = "Tiempo(Segundos)",
       y = "Densidad") +
  theme_minimal()

hist_hierarchy <- ggplot(datos, aes(x = met_hierchy)) +
  geom_histogram(aes(y = ..density..), bins = 100, color = "black", fill = "lightblue", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(datos$met_hierchy), sd = sd(datos$met_hierchy)), color = "red", size = 1) +
  labs(title = "Aproximación normal de met_hierarchy (ASI)",
       x = "Tiempo(Segundos)",
       y = "Densidad") +
  theme_minimal()

grid.arrange(hist_hclust, hist_diana, hist_similarity, hist_hierarchy, ncol = 2)

#===============================================================================
# 5.2) QQ RESIDUOS
par(mfrow = c(2, 2))

qqnorm(datos$met_hclust, main = "met_hclust (LA)")
qqline(datos$met_hclust)

qqnorm(datos$met_diana, main = "met_diana (LA)")
qqline(datos$met_diana)

qqnorm(datos$met_sim, main = "met_similarity (ASI)")
qqline(datos$met_sim)

qqnorm(datos$met_hierchy, main = "met_hierarchy (ASI)")
qqline(datos$met_hierchy)
#===============================================================================
#-----------------------------  SUPUESTOS --------------------------------------
#===============================================================================
# 6) SUPUESTO DE NORMALIDAD
#6.1) Anderson-Darling
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
leveneTest(Tiempo ~ metodo , data = datos_modificados, center = "median")
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

Tiempos_Y <- ifelse(y <= cuartiles_y[1], 1,
                    ifelse(y <= cuartiles_y[2], 2,
                           ifelse(y <= cuartiles_y[3], 3, 4)))

# Union Y y X
tiempo_y_metodos <- data.frame(Tiempos_Y ,Metodos_x)
tabla_contingencia <- table( y =  tiempo_y_metodos$Tiempos_Y , x = tiempo_y_metodos$Metodos_x)

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
# ----------------------     REPRESENTACION GRAFICA ----------------------------
#===============================================================================
# 5.1) CURVA DE DENSIDAD NORMAL
hist_hclust <- ggplot(datos, aes(x = met_hclust)) +
  geom_histogram(aes(y = ..density..), bins = 100, color = "black", fill = "lightblue", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(datos$met_hclust), sd = sd(datos$met_hclust)), color = "red", size = 1) +
  labs(title = "Aproximación normal de met_hclust (LA)",
       x = "Memoria(Mb)",
       y = "Densidad") +
  theme_minimal()

hist_diana <- ggplot(datos, aes(x = met_diana)) +
  geom_histogram(aes(y = ..density..), bins = 100, color = "black", fill = "lightblue", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(datos$met_diana), sd = sd(datos$met_diana)), color = "red", size = 1) +
  labs(title = "Aproximación normal de met_diana (LA)",
       x = "Memoria(Mb)",
       y = "Densidad") +
  theme_minimal()

hist_similarity <- ggplot(datos, aes(x = met_sim)) +
  geom_histogram(aes(y = ..density..), bins = 100, color = "black", fill = "lightblue", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(datos$met_sim), sd = sd(datos$met_sim)), color = "red", size = 1) +
  labs(title = "Aproximación normal de met_similarity (ASI)",
       x = "Memoria(Mb)",
       y = "Densidad") +
  theme_minimal()

hist_hierarchy <- ggplot(datos, aes(x = met_hierchy)) +
  geom_histogram(aes(y = ..density..), bins = 100, color = "black", fill = "lightblue", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = mean(datos$met_hierchy), sd = sd(datos$met_hierchy)), color = "red", size = 1) +
  labs(title = "Aproximación normal de met_hierarchy (ASI)",
       x = "Memoria(Mb)",
       y = "Densidad") +
  theme_minimal()

grid.arrange(hist_hclust, hist_diana, hist_similarity, hist_hierarchy, ncol = 2)

#===============================================================================
# 5.2) QQ RESIDUOS
par(mfrow = c(2, 2))

qqnorm(datos$met_hclust, main = "met_hclust (LA)")
qqline(datos$met_hclust)

qqnorm(datos$met_diana, main = "met_diana (LA)")
qqline(datos$met_diana)

qqnorm(datos$met_sim, main = "met_similarity (ASI)")
qqline(datos$met_sim)

qqnorm(datos$met_hierchy, main = "met_hierarchy (ASI)")
qqline(datos$met_hierchy)
#===============================================================================
#-----------------------------  SUPUESTOS --------------------------------------
#===============================================================================
# 6) SUPUESTO DE NORMALIDAD
#6.1) Anderson-Darling
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
datos_modificados <- gather(datos, key = "metodo", value = "Memoria")
datos_modificados$metodo <- as.factor(datos_modificados$metodo)
leveneTest(Memoria ~ metodo , data = datos_modificados, center = "median")
#7.1) Bartlett
# Crear un vector 'tiempo' que contenga todos los valores de Memoria
Memoria <- c(datos$met_hclust, datos$met_diana,
            datos$met_sim, datos$met_hierchy)

# Crear un factor 'metodo' que indique el método correspondiente al memoria
metodo <- factor(rep(c("met_hclust", "met_diana", "met_similarity", "met_hierarchy"),
                     each = nrow(datos)))

bartlett.test(Memoria ~ metodo)
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
variable_y <- data.frame(memoria_total = valores_concatenadosy)
y <- (variable_y$memoria_total)
cuartiles_y <- quantile(y, probs = c(0.25, 0.5, 0.75))

Memoria_Y <- ifelse(y <= cuartiles_y[1], 1,
                    ifelse(y <= cuartiles_y[2], 2,
                           ifelse(y <= cuartiles_y[3], 3, 4)))

# Union Y y X
Memoria_y_metodos <- data.frame(Memoria_Y ,Metodos_x)
tabla_contingencia <- table( y =  Memoria_y_metodos$Memoria_Y , x = Memoria_y_metodos$Metodos_x)

# Prueba de Chi-cuadrado de independencia gl = (4-1)(4-1) = 9
chisq.test(tabla_contingencia)
#===============================================================================
#==========================  FIN DE MEMORIA DE LA Y ASI   ======================
#===============================================================================