# Lista de paquetes necesarios
packages <- c("Rcpp", "rchic", "tcltk2", "stringr", "ggplot2", "factoextra", 
              "fpc", "NbClust", "cluster", "clValid", "kohonen", "mclust", 
              "reshape2", "purrr", "dplyr", "dendextend", "corrplot", "FactoMineR", 
              "igraph", "data.table", "microbenchmark", "readxl", "openxlsx", "pryr")

# Función para instalar y cargar paquetes
install_and_load <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Instalar y cargar todos los paquetes
sapply(packages, install_and_load)

# Mostrar el directorio de trabajo actual
getwd()
#===============================================================================
#Nombre: Jonathan Calvopiña
#Fecha: 17/05/2024
#Objetivo: Métodos clúster jerárquicos de LA.
#===============================================================================

#===============================================================================
# 3)  Funcion de los métodos clúster jerarquico de LA y ASI.
#===============================================================================

# Método 1 (hclust)
metodo_hclust <- function(DataBase){
  d <- dist(DataBase, method = "euclidean") 
  met_hclust <- hclust(d, method = "complete") 
  plot(met_hclust, cex = 0.6, hang = -1) 
}

# Método 2 (Diana)
metodo_diana <- function(DataBase){
  met_diana <- diana(DataBase)
  pltree(met_diana, cex = 0.6, hang = -1)
}

# Método 3 (similarity)
metodo_similarity <- function(DataBase, file_path) {
  # Eliminar la primera columna
  DataBase <- DataBase[, -1]
  write.table(x = DataBase, file = file_path, sep = ";",
              row.names = FALSE, col.names = TRUE)
  hs <- callSimilarityTree(file_path , contribution.supp = FALSE,
                           typicality.supp = FALSE,
                           verbose=FALSE)
}

# Método 4 (hierarchy)
metodo_hierarchy <- function(DataBase, file_path){
  # Eliminar la primera columna
  DataBase <- DataBase[, -1]
  write.table(x = DataBase, file = file_path, sep = ";",
              row.names = FALSE, col.names = TRUE)
  hr <- callHierarchyTree(file_path, contribution.supp = FALSE,
                          typicality.supp = FALSE,
                          computing.mode = 3, verbose = FALSE)
}

#===============================================================================
# 4) Ejecutar los metodos LA con las 383 bases de datos aleatorias generadas
# anteriormente en la (1 Generacion Base de datos aleatorio)
#===============================================================================

tiempo1 <- numeric(fin_data - inicio_data + 1)
tiempo2 <- numeric(fin_data - inicio_data + 1)
tiempo3 <- numeric(fin_data - inicio_data + 1)
tiempo4 <- numeric(fin_data - inicio_data + 1)

memoria1 <- numeric(fin_data - inicio_data + 1)
memoria2 <- numeric(fin_data - inicio_data + 1)
memoria3 <- numeric(fin_data - inicio_data + 1)
memoria4 <- numeric(fin_data - inicio_data + 1)


# METODOS DE LA ( METODO 1 = AGLOMERATIVO, METODO 2 = DIANA)
for (i in inicio_data:fin_data) {
  file_path <- paste0(directorio, "File_", sprintf("%03d", i), ".csv")
  DataBase <- read.csv(file_path, sep = ";", header = TRUE, row.names = NULL)  # Cambiar row.names a NULL
  
  gc() # Limpieza de memoria
  
  # Método 1
  memoria_antes_1 <- pryr::mem_used()
  micro_1 <- microbenchmark::microbenchmark(metodo_hclust(DataBase), times = 1L)
  tiempo1[i - inicio_data + 1] <- median(micro_1$time) / 1e9  # (segundos)
  memoria_despues_1 <- pryr::mem_used()
  memoria_utilizada_1 <- (memoria_despues_1 - memoria_antes_1) / 2^20  # (MB)
  memoria1[i - inicio_data + 1] <- abs(memoria_utilizada_1)
  
  gc() # Limpieza de memoria
  
  # Método 2
  memoria_antes_2 <- pryr::mem_used()
  micro_2 <- microbenchmark::microbenchmark(metodo_diana(DataBase), times = 1L)
  tiempo2[i - inicio_data + 1] <- median(micro_2$time) / 1e9  # (segundos)
  memoria_despues_2 <- pryr::mem_used()
  memoria_utilizada_2 <- (memoria_despues_2 - memoria_antes_2) / 2^20  # (MB)
  memoria2[i - inicio_data + 1] <- abs(memoria_utilizada_2)
}

# METODOS DE LA ( METODO 3 = SIMILARIDAD, METODO 2 = COHESION)
for(i in inicio_data:fin_data){
  file_path <- paste0(directorio, "File_", sprintf("%03d", i), ".csv")
  DataBase <- read.csv(file_path, sep = ";", header = TRUE, row.names = NULL)  # Cambiar row.names a NULL
  
  # Eliminar la primera columna de la base de datos
  # DataBase <- DataBase[, -1]
  
  gc() # Limpieza de memoria
  
  # Método 3
  memoria_antes_3 <- pryr::mem_used()
  micro_3 <- microbenchmark::microbenchmark(metodo_similarity(DataBase, file_path), times = 1L)
  tiempo3[i - inicio_data + 1] <- mean(micro_3$time) / 1e9  # (segundos)
  memoria_despues_3 <- pryr::mem_used()
  memoria_utilizada_3 <- (memoria_despues_3 - memoria_antes_3) / 2^20  # (MB)
  memoria3[i - inicio_data + 1] <- abs(memoria_utilizada_3)
  
  gc() # Limpieza de memoria
  
  # Método 4
  memoria_antes_4 <- pryr::mem_used()
  micro_4 <- microbenchmark::microbenchmark(metodo_hierarchy(DataBase, file_path), times = 1L)
  tiempo4[i - inicio_data + 1] <- mean(micro_4$time) / 1e9  # (segundos)
  memoria_despues_4 <- pryr::mem_used()
  memoria_utilizada_4 <- (memoria_despues_4 - memoria_antes_4) / 2^20  # (MB)
  memoria4[i - inicio_data + 1] <- abs(memoria_utilizada_4)
}

# METODOS DE LA ( METODO 1 = AGLOMERATIVO, METODO 2 = DIANA)
# METODOS DE LA ( METODO 3 = SIMILARIDAD, METODO 2 = COHESION)

# Crear un dataframe con los resultados de los tiempos
resultados_tiempo <- data.frame(met_hclust = tiempo1,
                                met_diana = tiempo2,
                                met_sim = tiempo3,
                                met_hierchy = tiempo4)

# Crear un dataframe con los resultados de la memoria
resultados_memoria <- data.frame(met_hclust = memoria1,
                                 met_diana = memoria2,
                                 met_sim = memoria3,
                                 met_hierchy = memoria4)

# Guardar los resultados en un archivo Excel
print(resultados_tiempo)
print(resultados_memoria)
#===============================================================================
resultados_path <- "Resultado_actual.xlsx"
write.xlsx(list(Tiempo = resultados_tiempo, Memoria = resultados_memoria), 
           resultados_path, rowNames = FALSE)

# Mensaje de confirmación
print("Resultados guardados correctamente en el archivo 'Resultado_actual.xlsx'")
#===============================================================================
# Nota : Se guarda en la carpeta Comparacion_LA_ASI
#===============================================================================