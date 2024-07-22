#===============================================================================
#Nombre: Jonathan Calvopiña
#Fecha: 17/05/2024
#Objetivo: Generar bases de datos aleatorias para comparar  clúster de LA y ASI
#===============================================================================
rm(list = setdiff(ls(), lsf.str()))
closeAllConnections()
#===============================================================================
# 2) Parámetros
#===============================================================================
# Cambiar el diretorio donde va a aguardarse las bases de datos aleatorios
#directorio <- c("C:/Users/hefes/OneDrive/Documentos/base/datos/")
directorio <- c(getwd())
num_var <- 100 # maximo de variables (100)
num_obs <- 1000 # maximo de observaciones (1000)
inicio_data <- 1 # inico de la primera base de datos (1)
fin_data <- 10 # fin de la ultima base de datos (383)
#===============================================================================
# 2.1) Generar las 383 base de datos aleatorios (tipo intervalo C = {x ∈ R})
#===============================================================================

funcion_data <- function(directorio, num_var, num_obs, inicio_data, fin_data) {
  for (i in inicio_data:fin_data) {
    n_Var <- round(runif(1) * num_var) # columnas (#variables <100)
    n_filas <- round(runif(1) * num_obs) # filas (# observaciones <1000)
    if (n_Var < 3) {
      n_Var <- round(runif(1)*100)
      if (n_Var < 3) {
        n_Var <- round(runif(1)*100)
      }
    }
    if (n_filas < 3) {
      n_filas <- round(runif(1)*1000)
    }
    DataBase <- replicate(n_Var, runif(n_filas,min =-1,max = 1))
    
    #===========================================================================
    # 2.1.1) Transformacion del tipo intervalo C ={x∈[-1,1]} a escalar C = [0,1]
    #===========================================================================
    min_val <- min(DataBase)
    max_val <- max(DataBase)
    DataBase <- (DataBase + abs(min_val))/(max_val - min_val)
    DataBase <- round(DataBase,0)
    #===========================================================================
    if (n_Var == ncol(DataBase)) {
      #
      rownames(DataBase) <- paste('C', 1:n_filas, sep = '')
      #
      colnames(DataBase) <- c('V1', paste('V', 2:n_Var, sep = ''))
      
      f <- paste('File_', sprintf("%03d", i), ".csv", sep = "")
      write.table(DataBase, file = paste(directorio, f, sep = ""), sep = ";",
                  quote = FALSE, row.names = TRUE, col.names = NA)
      rm(DataBase)
    }
    
  }
  
}
funcion_data(directorio, num_var, num_obs, inicio_data, fin_data)
#===============================================================================
# Nota: Este se guarda automaticamente en la carpeta PROCESO COMPARACION R
# (LA Y ASI)
# Nota: Se guarda en binario ya que se encontro conflicto al guardar y 
# transformar  directamente en intervalo [-1,1 ]
#===============================================================================
