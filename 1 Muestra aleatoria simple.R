#===============================================================================
#Nombre: Jonathan Calvopiña
#Fecha: 17/05/2024
#Objetivo: Generar el Muestreo aleatorio simple para LA y ASI
#===============================================================================
#Parametros
N <- 100000 # Tamano de la poblacion
alpha <- 0.05 # Nivel de significancia 5%
Z <- qnorm(1 - alpha/2) # Cuantil de la distribucion normal estandar (α = 0.05)
E <- 0.10  #error de estimacion 10%
S <- 1 # Desviacion estandar

#Calculo el tamano de la muestra (metodo de muestreo aleatorio simple)
n <- (S^2) / (((E^2) / (Z^2)) + ((S^2) / N))
n <- round(n,3)
na <- ceiling(n)  # Redondear al número entero más cercano
cat("El tamaño de la muestra calculada es:", n ,"≈",na, "bases de datos\n")
#===============================================================================