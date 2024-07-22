#===============================================================================
#-----------------------------  TIEMPO DE LA Y ASI  ----------------------------
#===============================================================================
# Nota: Se debe ejecutar primero 6 Aproximacion box-cox
#===============================================================================
# 15) POST PRUEBA 
# 15.1) Kruskal-Wallis H test
resultado_kruskal <- kruskal.test(Tiempos_y ~ Metodos_x, data = tiempo_y_metodos )
resultado_kruskal

# En este caso se cambia each = 10 por each = 383 ya que son 383 bases de datos
# es decir 383 tiempos que hemos hecho de calcular su tiempo

# 15.2) Mann Whitney Wilcoxon U-test
datos <- data.frame(grupo = rep(c("hclust", 
                                  "diana", "similary","hierarchy"), each = 10), 
                    Tiempos_y = c(x1, x2,  x3, x4))

# Realizar la prueba de Wilcoxon entre todos los pares de grupos
resultado_pairwise <- pairwise.wilcox.test(datos$Tiempos_y,
                                           datos$grupo,
                                           p.adjust.method = "bonferroni")
resultado_pairwise
################################################################################
# install.packages("PMCMRplus")
# install.packages("dunn.test")
library(dunn.test)
library(PMCMRplus)

# 15.2) prueba de Dunn
resultado_posthoc <- dunn.test(datos$Tiempos_y, g = datos$grupo, method = "bonferroni")
print(resultado_posthoc)
################################################################################


#===============================================================================
#-----------------------------  MEMORIA DE LA Y ASI  ----------------------------
#===============================================================================
# Nota: Se debe ejecutar primero 6 Aproximacion box-cox
#===============================================================================
# 16) POST PRUEBA 
# 16.1) Kruskal-Wallis H test
resultado_kruskal <- kruskal.test(Memoria_y ~ Metodos_x, data = Memoria_y_metodos)
resultado_kruskal

# En este caso se cambia each = 10 por each = 383 ya que son 383 bases de datos
# es decir 383 memoria que hemos hecho de calcular su memoria

# 16.2) Mann Whitney Wilcoxon U-test
datos <- data.frame(grupo = rep(c("hclust", 
                                  "diana", "similary","hierarchy"), each = 10), 
                    Memoria_y = c(x1, x2,  x3, x4))

# Realizar la prueba de Wilcoxon entre todos los pares de grupos
resultado_pairwise <- pairwise.wilcox.test(datos$Memoria_y,
                                           datos$grupo,
                                           p.adjust.method = "bonferroni")
resultado_pairwise
################################################################################
# install.packages("PMCMRplus")
# install.packages("dunn.test")
library(dunn.test)
library(PMCMRplus)

# 16.3) prueba de Dunn
resultado_posthoc <- dunn.test(datos$Memoria_y, g = datos$grupo, method = "bonferroni")
print(resultado_posthoc)
################################################################################

