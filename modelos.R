 #### METODOS DE ORDENACION ###

### SIN Restricción ###

### PCoA (chatGPT) #####
##Librerias necesarias
library(vegan)
library(ape)

# Se genera la database
datos <- data.frame(
  "leaf_length" = c(10, 9.7, 5),
  "red_flower" = c(0, 0, 1),
  "white_flower" = c(1, 0, 0),
  "second_root" = c(2, 0, 1),
  "glan_density" = c(0, 1, 2)
)
rownames(datos) <- c("Sp1", "Sp2", "Sp3")

# Se hace una matriz de distancia
dist_matrix <- vegan::vegdist(datos, method = "euclidean")

# Para hacer el PCoA
pcoa_result <- ape::pcoa(dist_matrix)

# Plot de los resultados
plot(pcoa_result$vectors[,1], 
     pcoa_result$vectors[,2], 
     xlab = "PC1", ylab = "PC2")

text(pcoa_result$vectors[,1], #Este es para poner 
     pcoa_result$vectors[,2], #los nombres de los puntos
     rownames(pcoa_result$vectors))


### CON Restricción ###

### RDA (QCBS R Workshop Series 2023) ####
library(vegan)

# Este paquete trae una base de datos "mite"

data("mite") #Este trae abundancias de 35 spp en 70 muestras 
data("mite.env") #Este son variables del suelo de esas 70 muestras 


### Transformar y estandarizar datos ###

## Se transforma el dataset con el metodo Hellinger que lo transforma
# en un espacio euclideano para compensar valores muy esparcidos
mite_hellinger <- decostand(mite, method = "hellinger")

## Se estandarizan los datos cuantitativos
mite.env$SubsDens <- decostand(mite.env$SubsDens, method = "standardize")
mite.env$WatrCont <- decostand(mite.env$WatrCont, method = "standardize")


### Seleccionar variables importantes ###

## RDA inicial con todos los datos ambientales
mite_rda_full <- rda(mite_hellinger ~ ., data = mite.env)

## Selección hacia adelante de las variables importantes
rda_fwd_sel <- ordiR2step(rda(mite_hellinger ~ 1, data = mite.env),
                      scope = formula(mite_rda_full), 
                      direction = "forward", 
                      R2scope = TRUE,
                      pstep = 1000, 
                      trace = FALSE)

rda_fwd_sel$call #Para revisar cuales son estas variables importantes
#Output:
#rda(formula = mite_hellinger ~ WatrCont + Shrub + Substrate + 
#Topo, data = mite.env)


### Revisar qué tanto explica ### 

## Para checar la significancia de estas variables que se seleccionaron
mite_fwd_signif <- rda(mite_hellinger ~ WatrCont + Shrub +
                                      Substrate + Topo + 
                                      SubsDens, 
                                      data = mite.env)

# Encuentra la R2 de este modelo con estas variables

RsquareAdj(mite_fwd_signif)$adj.r.squared
#Output:
#[1] 0.4367038


### Revisar su significancia ###

anova.cca(mite_fwd_signif, step = 1000)


### Hacer el plot ###

# Scaling 1
ordiplot(mite_fwd_signif, scaling = 1, main = "Mite RDA - Scaling 1")

# Scaling 2
ordiplot(mite_fwd_signif, scaling = 2, main = "Mite RDA - Scaling 2")

