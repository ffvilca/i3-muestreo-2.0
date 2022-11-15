censo <- rio::import(file.choose())
manzanas <- rio::import(file.choose())

library(tidyverse)
library(survey)


# Viviendas por zona ------------------------------------------------------

censo <- censo %>% 
  filter(AREA == 1, P01 != c(9,10), P02 == 1) %>% 
  select(-c(P03A, P03B, P03C, P04, P05, REGION_15R, PROVINCIA_15R, COMUNA_15R, AREA,
            P01, P02)) 

norte <- c(1,2,3,4,15)
centro <- c(6,7,8,9,16)
sur <- c(9,14,10,11,12)
rm <- 13

str(censo)

censo1 <- censo %>% 
  mutate(zona = ifelse(REGION == rm, 4, 
                       ifelse(REGION %in% norte, 1,
                              ifelse(REGION %in% centro, 2, 3)))) %>% 
  select(-REGION)

censo1$zona <- as.factor(censo1$zona)

viv.zona <- table(censo1$zona) #elementos totales de la población M


# Manzanas por zona -------------------------------------------------------

manzanas <- manzanas %>% 
  mutate(zona = ifelse(REGION == rm, 4, 
                       ifelse(REGION %in% norte, 1,
                              ifelse(REGION %in% centro, 2, 3)))) %>% 
  select(-REGION)

manzanas1 <- manzanas %>% 
  filter(P01_1 != 0, AREA == 1, VIV_PART != 0) %>% 
  select(-c(P01_1:P03C_5))

man.zona <- table(manzanas1$zona) #número de conglomerados por estrato (primera etapa) N

## Probabilidad de seleccionar las manzanas, asumiendo probabilidad equitativa
p.m <- manzanas.zona/man.zona #n/N
#manzanas.zona -> n conglomerados seleccionados por MAS

viv.manzana <- list(z1 = manzanas1$VIV_PART[which(manzanas1$zona == 1)],
                    z2 = manzanas1$VIV_PART[which(manzanas1$zona == 2)],
                    z3 = manzanas1$VIV_PART[which(manzanas1$zona == 3)],
                    z4 = manzanas1$VIV_PART[which(manzanas1$zona == 4)]) #elementos por conglomerado Mi

viviendas.manzana <- list(z1 = distinct(datos[which(datos$zona==1),], 
                                        IdMz, .keep_all = TRUE)$Nviv,
                          z2 = distinct(datos[which(datos$zona==2),], 
                                        IdMz, .keep_all = TRUE)$Nviv,
                          z3 = distinct(datos[which(datos$zona==3),], 
                                        IdMz, .keep_all = TRUE)$Nviv,
                          z4 = distinct(datos[which(datos$zona==4),], 
                                        IdMz, .keep_all = TRUE)$Nviv) #elementos por conglomerado Mi seleccionado


selec.manzana <- list(z1 = count(datos[which(datos$zona==1),], IdMz)[,2],
                      z2 = count(datos[which(datos$zona==2),], IdMz)[,2],
                      z3 = count(datos[which(datos$zona==3),], IdMz)[,2],
                      z4 = count(datos[which(datos$zona==4),], IdMz)[,2]) #elementos seleccionados del conglomerado i mi


# Pesos muestrales por zona y manzana -------------------------------------

#pi=mi/M probabilidad de selección del conglomerado

pesos1 <- viviendas.manzana[[1]]/selec.manzana[[1]]*p.m[1]^(-1)
pesos2 <- viviendas.manzana[[2]]/selec.manzana[[2]]*p.m[2]^(-1)
pesos3 <- viviendas.manzana[[3]]/selec.manzana[[3]]*p.m[3]^(-1)
pesos4 <- viviendas.manzana[[4]]/selec.manzana[[4]]*p.m[4]^(-1)




