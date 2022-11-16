base <- rio::import("1.datos/7_1_vivienda.xls", sheet = "Región")
colnames(base) <- base[1,]

norte <- c(1,2,3,4,15)
centro <- c(6,7,8,9,16)
sur <- c(9,14,10,11,12)
rm <- 13

base1 <- base %>% 
  filter(ÁREA == "Urbano") %>% 
  select(ORDEN:ÁREA, `VIVIENDAS PARTICULARES OCUPADAS CON MORADORES PRESENTES`) %>% 
  mutate(zona = ifelse(`CÓDIGO REGIÓN` == rm, 4, 
                      ifelse(`CÓDIGO REGIÓN` %in% norte, 1,
                             ifelse(`CÓDIGO REGIÓN` %in% centro, 2, 3))))

base1 <- base1[-1,]

# Cantidades totales ------------------------------------------------------

viviendas.totales <- sum(as.numeric(base1[,5]))

viviendas.zona <- list(z1 = sum(as.numeric(base1[which(base1$zona == 1),5])),
                       z2 = sum(as.numeric(base1[which(base1$zona == 2),5])),
                       z3 = sum(as.numeric(base1[which(base1$zona == 3),5])),
                       z4 = sum(as.numeric(base1[which(base1$zona == 4),5])))


# Cantidades muestrales ---------------------------------------------------

viviendas.muestrales.z <- list(z1 = sum(distinct(datos[which(datos$zona==1),], 
                                             IdMz, .keep_all = TRUE)$Nviv),
                               z2 = sum(distinct(datos[which(datos$zona==2),], 
                                             IdMz, .keep_all = TRUE)$Nviv),
                               z3 = sum(distinct(datos[which(datos$zona==3),], 
                                             IdMz, .keep_all = TRUE)$Nviv),
                               z4 = sum(distinct(datos[which(datos$zona==4),], 
                                             IdMz, .keep_all = TRUE)$Nviv))

viviendas.muestrales.t <- sum(as.numeric(viviendas.muestrales.z))

viviendas.manzana <- list(z1 = distinct(datos[which(datos$zona==1),], 
                                        IdMz, .keep_all = TRUE)$Nviv,
                          z2 = distinct(datos[which(datos$zona==2),], 
                                        IdMz, .keep_all = TRUE)$Nviv,
                          z3 = distinct(datos[which(datos$zona==3),], 
                                        IdMz, .keep_all = TRUE)$Nviv,
                          z4 = distinct(datos[which(datos$zona==4),], 
                                        IdMz, .keep_all = TRUE)$Nviv)

# Probabilidades ----------------------------------------------------------

select.manzana <- list(z1 = viviendas.manzana[[1]]/viviendas.zona[[1]],
                       z2 = viviendas.manzana[[2]]/viviendas.zona[[2]],
                       z3 = viviendas.manzana[[3]]/viviendas.zona[[3]],
                       z4 = viviendas.manzana[[4]]/viviendas.zona[[4]])

select.vivienda <- list(z1 = viviendas.manzana[[1]]/viviendas.muestrales.z[[1]],
                        z2 = viviendas.manzana[[2]]/viviendas.muestrales.z[[2]],
                        z3 = viviendas.manzana[[3]]/viviendas.muestrales.z[[3]],
                        z4 = viviendas.manzana[[4]]/viviendas.muestrales.z[[4]])

pesos1 <- select.manzana[[1]]^(-1)/select.vivienda[[1]]
pesos2 <- select.manzana[[2]]^(-1)/select.vivienda[[2]]
pesos3 <- select.manzana[[3]]^(-1)/select.vivienda[[3]]
pesos4 <- select.manzana[[4]]^(-1)/select.vivienda[[4]]
