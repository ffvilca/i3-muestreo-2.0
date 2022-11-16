library(tidyverse)
library(survey)

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

# Cantidades totales zona ------------------------------------------------------

viviendas.totales <- sum(as.numeric(base1[,5]))

viviendas.zona <- list(z1 = sum(as.numeric(base1[which(base1$zona == 1),5])),
                       z2 = sum(as.numeric(base1[which(base1$zona == 2),5])),
                       z3 = sum(as.numeric(base1[which(base1$zona == 3),5])),
                       z4 = sum(as.numeric(base1[which(base1$zona == 4),5])))


# Cantidades muestrales zona ---------------------------------------------------

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

select.zona <- list(z1 = viviendas.muestrales.z[[1]]/viviendas.zona[[1]],
                    z2 = viviendas.muestrales.z[[2]]/viviendas.zona[[2]],
                    z3 = viviendas.muestrales.z[[3]]/viviendas.zona[[3]],
                    z4 = viviendas.muestrales.z[[4]]/viviendas.zona[[4]])

# Factores según zona -----------------------------------------------------

fexp1 <- 1/select.zona[[1]]
fexp2 <- 1/select.zona[[2]]
fexp3 <- 1/select.zona[[3]]
fexp4 <- 1/select.zona[[4]]

# Factores según zona/manzana ---------------------------------------------

fexp1m <- select.manzana[[1]]^(-1)/select.vivienda[[1]]
fexp2m <- select.manzana[[2]]^(-1)/select.vivienda[[2]]
fexp3m <- select.manzana[[3]]^(-1)/select.vivienda[[3]]
fexp4m <- select.manzana[[4]]^(-1)/select.vivienda[[4]]


# Cantidades totales si tiene tula o no -----------------------------------

sexo_pob


# Cantidades muestrales si tiene tula o no --------------------------------

persona.mu <- list(tula = count(datos[which(datos$sexo == 1),], sexo)[,2],
                   notula = count(datos[which(datos$sexo == 2),], sexo)[,2])


# Probabilidades si tiene tula o no ---------------------------------------

select.persona <- list(tula = persona.mu[[1]]/sexo_pob[1,2],
                       notula = persona.mu[[2]]/sexo_pob[2,2])


# Factores de expansión según tula ----------------------------------------

fexpt <- select.persona[[1]]^(-1)
fexpnt <- select.persona[[2]]^(-1)


# Cantidades totales edad -------------------------------------------------

cat.edad_pob


# Cantidades muestrales edad ----------------------------------------------

cat.edadmuestral <- list(j = count(datos[which(datos$cat.edad == 1),], cat.edad)[,2],
                         ntj = count(datos[which(datos$cat.edad == 2),], cat.edad)[,2],
                         ntv = count(datos[which(datos$cat.edad == 3),], cat.edad)[,2],
                         v = count(datos[which(datos$cat.edad == 4),], cat.edad)[,2])


# Probabilidad edad -------------------------------------------------------

select.edad <- list(j = cat.edadmuestral[[1]]/cat.edad_pob[1,2],
                    ntj = cat.edadmuestral[[2]]/cat.edad_pob[2,2],
                    ntv = cat.edadmuestral[[3]]/cat.edad_pob[3,2],
                    v = cat.edadmuestral[[4]]/cat.edad_pob[4,2])


# Factores de expansión según edad ----------------------------------------

fexpj <- 1/select.edad[[1]]
fexpntj <- 1/select.edad[[2]]
fexpntv <- 1/select.edad[[3]]
fexpv <- 1/select.edad[[4]]


# Probabilidades conjuntas ------------------------------------------------

pepito <- censo_limpio %>% 
  select(Zona, Sexo, cat.edad, Poblacion_2021) %>% 
  group_by(Zona, Sexo, cat.edad) %>% 
  summarise(Pob_grupo = sum(Poblacion_2021))

juanito <- datos %>% 
  select(zona, sexo, cat.edad) %>% 
  count(zona, sexo, cat.edad)

prob <- juanito$n/pepito$Pob_grupo

fexp <- 1/prob
