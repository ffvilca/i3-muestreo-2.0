library(survey)
library(tidyverse)
library(anesrake)
library(expss)
library(here)
library(sampling)

datos <- rio::import(here("1.datos/Encuesta.xlsx"))
censo_p <- rio::import(here("1.datos/estimaciones-y-proyecciones-2002-2035-comunas.xlsx"))
base <- rio::import(here("1.datos/7_1_vivienda.xls"), sheet = "Región")
colnames(base) <- base[1,]

norte <- c(1,2,3,4,15)
centro <- c(6,7,8,9,16)
sur <- c(9,14,10,11,12)
rm <- 13

# a) --------------------------------------------------------------


base1 <- base %>% 
  filter(ÁREA == "Urbano") %>% 
  select(ORDEN:ÁREA, `VIVIENDAS PARTICULARES OCUPADAS CON MORADORES PRESENTES`) %>% 
  mutate(zona = ifelse(`CÓDIGO REGIÓN` == rm, 4, 
                       ifelse(`CÓDIGO REGIÓN` %in% norte, 1,
                              ifelse(`CÓDIGO REGIÓN` %in% centro, 2, 3))))

base1 <- base1[-1,]

censo_limpio <- censo_p %>% 
  select(Zona,`Sexo\r\n1=Hombre\r\n2=Mujer`,Edad,`Poblacion 2021`) %>% 
  rename("Sexo" = `Sexo\r\n1=Hombre\r\n2=Mujer`,"Poblacion_2021" = `Poblacion 2021`) %>% 
  filter(Edad >= 18) %>% 
  mutate("cat.edad" = ifelse(18 <= Edad & Edad <=29,1,
                             ifelse(30<= Edad & Edad <=44,2,
                                    ifelse(45<= Edad & Edad <=59, 3,4)))) %>% 
  select(Zona, Sexo,cat.edad,Poblacion_2021)

pesos_pob <- censo_limpio %>%  
  group_by(Zona, Sexo, cat.edad) %>%
  summarise(Pob_grupo = sum(Poblacion_2021)) %>% 
  mutate(Prob_grupo = Pob_grupo/sum(Pob_grupo))

pesos_muestra <- datos %>% 
  select(zona, sexo, cat.edad) %>% 
  group_by(zona, sexo, cat.edad) %>%
  count()

sexo_pob <- censo_limpio %>% 
  select(Sexo, Poblacion_2021) %>% 
  group_by(Sexo) %>% 
  summarise(Pob_grupo = sum(Poblacion_2021)) %>% 
  mutate(Prob_grupo = Pob_grupo/sum(Pob_grupo))

zona_pob <- censo_limpio %>% 
  select(Zona, Poblacion_2021) %>% 
  group_by(Zona) %>% 
  summarise(Pob_grupo = sum(Poblacion_2021)) %>% 
  mutate(Prob_grupo = Pob_grupo/sum(Pob_grupo))

cat.edad_pob <- censo_limpio %>% 
  select(cat.edad, Poblacion_2021) %>% 
  group_by(cat.edad) %>% 
  summarise(Pob_grupo = sum(Poblacion_2021)) %>% 
  mutate(Prob_grupo = Pob_grupo/sum(Pob_grupo))


## Factores de expansión y probabilidades conjuntas ----

poblacion <- censo_limpio %>% 
  select(Zona, Sexo, cat.edad, Poblacion_2021) %>% 
  group_by(Zona, Sexo, cat.edad) %>% 
  summarise(Pob_grupo = sum(Poblacion_2021))

muestra <- datos %>% 
  select(zona, sexo, cat.edad) %>% 
  count(zona, sexo, cat.edad)

prob <- muestra$n/poblacion$Pob_grupo #probabilidad de que sea de la zona, sea de algún sexo y pertenezca a cierta categoría de edad

fexp <- 1/prob #factores de expansión

w <- sum(fexp)

## Factores de expansión y probabilidades marginales ----

### Por zona ----

viviendas.zona <- list(z1 = sum(as.numeric(base1[which(base1$zona == 1),5])),
                       z2 = sum(as.numeric(base1[which(base1$zona == 2),5])),
                       z3 = sum(as.numeric(base1[which(base1$zona == 3),5])),
                       z4 = sum(as.numeric(base1[which(base1$zona == 4),5])))

viviendas.muestrales.z <- list(z1 = sum(distinct(datos[which(datos$zona==1),], 
                                                 IdMz, .keep_all = TRUE)$Nviv),
                               z2 = sum(distinct(datos[which(datos$zona==2),], 
                                                 IdMz, .keep_all = TRUE)$Nviv),
                               z3 = sum(distinct(datos[which(datos$zona==3),], 
                                                 IdMz, .keep_all = TRUE)$Nviv),
                               z4 = sum(distinct(datos[which(datos$zona==4),], 
                                                 IdMz, .keep_all = TRUE)$Nviv))

select.zona <- list(z1 = viviendas.muestrales.z[[1]]/viviendas.zona[[1]],
                    z2 = viviendas.muestrales.z[[2]]/viviendas.zona[[2]],
                    z3 = viviendas.muestrales.z[[3]]/viviendas.zona[[3]],
                    z4 = viviendas.muestrales.z[[4]]/viviendas.zona[[4]])

fexp1 <- 1/select.zona[[1]]
fexp2 <- 1/select.zona[[2]]
fexp3 <- 1/select.zona[[3]]
fexp4 <- 1/select.zona[[4]]

### Por sexo ----

persona.mu <- list(hombre = count(datos[which(datos$sexo == 1),], sexo)[,2],
                   mujer = count(datos[which(datos$sexo == 2),], sexo)[,2])

select.persona <- list(hombre = persona.mu[[1]]/sexo_pob[1,2],
                       mujer = persona.mu[[2]]/sexo_pob[2,2])

fexph <- select.persona[[1]]^(-1)
fexpm <- select.persona[[2]]^(-1)

### por categoría de edad ----

cat.edadmuestral <- list(c1 = count(datos[which(datos$cat.edad == 1),], cat.edad)[,2],
                         c2 = count(datos[which(datos$cat.edad == 2),], cat.edad)[,2],
                         c3 = count(datos[which(datos$cat.edad == 3),], cat.edad)[,2],
                         c4 = count(datos[which(datos$cat.edad == 4),], cat.edad)[,2])

select.edad <- list(c1 = cat.edadmuestral[[1]]/cat.edad_pob[1,2],
                    c2 = cat.edadmuestral[[2]]/cat.edad_pob[2,2],
                    c3 = cat.edadmuestral[[3]]/cat.edad_pob[3,2],
                    c4 = cat.edadmuestral[[4]]/cat.edad_pob[4,2])

fexpc1 <- 1/select.edad[[1]]
fexpc2 <- 1/select.edad[[2]]
fexpc3 <- 1/select.edad[[3]]
fexpc4 <- 1/select.edad[[4]]

## Cruce votos sin ponderar ----

cro_cpct(datos$P2v, datos$P1v)


## Cruce votos ponderados ----

muestra.p <- muestra %>% 
  mutate(w = fexp) %>% 
  select(-n)

datos <- datos %>% mutate(pesos = NA)
for(i in 1:32){
  datos$pesos[which(datos$zona == muestra.p$zona[i] & 
                    datos$sexo == muestra.p$sexo[i] &
                    datos$cat.edad == muestra.p$cat.edad[i])] <- muestra.p$w[i]
}

cro_cpct(datos$P2v, datos$P1v, weight = datos$pesos)

# b) --------------------------------------------------------------

real <- poblacion$Pob_grupo/sum(poblacion$Pob_grupo)
observado.p <- muestra.p$w/sum(muestra.p$w)

pesos.n <- real/observado.p

datos <- datos %>% mutate(g = NA)
for(i in 1:32){
  datos$g[which(datos$zona == muestra.p$zona[i] & 
                      datos$sexo == muestra.p$sexo[i] &
                      datos$cat.edad == muestra.p$cat.edad[i])] <- pesos.n[i]
}

cro_cpct(datos$P2v, datos$P1v, weight = datos$g)

# c) ----------------------------------------------------------------------


# por tablita de enunciadito

gse_pob <- c(8.6, 18.8 ,26.3 ,46.3 )/100

target_c <- list(
  sexo = c(
    "1" = sexo_pob$Prob_grupo[1], 
    "2"  = sexo_pob$Prob_grupo[2]
  ),
  cat.edad = c(
    "1" = cat.edad_pob$Prob_grupo[1], 
    "2" = cat.edad_pob$Prob_grupo[2], 
    "3" = cat.edad_pob$Prob_grupo[3], 
    "4" = cat.edad_pob$Prob_grupo[4]
  ),
  zona = c(
    "1" = zona_pob$Prob_grupo[1],
    "2" = zona_pob$Prob_grupo[2],
    "3" = zona_pob$Prob_grupo[3],
    "4" = zona_pob$Prob_grupo[4]
  ),
  gse = gse_pob
)

anesrakefinder(target_c, datos, choosemethod = "total") 


raking_c <- anesrake(
  target_c, 
  data.frame(datos), 
  caseid = datos$num, 
  verbose= FALSE, 
  cap = 5,
  choosemethod = "total",
  type = "pctlim",nlim = 5,
  pctlim = .05 , 
  iterate = TRUE,
  force1 = TRUE
)

summary(raking_c)

Pobla_Chile <- sum(censo_limpio$Poblacion_2021)

datos_c <- datos %>% 
  mutate(pond = raking_c$weightvec)

cro_cpct(datos_c$P2v,list(datos_c$P1v ,total()), weight=datos_c$pond)


# d) ----------------------------------------------------------------------


# por tablita de enunciadito

voto_pob <- c("1" = 53.0, "2" = 12.1,"3"= 13.1,"4" = 21.8)/100

voto_muestra <- datos %>% 
  mutate("voto_P1V" = ifelse(P1v == 1,"2",
                             ifelse(P1v == 2, "3",
                                    ifelse(P1v == 8,"1","4")))) %>% 
  group_by(voto_P1V) %>% 
  count() %>% 
  mutate("prob_P1v" = n/1007)

datos_d <- datos %>% 
  mutate("votos" = ifelse(P1v == 1,"2",
                             ifelse(P1v == 2, "3",
                                    ifelse(P1v == 8,"1","4"))))

target_d <- list(
  sexo = c(
    "1" = sexo_pob$Prob_grupo[1], 
    "2"  = sexo_pob$Prob_grupo[2]
  ),
  cat.edad = c(
    "1" = cat.edad_pob$Prob_grupo[1], 
    "2" = cat.edad_pob$Prob_grupo[2], 
    "3" = cat.edad_pob$Prob_grupo[3], 
    "4" = cat.edad_pob$Prob_grupo[4]
  ),
  zona = c(
    "1" = zona_pob$Prob_grupo[1],
    "2" = zona_pob$Prob_grupo[2],
    "3" = zona_pob$Prob_grupo[3],
    "4" = zona_pob$Prob_grupo[4]
  ),
  votos = voto_pob
)

raking_d <- anesrake(
  target_d, 
  data.frame(datos_d), 
  caseid = datos$num, 
  verbose= FALSE, 
  cap = 5,
  choosemethod = "total",
  type = "pctlim",nlim = 5,
  pctlim = .05 , 
  iterate = TRUE,
  force1 = TRUE
)

summary(raking_d)

datos_d <- datos %>% 
  mutate(pond = raking_d$weightvec)

cro_cpct(datos_d$P2v,list(datos_d$P1v ,total()), weight=datos_d$pond)


# e) ----------------------------------------------------------------------

pob.chile <- 15200840

## ponderador a) ----

tabla <- datos %>% 
  mutate(fexp = pesos/sum(pesos)*pob.chile)

cro_cpct(tabla$P2v, tabla$P1v, weight = tabla$fexp)
cro(tabla$P2v, tabla$P1v, weight = tabla$fexp)

## ponderador b) ----
tablab <- datos %>% 
  mutate(fexp = g/sum(g)*pob.chile)

cro_cpct(tablab$P2v, tablab$P1v, weight = tablab$fexp)
cro(tablab$P2v, tablab$P1v, weight = tablab$fexp)

## ponderador c) ----

tablac <- datos_c %>% 
  mutate(fexp = pond/sum(pond)*pob.chile)

cro_cpct(tablac$P2v, tablac$P1v, weight = tablac$fexp)
cro(tablac$P2v, tablac$P1v, weight = tablac$fexp)

## ponderador d) ----

tablad <- datos_d %>% 
  mutate(fexp =  pond/sum(pond)*pob.chile)

cro_cpct(tablad$P2v, tablad$P1v, weight = tablad$fexp)
cro(tablad$P2v, tablad$P1v, weight = tablad$fexp)
