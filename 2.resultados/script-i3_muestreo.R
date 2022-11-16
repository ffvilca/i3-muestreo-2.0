
datos <- rio::import("1.datos/Encuesta.xlsx")
library(survey)
library(tidyverse)
library(anesrake)

# a) --------------------------------------------------------------

#Primero se eligen las manzanas
#Segundo se elige la vivienda
#Al final se elige la persona

datos$zona <- as.factor(datos$zona)

aux <- matrix(table(datos$zona, datos$IdMz), nrow = 4)

manzanas.zona <- rowSums(ifelse(aux!=0, 1, 0))

## Probabilidades de selección de la vivienda

datos <- datos %>% 
  mutate(mxzona = manzanas.zona[zona]) %>% 
  mutate(p.sel1 =  (1/mxzona) * (1/Nviv), #para la etapa 1 (vivienda)
         p.sel2 = (1/Nelig)) #para la etapa 2 (persona)

# b) --------------------------------------------------------------



# Cruce Primera vuelta v/s Segunda Vuelta

sort(table(datos$P1v))
sort(table(datos$P2v))
cruce_1v_2v <- table(datos$P1v,datos$P2v)
round(prop.table(cruce_1v_2v,1)*100,3)


censo_p <- rio::import("1.datos/estimaciones-y-proyecciones-2002-2035-comunas.xlsx")
View(censo_p)

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

# por tablita de enunciadito

gse_pob <- c(8.6, 18.8 ,26.3 ,46.3 )/100

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
  gse = gse_pob
)

anesrake(
  target_d, 
  data.frame(datos), 
  caseid = datos$num, 
  verbose= TRUE, 
  cap = 5,
  choosemethod = "total",
  type = "pctlim", 
  pctlim = .05 , 
  iterate = TRUE,
  force1 = TRUE
)
