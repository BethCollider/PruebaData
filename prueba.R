#Cargas librerias o paquetes
library(tidyverse)
library(dplyr)
library(stringr)

#Cargar bases de datos 
library(readr)

data <- read.csv("rejeans.csv")
clave <- read.csv("key.csv")
registro <- 189

#ref <- inner_join(key,data, by='')


#Region de compra
GJR <- (sum(data$X16 == "") - registro)*(-1)
view(GJR)
MTX <- (sum(data$X17 == "")- registro)*(-1)
view(MTX)
PUE <- (sum(data$X18 == "")- registro)*(-1)
view(PUE)
CDMX <- (sum(data$X19 == "")- registro)*(-1)
view(CDMX)

GJR <- (sum(hombres$X16 == "") - 73)*(-1)
view(GJR)
MTX <- (sum(hombres$X17 == "")- 73)*(-1)
view(MTX)
PUE <- (sum(hombres$X18 == "")- 73)*(-1)
view(PUE)
CDMX <- (sum(hombres$X19 == "")- 73)*(-1)
view(CDMX)


TotalEst <- GJR+MTX+PUE+CDMX
view(TotalEst)


#edad
Edad18_24 <- (sum(data$X11 == "") - registro)*(-1)
view(Edad18_24)
Edad25_34 <- (sum(data$X12 == "")- registro)*(-1)
view(Edad25_34)
Edad35_44 <- (sum(data$X13 == "")- registro)*(-1)
view(Edad35_44)
Edad45_54 <- (sum(data$X14 == "")- registro)*(-1)
view(Edad45_54)
Edad55_65 <- (sum(data$X15 == "")- registro)*(-1)
view(Edad55_65)

#edad mujeres
Edad18_24 <- (sum(hombres$X11 == "") - 73)*(-1)
view(Edad18_24)
Edad25_34 <- (sum(hombres$X12 == "")- 73)*(-1)
view(Edad25_34)
Edad35_44 <- (sum(hombres$X13 == "")- 73)*(-1)
view(Edad35_44)
Edad45_54 <- (sum(hombres$X14 == "")- 73)*(-1)
view(Edad45_54)
Edad55_65 <- (sum(hombres$X15 == "")- 73)*(-1)
view(Edad55_65)


TotalEdad <- Edad18_24+Edad25_34+Edad35_44
view(TotalEdad)

# Recomendación de marca
Ed <- data %>%
  select(X11:X15) %>% 
  pivot_longer(cols = starts_with("X"), names_to = "Columna") %>%
  count(Columna, value) %>%
  arrange(desc(n))

#Compras dobles segun edad
TotalDD <- TotalEdad - registro 

#compras unitarias segun edad
Totalsin <- registro - TotalDD


mujer <- data %>% filter(X9 == 2)
hombres <- data %>% filter(X9 == 1)

#hombres vs mujeres
Totalmujer <- 116
Totalhombres <- 73
TotalGenero <- Totalmujer + Totalhombres






#en los ultimos 12 meses un total de 216 personas de un apoblacion de 1164
#compro jeans 



#lugar de compra
TiendaDepar <- (sum(data$X35 == "") - registro)*(-1)
TiendaMarcar <- (sum(data$X36 == "") - registro)*(-1)
TiendaMMarcar <- (sum(data$X37 == "") - registro)*(-1)
TiendaCentroC <- (sum(data$X38 == "") - registro)*(-1)
TiendaeComerEsp  <- (sum(data$X39 == "") - registro)*(-1)
TiendaeComerpopu  <- (sum(data$X40 == "") - registro)*(-1)
TiendaTianguis  <- (sum(data$X41 == "") - registro)*(-1)
TiendaAutoS  <- (sum(data$X42 == "") - registro)*(-1)


TotalTiendaC<- TiendaDepar + TiendaMarcar + TiendaMMarcar + TiendaCentroC
                + TiendaeComerEsp + TiendaeComerpopu + TiendaTianguis + TiendaAutoS

#personas que fueron a comprar en mas de 2 lugares diferentes jeans 
TotalTiendaDD <- TotalTiendaC -registro

#personas que compran sus jeans en un solo lugar
TotaltiendaSin <- registro - TotalTiendaDD

pago <- data %>% 
  group_by(X43)
#tipo de pago
 efectivo <- 112
 deposito <- 4
 mercadoP <- 2
 TC <- 29
 TD <- 27
 TDeparta <- 9
 trasferencia <- 6
 
 TotalFormaP <-  efectivo +deposito + mercadoP +TC + TD +TDeparta +trasferencia 

#frecuencia de compra 15 dias menos de 15 dias anual mes, 3meses 6 meses
 
 D15 <- 3
 DMenorF <-10
 Anual <- 48
 Mensual <- 24
 M3 <- 46
 M6 <- 58
 
 TotalFrecuenciaCom <-  D15 +DMenorF + Anual +Mensual +M3 + M6 


#cuales son los atributos para comprar los jeans
P1 <- mean(data$X47) 
P2 <- mean(data$X48 ) 
 P3 <- mean(data$X49 ) 
 P4 <- mean(data$X50 ) 
 P5  <- mean(data$X51 ) 
 P6 <- mean(data$X52 )
 P8  <- mean(data$X53 ) 
 P9  <- mean(data$X54 ) 
 P10  <- mean(data$X55 ) 
 
 
 #gastos de ropa mensual
 maximo <- pmin(data$X46)

View(maximo)



summary(data$X46)


#AmeriaEagle marca favoritas de jeams
like <-data %>% 
  count(data$X148)

#Marca de jeans que comprarn con mas frecuencia
compramas <-data %>% 
  count(data$X147)

# Recomendación de marca
ProbaCompra <- data %>%
  select(X120:X145) %>% 
  pivot_longer(cols = starts_with("X"), names_to = "Columna") %>%
  count(Columna, value) %>%
  arrange(desc(n))

porcentajeProbaCompra <- data %>%
  select(X120:X145) %>%
  pivot_longer(cols = starts_with("X"), names_to = "Columna") %>%
  mutate(
    Rango = case_when(
      value %in% 9:10 ~ "10-9",
      value %in% 7:8 ~ "8-7",
      value %in% 0:6 ~ "0-6",
      TRUE ~ "Otros"
    )
  ) %>%
  group_by(Columna, Rango) %>%
  summarize(Count = n()) %>%
  group_by(Columna) %>%
  mutate(Percentage = (Count / sum(Count)) * 100) %>%
  arrange(desc(Columna), desc(Rango))



#Cuales marca de jeans has comprado

MasPopularCompra <- data %>%
  select(X94:X118) %>%
  pivot_longer(cols = starts_with("X"), names_to = "Columna") %>%
  count(Columna, value) %>%
  arrange(desc(n))

#mas conosidas
MasConocidos <- data %>%
  select(X58:X82) %>%
  pivot_longer(cols = starts_with("X"), names_to = "Columna") %>%
  count(Columna, value) %>%
  arrange(desc(n))
