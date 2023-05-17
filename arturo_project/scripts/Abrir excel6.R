#Abrir excel


# Instalar/Cargar paquetes ####
pacman::p_load(tidyverse, # Manejo y tratamiento de datos
               rio,       # Importación y exportación de bases de datos
               REDCapR)   # Importación de datos del REDCap
library(dplyr)
if (!require('ggplot2'))
  install.packages("ggplot2")
library(ggplot2) 

# Importar casos confirmados desde Excel ####
#sexta planilla - SPI
df_6 <- import("datos/SIPAP_pestañas/Entidades Financieras 06.xlsx")

df_6 <- format(df_6, scientific = FALSE)

df_6$Guaranies_Importe_destino<- as.numeric(na_if(df_6$`Importe Destino`, ""))

df_6$Guaranies_Importe_destino[is.na(df_6$Guaranies_Importe_destino)] <- 0

FvMG = data.frame(x=df_6$`Año Mes`, y = df_6$Guaranies_Importe_destino) 

datos_ceros_montos <- subset(FvMG, FvMG$y != 0)

Datos <- format(datos_ceros_montos, scientific = FALSE)

Datos$y <- datos_ceros_montos$y/1000000000

#Guaranies

ggplot(Datos, aes( x = as.factor(x) , y= y )) + geom_point() + labs(x = "Meses", y = "Importe destino en miles de millones", title = "Transferencias por Montos")  

ggplot(Datos, aes(x = as.factor(x) , y= y  )) + geom_bar(stat = "identity") + labs( x = "Meses", y = "Importe destino en miles de millones", title = "Transferencias por Montos")  


# Libraries barplot
library(ggplot2)
library(dplyr)
Datos1 <- format(datos_ceros_montos, scientific = FALSE)
Datos1$y <- datos_ceros_montos$y/1000000000000

#Guaranies ####
barplot(height=Datos1$y, names=Datos1$x, border="#202020", col="blue", 
      
        ylab ="Importe destino en miles de millones", 
        main = "Transferencias por Montos",
        ylim=c(0,4),las=2)

#otro grafico####
ggplot(Datos, aes( x = as.factor(x) , y= y )) +
  geom_bar(stat = "identity") +
  geom_point() + 
  geom_segment( aes(x = x, xend=x, y=0, yend=y)) + 
  labs( x = "Meses", y = "Importe destino en miles de millones", 
        title = "Transferencias por Montos") 


library(ggplot2)
library(dplyr)
#suma_por_añoG$año <- as.Date(suma_por_añoG$año)

# Plot
Datos1 %>%
  tail(10) %>%
  ggplot( aes( x = as.factor(x) , y= y , group = 1)) +
  geom_line() +
  geom_point()+
  labs( x = "Meses", y = "Importe destino en miles de millones", 
        title = "Transferencias por Montos")
