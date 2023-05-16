#Abrir excel


# Instalar/Cargar paquetes
pacman::p_load(tidyverse, # Manejo y tratamiento de datos
               rio,       # Importación y exportación de bases de datos
               REDCapR)   # Importación de datos del REDCap
library(dplyr)
if (!require('ggplot2'))
  install.packages("ggplot2")
library(ggplot2) 

# Importar casos confirmados desde Excel 
#Segunda planilla
df_3 <- import("datos/SIPAP_pestañas/Entidades Financieras 02.xlsx")

## Compute row and column sums for a matrix:

#Limpiar los valores de Euros se eliminan los valores NA


df_3 <- format(df_3, scientific = FALSE)
#Euros Importe

df_3$Euros_Importe_destino<- as.numeric(na_if(df_3$`EUR Importe Destino`, ""))

df_3$Euros_Importe_destino[is.na(df_3$Euros_Importe_destino)] <- 0


#Dolares Importe
df_3$Dolares_Importe_destino<- as.numeric(na_if(df_3$`USD Importe Destino`, ""))

df_3$Dolares_Importe_destino[is.na(df_3$Dolares_Importe_destino)] <- 0


#Guaranies Importe
df_3$Guaranies_Importe_destino<- as.numeric(na_if(df_3$`PYG Importe Destino`, ""))

df_3$Guaranies_Importe_destino[is.na(df_3$Guaranies_Importe_destino)] <- 0

#Euros Cantidad

df_3$`EUR Cantidad`<- as.numeric(na_if(df_3$`EUR Cantidad`, ""))

df_3$`EUR Cantidad`[is.na(df_3$`EUR Cantidad`)] <- 0

#Dolares Cantidad
df_3$`USD Cantidad`<- as.numeric(na_if(df_3$`USD Cantidad`, ""))

df_3$`USD Cantidad`[is.na(df_3$`USD Cantidad`)] <- 0

#Guaranies Cantidad
df_3$`PYG Cantidad`<- as.numeric(na_if(df_3$`PYG Cantidad`, ""))

df_3$`PYG Cantidad`[is.na(df_3$`PYG Cantidad`)] <- 0



#Fecha vs Monto

#Euros
FvM = data.frame(x=df_3$`Año`, y = df_3$Euros_Importe_destino) 

datos_ceros_Montos <- subset(FvM, FvM$y != 0)

#Dolares
FvMD = data.frame(x=df_3$`Año`, y = df_3$Dolares_Importe_destino) 

datos_ceros_MontosD<- subset(FvMD, FvMD$y != 0)


#Guaranies
FvMG = data.frame(x=df_3$`Año`, y = df_3$Guaranies_Importe_destino) 

datos_ceros_Montos <- subset(FvMG, FvMG$y != 0)


# convertir las fechas 

df_3$Año <- format(as.Date(paste(df_3$`Año`, "01", sep = "/"), "%Y/%m/%d"), "%Y")


#Agrupar valores

#Euros
suma_por_año <- aggregate(df_3$Euros_Importe_destino, by = list(año = df_3$Año), FUN = sum)
suma_por_año$xd <- suma_por_año$x/1000000
suma_por_año <- suma_por_año %>% filter(año != "2023")

#Dolares
#df_3$Dolares_Importe_destino <- format(df_3$Dolares_Importe_destino, scientific = FALSE)
suma_por_añoD <- aggregate(df_3$Dolares_Importe_destino, by = list(año = df_3$Año), FUN = sum)
suma_por_añoD$xd <- suma_por_añoD$x/1000000000
suma_por_añoD <- suma_por_añoD %>% filter(año != "2023")
#suma_por_añoD$xd <- trunc(suma_por_añoD$xd,0)
#suma_por_añoD$xd <- format(suma_por_añoD$xd, scientific = FALSE)
#View(suma_por_añoD)

#Guaranies
#df_3$Guaranies_Importe_destino <- format(df_3$Guaranies_Importe_destino, scientific = FALSE)
suma_por_añoG <- aggregate(df_3$Guaranies_Importe_destino, by = list(año = df_3$Año), FUN = sum)
suma_por_añoG$xd <- suma_por_añoG$x/10000000000000
suma_por_añoG <- suma_por_añoG %>% filter(año != "2023")


#Histograma

#Euros
ggplot(suma_por_año, aes( xd, año)) + geom_point() + labs(y = "Año", x = "Importe destino en millones", title = "Transferencias por Montos y Moneda Euro")  

ggplot(suma_por_año, aes(x = as.factor(año) , y= xd  )) + geom_bar(stat = "identity") + labs( x = "Año", y = "Importe destino en millones", title = "Transferencias por Montos y Moneda Euro")  

#Dolares
ggplot(suma_por_añoD, aes( xd, año)) + geom_point() + labs(y = "Año", x = "Importe destino en miles de millones", title = "Transferencias por Montos y Moneda Dolares")  

ggplot(suma_por_añoD, aes(x = as.factor(año) , y= xd  )) + geom_bar(stat = "identity") + labs( x = "Año", y = "Importe destino en miles de millones", title = "Transferencias por Montos y Moneda Dolares")  

#Guaranies
ggplot(suma_por_añoG, aes( xd, año)) + geom_point() + labs(y = "Año", x = "Importe destino en miles de millones", title = "Transferencias por Montos y Moneda Guaranies")  

ggplot(suma_por_añoG, aes(x = as.factor(año) , y= xd  )) + geom_bar(stat = "identity") + labs( x = "Año", y = "Importe destino en miles de millones", title = "Transferencias por Montos y Moneda Guaranies")  

# Libraries barplot
library(ggplot2)
library(dplyr)

#Guaranies
barplot(height=suma_por_añoG$xd, names=suma_por_añoG$año, border="#202020", col="green", 
        xlab ="Año", 
        ylab ="Importe destino en miles de millones", 
        main = "Transferencias por Montos y Moneda Guaranies",
        ylim=c(0,40),las=1)

#Euros
barplot(height=suma_por_año$xd, names=suma_por_año$año, border="#202020", col="yellow", 
        xlab ="Año", 
        ylab ="Importe destino en millones", 
        main = "Transferencias por Montos y Moneda Euros",
        ylim=c(0,25),las=1)

#Dolares
barplot(height=suma_por_añoD$xd, names=suma_por_añoD$año, border="#202020", col="red", 
        xlab ="Año", 
        ylab ="Importe destino en miles de millones", 
        main = "Transferencias por Montos y Moneda Dolares",
        ylim=c(0,35), las=1)

#otro grafico
ggplot(suma_por_añoG, aes(x = año , y= xd  )) +
  geom_bar(stat = "identity") +
  geom_point() + 
  geom_segment( aes(x = año, xend=año, y=0, yend=xd)) + 
  labs( x = "Año", y = "Importe destino en miles de millones", 
        title = "Transferencias por Montos y Moneda Guaranies")  



