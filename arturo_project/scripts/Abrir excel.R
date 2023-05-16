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
df_2  <- import("datos/SIPAP_pestañas/Entidades Financieras 01.xlsx")

## Compute row and column sums for a matrix:

#Limpiar los valores de Euros se eliminan los valores NA


df_2 <- format(df_2, scientific = FALSE)
#Euros Importe

df_2$Euros_Importe_destino<- as.numeric(na_if(df_2$`EUR Importe Destino`, ""))

df_2$Euros_Importe_destino[is.na(df_2$Euros_Importe_destino)] <- 0


#Dolares Importe
df_2$Dolares_Importe_destino<- as.numeric(na_if(df_2$`USD Importe Destino`, ""))

df_2$Dolares_Importe_destino[is.na(df_2$Dolares_Importe_destino)] <- 0


#Guaranies Importe
df_2$Guaranies_Importe_destino<- as.numeric(na_if(df_2$`PYG Importe Destino`, ""))

df_2$Guaranies_Importe_destino[is.na(df_2$Guaranies_Importe_destino)] <- 0

#Euros Cantidad

df_2$`EUR Cantidad`<- as.numeric(na_if(df_2$`EUR Cantidad`, ""))

df_2$`EUR Cantidad`[is.na(df_2$`EUR Cantidad`)] <- 0

#Dolares Cantidad
df_2$`USD Cantidad`<- as.numeric(na_if(df_2$`USD Cantidad`, ""))

df_2$`USD Cantidad`[is.na(df_2$`USD Cantidad`)] <- 0

#Guaranies Cantidad
df_2$`PYG Cantidad`<- as.numeric(na_if(df_2$`PYG Cantidad`, ""))

df_2$`PYG Cantidad`[is.na(df_2$`PYG Cantidad`)] <- 0


#Fecha vs Monto

#Euros
FvM = data.frame(x=df_2$`Año Mes`, y = df_2$Euros_Importe_destino) 

datos_ceros_montos <- subset(FvM, FvM$y != 0)

#Dolares
FvMD = data.frame(x=df_2$`Año Mes`, y = df_2$Dolares_Importe_destino) 

datos_ceros_montosD<- subset(FvMD, FvMD$y != 0)


#Guaranies
FvMG = data.frame(x=df_2$`Año Mes`, y = df_2$Guaranies_Importe_destino) 

datos_ceros_montos <- subset(FvMG, FvMG$y != 0)


# convertir las fechas 

#fecha <- "2014/10"
#año <- format(as.Date(paste(fecha, "01", sep = "/"), "%Y/%m/%d"), "%Y")

df_2$Año <- format(as.Date(paste(df_2$`Año Mes`, "01", sep = "/"), "%Y/%m/%d"), "%Y")


#Agrupar valores


#df_4 <- filter(df_2, Año == 2014)
#sum(df_2$Euros_Importe_destino )

#df_3 <- df_2 %>%
#  group_by(Euros_Importe_destino)  %>% filter(df_2, Año == 2014)

?aggregate

#Euros

suma_por_año <- aggregate(df_2$Euros_Importe_destino, by = list(año = df_2$Año), FUN = sum)
suma_por_año$xd <- suma_por_año$x/1000000
suma_por_año <- suma_por_año %>% filter(año != "2023")

#Dolares
?trunc
#df_2$Dolares_Importe_destino <- format(df_2$Dolares_Importe_destino, scientific = FALSE)
suma_por_añoD <- aggregate(df_2$Dolares_Importe_destino, by = list(año = df_2$Año), FUN = sum)
suma_por_añoD$xd <- suma_por_añoD$x/10000000000000
suma_por_añoD <- suma_por_añoD %>% filter(año != "2023")
#suma_por_añoD$xd <- trunc(suma_por_añoD$xd,0)
#suma_por_añoD$xd <- format(suma_por_añoD$xd, scientific = FALSE)
#View(suma_por_añoD)

#Guaranies
#df_2$Guaranies_Importe_destino <- format(df_2$Guaranies_Importe_destino, scientific = FALSE)
suma_por_añoG <- aggregate(df_2$Guaranies_Importe_destino, by = list(año = df_2$Año), FUN = sum)
suma_por_añoG$xd <- suma_por_añoG$x/1000000000
suma_por_añoG <- suma_por_añoG %>% filter(año != "2023")

#Histograma

#Euros
ggplot(suma_por_año, aes( xd, año)) + geom_point() + labs(y = "Año", x = "Importe destino en miles de millones", title = "Transferencias por Montos y Moneda Euro")  

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
main = "Transferencias por montos y Moneda Guaranies",
ylim=c(0,10),las=1)

#Euros
barplot(height=suma_por_año$xd, names=suma_por_año$año, border="#202020", col="yellow", 
xlab ="Año", 
ylab ="Importe destino en millones", 
main = "Transferencias por montos y Moneda Euros",
ylim=c(0,10),las=1)

#Dolares
barplot(height=suma_por_añoD$xd, names=suma_por_añoD$año, border="#202020", col="red", 
xlab ="Año", 
ylab ="Importe destino en millones", 
main = "Transferencias por montos y Moneda Dolares",
ylim=c(0,10), las=1)

#otro grafico
ggplot(suma_por_añoG, aes(x = año , y= xd  )) +
  geom_bar(stat = "identity") +
  geom_point() + 
  geom_segment( aes(x = año, xend=año, y=0, yend=xd)) + 
  labs( x = "Año", y = "Importe destino en miles de millones", 
  title = "Transferencias por montos y Moneda Guaranies")  




