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

df_2$Euros_Importe_destino<- as.numeric(na_if(df_2$`EUR Importe Destino`, ""))

df_2$Euros_Importe_destino[is.na(df_2$Euros_Importe_destino)] <- 0



df_2$Dolares_Importe_destino<- as.numeric(na_if(df_2$`USD Importe Destino`, ""))

df_2$Euros_Importe_destino[is.na(df_2$Euros_Importe_destino)] <- 0


df_2$`EUR Cantidad`<- as.numeric(na_if(df_2$`EUR Cantidad`, ""))

df_2$`EUR Cantidad`[is.na(df_2$`EUR Cantidad`)] <- 0

 
 #Fecha vs Monto
 
 
 FvM = data.frame(x=df_2$`Año Mes`, y = df_2$Euros_Importe_destino) 

 datos_ceros_montos <- subset(FvM, FvM$y != 0)
 
 # convertir las fechas 
 
 #fecha <- "2014/10"
 #año <- format(as.Date(paste(fecha, "01", sep = "/"), "%Y/%m/%d"), "%Y")
 
 df_2$Año <- format(as.Date(paste(df_2$`Año Mes`, "01", sep = "/"), "%Y/%m/%d"), "%Y")
 
 
 #Agrupar valores

 
 df_4 <- filter(df_2, Año == 2014)
 sum(df_2$Euros_Importe_destino )

 df_3 <- df_2 %>%
   group_by(Euros_Importe_destino)  %>% filter(df_2, Año == 2014)
 
 ?aggregate
 
 suma_por_año$xd <- suma_por_año$x/1000000
 
 suma_por_año <- aggregate(df_2$Euros_Importe_destino, by = list(año = df_2$Año), FUN = sum)
 
 ggplot(suma_por_año, aes( xd, año)) + geom_point() + labs(y = "Año Mes", x = "Importe destino en millones", title = "Transferencias por montos y Moneda Euro")  
 
 ggplot(suma_por_año, aes(x = as.factor(año) , y= xd  )) + geom_bar(stat = "identity") + labs( x = "Año Mes", y = "Importe destino en millones", title = "Transferencias por montos y Moneda Euro")  
  