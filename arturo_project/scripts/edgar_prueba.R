#Abrir excel


# Instalar/Cargar paquetes
pacman::p_load(tidyverse, # Manejo y tratamiento de datos
               rio,       # Importación y exportación de bases de datos
               REDCapR)   # Importación de datos del REDCap


# Importar casos confirmados desde Excel 
df_2  <- import("datos/SIPAP_pestañas/Entidades Financieras 01.xlsx")
str(df_2)
df_2$`EUR Cantidad`
a <-df_2[2,5]
df_2  <- format(df_2, scientific = FALSE)
df_3 <- import("datos/SIPAP.xlsx", 
               which ="Entidades Financieras 01") # Se indica el nombre de la hoja 


#limpiar los datos(los valores N/A poner cero)

df_4 %>% 
  pivot_wider(names_from = Indice, values_from = seen)
?hist

df_3 %>% pivot_wider(
  names_from = Indice, 
  values_from = seen
  values_fill = 1
)

df_3 %>% 
  select(Indice) %>%
  distinct() %>% #this line removes duplicates
  count()


# tail() muestra las ultimas filas 

tail(df_3$How.likely.are.you.to.recommend.R.to.a.colleague..friend..or.family.member., 
     n = 20)

# Cambiar el nombre de las variables

df_3 = df_3 %>%
  rename("indice" = "Nada")


names(raw_data{1.})

names(df_3[1:8])

names(renamed[1:2])
?geom_histogram()

ggplot(aes(x = time, y = variable), data = data) + geom_line()


#-----con artruro----------------------------------------------------------------------------

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

df_2$Euros_Importe_destino[is.na(df_2$Euros_Importe_destino)] <- 0


#Guaranies Importe
df_2$Guaranies_Importe_destino<- as.numeric(na_if(df_2$`PYG Importe Destino`, ""))

df_2$Guaranies_Importe_destino[is.na(df_2$Euros_Importe_destino)] <- 0

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

datos_ceros_montosD <- subset(FvMD, FvMD$y != 0)

#Guaranies
FvMG = data.frame(x=df_2$`Año Mes`, y = df_2$Guaranies_Importe_destino) 

datos_ceros_montos <- subset(FvMG, FvMG$y != 0)

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

#Euros

suma_por_año <- aggregate(df_2$Euros_Importe_destino, by = list(año = df_2$Año), FUN = sum)

suma_por_año$xd <- suma_por_año$x/1000000

#Dolares

suma_por_añoD <- aggregate(df_2$Dolares_Importe_destino, by = list(año = df_2$Año), FUN = sum)

suma_por_añoD$xd <- suma_por_añoD$x/1000000

#Guaranies
suma_por_añoG <- aggregate(df_2$Guaranies_Importe_destino, by = list(año = df_2$Año), FUN = sum)

suma_por_añoG$xd <- suma_por_añoG$x/1000000






#Histograma

#Euros
ggplot(suma_por_año, aes( xd, año)) + geom_point() + labs(y = "Año Mes", x = "Importe destino en millones", title = "Transferencias por montos y Moneda Euro")  

ggplot(suma_por_año, aes(x = as.factor(año) , y= xd  )) + geom_bar(stat = "identity") + labs( x = "Año Mes", y = "Importe destino en millones", title = "Transferencias por montos y Moneda Euro")  

#Dolares
ggplot(suma_por_añoD, aes( xd, año)) + geom_point() + labs(y = "Año Mes", x = "Importe destino en millones", title = "Transferencias por montos y Moneda Euro")  

ggplot(suma_por_añoD, aes(x = as.factor(año) , y= xd  )) + geom_bar(stat = "identity") + labs( x = "Año Mes", y = "Importe destino en millones", title = "Transferencias por montos y Moneda Euro")  


#Guaranies
ggplot(suma_por_añoG, aes( xd, año)) + geom_point() + labs(y = "Año Mes", x = "Importe destino en millones", title = "Transferencias por montos y Moneda Euro")  

ggplot(suma_por_añoG, aes(x = as.factor(año) , y= xd  )) + geom_bar(stat = "identity") + labs( x = "Año Mes", y = "Importe destino en millones", title = "Transferencias por montos y Moneda Euro")  


