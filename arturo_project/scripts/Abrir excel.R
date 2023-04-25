#Abrir excel


# Instalar/Cargar paquetes
pacman::p_load(tidyverse, # Manejo y tratamiento de datos
               rio,       # Importación y exportación de bases de datos
               REDCapR)   # Importación de datos del REDCap
library(dplyr)

# Importar casos confirmados desde Excel 
df_2  <- import("datos/SIPAP_pestañas/Entidades Financieras 01.xlsx")

df_3 <- import("datos/SIPAP.xlsx", 
               which ="Entidades Financieras 01") # Se indica el nombre de la hoja 


#limpiar los datos(los valores N/A poner cero)

df_4 %>% 
  pivot_wider(names_from = Indice, values_from = seen)


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
 
df_2$Prueba <- as.numeric(na_if(df_2$`EUR Cantidad`, ""))
 
df3 <- colSums(df_2$Prueba,1,2)

## Compute row and column sums for a matrix:

#Limpiar los valores de Euros se eliminan los valores NA


df_2 <- format(df_2, scientific = FALSE)

df_2$Euros_Importe_destino<- as.numeric(na_if(df_2$`EUR Importe Destino`, ""))

df_2$Euros_Importe_destino[is.na(df_2$Euros_Importe_destino)] <- 0



df_2$Dolares_Importe_destino<- as.numeric(na_if(df_2$`USD Importe Destino`, ""))

df_2$Euros_Importe_destino[is.na(df_2$Euros_Importe_destino)] <- 0


df_2$`EUR Cantidad`<- as.numeric(na_if(df_2$`EUR Cantidad`, ""))

df_2$`EUR Cantidad`[is.na(df_2$`EUR Cantidad`)] <- 0


 
 #Eliminar columna prueba
 
 df_2$Prueba <- NULL

 ?hist
 
 hist(df_2$Euros_Importe_destino, main = "Transferencias en Euros", labels = "Euros" , plot = TRUE,
      xlab ="Montos", ylab = "Cantidad",
      ylim = df_2$`Año Mes`)
 
 # Cargar la librería "ggplot2"
 library(ggplot2) 
 
 # discrete value continuous scale r solution 
 a = data.frame(x=df_2$`EUR Cantidad`, y = df_2$Euros_Importe_destino) 
 ggplot(a, aes(x, y)) + geom_point() + scale_y_continuous(limits = c(0, 7))
 
 a = data.frame(x = 1:10, y = c(1:5)) 
 ggplot(a, aes(df_2$`EUR Cantidad`, df_2$Euros_Importe_destino)) + geom_point() + scale_y_continuous(limits = c(0, 7))
 
 ggplot(a, aes(x, y))
 
 ggplot(datos_sin_fila, aes(x, y)) + geom_point() 
 
 #Eliminar ceros
 
 datos_sin_fila <- subset(a, a$x != 0)
 