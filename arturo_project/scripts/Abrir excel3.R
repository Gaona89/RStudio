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


df_5 <- import("datos/SIPAP_pestañas/Entidades Financieras 05.xlsx")

df_5 <- format(df_5, scientific = FALSE)

df_5$Guaranies_Importe_destino<- as.numeric(na_if(df_5$`Importe Destino`, ""))

df_5$Guaranies_Importe_destino[is.na(df_5$Guaranies_Importe_destino)] <- 0


FvMG = data.frame(x=df_5$`Año Mes`, y = df_5$Guaranies_Importe_destino) 

datos_ceros_montos <- subset(FvMG, FvMG$y != 0)

df_5$Año <- format(as.Date(paste(df_5$`Año Mes`, "01", sep = "/"), "%Y/%m/%d"), "%Y")

?aggregate

suma_por_año <- aggregate(df_5$Guaranies_Importe_destino, by = list(año = df_5$Año), FUN = sum)

suma_por_año$xd <- suma_por_año$x/10000000000


suma_por_añoG <- aggregate(df_5$Guaranies_Importe_destino, by = list(año = df_5$Año), FUN = sum)
suma_por_añoG$xd <- suma_por_añoG$x/1000000000000


#Guaranies
ggplot(suma_por_añoG, aes( xd, año)) + geom_point() + labs(y = "Año", x = "Importe destino en miles de millones", title = "Transferencias por montos y Moneda Guaranies")  

ggplot(suma_por_añoG, aes(x = as.factor(año) , y= xd  )) + geom_bar(stat = "identity") + labs( x = "Año", y = "Importe destino en miles de millones", title = "Transferencias por montos y Moneda Guaranies")  



# Libraries barplot
library(ggplot2)
library(dplyr)

#Guaranies
barplot(height=suma_por_añoG$xd, names=suma_por_añoG$año, border="#202020", col="green", 
        xlab ="Año", 
        ylab ="Importe destino en miles de millones", 
        main = "Transferencias por montos y Moneda Guaranies",
        ylim=c(0,15),las=1)

#otro grafico
ggplot(suma_por_añoG, aes(x = año , y= xd  )) +
  geom_bar(stat = "identity") +
  geom_point() + 
  geom_segment( aes(x = año, xend=año, y=0, yend=xd)) + 
  labs( x = "Año", y = "Importe destino en miles de millones", 
        title = "Transferencias por montos y Moneda Guaranies") 


library(ggplot2)
library(dplyr)
suma_por_añoG$año <- as.Date(suma_por_añoG$año)

# Plot
suma_por_añoG %>%
  tail(10) %>%
  ggplot( aes(x=año, y=xd, group = 1)) +
  geom_line() +
  geom_point()+
  labs( x = "Año", y = "Importe destino en miles de millones", 
        title = "Transferencias por montos y Moneda Guaranies")

suma_por_añoG1 <- suma_por_añoG

suma_por_añoG1 %>%
  ggplot( aes(x = año, y = xd, group=1)) +
  geom_line() +
  geom_point() +
  #  scale_color_viridis(discrete = TRUE) +
  ggtitle("Transferencias por montos y Moneda Guaranies") +
  theme_ipsum() +
  ylab("Importe destino en miles de millones") +
  transition_reveal(as.numeric(año))


suma_por_añoG1 %>%
  ggplot( aes(x = año, y = xd, group=1)) +
  geom_bar(stat='identity') +
  theme_bw() +
  #  scale_color_viridis(discrete = TRUE) +
  ggtitle("Transferencias por montos y Moneda Guaranies") +
  theme_ipsum() +
  ylab("Importe destino en miles de millones") +
  transition_reveal(as.numeric(año))


  
