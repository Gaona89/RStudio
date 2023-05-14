# Instalar/Cargar paquetes
pacman::p_load(tidyverse, # Manejo y tratamiento de datos
               rio,       # Importación y exportación de bases de datos
               REDCapR)   # Importación de datos del REDCap
library(dplyr)
if (!require('ggplot2'))
  install.packages("ggplot2")
library(ggplot2) 


if (!require('gganimate'))
  install.packages("gganimate")
if (!require('hrbrthemes'))
  install.packages("hrbrthemes")

library(gganimate)
library(hrbrthemes)


##########3 area #################################33
#ok
df_area <- import("datos/SIPAP_pestañas/Entidades Financieras 04.xlsx")
view(df_area)
df_area_limpio<- df_area %>%  filter(df_area$`Entidad Financiera` %in% c("PYG"))
df_area_limpio$`Entidad Financiera` <- NULL

df_area_limpio1 <- df_area_limpio %>% 
  pivot_longer(!Banco, names_to = c("Fecha"), values_to = "monto")


don2 <- df_area_limpio1 #DON2 es un temporal- cambiar despues
don2$monto[is.na(don2$monto)] <- 0
#don2$monto <- format(don2$monto, scientific = FALSE)
#don2$monto <- as.numeric( don2$monto )

don2$Año <- format(as.Date(paste(don2$Fecha, "01", sep = "/"), "%Y/%m/%d"), "%Y")

suma_por_añoBanco <- aggregate(don2$monto, by = list(año = don2$Año, banco = don2$Banco), FUN = sum)

suma_por_añoBanco$xd <- suma_por_añoBanco$x/10000000000

#ok

# Plot

suma_por_añoBanco %>%
  ggplot( aes(x = año, y =xd, group=banco, color=banco)) +
  geom_line() +
  geom_point() +
  #scale_color_viridis(discrete = TRUE) +
  ggtitle("Transacciones por Entidades") +
  theme_ipsum() +
  ylab("Importe destino en miles de millones") +
  transition_reveal(as.numeric(año))

# Save at gif:
anim_save("transaccon.gif")

