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


##### Remplazo de codigos 
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "AFDEPYPAXXXX", "AGEN FINANC DE DESAR")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "AMAMPYPAXXXX", "BASA S.A.")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "BBVAPYPAXXXX", "BBVA")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "BCNAPYPAXXXX", "CONTINENTAL ")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "BCOPPYPAXXXX", "CO P/COMERC & PROD")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "BGNBPYPXXXXX", "GNB")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "BISAPYPEXXXX", "RIO")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "BNFAPYPAXXXX", "BNF")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "BNITPYPAXXXX", "ATLAS")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "BRASPYPXXXXX", "BANCO DO BRASIL")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "BSUDPYPXXXXX", "SUDAMERIS")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "CITIUS33XASR", "CITIBANK")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "COMAPYPAXXXX", "UENO")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "FAMIPYPAXXXX", "FAMILIAR")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "FICSPYPAXXXX", "FIC ")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "FINLPYPAXXXX", "FINLATINA ")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "FIPJPYPAXXXX", "PARAGUAYO JAPONESA ")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "FNXAPYPAXXXX", "FINEXPAR S.A.E.C.A.")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "FRIOPYPAXXXX", "FINANCIERA RIO")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "IIFAPYPAXXXX", "GRUP INTER DE FINANZAS")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "MIHAPYPAXXXX", "MINISTERIO DE HACIENDA")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "NACNPYPAXXXX", "BANCO ARGENTINA")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "RGSAPYPEXXXX", "REGIONAL")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "SOLAPYPAXXXX", "SOLAR ")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "TUFIPYPAXXXX", "TU FINANCIERA")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "UBBRPYPXXXXX", "ITAU")
suma_por_añoBanco$banco <- replace(suma_por_añoBanco$banco, suma_por_añoBanco$banco == "VISCPYPAXXXX", "VISION")

##### Remplazo de codigos 

#ok

# Plot

anim <- suma_por_añoBanco %>%
  ggplot( aes(x = año, y =xd, group=banco, color=banco)) +
  geom_line() +
  geom_point() +
  geom_text(
    label=c(suma_por_añoBanco$banco),
    nudge_x=0.45, nudge_y=0.1,
    check_overlap=T
  )+
  #scale_color_viridis(discrete = TRUE) +
  ggtitle("Transacciones por Entidades") +
  theme_ipsum() +
  ylab("Importe destino en miles de millones") +
  transition_reveal(as.numeric(año))



# animate it
animate(anim,
        width = 1000, height = 1000,
        nframes = 480, fps = 25)

# Save at gif:
anim_save("transaccon_etiqueta_rescalado.gif")





