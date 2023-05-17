# Instalar/Cargar paquetes ####
pacman::p_load(tidyverse, # Manejo y tratamiento de datos
               rio,       # Importación y exportación de bases de datos
               REDCapR)   # Importación de datos del REDCap
library(dplyr)
if (!require('ggplot2'))
  install.packages("ggplot2")
library(ggplot2) 
if (!require('gganimate'))
  install.packages("gganimate")
library(gganimate)
if (!require('hrbrthemes'))
  install.packages("hrbrthemes")
library(hrbrthemes)


#Importa datos #################################

df_area <- import("datos/SIPAP_pestañas/Entidades Financieras 04.xlsx")
view(df_area)

# Moneda PYG ####
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


##### Remplazo de codigos ####
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


#limpiamos datos de bancos que no poseen datos

suma_por_añoBanco <- subset(suma_por_añoBanco, xd != 0)

#Se excluye el año 2023 debido a que no se culimno aun el año
suma_por_añoBanco <- subset(suma_por_añoBanco, año != 2023)

  df_porcentaje <- suma_por_añoBanco  %>%
    group_by(banco) %>%
    summarize(total_monto = sum(x)) %>%
    mutate(porcentaje = total_monto / sum(total_monto) * 100)



#  
    df_porcentaje$porcentaje <-round(df_porcentaje$porcentaje, digits = 2)
    
    sum(df_porcentaje$porcentaje)
    


    ## Crear el gráfico de porcentajes ####
    ggplot(df_porcentaje, aes(x = porcentaje, y = banco, group = 1)) +
      geom_line() +
      geom_point() +
      geom_text(aes(label = paste(porcentaje, "%")),
                hjust = -0.2, vjust = 0.5, size = 3) +
      labs(x = "Porcentaje", y = "Entidades", title = "Porcentajes total montos por entidad (PYG)")

  
##### Grafico dinamico #### 


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
  ggtitle("Transacciones por Entidades (PYG)") +
  theme_ipsum() +
  ylab("Importe destino en miles de millones") +
  transition_reveal(as.numeric(año))



### Rescaldado ####

animate(anim,
        width = 1000, height = 600,
        nframes = 100, fps = 5)

### Guardar gif: ####
anim_save("transaccon_etiqueta_rescalado_Guaranines.gif")





#Moneda USD ####

df_area_limpioD<- df_area %>%  filter(df_area$`Entidad Financiera` %in% c("USD"))
df_area_limpioD$`Entidad Financiera` <- NULL

df_area_limpioD1 <- df_area_limpioD %>% 
  pivot_longer(!Banco, names_to = c("Fecha"), values_to = "monto")


donD2 <- df_area_limpioD1 #DON2 es un temporal- cambiar despues
donD2$monto[is.na(donD2$monto)] <- 0
#don2$monto <- format(don2$monto, scientific = FALSE)
#don2$monto <- as.numeric( don2$monto )

donD2$Año <- format(as.Date(paste(donD2$Fecha, "01", sep = "/"), "%Y/%m/%d"), "%Y")

suma_por_añoBancoD <- aggregate(donD2$monto, by = list(año = donD2$Año, banco = donD2$Banco), FUN = sum)

suma_por_añoBancoD$xd <- suma_por_añoBancoD$x/1000000


##### Remplazo de codigos ####
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "AFDEPYPAXXXX", "AGEN FINANC DE DESAR")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "AMAMPYPAXXXX", "BASA S.A.")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "BBVAPYPAXXXX", "BBVA")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "BCNAPYPAXXXX", "CONTINENTAL ")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "BCOPPYPAXXXX", "CO P/COMERC & PROD")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "BGNBPYPXXXXX", "GNB")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "BISAPYPEXXXX", "RIO")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "BNFAPYPAXXXX", "BNF")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "BNITPYPAXXXX", "ATLAS")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "BRASPYPXXXXX", "BANCO DO BRASIL")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "BSUDPYPXXXXX", "SUDAMERIS")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "CITIUS33XASR", "CITIBANK")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "COMAPYPAXXXX", "UENO")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "FAMIPYPAXXXX", "FAMILIAR")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "FICSPYPAXXXX", "FIC ")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "FINLPYPAXXXX", "FINLATINA ")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "FIPJPYPAXXXX", "PARAGUAYO JAPONESA ")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "FNXAPYPAXXXX", "FINEXPAR S.A.E.C.A.")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "FRIOPYPAXXXX", "FINANCIERA RIO")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "IIFAPYPAXXXX", "GRUP INTER DE FINANZAS")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "MIHAPYPAXXXX", "MINISTERIO DE HACIENDA")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "NACNPYPAXXXX", "BANCO ARGENTINA")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "RGSAPYPEXXXX", "REGIONAL")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "SOLAPYPAXXXX", "SOLAR ")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "TUFIPYPAXXXX", "TU FINANCIERA")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "UBBRPYPXXXXX", "ITAU")
suma_por_añoBancoD$banco <- replace(suma_por_añoBancoD$banco, suma_por_añoBancoD$banco == "VISCPYPAXXXX", "VISION")

#limpiamos datos de bancos qu eno poseen datos

suma_por_añoBancoD <- subset(suma_por_añoBancoD, xd != 0)

#Se excluye el año 2023 debido a que no se culimno aun el año
suma_por_añoBancoD <- subset(suma_por_añoBancoD, año != 2023)


df_porcentajeD <- suma_por_añoBancoD  %>%
  group_by(banco) %>%
  summarize(total_monto = sum(x)) %>%
  mutate(porcentaje = total_monto / sum(total_monto) * 100)



#  
df_porcentajeD$porcentaje <-round(df_porcentajeD$porcentaje, digits = 2)

sum(df_porcentajeD$porcentaje)


## Crear el gráfico de porcentajes ####
ggplot(df_porcentajeD, aes(x = porcentaje, y = banco, group = 1)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = paste(porcentaje, "%")),
            hjust = -0.2, vjust = 0.5, size = 3) +
  labs(x = "Porcentaje", y = "Entidades", title = "Porcentajes total montos por entidad (USD)")


##### Crea el grafico dinamico #### 


animD <- suma_por_añoBancoD %>%
  ggplot( aes(x = año, y =xd, group=banco, color=banco)) +
  geom_line() +
  geom_point() +
  geom_text(
    label=c(suma_por_añoBancoD$banco),
    nudge_x=0.45, nudge_y=0.1,
    check_overlap=T
  )+
  #scale_color_viridis(discrete = TRUE) +
  ggtitle("Transacciones por Entidades (USD)") +
  theme_ipsum() +
  ylab("Importe destino en millones") +
  transition_reveal(as.numeric(año))



## Rescalado ####

animate(animD,
        width = 1000, height = 600,
        nframes = 100, fps = 5)

## Guardar gif ####
anim_save("transaccon_etiqueta_rescalado_Dolares.gif")



# Moneda EUR ####



df_area_limpioE<- df_area %>%  filter(df_area$`Entidad Financiera` %in% c("EUR"))
df_area_limpioE$`Entidad Financiera` <- NULL

df_area_limpioE1 <- df_area_limpioE %>% 
  pivot_longer(!Banco, names_to = c("Fecha"), values_to = "monto")


donE2 <- df_area_limpioE1 #DON2 es un temporal- cambiar despues
donE2$monto[is.na(donE2$monto)] <- 0
#don2$monto <- format(don2$monto, scientific = FALSE)
#don2$monto <- as.numeric( don2$monto )

donE2$Año <- format(as.Date(paste(donE2$Fecha, "01", sep = "/"), "%Y/%m/%d"), "%Y")

suma_por_añoBancoE <- aggregate(donE2$monto, by = list(año = donE2$Año, banco = donE2$Banco), FUN = sum)

suma_por_añoBancoE$xd <- suma_por_añoBancoE$x/1000


##### Remplazo de codigos  ####
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "AFDEPYPAXXXX", "AGEN FINANC DE DESAR")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "AMAMPYPAXXXX", "BASA S.A.")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "BBVAPYPAXXXX", "BBVA")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "BCNAPYPAXXXX", "CONTINENTAL ")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "BCOPPYPAXXXX", "CO P/COMERC & PROD")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "BGNBPYPXXXXX", "GNB")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "BISAPYPEXXXX", "RIO")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "BNFAPYPAXXXX", "BNF")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "BNITPYPAXXXX", "ATLAS")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "BRASPYPXXXXX", "BANCO DO BRASIL")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "BSUDPYPXXXXX", "SUDAMERIS")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "CITIUS33XASR", "CITIBANK")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "COMAPYPAXXXX", "UENO")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "FAMIPYPAXXXX", "FAMILIAR")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "FICSPYPAXXXX", "FIC ")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "FINLPYPAXXXX", "FINLATINA ")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "FIPJPYPAXXXX", "PARAGUAYO JAPONESA ")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "FNXAPYPAXXXX", "FINEXPAR S.A.E.C.A.")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "FRIOPYPAXXXX", "FINANCIERA RIO")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "IIFAPYPAXXXX", "GRUP INTER DE FINANZAS")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "MIHAPYPAXXXX", "MINISTERIO DE HACIENDA")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "NACNPYPAXXXX", "BANCO ARGENTINA")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "RGSAPYPEXXXX", "REGIONAL")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "SOLAPYPAXXXX", "SOLAR ")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "TUFIPYPAXXXX", "TU FINANCIERA")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "UBBRPYPXXXXX", "ITAU")
suma_por_añoBancoE$banco <- replace(suma_por_añoBancoE$banco, suma_por_añoBancoE$banco == "VISCPYPAXXXX", "VISION")




#limpiamos datos de bancos qu eno poseen datos

suma_por_añoBancoE <- subset(suma_por_añoBancoE, xd != 0)

#Se excluye el año 2023 debido a que no se culimno aun el año
suma_por_añoBancoE <- subset(suma_por_añoBancoE, año != 2023)


df_porcentajeE <- suma_por_añoBancoE  %>%
  group_by(banco) %>%
  summarize(total_monto = sum(x)) %>%
  mutate(porcentaje = total_monto / sum(total_monto) * 100)



#  
df_porcentajeE$porcentaje <-round(df_porcentajeE$porcentaje, digits = 2)

sum(df_porcentajeE$porcentaje)


## Crear el gráfico de procentajes ####
ggplot(df_porcentajeE, aes(x = porcentaje, y = banco, group = 1)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = paste(porcentaje, "%")),
            hjust = -0.2, vjust = 0.5, size = 3) +
  labs(x = "Porcentaje", y = "Entidades", title = "Porcentajes total montos por entidad (EUR)")


##### Grafico dinamico ####


animE <- suma_por_añoBancoE %>%
  ggplot( aes(x = año, y =xd, group=banco, color=banco)) +
  geom_line() +
  geom_point() +
  geom_text(
    label=c(suma_por_añoBancoE$banco),
    nudge_x=0.45, nudge_y=0.1,
    check_overlap=T
  )+
  #scale_color_viridis(discrete = TRUE) +
  ggtitle("Transacciones por Entidades (EUR)") +
  theme_ipsum() +
  ylab("Importe destino en miles") +
  transition_reveal(as.numeric(año))



## Rescalado ####

animate(animE,
        width = 1000, height = 600,
        nframes = 100, fps = 5)

##Guardar GIF ####
anim_save("transaccon_etiqueta_rescalado_EUROS.gif")
