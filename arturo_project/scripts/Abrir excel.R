#Abrir excel


# Instalar/Cargar paquetes
pacman::p_load(tidyverse, # Manejo y tratamiento de datos
               rio,       # Importación y exportación de bases de datos
               REDCapR)   # Importación de datos del REDCap


# Importar casos confirmados desde Excel 
df_2  <- import("datos/SIPAP.xlsx")

df_3 <- import("datos/SIPAP.xlsx", 
               which ="Entidades Financieras 01") # Se indica el nombre de la hoja 
