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