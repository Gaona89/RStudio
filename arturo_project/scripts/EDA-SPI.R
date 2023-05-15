#Verificamos datos de transferencias SPI, el nuevo modulo de BCP desde sus inicios hasta 2023-03
# vemos cuales son los montos minumos maximos y la meia de los monstos en guaranies.


if (!require('tidyverse'))
  install.packages("tidyverse")
library(tidyverse)

if (!require('rio'))
  install.packages("rio")
library(rio)

if (!require('dlookr'))
  install.packages("dlookr")
library(dlookr)

if (!require('ISLR'))
  install.packages("ISLR")
library(ISLR)

library(ISLR)

library(dplyr)

if (!require('explore'))
  install.packages("explore")
library(explore)

if (!require('ggraptR'))
  install.packages("ggraptR")
library(ggraptR)

if (!require('summarytools'))
  install.packages("summarytools")
library(summarytools)

library(ISLR)

df_area <- import("datos/SIPAP_pestañas/Entidades Financieras 07.xlsx")

describe(df_area)

normality(df_area)

df_area_clean <- subset(df_area, select = -1)

colnames(df_area_clean)[colnames(df_area_clean) == "...2"] <- "Bancos"

df_area_clean[is.na(df_area_clean)] <- 0


describe(df_area_clean)

#Eliminamos bancos que no utilizan la mensajeria SPI

df_area_clean <- subset(df_area_clean, Bancos != "ANAAPYPAXXXX")

df_area_clean <- subset(df_area_clean, Bancos != "ARAFPYPAXXXX")

df_area_clean <- subset(df_area_clean, Bancos != "INFAPYPAXXXX")

df_datos_2022<-describe(df_area_clean)
  


