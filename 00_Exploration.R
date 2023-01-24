library(readr)
# install.packages("data.table")
library(data.table)
# install.packages("ggplot2")
library(ggplot2)
# install.packages('viridis')
library(viridis)
library(dplyr)
# install.packages("ggpmisc")
library(ggpmisc)
# install.packages("tibble")
library(magrittr) # needs to be run every time you start R and want to use %>%
# install.packages("broom")
# install.packages("ggpubr")
library(ggpmisc)
library(tibble)
library(dplyr)
library(quantreg)
# install.packages("tidyverse")
library(tidyverse) 
library(broom)
library(ggpubr)


path <- "C:/Users/Benjamin/Desktop/IWEPS/HFCS_UDB_3_2_ASCII/H1.csv"
path
data_complete <- read_csv(path, 
                        locale = locale(encoding ="UTF-8"),
                        show_col_types = FALSE)

data_complete <- as.data.table(data_complete)


data_belgique <- data_complete[SA0100 == 'BE',]

nrow(data_belgique)

liste_variables <- c(
  ### Core variables
  
  "SA0010", #Identifiant
  "SA0100", #Pays
  "SA0110", #Past household ID
  "RA0200", #Genre
  "RA0400", #Pays de naissance
  "PA0100", #Statut marital
  "PA0200", #Plus haut niveau d'éducation reçu
  "HDZ0310", #Life satisfaction

  "HH0100", # any substantial gift or inheritance received 134
  "HH0110", # no of gifts/inheritances received 134
  "HH020$x", # gift/inheritance $x: year gift/inheritance received 135
  "HH030$x", # gift/inheritance $x: what kind of assets received 135
  "HH040$x", # gift/inheritance $x: value 136
  "HH050$x", # gift/inheritance $x: type of transfer (gift/inheritance) 136
  "HH060$x", # gift/inheritance $x: from whom received 137

#### Derived variables

"DA1000",# Total real assets 1 (incl. business wealth, vehicles and valuables) 146
"DA1000i",# Has real assets 146
"DA1000SH",# Real assets as share of gross wealth 146
"DA1110",# Value of household's main residence 146
"DA1110i",# Has HMR 146
"DA1120",# Value of other real estate property 147
"DA1120i",# Has other real estate property 147
"DA1121",# Value of other real estate property used for business activities 147
"DA1121i",# Has other real estate property for business 147
"DA1122",# Value of other real estate property not for business activities 147
"DA1122i",# Has other real estate property not for business 147
"DA1130",# Value of household's vehicles 148
"DA1130i",# Has vehicles 148
"DA1131",# Valuables 148
"DA1131i",# Has valuables 148

"DA1400",# Real estate (incl. property used for business activities 149
"DA1400i",# Has real estate wealth 149

"DA2100",# Total financial assets 1 (excl. public and occupational pension plans) 149
"DA2100i",# Has financial assets
"DA2100SH",# Financial assets as share of gross wealth

"DATOP10",# Country top 10% gross wealth 155

"DH0001",# Number of household members 155
"DH0002",# Consumption units (OECD modified) 155
"DH0003",# Number of economically active members in household 155
"DH0004",# Number of household members in employment 156
"DH0006",# Number of household members 16+ 156
"DH14P",# Number of household members aged 14+ 156
"DHaged65plus",# Household members aged 65 or more 156

"DHAGEH1B",# Age of reference person in brackets, Canberra definition ==> Le bas de la tranche d'âge

"DHEDUH1",# Education of reference person, Canberra definition 158

"DHEMPH1",# Main labour status of reference person, Canberra definition 158
"DHGENDERH1",# Gender of reference person, Canberra definition 159

"DHHST",# Housing status 159
"DHHTYPE",# Household type ==> Nb d'enfants + âge des gens

"DHLIFESATIS",# Life satisfaction

"DI1100",# Employee income
"DI1100i",# Has employee 
"DI1200",# Self-employment income
"DI1200i",# Has self-employment income
"DI1300",# Rental income from real estate property
"DI1300i",# Has rental income from real estate property
"DI1400",# Income from financial assets
"DI1400i",# Has income from financial investments
"DI1410",# Income from financial assets, gross of interest payments
"DI1410i",# Has income from financial assets, gross of interest payments

"DI1412",# Interest payments ==> Ce que paie le ménage en remboursements de prets ???
  
"DI1420",# Income from private business other than self-employment
"DI1420i",# Has income from private business other than self-employment
"DI1500",# Income from pensions
"DI1500i",# Has income from pensions
"DI1510",# Income from public pensions
"DI1510i",# Has income from public pensions
"DI1520",# Income from occupational and private pensions
"DI1520i",# Has income from occupational and private pensions
"DI1600",# Regular social transfers (except pensions)
"DI1600i",# Has income from regular social transfers (except pensions)
"DI1610",# Unemployment benefits
"DI1610i",# Has income from unemployment benefits

"DI2000",# Total household gross income 2, including interest payments ==> ANNUEL
"DI2000eq",# Total household gross income, equalised ==> Par part fiscale
"DI2100",# Total household gross income and financial assistance from relatives and friends

"DITOP10",# Country top 10% total gross income ==> Quand on prend tous les revenus

"DNHW",# Net housing wealth ===> Quand on a retiré les prêts à rembourser, ce que le ménage possède VRAIMENT
"DNTOP10",# Top 10% net wealth

"DOEINHERIT",# Expecting to receive inheritance in the future
"DOGIFTINHER",# Amount of received gifts and inheritances ====> ATTENTION on peut retirer la résidence principale si elle a déjà été déclarée comme acquise par héritage
"DOHHSQM",# HMR value per square meter

)

data_merged <- data_merged[,..liste_variables]

setnames(data_merged,'QHHNUM',"Identifiant_menage")
