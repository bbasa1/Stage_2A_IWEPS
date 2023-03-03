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

num_table <- 1
path_d <- paste("C:/Users/Benjamin/Desktop/IWEPS/HFCS_UDB_4_0 _early_diss_ASCII/d",num_table,".csv", sep = "")
path_h <- paste("C:/Users/Benjamin/Desktop/IWEPS/HFCS_UDB_4_0 _early_diss_ASCII/h",num_table,".csv", sep = "")
path_p <- paste("C:/Users/Benjamin/Desktop/IWEPS/HFCS_UDB_4_0 _early_diss_ASCII/p",num_table,".csv", sep = "")

# path <- "C:/Users/Benjamin/Desktop/IWEPS/HFCS_UDB_3_2_ASCII/H1.csv"

# Dérivée
data_derivated <- read_csv(path_d, 
                      locale = locale(encoding ="UTF-8"),
                      show_col_types = FALSE)

data_derivated <- as.data.table(data_derivated)
data_derivated
nrow(data_derivated)

# Logement
data_house <- read_csv(path_h, 
                           locale = locale(encoding ="UTF-8"),
                           show_col_types = FALSE)

data_house <- as.data.table(data_house)
data_house
nrow(data_house)

# Ménage
data_perso <- read_csv(path_p, 
                           locale = locale(encoding ="UTF-8"),
                           show_col_types = FALSE)

data_perso <- as.data.table(data_perso)
data_perso
nrow(data_perso)


data_complete <- merge(data_derivated, data_house, by= c("sa0010", "sa0100"))

nrow(data_complete)

# data_complete$sa0010

# data_belgique <- data_complete[SA0100 == 'BE',]
# data_complete$HB0800
# nrow(data_belgique)

liste_variables <- c(
  ### Core variables
  
  "sa0010", #Identifiant
  "sa0100", #Pays
  "sa0200", #Année
  "sa0110", #Past household ID
  "ra0200", #Genre
  # "ra0400", #Pays de naissance
  "ra0100", #Statut marital
  "pa0200", #Plus haut niveau d'éducation reçu
  "hdz0310", #Life satisfaction

  "hh0100", # any substantial gift or inheritance received 134
  "hh0110", # no of gifts/inheritances received 134
  # "hh020$x", # gift/inheritance $x: year gift/inheritance received 135
  # "hh030$x", # gift/inheritance $x: what kind of assets received 135
  # "hh040$x", # gift/inheritance $x: value 136
  # "hh050$x", # gift/inheritance $x: type of transfer (gift/inheritance) 136
  # "hh060$x", # gift/inheritance $x: from whom received 137

#### Derived variables

# "DA1000",# Total real assets 1 (incl. business wealth, vehicles and valuables) 146
# "DA1000i",# Has real assets 146
# "DA1000SH",# Real assets as share of gross wealth 146
# "DA1110",# Value of household's main residence 146
# "DA1110i",# Has HMR 146
# "DA1120",# Value of other real estate property 147
# "DA1120i",# Has other real estate property 147
# "DA1121",# Value of other real estate property used for business activities 147
# "DA1121i",# Has other real estate property for business 147
# "DA1122",# Value of other real estate property not for business activities 147
# "DA1122i",# Has other real estate property not for business 147
# "DA1130",# Value of household's vehicles 148
# "DA1130i",# Has vehicles 148
# "DA1131",# Valuables 148
# "DA1131i",# Has valuables 148

# "DA1400",# Real estate (incl. property used for business activities 149
# "DA1400i",# Has real estate wealth 149

"da2100",# Total financial assets 1 (excl. public and occupational pension plans) 149
"da2100i",# Has financial assets
"da2100sh",# Financial assets as share of gross wealth

"datop10",# Country top 10% gross wealth 155

"dh0001",# Number of household members 155
# "DH0002",# Consumption units (OECD modified) 155
# "DH0003",# Number of economically active members in household 155
# "DH0004",# Number of household members in employment 156
# "DH0006",# Number of household members 16+ 156
# "DH14P",# Number of household members aged 14+ 156
# "DHaged65plus",# Household members aged 65 or more 156

"dhageh1b",# Age of reference person in brackets, Canberra definition ==> Le bas de la tranche d'âge

"dheduh1",# Education of reference person, Canberra definition 158

"dhemph1",# Main labour status of reference person, Canberra definition 158
"dhgenderh1",# Gender of reference person, Canberra definition 159

"dhhst",# Housing status 159
"dhhtype",# Household type ==> Nb d'enfants + âge des gens

"dhlifesatis",# Life satisfaction
# 
# "DI1100",# Employee income
# "DI1100i",# Has employee 
# "DI1200",# Self-employment income
# "DI1200i",# Has self-employment income
# "DI1300",# Rental income from real estate property
# "DI1300i",# Has rental income from real estate property
# "DI1400",# Income from financial assets
# "DI1400i",# Has income from financial investments
# "DI1410",# Income from financial assets, gross of interest payments
# "DI1410i",# Has income from financial assets, gross of interest payments
# 
# "DI1412",# Interest payments ==> Ce que paie le ménage en remboursements de prets ???
#   
# "DI1420",# Income from private business other than self-employment
# "DI1420i",# Has income from private business other than self-employment
# "DI1500",# Income from pensions
# "DI1500i",# Has income from pensions
# "DI1510",# Income from public pensions
# "DI1510i",# Has income from public pensions
# "DI1520",# Income from occupational and private pensions
# "DI1520i",# Has income from occupational and private pensions
# "DI1600",# Regular social transfers (except pensions)
# "DI1600i",# Has income from regular social transfers (except pensions)
# "DI1610",# Unemployment benefits
# "DI1610i",# Has income from unemployment benefits
# 
# "DI2000",# Total household gross income 2, including interest payments ==> ANNUEL
# "DI2000eq",# Total household gross income, equalised ==> Par part fiscale
# "DI2100",# Total household gross income and financial assistance from relatives and friends

"ditop10",# Country top 10% total gross income ==> Quand on prend tous les revenus

"dnhw",# Net housing wealth ===> Quand on a retiré les prêts à rembourser, ce que le ménage possède VRAIMENT
"dntop10",# Top 10% net wealth

"doeinherit",# Expecting to receive inheritance in the future
"dogiftinher",# Amount of received gifts and inheritances ====> ATTENTION on peut retirer la résidence principale si elle a déjà été déclarée comme acquise par héritage
"dohhsqm",# HMR value per square meter
'hb0800' #property value at the time of its acquisition
)

# ra0200, ra0100, pa0200, hdz0310, dhlifesatis, doeinherit

# liste_variables <- c('SA0100', #pays
#                      'HB0800' #property value at the time of its acquisition
#                      )

data_merged <- data_complete[,..liste_variables]

data_merged[, mean(HB0800, na.rm = TRUE), by = SA0100]

data_merged

setnames(data_merged,'QHHNUM',"Identifiant_menage")





