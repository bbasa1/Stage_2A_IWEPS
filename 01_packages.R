
dico_pays <- c(
  "FR" ="France",
  "AT" ="Autriche",
  "BE" ="Belgique",
  "BG" ="Bulgarie",
  "CH" ="Suisse",
  "CY" ="Chypre",
  "CZ" ="République Tchèque",
  "DE" ="Allemagne",
  "DK" ="Dannemark",
  "EE" ="Estonie",
  "ES" ="Espagne",
  "FI" ="Finlande",
  "GR" ="Grèce",
  "HR" ="Croatie",
  "HU" ="Hongrie",
  "IE" ="Irlande",
  "IS" ="Islande",
  "IT" ="Italie",
  "LT" ="Lituanie",
  "LU" ="Luxembourg",
  "LV" ="Lettonie",
  "MT" ="Malte",
  "NL" ="Pays-bas",
  "NO" ="Norvège",
  "PL" ="Pologne",
  "PT" ="Portugal",
  "RO" ="Roumanie",
  "SE" ="Suisse",
  "SI" ="Slovénie",
  "SK" ="Slovakie",
  "UK" ="Royaume-Unis"
)

dico_pays_anglais <- c(
  "FR" ="France",
  "AT" ="Austria",
  "BE" ="Belgium",
  "BG" ="Bulgaria",
  # "CH" ="Suisse",
  "CY" ="Cyprus",
  "CZ" = "Czech Republic",
  "DE" ="Germany",
  "DK" ="Denmark",
  "EE" ="Estonia",
  "ES" ="Spain",
  "FI" ="Finland",
  "GR" ="Greece",
  "HR" ="Croatia",
  "HU" ="Hungary",
  "IE" ="Ireland",
  # "IS" ="Islande",
  "IT" ="Italy",
  "LT" ="Lithuania",
  "LU" ="Luxembourg",
  # "LV" ="Lettonie",
  "MT" ="Malta",
  "NL" ="Netherlands",
  "NO" ="Norway",
  "PL" ="Poland",
  "PT" ="Portugal",
  "RO" ="Romania",
  "SE" ="Switzerland",
  "SI" ="Slovenia",
  "SK" ="Slovak Republi",
  "UK" ="United Kingdom"
)



library(readr)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
library(ggpmisc)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(ggpmisc)
library(tibble)
library(quantreg)
library(tidyverse) 
library(broom)
library(ggpubr)
library(base)
library(survey)
library(questionr)
library(jtools)
library(GoFKernel)
library(finalfit)
library(xgboost)
# library(xlsx)        
library(openxlsx) # Load on each new session
library(emdbook)

library(Matching)
library(rgenoud)
library(MatchIt)
library(rgenoud)
library(optmatch)
library(marginaleffects)


library(mitools)
library(srvyr)

library(pdftools)

library(ggplotify)
library(scales)

library(gridExtra)
library(base)


library(reticulate)


library(Zelig)
# devtools::install_github('IQSS/Zelig') # N'installer que les packages CRAN en cas de pbm


library(epitools)
