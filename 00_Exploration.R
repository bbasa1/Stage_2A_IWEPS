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
