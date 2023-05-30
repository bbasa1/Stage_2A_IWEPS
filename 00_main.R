################################################################################
########################### SCRIPT PRINCIPAL ###################################
################################################################################


################################################################################
# ============================ 01 PARAMETRES ===================================
################################################################################


# Le dossier général
repgen <- "C:/Users/Benjamin/Desktop/IWEPS"
# Les sous-dossiers
repo_prgm <- paste(repgen, "Stage_2A_IWEPS" , sep = "/")
repo_sorties <- paste(repgen, "Sorties" , sep = "/")
repo_data <- paste(repgen, "Data" , sep = "/")
repo_inter <- paste(repgen, "Bases_intermediaires" , sep = "/")


source(paste(repo_prgm , "01_packages.R" , sep = "/"))


# Les sous-sous-dossiers qui contiennent les données
liste_sous_fichiers_data <- c("HFCS_UDB_1_5_ASCII", "HFCS_UDB_2_4_ASCII", "HFCS_UDB_3_2_ASCII", "HFCS_UDB_4_0_early_diss_ASCII")
sous_repo_data <- paste(repo_data, liste_sous_fichiers_data, sep = "/")

# Importation des données
# h = core household files ==> On en a besoin pour connaître l'année
# p = core personal files
# hn = non-core household files
# pn = non-core personal files
# d = derivated variables files ==> Celui qu'on va principalement utiliser


num_table <- 1 ### Change les poids assignés par eurostat
num_vague_max <- 4 ### Le nombre de vague qu'on veut concaténer ATTENTION la dernière vague a des noms de colonnes en MINUSCULE. Ca pose pbm dans la concaténation...

################################################################################
# ============================ 02 IMPORTATION ==================================
################################################################################


source(paste(repo_prgm , "02_importation_data.R" , sep = "/"))


table(data_complete$SA0200)
table(data_complete$SA0100)
table(data_complete$VAGUE)
colnames(data_complete)


########## Pour étudier les différences entre les différentes versions

data_complete_1 <- copy(data_complete)

num_table <- 2 ### Change les poids assignés par eurostat
source(paste(repo_prgm , "02_importation_data.R" , sep = "/"))
data_complete_2 <- copy(data_complete)


num_table <- 1 ### Change les poids assignés par eurostat
source(paste(repo_prgm , "02_importation_data.R" , sep = "/"))
head(data_complete$HW0010,100)

table(data_complete_1[data_complete_1$DN3001 != data_complete_2$DN3001])

data_complete_1[data_complete_1$SA0010  != data_complete_2$SA0010]

table(data_complete_1  == data_complete_2)

head(data_complete_1[data_complete_1$DOEINHERIT  != data_complete_2$DOEINHERIT ]$DOEINHERIT , 10)
head(data_complete_2[data_complete_1$DOEINHERIT  != data_complete_2$DOEINHERIT ]$DOEINHERIT , 10)

nrow(data_complete_1)*length(colnames(data_complete_1))
2277054 + 37663527




########## A discuter : 
# Les changements entre les fichiers ne viennent pas des poids mais de certaines valeurs. J'imagine les valeurs imputées ?
# En tout cas elles ont l'air de relativement peu changer entre les différents fichiers, de l'ordre de qq % en général
# Etats des lieux : Il existe au moins une différence entre les deux premières imputations dans 2277054/59424064 = 4% des cases où il y a du changement
# Pour obtenir ce chiffre = table(data_complete_1  == data_complete_2) effectifs FALSE/nrow(data_complete_1)*length(colnames(data_complete_1))



















