################################################################################
########################### SCRIPT PRINCIPAL ###################################
################################################################################
source(paste(repo_prgm , "01_packages.R" , sep = "/"))


# Le dossier général
repgen <- "C:/Users/Benjamin/Desktop/IWEPS"
# Les sous-dossiers
repo_prgm <- paste(repgen, "Stage_2A_IWEPS" , sep = "/")
repo_sorties <- paste(repgen, "Sorties" , sep = "/")
repo_data <- paste(repgen, "Data" , sep = "/")
repo_inter <- paste(repgen, "Bases_intermediaires" , sep = "/")


# Les sous-sous-dossiers qui contiennent les données
liste_sous_fichiers_data <- c("HFCS_UDB_1_5_ASCII", "HFCS_UDB_2_4_ASCII", "HFCS_UDB_3_2_ASCII", "HFCS_UDB_4_0 _early_diss_ASCII")
sous_repo_data <- paste(repo_data, liste_sous_fichiers_data, sep = "/")

# Importation des données
# h = core household files ==> On en a besoin pour connaître l'année
# p = core personal files
# hn = non-core household files
# pn = non-core personal files
# d = derivated variables files ==> Celui qu'on va principalement utiliser

source(paste(repo_prgm , "02_importation_data.R" , sep = "/"))
