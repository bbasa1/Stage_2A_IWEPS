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

num_table <- 1
path_d <- paste(sous_repo_data,"/d",num_table,".csv", sep = "")
data_derivated <- read_csv(path_d[1], 
                           locale = locale(encoding ="UTF-8"),
                           show_col_types = FALSE)

data_derivated <- as.data.table(data_derivated)
data_derivated
nrow(data_derivated)

path_h <- paste(sous_repo_data,"/h",num_table,".csv", sep = "")
data_house <- read_csv(path_h[1], 
                       locale = locale(encoding ="UTF-8"),
                       show_col_types = FALSE)

data_house <- as.data.table(data_house)
data_house
nrow(data_house)


data_complete <- merge(data_derivated, data_house, by= "ID") 
nrow(data_complete)

data_complete
colnames(data_derivated)
# all(data_complete$IM0100.x == data_complete$IM0100.y) # Normalement on a redondance pour : survey SA0010 (=identifiant) SA0100 (=pays) IM0100 (=implicate ID)

