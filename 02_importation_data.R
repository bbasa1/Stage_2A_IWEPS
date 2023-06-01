################################################################################
# ============================ 02 IMPORTATION ==================================
################################################################################

# Structure du script : 
#   - Initialisation de la boucle sur la première vague
#   - Boucle jusqu'à arriver à la dernière vague voulue num_vague_max
#   
# ATTENTION : On fusionne code housold uniquement sur les variables années et pays, sinon on ne garde que derivated variables
# ATTENTION : Les noms de colonnes sont globalement passées en majuscule à cause de la dernière vague d'enquête qui met tout en majuscule



liste_cols_core_household <- c("sa0010", # household identification number
                               "sa0100", # country
                               "sa0200", # survey vintage
                               "id",
                               "sa0210", # Vintage of last interview (household)
                               "SA0110" # Past household ID
                               ) ### A mettre en minuscule



###### On prépare la boucle avec la première importation
num_vague <- 1
path_d <- paste(sous_repo_data,"/d",num_table,".csv", sep = "")
data_derivated <- read_csv(path_d[num_vague], 
                           locale = locale(encoding ="UTF-8"),
                           show_col_types = FALSE)

data_derivated <- as.data.table(data_derivated)
data_derivated
nrow(data_derivated)
try(setnames(data_derivated, "id", "ID"), silent=TRUE) #Parfois les colonnes ont des noms en minuscule...
try(setnames(data_derivated, "survey", "SURVEY"), silent=TRUE) # Cette variable peut avoir deux noms 
try(setnames(data_derivated, "Survey", "SURVEY"), silent=TRUE) # Cette variable peut avoir deux noms



###### Pour avoir le pays et l'année de référence il faut le household, mais on n'a pas besoin du reste
path_h <- paste(sous_repo_data,"/h",num_table,".csv", sep = "")
data_house <- read_csv(path_h[num_vague], 
                       locale = locale(encoding ="UTF-8"),
                       show_col_types = FALSE)

data_house <- as.data.table(data_house)
for(nom_col in liste_cols_core_household){
  try(setnames(data_house, nom_col, toupper(nom_col)), silent=TRUE)
}
liste_cols_core_household_new <- toupper(liste_cols_core_household)

for(var in liste_cols_core_household_new){
  print(var)
  if(is.null(data_house[[var]])){ #C'est ce qui concerne le past. Mais pour la première vague forcément il n'y en n'a pas...
    data_house[, eval(var) := NaN]
  }
}
# 
# if(is.null(data_house$SA0210)){ #C'est ce qui concerne le past. Mais pour la première vague forcément il n'y en n'a pas...
#   data_house[, SA0210 := NaN]
# }
# if(is.null(data_house$SA0110)){
#   data_house[, SA0110 := NaN]
# }

data_house <- data_house[,..liste_cols_core_household_new]
nrow(data_house)


data_complete <- merge(data_derivated, data_house, by= c("ID", "SA0100", "SA0010"))
data_complete[, VAGUE := 1]
nrow(data_complete)


print("=========================================")
print(paste("Vague numéro", num_vague))
print(paste("Validation par le nombre de ligne =", (nrow(data_complete) == nrow(data_derivated))&(nrow(data_complete) == nrow(data_house))))
print(paste("Nombre de ligne tot =", nrow(data_complete)))
print(paste("Nombre de colonnes tot =", length(colnames(data_complete))))
print("=========================================")


for(num_vague in 2:num_vague_max){

  # Derivated
  path_d <- paste(sous_repo_data,"/d",num_table,".csv", sep = "")
  data_derivated <- read_csv(path_d[num_vague], 
                             locale = locale(encoding ="UTF-8"),
                             show_col_types = FALSE)
  data_derivated <- as.data.table(data_derivated)
  
  ### On passe toutes les colonnes en majuscule...
  for(nom_col in colnames(data_derivated)){
    try(setnames(data_derivated, nom_col, toupper(nom_col)), silent=TRUE)
  }
  print("Dernière vague prise en compte ==> Toutes les colonnes sont passées en majuscules")
  for(nom_col in colnames(data_complete)){
    try(setnames(data_complete, nom_col, toupper(nom_col)), silent=TRUE)
  }
  
  
  # try(setnames(data_derivated, "survey", "SURVEY"), silent=TRUE) # Cette variable peut avoir deux noms 
  # try(setnames(data_derivated, "Survey", "SURVEY"), silent=TRUE) # Cette variable peut avoir deux noms 
  
  
  
  # Household
  path_h <- paste(sous_repo_data,"/h",num_table,".csv", sep = "")
  data_house <- read_csv(path_h[num_vague], 
                         locale = locale(encoding ="UTF-8"),
                         show_col_types = FALSE)
  data_house <- as.data.table(data_house)
  for(nom_col in liste_cols_core_household){
    try(setnames(data_house, nom_col, toupper(nom_col)), silent=TRUE)
  }
  liste_cols_core_household_new <- toupper(liste_cols_core_household)
  for(var in liste_cols_core_household_new){
    if(is.null(data_house[[var]])){ #C'est ce qui concerne le past. Mais pour la première vague forcément il n'y en n'a pas...
      data_house[, eval(var) := NaN]
    }
  }
  data_house <- data_house[,..liste_cols_core_household_new]
  
  # Merge et concat
  data_complete_loc <- merge(data_derivated, data_house, by= c("ID", "SA0100", "SA0010"))
  
  # colnames(data_complete_loc)
  # colnames(data_complete)

  data_complete_loc[, VAGUE := num_vague]
  
  
  data_complete <- rbindlist(list(data_complete,
                                  data_complete_loc), fill=TRUE)
  
  print("=========================================")
  print(paste("Vague numéro", num_vague))
  print(paste("Validation par le nombre de ligne =", (nrow(data_complete_loc) == nrow(data_derivated))&(nrow(data_complete_loc) == nrow(data_house))))
  print(paste("Nombre de ligne tot =", nrow(data_complete)))
  print(paste("Nombre de colonnes tot =", length(colnames(data_complete))))
  print("=========================================")
  
}


# 
# num_vague <- 4
# if(num_vague == 4){
#   print("Dernière vague prise en compte ==> Toutes les colonnes sont passées en majuscules")
#   for(nom_col in colnames(data_complete)){
#     try(setnames(data_complete, nom_col, toupper(nom_col)), silent=TRUE)
#   }
# }
# 
# # Derivated
# path_d <- paste(sous_repo_data,"/d",num_table,".csv", sep = "")
# data_derivated <- read_csv(path_d[num_vague], 
#                            locale = locale(encoding ="UTF-8"),
#                            show_col_types = FALSE)
# data_derivated <- as.data.table(data_derivated)
# 
# if(num_vague == 4){ ### On doit passer toutes les colonnes en majuscules dans ce cas
#   for(nom_col in colnames(data_derivated)){
#     try(setnames(data_derivated, nom_col, toupper(nom_col)), silent=TRUE)
#   }
# }
# # data_derivated
# # data_complete
# # 
# # colnames(data_derivated)
# 
# # Household
# path_h <- paste(sous_repo_data,"/h",num_table,".csv", sep = "")
# data_house <- read_csv(path_h[num_vague], 
#                        locale = locale(encoding ="UTF-8"),
#                        show_col_types = FALSE)
# data_house <- as.data.table(data_house)
# for(nom_col in liste_cols_core_household){
#   try(setnames(data_house, nom_col, toupper(nom_col)), silent=TRUE)
# }
# liste_cols_core_household_new <- toupper(liste_cols_core_household)
# data_house <- data_house[,..liste_cols_core_household_new]
# 
# # Merge et concat
# data_complete_loc <- merge(data_derivated, data_house, by= c("ID", "SA0100"))
# 
# # colnames(data_complete_loc)
# # colnames(data_complete)
# 
# data_complete_loc[, Vague := num_vague]
# 
# 
# data_complete_2 <- rbindlist(list(data_complete,
#                                 data_complete_loc), fill=TRUE)
# 
# length(colnames(data_complete_2))
