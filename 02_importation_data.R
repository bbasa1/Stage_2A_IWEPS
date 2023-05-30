liste_cols_core_household <- c("sa0010", # household identification number
                               "sa0100", # country
                               "sa0200", # survey vintage
                               "id"
                               ) ### A mettre en minuscule

num_table <- 1
num_vague_max <- 4

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
data_house <- data_house[,..liste_cols_core_household_new]
nrow(data_house)


data_complete <- merge(data_derivated, data_house, by= "ID") 
nrow(data_complete)


print(paste("Validation par le nombre de ligne =", (nrow(data_complete) == nrow(data_derivated))&(nrow(data_complete) == nrow(data_house))))
print(paste("Nombre de ligne tot =", nrow(data_complete)))
print(paste("Nombre de colonnes tot =", length(colnames(data_complete))))
print("=================")


for(num_vague in c(2,num_vague_max)){
  # Derivated
  path_d <- paste(sous_repo_data,"/d",num_table,".csv", sep = "")
  data_derivated <- read_csv(path_d[num_vague], 
                             locale = locale(encoding ="UTF-8"),
                             show_col_types = FALSE)
  data_derivated <- as.data.table(data_derivated)
  try(setnames(data_derivated, "id", "ID"), silent=TRUE) #Parfois les colonnes ont des noms en minuscule...
  
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
  data_house <- data_house[,..liste_cols_core_household_new]
  
  # Merge et concat
  data_complete_loc <- merge(data_derivated, data_house, by= "ID") 
  data_complete <- rbindlist(list(data_complete,
                                  data_complete_loc), fill=TRUE)
  
  print(paste("Validation par le nombre de ligne =", (nrow(data_complete) == nrow(data_derivated))&(nrow(data_complete) == nrow(data_house))))
  print(paste("Nombre de ligne tot =", nrow(data_complete)))
  print(paste("Nombre de colonnes tot =", length(colnames(data_complete))))
  print("=================")
}


# path_d <- paste(sous_repo_data,"/d",num_table,".csv", sep = "")
# data_derivated <- read_csv(path_d[num_vague], 
#                            locale = locale(encoding ="UTF-8"),
#                            show_col_types = FALSE)
# 
# data_derivated <- as.data.table(data_derivated)
# data_derivated
# nrow(data_derivated)
# try(setnames(data_derivated, "id", "ID"), silent=TRUE) #Parfois les colonnes ont des noms en minuscule...
# 
# 
# 
# 
# ###### Pour avoir le pays et l'année de référence il faut le household, mais on n'a pas besoin du reste
# path_h <- paste(sous_repo_data,"/h",num_table,".csv", sep = "")
# data_house <- read_csv(path_h[num_vague], 
#                        locale = locale(encoding ="UTF-8"),
#                        show_col_types = FALSE)
# 
# data_house <- as.data.table(data_house)
# for(nom_col in liste_cols_core_household){
#   print(nom_col)
#   try(setnames(data_house, nom_col, toupper(nom_col)), silent=TRUE)
# }
# liste_cols_core_household_new <- toupper(liste_cols_core_household)
# data_house <- data_house[,..liste_cols_core_household_new]
# nrow(data_house)
# 
# 
# 
# data_complete_loc <- merge(data_derivated, data_house, by= "ID") 
# data_complete <- rbindlist(list(data_complete,
#                               data_complete_loc), fill=TRUE)
# 
# 
# 
# 
# length(colnames(data_complete_2))
# length(colnames(data_complete))
# length(colnames(data_complete_loc))
# setdiff(colnames(data_complete_loc),colnames(data_complete))
# 
# data_complete_loc$DOABLETOSAVE
# 
# colnames(data_complete)
