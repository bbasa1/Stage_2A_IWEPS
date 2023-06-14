################################################################################
# ============================ 02 IMPORTATION ==================================
################################################################################

# Structure du script : 
#   - Initialisation de la boucle sur la première vague
#   - Boucle jusqu'à arriver à la dernière vague voulue num_vague_max
#   
# ATTENTION : On fusionne code housold uniquement sur les variables années et pays, sinon on ne garde que derivated variables
# ATTENTION : Les noms de colonnes sont globalement passées en majuscule à cause de la dernière vague d'enquête qui met tout en majuscule

# liste_cols_core_household <- c("sa0010", # household identification number
#                                "sa0100", # country
#                                "sa0200", # survey vintage
#                                "id",
#                                "sa0210", # Vintage of last interview (household)
#                                "SA0110", # Past household ID
#                                "hb0700", # year of property acquisition
#                                "HH0201", # year gift/inheritance received
#                                "HH0202",
#                                "FHH0203"
#                                ) ### A mettre en minuscule


# Importation des données
# h = core household files ==> On en a besoin pour connaître l'année
# p = core personal files
# hn = non-core household files
# pn = non-core personal files
# d = derivated variables files ==> Celui qu'on va principalement utiliser


importation_toutes_vagues <- function(num_table_loc){
  ###### On prépare la boucle avec la première importation
  num_vague <- 1
  path_d <- paste(sous_repo_data,"/d",num_table_loc,".csv", sep = "")
  data_derivated <- read_csv(path_d[num_vague], 
                             locale = locale(encoding ="UTF-8"),
                             show_col_types = FALSE)
  
  data_derivated <- as.data.table(data_derivated)
  nrow(data_derivated)
  try(setnames(data_derivated, "id", "ID"), silent=TRUE) #Parfois les colonnes ont des noms en minuscule...
  try(setnames(data_derivated, "survey", "SURVEY"), silent=TRUE) # Cette variable peut avoir deux noms 
  try(setnames(data_derivated, "Survey", "SURVEY"), silent=TRUE) # Cette variable peut avoir deux noms
  
  
  
  ###### Pour avoir le pays et l'année de référence il faut le household, mais on n'a pas besoin du reste
  path_h <- paste(sous_repo_data,"/h",num_table_loc,".csv", sep = "")
  data_house <- read_csv(path_h[num_vague], 
                         locale = locale(encoding ="UTF-8"),
                         show_col_types = FALSE)
  
  data_house <- as.data.table(data_house)
  
  liste_cols_core_household <- colnames(data_house)
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
  nrow(data_house)
  
  
  data_complete <- merge(data_derivated, data_house, by= c("ID", "SA0100", "SA0010", "HW0010"))
  data_complete[, VAGUE := 1]
  intersection_colnames <- intersect(liste_var_interet, colnames(data_complete))
  data_complete <- data_complete[,..intersection_colnames]
  
  print("=========================================")
  print(paste("Vague numéro", num_vague))
  print(paste("Validation par le nombre de ligne =", (nrow(data_complete) == nrow(data_derivated))&(nrow(data_complete) == nrow(data_house))))
  print(paste("Nombre de ligne tot =", nrow(data_complete)))
  print(paste("Nombre de colonnes tot =", length(colnames(data_complete))))
  print("=========================================")
  
  
  for(num_vague in 2:num_vague_max){
  
    # Derivated
    path_d <- paste(sous_repo_data,"/d",num_table_loc,".csv", sep = "")
    data_derivated <- read_csv(path_d[num_vague], 
                               locale = locale(encoding ="UTF-8"),
                               show_col_types = FALSE)
    data_derivated <- as.data.table(data_derivated)
    
    ### On passe toutes les colonnes en majuscule...
    for(nom_col in colnames(data_derivated)){
      try(setnames(data_derivated, nom_col, toupper(nom_col)), silent=TRUE)
    }
    for(nom_col in colnames(data_complete)){
      try(setnames(data_complete, nom_col, toupper(nom_col)), silent=TRUE)
    }
    
    # Household
    path_h <- paste(sous_repo_data,"/h",num_table_loc,".csv", sep = "")
    data_house <- read_csv(path_h[num_vague], 
                           locale = locale(encoding ="UTF-8"),
                           show_col_types = FALSE)
    data_house <- as.data.table(data_house)
    liste_cols_core_household <- colnames(data_house)
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
    data_complete_loc <- merge(data_derivated, data_house, by= c("ID", "SA0100", "SA0010", "HW0010"))
    
    # colnames(data_complete_loc)
    # colnames(data_complete)
  
    data_complete_loc[, VAGUE := num_vague]
    intersection_colnames <- intersect(liste_var_interet, colnames(data_complete_loc))
    data_complete_loc <- data_complete_loc[,..intersection_colnames]
    
    
    data_complete <- rbindlist(list(data_complete,
                                    data_complete_loc), fill=TRUE)
    
    print("=========================================")
    print(paste("Vague numéro", num_vague))
    print(paste("Validation par le nombre de ligne =", (nrow(data_complete_loc) == nrow(data_derivated))&(nrow(data_complete_loc) == nrow(data_house))))
    print(paste("Nombre de ligne tot =", nrow(data_complete)))
    print(paste("Nombre de colonnes tot =", length(colnames(data_complete))))
    print("=========================================")
    
  }

  return(data_complete)
}



importation_une_vagues <- function(num_vague_loc){
  ### N'importe qu'une seule vague de l'enquête, mais importe également les poids associés
  ### Retourne une svyimputationList
  path_H <- paste(sous_repo_data[num_vague_loc],"/h",1:5,".csv", sep = "")
  path_D <- paste(sous_repo_data[num_vague_loc],"/d",1:5,".csv", sep = "")
  list_tab = c("H","D")
  
  
  for (i in list_tab){
    for (k in 1:5){
      txt1 <- paste("Table_",i, k,"<-", sep ="")
      txt2 <- paste("read_csv(path_",i,"[",k,"], locale = locale(encoding ='UTF-8'), show_col_types = FALSE)", sep = "")
      txt <- paste(txt1, txt2)
      eval(parse(text = txt))
    }
  }
  
  for (k in 1:5){
    txt1 <- paste("imp",k,"<-", sep ="")
    txt2 <- paste("merge(Table_H",k,", Table_D",k,", by = c('SA0010','SA0100', 'IM0100', 'HW0010'))", sep = "")
    txt <- paste(txt1, txt2)
    eval(parse(text = txt))
    
    
    col_intersection <- intersect(liste_var_interet, colnames(paste("imp",k, sep ="")))
    txt2 <- paste(paste("imp",k,"[,..col_intersection]", sep =""))
    
  }
  
  path_W <- paste(sous_repo_data[num_vague_loc],"/w",".csv", sep = "")
  W_complete <- read_csv(path_W,
                         locale = locale(encoding ="UTF-8"),
                         show_col_types = FALSE)
  W_complete <- as.data.table(W_complete)
  
  
  
  liste_repweg <- names(W_complete)[names(W_complete) %like% "wr"]
  repweg <- W_complete[,..liste_repweg]
  
  f_dowle2 <- function(DT){
    for (i in names(DT))
      DT[is.na(get(i)), (i):=0]
    return(DT)
  }
  
  repweg <- f_dowle2(repweg)
  
  
  hfcs.design = svrepdesign(repweights=repweg, weights= ~HW0010,
                            data=imputationList(list(imp1,imp2,imp3,imp4,imp5)),
                            scale=1,rscale=rep(1/999,1000),mse=FALSE,type='other',
                            combined.weights=TRUE)
  
  return(hfcs.design)
}
  