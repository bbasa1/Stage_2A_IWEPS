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



liste_var_continues <- c("HH0401", "HH0402", "HH0403", "HH0201", "HH0202", "HH0203", "HB0700","HW0010", "DA1000", "DA2100", "DA3001", "DI2000", "DI2100", "DL1000", "DN3001", "DNFPOS", "DNHW", "DOGIFTINHER", "DA1120")
liste_var_categorielles <- c("IM0100","SA0100","DHHTYPE","DOINHERIT", "DA1110I","SA0110","SA0210", "SA0010" ,"DATOP10", "DHAGEH1", "DHEDUH1", "DHGENDERH1", "DHLIFESATIS", "DITOP10", "DLTOP10", "DNTOP10", "DOEINHERIT", "VAGUE", "SA0200", "DHAGEH1B")
liste_var_interet <- union(liste_var_continues, liste_var_categorielles)



source(paste(repo_prgm , "01_packages.R" , sep = "/"))



# Les sous-sous-dossiers qui contiennent les données
liste_sous_fichiers_data <- c("HFCS_UDB_1_5_ASCII", "HFCS_UDB_2_4_ASCII", "HFCS_UDB_3_2_ASCII", "HFCS_UDB_4_0_early_diss_ASCII")
sous_repo_data <- paste(repo_data, liste_sous_fichiers_data, sep = "/")


num_vague <- 1

path_H <- paste(sous_repo_data[num_vague],"/h",1:5,".csv", sep = "")
path_D <- paste(sous_repo_data[num_vague],"/d",1:5,".csv", sep = "")
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

path_W <- paste(sous_repo_data[num_vague],"/w",".csv", sep = "")
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


#depending on the variables included in your five implicates you might want to compute also the following:
MIcombine(with(hfcs.design,svytotal(~DA3001)))
MIcombine(with(hfcs.design,svyratio(~DA3001,~DH0001)))





hfcs.design




sum(is.na(repweg))

sum(is.na(repweg))/(nrow(repweg)*length(repweg))

head(W_complete,10)



# for (i in 1:length(list_tab)){
#   for (k in 1:5){
#     txt = paste("load('",list_tab[i],k,".RData')",sep="")
#     # print(txt)
#     message(paste("Loading of table ",list_tab[i],", implicat ",k,sep=""))
#     eval(parse(text=txt))
#   }
# }
# for (j in 1:5){
#   txt = paste("tab=H",j,"[,c('SA0010','SA0100','HW0010','HB0100', 'IM0100' )] \n rm(H",j,") \n imp",j,"=merge(P",j,",tab,by=c('SA0010','SA0100', 'IM0100')) \n rm(P",j,")",sep="")
#   message(paste("Merging household and personal information",j,sep=""))
#   eval(parse(text=txt))
# }


data_house <- read_csv(path_h[num_vague], 
                       locale = locale(encoding ="UTF-8"),
                       show_col_types = FALSE)



list_tab = c("H")
for (i in 1:length(list_tab)){
  for (k in 1:5){
    txt = paste("load('",list_tab[i],k,".RData')",sep="")
    # print(txt)
    message(paste("Loading of table ",list_tab[i],", implicat ",k,sep=""))
    eval(parse(text=txt))
  }
}
H.complete<-










liste_var_continues <- c("HH0401", "HH0402", "HH0403", "HH0201", "HH0202", "HH0203", "HB0700","HW0010", "DA1000", "DA2100", "DA3001", "DI2000", "DI2100", "DL1000", "DN3001", "DNFPOS", "DNHW", "DOGIFTINHER", "DA1120")
liste_var_categorielles <- c("IM0100","SA0100","DHHTYPE","DOINHERIT", "DA1110I","SA0110","SA0210", "SA0010" ,"DATOP10", "DHAGEH1", "DHEDUH1", "DHGENDERH1", "DHLIFESATIS", "DITOP10", "DLTOP10", "DNTOP10", "DOEINHERIT", "VAGUE", "SA0200", "DHAGEH1B")
liste_var_interet <- union(liste_var_continues, liste_var_categorielles)


num_vague<- 1

path_d <- paste(sous_repo_data,"/d",num_table,".csv", sep = "")
data_derivated <- read_csv(path_d[s], 
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


data_complete <- merge(data_derivated, data_house, by= c("ID", "SA0100", "SA0010", "HW0010"))
data_complete[, VAGUE := num_vague]
intersection_colnames <- intersect(liste_var_interet, colnames(data_complete))
data_complete <- data_complete[,..intersection_colnames]








num_table <- 5 ### Change les poids assignés par eurostat



data_pays <- data_complete[SA0100 == pays]
nom_pays <- dico_pays[pays]
data_pays_5 <- copy(data_pays)



num_vague <- 1
num_table <- 1
path_w <- paste(sous_repo_data,"/w",num_table,".csv", sep = "")
table_W_1 <- read_csv(path_d[num_vague], 
                      locale = locale(encoding ="UTF-8"),
                      show_col_types = FALSE)


txt <- paste("table_W_", num_vague, sep = "")


repo_sorties <- paste(repo_sorties, nom_pays, sep = "/")
if(!file.exists(repo_sorties)){
  dir.create(repo_sorties)
}
