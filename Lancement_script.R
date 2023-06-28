pays <- "IT"
num_vague <- 2 # Pour les graphiques

montant_heritage_min <- 10000 # Le montant d'héritage au delà duquel on considère l'héritage reçu comme conséquant. Pour la partie économétrie
faire_tourner_recherche_pvalue_opti <- FALSE
nb_points_recherche <- 100
liste_montant_initial <- lseq(100, 500000, nb_points_recherche)
source(paste(repo_prgm , "00_main.R" , sep = "/"))


# Le dossier général
repgen <- "C:/Users/Benjamin/Desktop/IWEPS"
# Les sous-dossiers
repo_prgm <- paste(repgen, "Stage_2A_IWEPS" , sep = "/")
repo_sorties <- paste(repgen, "Sorties" , sep = "/")
repo_data <- paste(repgen, "Data" , sep = "/")
repo_inter <- paste(repgen, "Bases_intermediaires" , sep = "/")


# for(pays in c("DE", "BE", "FR", "IT")){

for(pays in c("DE","FR", "BE", "IT", "PT", "ES")){
  for(num_vague in 2:2){
    print(paste("Pour le ", pays, " & vague ", num_vague, " : ", sep =))
    try(source(paste(repo_prgm , "00_main.R" , sep = "/")))
  }}

for(pays in c("DE","FR", "BE", "IT", "PT", "ES")){
for(num_vague in 1:4){
    try(source(paste(repo_prgm , "00_main.R" , sep = "/")))
  }
}
