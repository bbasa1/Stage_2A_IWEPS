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


pays <- "FR"
num_vague <- 3 # Pour les graphiques

montant_heritage_min <- 10000 # Le montant d'héritage au delà duquel on considère l'héritage reçu comme conséquant. Pour la partie économétrie

faire_tourner_recherche_pvalue_opti <- FALSE

################################################################################
# ============================ 02 IMPORTATION ==================================
################################################################################
num_vague_max <- 4 ### Le nombre de vague qu'on veut concaténer ATTENTION la dernière vague a des noms de colonnes en MAJUSCULE Ca pose pbm dans la concaténation...

liste_var_continues <- c("HH0401", "HH0402", "HH0403", "HH0201", "HH0202", "HH0203",
                         "HB0700","HW0010", "DA1000", "DA2100", "DA3001", "DI2000", "DI2100",
                         "DL1000", "DN3001", "DNFPOS", "DNHW", "DOGIFTINHER", "DA1120", "DA1110")
liste_var_categorielles <- c("SA0100","DHHTYPE","DOINHERIT", "DA1110I","SA0110","SA0210",
                             "SA0010" ,"DATOP10", "DHAGEH1", "DHEDUH1", "DHGENDERH1",
                             "DITOP10", "DLTOP10", "DNTOP10", "DOEINHERIT", "VAGUE",
                             "SA0200", "DHAGEH1B", "DA1120I", "DHEMPH1",
                             "HH0301A", "HH0301B", "HH0301C", "HH0301D",
                             "HH0301E", "HH0301F", "HH0301H", "HH0301I", "HH0301J",
                             "HH0302A", "HH0302B", "HH0302C", "HH0302D",
                             "HH0302E", "HH0302F", "HH0302H", "HH0302I", "HH0302J",
                             "HH0303A", "HH0303B", "HH0303C", "HH0303D",
                             "HH0303E", "HH0303F", "HH0303H", "HH0303I", "HH0303J")
liste_var_interet <- union(liste_var_continues, liste_var_categorielles)


source(paste(repo_prgm , "02_importation_data.R" , sep = "/"))
data_complete <- importation_toutes_vagues(num_table_loc = 1)
data_pays <- data_complete[SA0100 == pays]
nom_pays <- dico_pays[pays]


repo_sorties_initial <- copy(repo_sorties)
repo_sorties <- paste(repo_sorties, nom_pays, sep = "/")
if(!file.exists(repo_sorties)){
  dir.create(repo_sorties)
}




################################################################################
# ============================  03 NETTOYAGE  ==================================
################################################################################

### On sélectionne un jeu réduit de données, et on force leur typage
source(paste(repo_prgm , "03_nettoyage_preparation.R" , sep = "/"))


############### Variables d'intérêt
# HW0010 = Le poids
# DA1000 = Total real assets (+ ventilations plus précises)
# DA2100 = Total financial assets (+ ventilations plus précises)
# DA3001 = DA1000 + DA2100 = Total assets
# DA1110 = Valeur HMR
# DA1120I = A des autres propriétés
# DATOP10 = Le décile de gross wealth/richesse brute au sein du pays
# DHAGEH1 = Âge de la personne de référence
# DHAGEH1B = Tranche d'âge de ...
# DHEDUH1 = Education de la personne de référence
# DHGENDERH1 = Genre de la personne de référence
# DHLIFESATIS = Life satisfaction
# DI2000 = Revenu total du ménage
# DI2000eq = DI2000/Nb personnes du ménage
# DI2100 + DI200 + Assistance financière famille et amis
# DITOP10 = Décile de revenu du ménage au sein du pays
# DL1000 = Total outstanding balance of household's liabilities = Le passif total (+ ventilations plus précises, également sur les DL1200)
# DLTOP10 = Décile des passifs du ménage
# DN3001 = Net wealth = DA3001 - DL1000
# DNFPOS = Net financial wealth = DA2100 - DL1000
# DNHW = Net housing wealth = DA1110 - DL1110
# DNTOP10 = Décile du net wealth du ménage
# DOEINHERIT = S'attend à recevoir un héritage dans le futur
# DOGIFTINHER = Montant des cadeaux et héritages reçus ==> ATTENTION : La résidence principale peut être exclue du montant, car elle est comptée dans la question sur le mode d'aquisition de celle-ci
# DOINHERIT = Substantial inheritance/gift received 
# VAGUE = La vague
# DA1120 = La valeur de la résidence principale
# SA0200 = Survey vintage
# SA0010 = Identifiant du ménage
# SA0210 = Vintage of last interview (household)
# SA0110 = Past household ID
# DA1110i = Fait d'être propriétaire
# HB0700 = year of property acquisition
# HH0201 = year gift/inheritance received
# HH0202
# HH0203
# HH0401 = Value of gift
# HH0402
# HH0403
## Héritage == remplacer x par 1, 2 ou 3...
# HH030xA = what kind of assets received ? Money
# HH030xB = what kind of assets received ? Dwelling
# HH030xC = what kind of assets received ? Use of a dwelling (under reserve or usufruct)
# HH030xD = what kind of assets received ? Land
# HH030xE = what kind of assets received ? Business
# HH030xF = what kind of assets received ? Securities, shares
# HH030xG = what kind of assets received ? Jewellery, furniture, artwork
# HH030xH = what kind of assets received ? Life insurance
# HH030xJ = what kind of assets received ? Car / vehicle I - Other assets (specify)

# DHHTYPE = Household type


intersection <- intersect(liste_var_interet, colnames(data_pays))
if(length(setdiff(liste_var_interet, intersection)) > 0){
  print(paste("Attention les variables : ", setdiff(liste_var_interet, intersection), "ont été sélectionnées mais ne sont pas présentent dans la table initiale", sep = " "))
}
data_pays <- data_pays %>% mutate_at(liste_var_continues, as.numeric)
data_pays <- data_pays %>% mutate_at(liste_var_categorielles, as.factor)

##### On ajoute la variable année d'achat - année d'héritage reçu
data_pays <- Ajout_premier_heritage(data_pays)
# En fait on ne veut pas le premier héritage obtenu mais le premier héritage CONSEQUANT obtenu
data_pays <- Ajout_premier_heritage_cons(data_pays, montant_heritage_min)
data_pays[(!is.na(HB0700) & !is.na(Montant_heritage_1)), Annee_achat_heritage := HB0700 - Annee_heritage_1]


## Petit histogramme pour visualiser
data_pays <- nettoyage_education(data_pays)






######### A-t-on bien des données de panel ?
list_output <- Creation_donnees_panel(data_pays)
vague_12 <- as.data.table(list_output[1])
vague_23 <- as.data.table(list_output[2])
vague_34 <- as.data.table(list_output[3])
vague_123 <- as.data.table(list_output[4])
vague_234 <- as.data.table(list_output[5])
vague_1234 <- as.data.table(list_output[6])


################################################################################
# ====================== 04 STAT DES & GRAPHIQUES ==============================
################################################################################
source(paste(repo_prgm , "04_graphiques.R" , sep = "/"))
liste_chemins <- c()

# Patrimoine net
var_decile <- "DNTOP10"
titre <- paste("Répartition du patrimoine net, normalisé par sexe (", nom_pays, "& vague ",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Patrimoine_net_sexe.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
xlabel <-"Quantile de patrimoine net"
facet <- "DHHTYPE" # On facet par type de ménage
var_normalisation <- c("DHGENDERH1","DHHTYPE") #On va mettre fill et normalisation par sexe
data_loc <- data_pays[!is.na(get(var_decile)) & VAGUE == num_vague]
liste_chemins <- append(liste_chemins, titre_save)
graphique_repartition_pat_quantile_sexe(data_loc, var_decile,var_normalisation, titre, titre_save, xlabel, facet, xlim_sup = 35)

# Patrimoine brut
var_decile <- "DATOP10"
titre <- paste("Répartition du patrimoine brut, normalisé par sexe (", nom_pays, " & vague ",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Patrimoine_brut_sexe.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
xlabel <-"Quantile de patrimoine brut"
facet <- "DHHTYPE"
var_normalisation <- c("DHGENDERH1","DHHTYPE") #On va mettre fill et normalisation par sexe
data_loc <- data_pays[!is.na(get(var_decile)) & VAGUE == num_vague]
liste_chemins <- append(liste_chemins, titre_save)
graphique_repartition_pat_quantile_sexe(data_loc, var_decile,var_normalisation, titre, titre_save, xlabel, facet, xlim_sup = 25)

# Dettes
var_decile <- "DLTOP10"
titre <- paste("Répartition des dettes, normalisé par sexe (", nom_pays, " & vague ",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Patrimoine_dettes_sexe.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
xlabel <-"Quantile de dettes"
facet <- "DHHTYPE"
var_normalisation <- c("DHGENDERH1","DHHTYPE") #On va mettre fill et normalisation par sexe
data_loc <- data_pays[!is.na(get(var_decile)) & VAGUE == num_vague]
liste_chemins <- append(liste_chemins, titre_save)
graphique_repartition_pat_quantile_sexe(data_loc, var_decile,var_normalisation, titre, titre_save, xlabel, facet, xlim_sup = 25)




######## On regarde la concentration des différents types de patrimoines #############
nb_quantiles <- 100
liste_type_patrimoines <- c("DA3001" = "Patrimoine brut",
                            "DA1000" = "Patrimoine physique",
                            "DA2100" = "Patrimoine financier",
                            "DL1000" = "Dettes",
                            "DN3001" = "Patrimoine net")

titre_fig <- paste("Fonction de répartition de la richesse détenue par les ménages (", nom_pays, " & vague ",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Concentration_patrimoine_par_type.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
data_loc <- data_pays[VAGUE == num_vague]
liste_chemins <- append(liste_chemins, titre_save)

graphique_contration_patrimoine(data_pays ,nb_quantiles, liste_type_patrimoines, titre_fig, titre_save)



############################## Dispersion du patrimoine en fonction de l'âge
liste_type_patrimoines <- c("DA3001" = "Patrimoine brut",
                            "DA1000" = "Patrimoine physique",
                            "DA2100" = "Patrimoine financier",
                            "DL1000" = "Dettes",
                            "DN3001" = "Patrimoine net")

data_loc <- data_pays[VAGUE == num_vague]
titre <- paste("Variance du patrimoine détenu par les ménages\nIntervalles de confiance à 95% (", nom_pays, " & vague ",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_variance_patrimoine.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
liste_chemins <- append(liste_chemins, titre_save)

graphique_variance_pat_age(data_loc, liste_type_patrimoines, titre, titre_save)




######################### Tracé : le date d'achat de la HMR - la date de l'héritage
titre <- paste("Distribution de la variable :\ndate d'aquisition de la résidence principale actuelle - date du premier don ou héritage reçu\n(", nom_pays, "& vague ",num_vague,")", sep = "")
xlabel <- "Année (coupé à 50) "
ylabel <- "Nombre d'occurence"
filllabel <- "Niveau d'éducation\nde la personne de\nréférence du ménage"
titre_save <-  paste(pays,"_V",num_vague,"_Distrib_diff_annees_heritage_achat_detaille.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
x <- "Annee_achat_heritage"
fill <- "label_education"
liste_breaks_fill <- c('< Brevet', 'Brevet', 'Bac', '> Bac')
data_loc <- data_pays[VAGUE == num_vague]
liste_breaks_x <- seq(-50, 50, 2)
limits_x <- c(-50,50)
liste_chemins <- append(liste_chemins, titre_save)

trace_distrib_variable(data_loc, x, fill, xlabel, ylabel,filllabel, titre, titre_save, liste_breaks_fill, liste_breaks_x, limits_x)


titre <- paste("Distribution de la variable :\ndate d'aquisition de la résidence principale actuelle - date du premier don ou héritage reçu\n (", nom_pays, " & vague ",num_vague,")", sep = "")
xlabel <- "Année (coupé à 50) "
ylabel <- "Nombre d'occurence"
filllabel <- NaN
titre_save <- paste(pays,"_V",num_vague,"_Distrib_diff_annees_heritage_achat_non_detaille.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
x <- "Annee_achat_heritage"
fill <- NaN
liste_breaks_fill <- NaN
data_loc <- data_pays[VAGUE == num_vague]
liste_breaks_x <- seq(-50, 50, 2)
limits_x <- c(-50,50)
liste_chemins <- append(liste_chemins, titre_save)

trace_distrib_variable(data_loc, x, NaN, xlabel, ylabel,NaN, titre, titre_save, liste_breaks_fill, liste_breaks_x, limits_x)







####### Positions dans la distribution du patrimoine
liste_type_patrimoines <- c("DA3001" = "Patrimoine brut",
                            "DA1000" = "Patrimoine physique",
                            "DA2100" = "Patrimoine financier",
                            "DL1000" = "Dettes",
                            "DN3001" = "Patrimoine net",
                            "DI2000" = "Revenu net du ménage")
nb_quantiles <- 100

titre_fig <- paste("Evolution des rangs d'appartenance des ménages entre les différentes vagues (", nom_pays, ")", sep = "")
titre_save <- paste(pays,"_evolution_rang_appartenance.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
faire_rang = TRUE

if(nrow(vague_123) > 0){
  liste_chemins <- append(liste_chemins, titre_save)
  graphique_evolution_position_vagues(vague_123 ,nb_quantiles, liste_type_patrimoines, titre_fig, titre_save, faire_rang, "123")
}else if(nrow(vague_23) > 0){
  liste_chemins <- append(liste_chemins, titre_save)
  graphique_evolution_position_vagues(vague_23 ,nb_quantiles, liste_type_patrimoines, titre_fig, titre_save, faire_rang, "23")
}

titre_fig <- paste("Evolution des patrimoines des ménages entre les différentes vagues (", nom_pays, ")", sep = "")
titre_save <- paste(pays,"_evolution_pat_appartenance.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
faire_rang = FALSE

if(nrow(vague_123) > 0){
  liste_chemins <- append(liste_chemins, titre_save)
  graphique_evolution_position_vagues(vague_123 ,nb_quantiles, liste_type_patrimoines, titre_fig, titre_save, faire_rang, "123")
}else if(nrow(vague_23) > 0){
  liste_chemins <- append(liste_chemins, titre_save)
  graphique_evolution_position_vagues(vague_23 ,nb_quantiles, liste_type_patrimoines, titre_fig, titre_save, faire_rang, "23")
}

######### Evolution du patrimoine des ménages entre les vagues ##############
liste_type_patrimoines <- c("DA3001" = "Patrimoine brut",
                            "DA1000" = "Patrimoine physique",
                            "DA2100" = "Patrimoine financier",
                            "DL1000" = "Dettes",
                            "DN3001" = "Patrimoine net")

liste_quantiles <- seq(0.2, 0.8, 0.2)
titre <- paste("Quantiles de l'évolution du patrimoine des ménages entre les vagues (", nom_pays, ") \n", sep = "")
titre_save <- paste(pays,"_quantiles_evolution_richesse_panel.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')

if(nrow(vague_123) > 0){
  liste_chemins <- append(liste_chemins, titre_save)
  graphique_evolution_pat_entre_vagues(vague_123, liste_type_patrimoines,liste_quantiles, titre, titre_save)
}

################################################################################
# ========================= 05 ECONOMETRIE =====================================
################################################################################


############################################################################################################################### 
########################## DiD ESSAI = EFFET DU FAIT D'AVOIR RECU UN HERITAGE SUR LE FAIT D'ACHETER UNE HMR ###################
########################## G = 1   <====> Reçu un héritage à la vague 3 MAIS PAS à la vague 2  ################################
pop_initiale_tot <- copy(data_pays[VAGUE %in% c(2,3),])
nrow(pop_initiale_tot)
SA0110_V3 <- vague_23$SA0110_V3 ## On récupère les identifiants ménages qui sont présents dans les deux vagues = ceux qui ont un identifiant sur la vague passée (la vague 2 donc)
pop_initiale_tot <- pop_initiale_tot[(SA0110 %in% SA0110_V3 & VAGUE == 3) | (SA0010 %in% SA0110_V3 & VAGUE == 2)] # Tout ceux qui ont l'identifiant passé sur la vague 3 et l'identifiant présent sur la vague 2
table(pop_initiale_tot$VAGUE)

pop_initiale_tot[, Reg_Y := 0]
pop_initiale_tot[(DA1110I == 1 & VAGUE == 3) | (DA1110I == 1 & VAGUE == 2), Reg_Y := 1] ## La population qui ont une HMR
pop_initiale_tot$Reg_Y <- as.numeric(pop_initiale_tot$Reg_Y)
table(pop_initiale_tot$Reg_Y)

pop_initiale_tot[, Reg_G := 0]
SA0110_V3 <- vague_23[DOINHERIT_V3 == 1 & DOINHERIT_V2 == 0]$SA0110_V3 ## On récupère les identifiants ménages de ceux qui ont reçu un héritage entre la vague 2 et la vague 3
pop_initiale_tot[(SA0110 %in% SA0110_V3 & VAGUE == 3) | (SA0010 %in% SA0110_V3 & VAGUE == 2), Reg_G := 1]
pop_initiale_tot$Reg_G <- as.numeric(pop_initiale_tot$Reg_G)
table(pop_initiale_tot$Reg_G)


pop_initiale_tot[, Reg_T := 0]
pop_initiale_tot[VAGUE == 3, Reg_T := 1] ## La date
pop_initiale_tot$Reg_T <- as.numeric(pop_initiale_tot$Reg_T)
table(pop_initiale_tot$Reg_T)

pop_initiale_tot[, Reg_D := Reg_G * Reg_T]

liste_cols_reg <- c("Reg_Y", "Reg_G", "Reg_T", "Reg_D")
liste_cols_reg_poids <- c("Reg_Y", "Reg_G", "Reg_T", "Reg_D", "HW0010")


pop_initiale_tot$Groupe <- paste("G",pop_initiale_tot$Reg_G, "_T",pop_initiale_tot$Reg_T, "_T",pop_initiale_tot$Reg_Y, sep = "")

comptes <- pop_initiale_tot[, .N, by = Groupe]
n <- min(comptes$N)
sous_pop_initiale <- as.data.table(pop_initiale_tot %>% group_by(Groupe) %>% slice_sample(n=n))
dw <- svydesign(ids = ~1, data = sous_pop_initiale[,..liste_cols_reg_poids], weights = ~ HW0010)
mysvyglm <- svyglm(formula = Reg_Y ~ Reg_G + Reg_D + Reg_T, design = dw)
summary(mysvyglm)


titre <- paste(pays,"_DD_2_heritage_achat.xlsx", sep = "")
titre <- paste(repo_sorties,titre, sep = "/")
write.xlsx(as.data.table(summary(mysvyglm)$coefficients, keep.rownames = TRUE), titre)


## Test de la common trend asumption sur les vagues 1 et 2
pop_test_hyp <- copy(data_pays[VAGUE %in% c(1,2),]) 
nrow(pop_test_hyp)
SA0110_V2 <- vague_12$SA0110_V2 ## On récupère les identifiants ménages qui sont présents dans les deux vagues
pop_test_hyp <- pop_test_hyp[SA0110 %in% SA0110_V2 & VAGUE == 2]
nrow(pop_test_hyp)


pop_test_hyp[, Reg_Y := 0]
pop_test_hyp[(DA1110I == 1 & VAGUE == 2) | (DA1110I == 1 & VAGUE == 1), Reg_Y := 1] ## La population qui ont une HMR
pop_test_hyp$Reg_Y <- as.numeric(pop_test_hyp$Reg_Y)
count(pop_test_hyp[ Reg_Y == 1])

pop_test_hyp[, Reg_G := 0]
SA0110_V2 <- vague_12[DOINHERIT_V2 == 1 & DOINHERIT_V1 == 0]$SA0110_V2 ## On récupère les identifiants ménages de ceux qui ont reçu un héritage entre la vague 2 et la vague 3
# vague_23 <- merge(x = vague_3, y = vague_2, by.x = 'SA0110_V3',by.y = 'SA0010_V2')
pop_test_hyp[(SA0110 %in% SA0110_V2 & VAGUE == 2) | (SA0010 %in% SA0110_V2 & VAGUE == 1), Reg_G := 1]
pop_test_hyp$Reg_G <- as.numeric(pop_test_hyp$Reg_G)
count(pop_test_hyp[ Reg_G == 1])

pop_test_hyp[, Reg_T := -1]
pop_test_hyp[VAGUE == 2, Reg_T := 0] ## La date
pop_test_hyp$Reg_T <- as.numeric(pop_test_hyp$Reg_T)

dw <- svydesign(ids = ~1, data = pop_test_hyp, weights = ~ HW0010)


sous_dw <- subset(dw, Reg_G == 1 & Reg_T == 0) # Un sous-échantillon
svymean(~Reg_Y, subset(dw, Reg_G == 1 & Reg_T == 0))[1]


svymean(~Reg_Y, subset(dw, Reg_G == 1 & Reg_T == 0))[1] - svymean(~Reg_Y, subset(dw, Reg_G == 1 & Reg_T == -1))[1]
svymean(~Reg_Y, subset(dw, Reg_G == 0 & Reg_T == 0))[1] - svymean(~Reg_Y, subset(dw, Reg_G == 0 & Reg_T == -1))[1]


table(pop_initiale_tot$Reg_D)





########################## DiD ESSAI EXPLORATION D'AUTRES VARIABLES ###################
########################## G = 1   <====> Reçu un héritage à la vague 3 MAIS PAS à la vague 2  ################################
var_Y_discrete <- FALSE
# DA1120

pop_initiale_tot <- copy(data_pays[VAGUE %in% c(2,3) & DA1120I == 1,])
SA0110_V3 <- vague_23$SA0110_V3 ## On récupère les identifiants ménages qui sont présents dans les deux vagues = ceux qui ont un identifiant sur la vague passée (la vague 2 donc)
pop_initiale_tot <- pop_initiale_tot[(SA0110 %in% SA0110_V3 & VAGUE == 3) | (SA0010 %in% SA0110_V3 & VAGUE == 2)] # Tout ceux qui ont l'identifiant passé sur la vague 3 et l'identifiant présent sur la vague 2

pop_initiale_tot[, Reg_Y := 0]
pop_initiale_tot[(DA1120I == 1 & VAGUE == 3) | (DA1120I == 1 & VAGUE == 2), Reg_Y := DA1120] ## La population qui ont une RES SECONDAIRE
pop_initiale_tot$Reg_Y <- as.numeric(pop_initiale_tot$Reg_Y)
hist(pop_initiale_tot$Reg_Y)
pop_initiale_tot[, Reg_G := 0]
SA0110_V3 <- vague_23[DOINHERIT_V3 == 1 & DOINHERIT_V2 == 0]$SA0110_V3 ## On récupère les identifiants ménages de ceux qui ont reçu un héritage entre la vague 2 et la vague 3
pop_initiale_tot[(SA0110 %in% SA0110_V3 & VAGUE == 3) | (SA0010 %in% SA0110_V3 & VAGUE == 2), Reg_G := 1]
pop_initiale_tot$Reg_G <- as.numeric(pop_initiale_tot$Reg_G)
pop_initiale_tot[, Reg_T := 0]
pop_initiale_tot[VAGUE == 3, Reg_T := 1] ## La date
pop_initiale_tot$Reg_T <- as.numeric(pop_initiale_tot$Reg_T)
table(pop_initiale_tot$Reg_T)
pop_initiale_tot[, Reg_D := Reg_G * Reg_T]
liste_cols_reg <- c("Reg_Y", "Reg_G", "Reg_T", "Reg_D")
liste_cols_reg_poids <- c("Reg_Y", "Reg_G", "Reg_T", "Reg_D", "HW0010")

if(var_Y_discrete){
  pop_initiale_tot$Groupe <- paste("G",pop_initiale_tot$Reg_G, "_T",pop_initiale_tot$Reg_T, "_T",pop_initiale_tot$Reg_Y, sep = "")
  comptes <- pop_initiale_tot[, .N, by = Groupe]
  n <- min(comptes$N)
  sous_pop_initiale <- as.data.table(pop_initiale_tot %>% group_by(Groupe) %>% slice_sample(n=n))
}else{
  sous_pop_initiale <- copy(pop_initiale_tot)
}

dw <- svydesign(ids = ~1, data = sous_pop_initiale[,..liste_cols_reg_poids], weights = ~ HW0010)
mysvyglm <- svyglm(formula = Reg_Y ~ Reg_G + Reg_D + Reg_T, design = dw)
summary(mysvyglm)





############################################################################################################################### 
########################## AVEC LA DATE D'ACHAT - LA DATE D'HERITAGE  ######################################################### 
######################### On passe tout ça sous forme de fonction pour voir facilement la dépendance avec le montant minimal d'héritage ################
source(paste(repo_prgm , "05_econometrie.R" , sep = "/"))
annee_min = -1
annee_max = 3


data_pays[(is.na(HB0700) & !is.na(Montant_heritage_1)), Annee_achat_heritage := - 99] # Pas d'achat mais un héritage
data_pays[(!is.na(HB0700) & is.na(Montant_heritage_1)), Annee_achat_heritage :=  99] # Achat mais pas d'héritage
data_pays[(!is.na(HB0700) & !is.na(Montant_heritage_1)), Annee_achat_heritage := HB0700 - Annee_heritage_1]


count(data_pays[Annee_achat_heritage %in% -98:98])
count(data_pays[Annee_achat_heritage == 99])
count(data_pays[Annee_achat_heritage == -99])

if(faire_tourner_recherche_pvalue_opti){
  liste_montant_initial <- lseq(3000, 500000, 250)
  data_loc <- copy(data_pays)[VAGUE == num_vague]
  titre_save <- paste(pays,"_V",num_vague,"_pval_coeff_G_reg_Y_sur_G_heritier.pdf", sep = "")
  titre_save <- paste(repo_sorties, titre_save, sep ='/')
  titre <- paste("Résultats des régressions de Y sur G\nen ne considérant comme population initiale que les héritiers (", nom_pays, " & vague ",num_vague,")", sep = "")
  liste_chemins <- append(liste_chemins, titre_save)
  que_heritiers <- TRUE
  
  recherche_p_value_otpi(liste_montant_initial, data_loc, annee_min = annee_min, annee_max = annee_max, faire_tracer = TRUE, titre, titre_save, que_heritiers)
  
  titre_save <- paste(pays,"_V",num_vague,"_pval_coeff_G_reg_Y_sur_G_toute_po.pdf", sep = "")
  titre_save <- paste(repo_sorties, titre_save, sep ='/')
  titre <- paste("Résultats des régressions de Y sur G\nen considérant comme population initiale toute la population (", nom_pays, " & vague ",num_vague,")", sep = "")
  liste_chemins <- append(liste_chemins, titre_save)
  que_heritiers <- FALSE
  
  recherche_p_value_otpi(liste_montant_initial, data_loc, annee_min = annee_min, annee_max = annee_max, faire_tracer = TRUE, titre, titre_save, que_heritiers)
}







### Si on a le montant optimal

melted <- recherche_p_value_otpi(c(8000,9000,10000), copy(data_pays), annee_min = annee_min, annee_max = annee_max, faire_tracer = FALSE)
# melted






#### Les odds ratios

sous_data_loc <- data_pays[Annee_achat_heritage <= 98]
montant_ini_loc <- 9000


# On créé les groupes
sous_data_loc[, Reg_Y := 0]
sous_data_loc[Annee_achat_heritage %in% annee_min:annee_max, Reg_Y := 1] # Ont acheté qq années après

sous_data_loc[, Reg_G := 0]
sous_data_loc[Montant_heritage_1 >= montant_ini_loc, Reg_G := 1] # Reçu un héritage conséquant


denylogit <- glm(Reg_Y ~ Reg_G, 
                  family = binomial(link = "logit"), 
                  data = sous_data_loc)

summary(denylogit)

dt_prep_logit <- as.data.table(summary(denylogit)$coefficients, keep.rownames = TRUE)
setnames(dt_prep_logit, "Pr(>|z|)", "pvalue")

G = 1
exp(dt_prep_logit$Estimate[1]*1 + G*dt_prep_logit$Estimate[2])

G=0
exp(dt_prep_logit$Estimate[1]*1 + G*dt_prep_logit$Estimate[2])


exp(dt_prep_logit$Estimate[2]) # ==> exp(beta_{0,j}) = Avoir un héritage conséquant multiplie par exp(beta_{0,j}) la chance d'acheter une HMR dans les trois ans par rapport à recevoir un héritage faible



############################################################################################################################### 
#################################### MATCHING ##################################
# 
# data_pays$DOINHERIT
# 
# liste_cols <- c("DOINHERIT", "DHAGEH1", "DHEDUH1", "DHGENDERH1", "DI2000", "DHHTYPE", "DA3001")
# sous_data_loc <- data_pays[,..liste_cols]
# setnames(sous_data_loc, "DA3001", "outcome")
# setnames(sous_data_loc, "DOINHERIT", "treatment")
# 
# sous_data_loc$DHAGEH1 <- as.numeric(sous_data_loc$DHAGEH1)
# 
# # Sans matching
# no_match <- matchit(treatment ~ DHAGEH1 + DHEDUH1 + DHGENDERH1+ DI2000 + DHHTYPE, data=sous_data_loc, method=NULL, distance = 'glm')
# summary(no_match)
# 
# 
# 
# # Nearest neighbot matching
# nearest_neighbor_match <- matchit(treatment ~ DHAGEH1 + DHEDUH1 + DHGENDERH1+ DI2000 + DHHTYPE, data=sous_data_loc, method="nearest", ratio=1, replace=F, distance = 'glm', caliper=0.2)
# # Summary of nearest neighbor matching results
# summary(nearest_neighbor_match, un = FALSE)
# # Nearest neighbor matching is a type of greedy matching. It matches the nearest control at the moment, and remove the matched control from the rest of the matching.
# # Nearest neighbor matching is fast but sensitive to the order of samples. It is not optimal for minimizing the total distance because the samples that are matched later in the process can only choose from the contorl units that have not been matched.
# 
# 
# 
# 
# # Optimal matching ==> Marche beaucoup mieux mais prend du temps...
# optimal_match <- matchit(treatment ~ DHAGEH1 + DHEDUH1 + DHGENDERH1+ DI2000 + DHHTYPE, data=sous_data_loc, method="optimal", ratio=1, replace=F, distance = 'glm')
# # Summary of optimal matching results
# summary(optimal_match, un = FALSE)
# # Optimal matching is also called optimal pair matching. Different from greedy matching, optimal matching minimizes the total distance across all pairs.
# # When there are not many close matches for the treatment group, optimal matching can be helpful for finding the best matches.
# 
# 
# 
# 
# # Full matching
# full_match <- matchit(treatment ~ DHAGEH1 + DHEDUH1 + DHGENDERH1+ DI2000 + DHHTYPE, data=sous_data_loc, method="full", ratio=1, replace=F, distance = 'glm')
# # Summary of full matching results
# summary(full_match, un = FALSE)
# # It is called full matching because all the test and control units are assigned to a subclass and are utilized in the matching.
# # It is optimal because the weighted average distances between the treated and control units in each subclass are minimized.
# # Full matching outputs weights that are computed based on subclasses. The weights can work similar to propensity score weights and be used to estimate a weighted treatment effect.
# 
# full_match$match.matrix
# 
# plot(full_match, type = "jitter", interactive = FALSE)
# plot(full_match, type = "density", interactive = FALSE,
#      which.xs = ~  DI2000 + DHHTYPE)
# plot(full_match, type = "density", interactive = FALSE,
#      which.xs = ~  DHAGEH1)
# 
# 
# plot(summary(full_match))
# 
# 
# full_match_data <- match.data(full_match)
# nrow(sous_data_loc) - nrow(full_match_data) ### Il ne manque que les unmatched !
# 
# # Quid de l'effet du traitement ?
# fit <- lm(outcome ~ treatment * (DHAGEH1 + DHEDUH1 + DHGENDERH1+ DI2000 + DHHTYPE), data = full_match_data, weights = weights)
# 
# avg_comparisons(fit,
#                 variables = "treatment",
#                 vcov = ~subclass,
#                 newdata = subset(full_match_data, treatment == 1),
#                 wts = "weights")
# 
# 
# 
# # Genetic matching
# generic_match <- matchit(treatment ~ DHAGEH1 + DHEDUH1 + DHGENDERH1+ DI2000 + DHHTYPE, data=sous_data_loc, method="genetic", pop.size=20)
# # Summary of genetic matching
# summary(generic_match, un = FALSE)
# # Genetic matching uses a generic search algorithm to find weights for each covariate to achieve optimal balance. The current matching is with replacement and the balance is evaluated using t-tests and Kolmogorov-Smirnov tests.
# 
# 

############################################################################################################################### 
######################### EXPLORATION : QUI POSSEDE QUOI ? QUI HERITE DE QUOI ? ############################################### 
############################################################################################################################### 

### On fait des stats des des populations proprio, non proprio, héritières, non héritières

# Pour les variables discrètes
liste_variables_loc <-c(
              # "DITOP10" = "Décile de revenu",
              "DLTOP10" = "Décile de dette",
              "DNTOP10" = "Décile de patrimoine net",
              "DOEINHERIT" = "S'attend à hériter",
              "DOINHERIT" = "A hérité",
              "DHEDUH1" = "Niveau d'éducation",
              "DHAGEH1" = "Tranche d'âge",
              "DATOP10" = "Décile de patrimoine brut",
              "DHGENDERH1" = 'Sexe',
              "DA1120I" = "Propriétaire d'autres biens immobiliers",
              "DHEMPH1" = "Statut professionel")


var_diff_loc = "DA1110I"
liste_legendes_loc = c("Non_prop" = "Non propriétaires", "Prop" = "Propriétaires","Total" = "Total")
data_loc <- data_pays
titre <- paste("Distribution de différentes variables socio-économiques \npour les ménages qui sont propriétaires de leur résidence principale, \net ceux qui ne le sont pas (", nom_pays, " & vague ",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Differences_prop_non_prop.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
liste_chemins <- append(liste_chemins, titre_save)
try(trace_distribution_X_non_X(data_loc, liste_variables_loc, titre, titre_save, num_vague, var_diff_loc, liste_legendes_loc))

# Et le revenu qui est continu
data_loc <- data_pays[VAGUE == num_vague]
data_loc <- nettoyage_DA1110I(data_loc)
x <- "DI2000"
fill <- "label_DA1110I"
xlabel <- "Revenu net annuel du ménage"
ylabel <- "Nombre de ménages"
filllabel <- "Ménage propriétaire de\nsa résidence principale"
titre <- paste("Distribution des revenus net annuels du ménage,\npour les ménages propriétaires de leur résidence principale et pour les non propriétaires (", nom_pays, " & vague ",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Differences_prop_non_prop_distrib_revenu.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
liste_chemins <- append(liste_chemins, titre_save)
trace_distrib_simple(data_loc, x, fill, titre, titre_save, xlabel, ylabel, filllabel)





liste_variables_loc <-c(
                        # "DITOP10" = "Décile de revenu",
                        "DLTOP10" = "Décile de dette",
                        "DNTOP10" = "Décile de patrimoine net",
                        "DOEINHERIT" = "S'attend à hériter",
                        # "DOINHERIT" = "A hérité",
                        "DHEDUH1" = "Niveau d'éducation",
                        "DHAGEH1" = "Tranche d'âge",
                        "DATOP10" = "Décile de patrimoine brut",
                        "DHGENDERH1" = 'Sexe',
                        "DA1110I" = "Est propriétaire de sa résidence principale",
                        "DA1120I" = "Propriétaire d'autres biens immobiliers",
                        "DHEMPH1" = "Statut professionel")


var_diff_loc = "DOINHERIT"
liste_legendes_loc = c("Non_prop" = "Non héritier", "Prop" = "Héritier","Total" = "Total")
data_loc <- data_pays
titre <- paste("Distribution de différentes variables socio-économiques \npour les ménages qui ont eu un héritage, \net ceux qui n'en ont pas eu (", nom_pays, " & vague ",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Differences_heritiers_non_heritiers.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
liste_chemins <- append(liste_chemins, titre_save)
try(trace_distribution_X_non_X(data_loc, liste_variables_loc, titre, titre_save, num_vague, var_diff_loc, liste_legendes_loc))

data_loc <- data_pays[VAGUE == num_vague]
data_loc <- nettoyage_DOINHERIT(data_loc)
x <- "DI2000"
fill <- "label_DOINHERIT"
xlabel <- "Revenu net annuel du ménage"
ylabel <- "Nombre de ménages"
filllabel <- "Ménage héritier"
titre <- paste("Distribution des revenus net annuels du ménage,\npour les ménages qui ont eu un héritage, \net ceux qui n'en ont pas eu (", nom_pays, " & vague ",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Differences_heritiers_non_heritiers_distrib_revenu.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
liste_chemins <- append(liste_chemins, titre_save)
trace_distrib_simple(data_loc, x, fill, titre, titre_save, xlabel, ylabel, filllabel)





liste_variables_loc <-c(
                        # "DITOP10" = "Décile de revenu",
                        "DLTOP10" = "Décile de dette",
                        "DNTOP10" = "Décile de patrimoine net",
                        # "DOEINHERIT" = "S'attend à hériter",
                        "DOINHERIT" = "A hérité",
                        "DHEDUH1" = "Niveau d'éducation",
                        "DHAGEH1" = "Tranche d'âge",
                        "DATOP10" = "Décile de patrimoine brut",
                        "DHGENDERH1" = 'Sexe',
                        "DA1110I" = "Est propriétaire de sa résidence principale",
                        "DA1120I" = "Propriétaire d'autres biens immobiliers",
                        "DHEMPH1" = "Statut professionel")



var_diff_loc = "DOEINHERIT"
liste_legendes_loc = c("Non_prop" = "Ne s'attend pas à hériter", "Prop" = "S'attend à hériter","Total" = "Total")
data_loc <- data_pays
titre <- paste("Distribution de différentes variables socio-économiques \npour les ménages qui attendent un héritage, \net ceux qui n'en attendent pas (", nom_pays, " & vague ",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Differences_attendent_her_non_attendent_her.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
liste_chemins <- append(liste_chemins, titre_save)
try(trace_distribution_X_non_X(data_loc, liste_variables_loc, titre, titre_save, num_vague, var_diff_loc, liste_legendes_loc))

# Pour les variables continues
data_loc <- data_pays[VAGUE == num_vague]
data_loc <- nettoyage_DOEINHERIT(data_loc)
x <- "DI2000"
fill <- "label_DOEINHERIT"
xlabel <- "Revenu net annuel du ménage"
ylabel <- "Nombre de ménages"
filllabel <- "Ménage qui s'attend à recevoir\nun héritage ou un don"
titre <- paste("Distribution des revenus net annuels du ménage,\npour les ménages qui attendent un héritage, \net ceux qui n'en attendent pas (", nom_pays, " & vague ",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Differences_attendent_her_non_attendent_her_distrib_revenu.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
if(!all(is.na(data_loc$label_DOEINHERIT))){
  liste_chemins <- append(liste_chemins, titre_save)
  trace_distrib_simple(data_loc, x, fill, titre, titre_save, xlabel, ylabel, filllabel)
}


# Pour les variables discrètes
liste_variables_loc <-c(
                        # "DITOP10" = "Décile de revenu",
                        "DLTOP10" = "Décile de dette",
                        "DNTOP10" = "Décile de patrimoine net",
                        "DOEINHERIT" = "S'attend à hériter",
                        "DOINHERIT" = "A hérité",
                        "DHEDUH1" = "Niveau d'éducation",
                        "DHAGEH1" = "Tranche d'âge",
                        "DATOP10" = "Décile de patrimoine brut",
                        "DHGENDERH1" = 'Sexe',
                        "DA1110I" = "Est propriétaire de sa résidence principale",
                        "DHEMPH1" = "Statut professionel")


var_diff_loc = "DA1120I"
liste_legendes_loc = c("Non_prop" = "Non possédants", "Prop" = "Possédants","Total" = "Total")
data_loc <- data_pays
titre <- paste("Distribution de différentes variables socio-économiques \npour les ménages qui possèdent d'autres biens immobiliers que leur HMR,\net les ménages qui n'en possèdent pas (", nom_pays, " & vague ",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Differences_prop_res_sec_non_prop.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
liste_chemins <- append(liste_chemins, titre_save)
try(trace_distribution_X_non_X(data_loc, liste_variables_loc, titre, titre_save, num_vague, var_diff_loc, liste_legendes_loc))

# Pour les variables continues
data_loc <- data_pays[VAGUE == num_vague]
data_loc <- nettoyage_DA1120I(data_loc)
x <- "DI2000"
fill <- "label_DA1120I"
xlabel <- "Revenu net annuel du ménage"
ylabel <- "Nombre de ménages"
filllabel <- "Ménage propriétaire d'autres biens\nimmobiliers que leur résidence principale"
titre <- paste("Distribution des revenus net annuels du ménage,\npour les ménages qui possèdent d'autres biens immobiliers que leur HMR,\net les ménages qui n'en possèdent pas (", nom_pays, " & vague ",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Differences_prop_res_sec_non_prop_distrib_revenu.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
liste_chemins <- append(liste_chemins, titre_save)
trace_distrib_simple(data_loc, x, fill, titre, titre_save, xlabel, ylabel, filllabel)















# 
# melted[, Variable := factor(
#   fcase(
#     Variable == "DITOP10" , "Décile de revenu",
#     Variable == "DLTOP10" , "Décile de dette",
#     Variable == "DNTOP10" , "Décile de patrimoine net",
#     Variable == "DOEINHERIT" , "S'attend à hériter",
#     Variable == "DOINHERIT" , "A hérité",
#     Variable == "DHAGEH1B" , "Tranche d'âge",
#     Variable == "DATOP10" , "Décile de patrimoine brut",
#     Variable == "DHEDUH1", "Niveau d'éducation",
#     Variable == "DHGENDERH1", 'Sexe'
#   ))]


# melted[, sum(Effectifs), by = c("Variable", "Vague")]






# #################### Avec l'objet d'imputations multiples ##########
# table(data_pays$DA1110I)
# 
# data_vague_2 <- importation_une_vagues(num_vague_loc = 2)
# MIcombine(with(data_vague_2,svytotal(~DA3001)))
# MIcombine(with(data_vague_2,svyratio(~DA3001,~DH0001)))
# 
# 
# with(data_vague_2, table(DA1110I, DOINHERIT))
# 
# 
# model <- with(data_vague_2, glm(DA1110i~ DOINHERIT))
# MIcombine(model)
# 
# 
# 
# MIcombine(with(data_vague_2,svytable(~ DA1110i + DOINHERIT)))
# 
# svytotal(~DA3001, data_vague_2)
# 
# 
# 
# res <- with(subset(data_vague_2, DOINHERIT == 1),
#             svyby(~DA1110I, svymean))
# summary(MIcombine(res))


















################################################################################
######################### FUSION DES PDF GENERES ###############################
################################################################################

titre_save <- paste("00_",pays,"_V",num_vague,"_cahier_graphique.pdf", sep = "")
titre_save <- paste(repo_sorties_initial, titre_save, sep ='/')

pdf_combine(liste_chemins, output = titre_save)



# liste_chemins <- append(liste_chemins, titre_save)


################################################################################
########################### TABLEAUX DE CHIFFRES ###############################
################################################################################

"DA1110I"
"DOINHERIT"
"DOEINHERIT"

colnames(data_pays)


var <- "DOEINHERIT"

locvar <- tableau_binaire("DOEINHERIT", data_pays[VAGUE == num_vague])
locvar

# 
# 
# dw <- svydesign(ids = ~1, data = data_pays[VAGUE == num_vague], weights = ~ HW0010)
# 
# svytable(~ Annee_enquete + Sexe_1H_2F, dw)
# 
# 
# 
# 
# 
# p <- as.ggplot(~ svyhist(~ DI2000, design = dw, breaks = "Sturges"))
# 
# p
# 
# svyboxplot(formula, design, all.outliers=FALSE,...)
# 














# 
# 
# # SA0200 = Survey vintage
# # SA0010 = household identification number
# # SA0210 = Vintage of last interview (household)
# # SA0110 = Past household ID
# 
# data_loc <- copy(data_pays)
# 
# # Pour faire l'évolution il faut commencer par mettre à 0 les patrimoines NaN
# data_loc[is.na(DA1000), DA1000 := 0]
# data_loc[is.na(DA2100), DA2100 := 0]
# data_loc[is.na(DL1000), DL1000 := 0]
# 
# vague_1 <- data_loc[VAGUE == 1,] # On récupère les vagues
# vague_2 <- data_loc[VAGUE == 2,]
# vague_3 <- data_loc[VAGUE == 3,]
# vague_4 <- data_loc[VAGUE == 4,]
# 
# colnames(vague_1) <- paste(colnames(vague_1),"V1",sep="_") # On renome pour ne pas avoir de conflits
# colnames(vague_2) <- paste(colnames(vague_2),"V2",sep="_")
# colnames(vague_3) <- paste(colnames(vague_3),"V3",sep="_")
# colnames(vague_4) <- paste(colnames(vague_4),"V4",sep="_")
# 
# # li <- c("VAGUE", "SA0010", "SA0200", "SA0110")
# # data_loc[,..li]
# # 
# # li1 <- c("VAGUE_V1", "SA0010_V1", "SA0200_V1", "SA0110_V1")
# # vague_1[,..li1]
# # li2 <- c("VAGUE_V2", "SA0010_V2", "SA0200_V2", "SA0110_V2")
# # vague_2[,..li2]
# # 
# # merge(x = vague_1, y = vague_2, by.x = 'SA0010_V1',by.y = 'SA0110_V2')
# 
# vague_12 <- merge(x = vague_2, y = vague_1, by.x = 'SA0110_V2',by.y = 'SA0010_V1')
# vague_123 <- merge(x = vague_3, y = vague_12, by.x = 'SA0110_V3',by.y = 'SA0010_V2') ## On s'arrête là parce qu'aucun ménage n'est enquêté 4x
# vague_1234 <- merge(x = vague_4, y = vague_123, by.x = 'SA0110_V4',by.y = 'SA0010_V3')
# 
# vague_23 <- merge(x = vague_3, y = vague_2, by.x = 'SA0110_V3',by.y = 'SA0010_V2')
# vague_234 <- merge(x = vague_4, y = vague_23, by.x = 'SA0110_V4',by.y = 'SA0010_V3') ## PERSONNE N'EST EN PANEL A LA VAGUE 4 ?????
# 
# vague_34 <- merge(x = vague_4, y = vague_3, by.x = 'SA0110_V4',by.y = 'SA0010_V3')
# 

# 
# pop_initiale_tot <- copy(data_pays[VAGUE %in% c(2,3),])
# nrow(pop_initiale_tot)
# SA0110_V3 <- vague_23$SA0110_V3 ## On récupère les identifiants ménages qui sont présents dans les deux vagues = ceux qui ont un identifiant sur la vague passée (la vague 2 donc)
# pop_initiale_tot <- pop_initiale_tot[(SA0110 %in% SA0110_V3 & VAGUE == 3) | (SA0010 %in% SA0110_V3 & VAGUE == 2)] # Tout ceux qui ont l'identifiant passé sur la vague 3 et l'identifiant présent sur la vague 2
# table(pop_initiale_tot$VAGUE)
# 
# pop_initiale_tot[, Reg_Y := 0]
# pop_initiale_tot[(DA1110I == 1 & VAGUE == 3) | (DA1110I == 1 & VAGUE == 2), Reg_Y := 1] ## La population qui ont une HMR
# pop_initiale_tot$Reg_Y <- as.numeric(pop_initiale_tot$Reg_Y)
# table(pop_initiale_tot$Reg_Y)
# 
# pop_initiale_tot[, Reg_G := 0]
# SA0110_V3 <- vague_23[DOINHERIT_V3 == 1 & DOINHERIT_V2 == 0]$SA0110_V3 ## On récupère les identifiants ménages de ceux qui ont reçu un héritage entre la vague 2 et la vague 3
# pop_initiale_tot[(SA0110 %in% SA0110_V3 & VAGUE == 3) | (SA0010 %in% SA0110_V3 & VAGUE == 2), Reg_G := 1]
# pop_initiale_tot$Reg_G <- as.numeric(pop_initiale_tot$Reg_G)
# table(pop_initiale_tot$Reg_G)
# 
# 
# pop_initiale_tot[, Reg_T := 0]
# pop_initiale_tot[VAGUE == 3, Reg_T := 1] ## La date
# pop_initiale_tot$Reg_T <- as.numeric(pop_initiale_tot$Reg_T)
# table(pop_initiale_tot$Reg_T)
# 
# pop_initiale_tot[, Reg_D := Reg_G * Reg_T]
# 
# liste_cols_reg <- c("Reg_Y", "Reg_G", "Reg_T", "Reg_D")
# liste_cols_reg_poids <- c("Reg_Y", "Reg_G", "Reg_T", "Reg_D", "HW0010")
# 
# 
# 
# pop_initiale_tot$Groupe <- paste("G",pop_initiale_tot$Reg_G, "_T",pop_initiale_tot$Reg_T, "_T",pop_initiale_tot$Reg_Y, sep = "")
# 
# comptes <- pop_initiale_tot[, .N, by = Groupe]
# n <- min(comptes$N)
# sous_pop_initiale <- as.data.table(pop_initiale_tot %>% group_by(Groupe) %>% slice_sample(n=n))
# dw <- svydesign(ids = ~1, data = sous_pop_initiale[,..liste_cols_reg_poids], weights = ~ HW0010)
# mysvyglm <- svyglm(formula = Reg_Y ~ Reg_G + Reg_D + Reg_T, design = dw)
# summary(mysvyglm)
# 
# 
# 
# # library(splitstackshape)
# # sous_pop_initiale <- stratified(indt=pop_initiale_tot, group=c("Reg_G", "Reg_T", "Reg_Y"),
# #                                 size=0.5, replace=TRUE,
# #                                 select=list(Reg_G=1, Reg_Y=0, Reg_T=1
# #                                               ))
# # table(sous_pop_initiale$Reg_Y)
# # table(sous_pop_initiale$Reg_G)
# # table(sous_pop_initiale$Reg_T)
# 
# comptes <- pop_initiale_tot[, .N, by = Groupe]
# n <- min(comptes$N)
# sous_pop_initiale <- as.data.table(pop_initiale_tot %>% group_by(Groupe) %>% slice_sample(n=n))
# dw <- svydesign(ids = ~1, data = sous_pop_initiale[,..liste_cols_reg_poids], weights = ~ HW0010)
# mysvyglm <- svyglm(formula = Reg_Y ~ Reg_G + Reg_D + Reg_T, design = dw)
# summary(mysvyglm)
# 
# # 
# # if(n < nrow(pop_initiale_tot[Reg_G == 0])){
# #   # sous_pop_initiale <- pop_initiale_tot[Reg_G == 0][sample(1:nrow(pop_initiale_tot[Reg_G == 0]), n$n), ]
# #   sous_pop_initiale <- pop_initiale_tot[,.SD[sample(1:nrow(pop_initiale_tot), n$n)],by = c("Reg_G", "Reg_T", "Reg_Y")]
# # }else{
# #   sous_pop_initiale <- pop_initiale_tot[Reg_G == 0]
# # }
# # # sous_pop_initiale <- rbindlist(list(sous_pop_initiale, pop_initiale_tot[Reg_G == 1]), fill=TRUE)
# 
# sous_pop_initiale[,..liste_cols_reg_poids]
# pop_initiale_tot[,..liste_cols_reg_poids]
# dw <- svydesign(ids = ~1, data = sous_pop_initiale[,..liste_cols_reg_poids], weights = ~ HW0010)
# mysvyglm <- svyglm(formula = Reg_Y ~ Reg_G + Reg_D + Reg_T, design = dw)
# summary(mysvyglm)
# 
# 
# table(sous_pop_initiale$Reg_Y)
# table(sous_pop_initiale$Reg_G)
# table(sous_pop_initiale$Reg_T)
# 
# 
# 
# 
# 
# 
# 
# 
# pop_initiale_tot[(SA0110 %in% SA0110_V3 & VAGUE == 3) & (SA0010 %in% SA0110_V3 & VAGUE == 2)]
# 
# pop_initiale_tot$SA0010
# 
# vague_23 <- merge(x = vague_3, y = vague_2, by.x = 'SA0110_V3',by.y = 'SA0010_V2')
# vague_23$SA0110_V3 == vague_23$SA0010_V2
# 
















