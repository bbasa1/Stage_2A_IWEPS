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


pays <- "BE" # Le pays qu'on souhaite
num_vague <- 2 # La vague qu'on souhaite
utiliser_data_sauvegardee <- TRUE # Mettre FALSE si la table concaténée pays/année n'a jamais été formée, mettre TRUE si elle a déjà été formée (permet de juste l'importer)
liste_pays_traces <- c("FR", 'IT', "DE", "BE", 'ES', 'PT', "HU")


montant_heritage_min <- 10000 # Le montant d'héritage au delà duquel on considère l'héritage reçu comme conséquant. Pour la partie économétrie
faire_tourner_recherche_pvalue_opti <- TRUE
nb_points_recherche <- 100
liste_montant_initial <- lseq(1, 500000, nb_points_recherche)

################################################################################
# ============================ 02 IMPORTATION ==================================
################################################################################
num_vague_max <- 4 ### Le nombre de vague qu'on veut concaténer ATTENTION la dernière vague a des noms de colonnes en MAJUSCULE Ca pose pbm dans la concaténation...

liste_var_continues <- c("HH0401", "HH0402", "HH0403", "HH0201", "HH0202", "HH0203",
                         "HB0700","HW0010", "DA1000", "DA2100", "DA3001", "DI2000", "DI2100",
                         "DL1000", "DN3001", "DNFPOS", "DNHW", "DOGIFTINHER", "DA1120", "DA1110", "DL2110", "HB2300", "HB0500", "HB0900")
liste_var_categorielles <- c("SA0100","DHHTYPE","DOINHERIT", "DA1110I","SA0110","SA0210",
                             "SA0010" ,"DATOP10", "DHAGEH1", "DHEDUH1", "DHGENDERH1",
                             "DITOP10", "DLTOP10", "DNTOP10", "DOEINHERIT", "VAGUE",
                             "SA0200", "DHAGEH1B", "DA1120I", "DHEMPH1",
                             "HH0301A", "HH0301B", "HH0301C", "HH0301D",
                             "HH0301E", "HH0301F", "HH0301H", "HH0301I", "HH0301J",
                             "HH0302A", "HH0302B", "HH0302C", "HH0302D",
                             "HH0302E", "HH0302F", "HH0302H", "HH0302I", "HH0302J",
                             "HH0303A", "HH0303B", "HH0303C", "HH0303D",
                             "HH0303E", "HH0303F", "HH0303H", "HH0303I", "HH0303J",
                             "PE0300", "HB0300")
liste_var_interet <- union(liste_var_continues, liste_var_categorielles)


source(paste(repo_prgm , "02_importation_data.R" , sep = "/"))
if(utiliser_data_sauvegardee){ # Bon autant juste importer la dable si elle est déjà sauvegardée
  load(file = paste(repo_data, "/Data_intermediaire/data_complete.Rdata", sep = ""))
  print("Table importée !")
}else{
  data_complete <- importation_toutes_vagues(num_table_loc = 1)
  save(data_complete, file = paste(repo_data, "/Data_intermediaire/data_complete.Rdata", sep = ""))
  print("Table créé et sauvegardée !")
}
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
# DA1110 = Valeur HMR ATTENTION ==> VALEUR DETENUE DE LA PROPRIETE !!!!!
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
# PE0300 = Job description ==> On créé la colonne PE0300_simpl qui ne garde que le premier caractère :
#         0=Armée | 1=Manager | 2=Cadres sups | 3=Techniciens | 4=Employée de bureau | 5=agent de service et de vente | 6=Agriculture, fôret et poissons | 7=Artisans | 8=Operateurs et assembleurs de machines | 9=ouvriers
# HB0300 = Statut de la résidence principale : 1=Proprio entier | 2=Proprio partiel | 3=Locataire | 4=Usage gratuit (inclu usufruit)
# DODSTOTAL = Ratio paiement des dettes total du ménage/salaire mensuel net ==> Inclu TOUS les prêts (maison, voiture, intérêts, ...)
# DL2110 = Remboursement mensuel pour le prêt HMR
# HB2300 = Loyer mensuel (hors électricité, internet...) SI LOCATAIRE == SI HB0300 = 3
# HB0500 = % of ownership of household main residence
# HB0900 = Valeur TOTALE du logement ==> Même si détenud qu'à 50/50 avec le conjoint par ex 
# HB2410 = number of properties other than household main residence ==> PBM : les immeubles contenant plusieurs appartements peuvent être comptés comme 1


intersection <- intersect(liste_var_interet, colnames(data_complete))
if(length(setdiff(liste_var_interet, intersection)) > 0){
  print(paste("Attention les variables : ", setdiff(liste_var_interet, intersection), "ont été sélectionnées mais ne sont pas présentent dans la table initiale", sep = " "))
}
data_complete <- data_complete %>% mutate_at(liste_var_continues, as.numeric)
data_complete <- data_complete %>% mutate_at(liste_var_categorielles, as.factor)


##### On ajoute la variable année d'achat - année d'héritage reçu
data_complete <- Ajout_premier_heritage(data_complete)
# En fait on ne veut pas le premier héritage obtenu mais le premier héritage CONSEQUANT obtenu
data_complete <- Ajout_premier_heritage_cons(data_complete, montant_heritage_min)
data_complete[(!is.na(HB0700) & !is.na(Montant_heritage_1)), Annee_achat_heritage := HB0700 - Annee_heritage_1]
data_complete[, achat_apres_heritage := HB0700 >= Annee_heritage_1]



## Pour certaines vagues la variable DA1110I n'est pas renseignée ==> On peut la créer
data_complete[is.na(DA1110I) & DA1110 == 0, DA1110I := "0"]
data_complete[is.na(DA1110I) & DA1110 > 0, DA1110I := "1"] # Honnêtement ça marche pas super, il reste encore bc de NAN parmi les variables sur le % de prop du logement, son prix,...


# Puis la charge mensuelle du logement (remboursement de prêt ou loyer)
data_complete[, Charge_logement := HB2300]
data_complete[DA1110I == 1, Charge_logement := DL2110]
data_complete[is.na(Charge_logement) & HB0300 != 3, Charge_logement := 0] #Pour ceux qui restent je pense qu'ils n'ont plus de charge car complètement propriétaires (ce sont surtout des vieux)
### Normalement il ne reste que les locataires qui n'ont pas renseignés leur loyer... ça fait qu'une ou deux lignes par vague ce n'est sans doute pas très grave
data_complete[, salaire_net_mensuel := DI2000/12]
data_complete[, salaire_net_mensuel_corr := DI2000/12]
data_complete[salaire_net_mensuel_corr == 0 , salaire_net_mensuel_corr := 1]
data_complete[, Charge_logement_salaire := 100 * Charge_logement/salaire_net_mensuel_corr]
data_complete[, Surcharge_logement := 0] ### Eurostat : On considère qu'un ménage est en surcharge des coûts de logement si le coût TOTAL du logement représente plus de 40% du revenu disponible
data_complete[Charge_logement_salaire >= 40, Surcharge_logement := 1]
data_complete$Surcharge_logement <- as.factor(data_complete$Surcharge_logement)



data_pays <- data_complete[SA0100 == pays]

## Petit nettoyage préalable
data_pays <- nettoyage_education(data_pays)
data_pays <- ISCO_simplifie(data_pays)


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
ylabel <- "Nombre de ménages"
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
ylabel <- "Nombre de ménages"
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
if(nrow(vague_23) > 0){

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

}



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
# hist(pop_initiale_tot$Reg_Y)
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

try(dw <- svydesign(ids = ~1, data = sous_pop_initiale[,..liste_cols_reg_poids], weights = ~ HW0010), silent = TRUE)
try(mysvyglm <- svyglm(formula = Reg_Y ~ Reg_G + Reg_D + Reg_T, design = dw), silent = TRUE)
try(summary(mysvyglm), silent = TRUE)




############################################################################################################################### 
########################## AVEC LA DATE D'ACHAT - LA DATE D'HERITAGE  ######################################################### 
######################### On passe tout ça sous forme de fonction pour voir facilement la dépendance avec le montant minimal d'héritage ################
source(paste(repo_prgm , "05_econometrie.R" , sep = "/"))

if(faire_tourner_recherche_pvalue_opti){
  # D'abord avec Régression linéaire, probit et logit
  data_loc <- copy(data_pays)[VAGUE == num_vague]
  titre_save <- paste(pays,"_V",num_vague,"_pval_coeff_G_reg_Y_sur_G_toute_po.pdf", sep = "")
  titre_save <- paste(repo_sorties, titre_save, sep ='/')
  titre <- paste("Résultats des régressions de Y sur G (", nom_pays, " & vague ",num_vague,")", sep = "")
  que_heritiers <- FALSE
  que_proprio <- FALSE
  que_logit <- FALSE
  if(!all(is.na(data_loc$Montant_heritage_1))){ #En Italie pour certaines vagues ce n'est pas renseigné...
    liste_chemins <- append(liste_chemins, titre_save)
    recherche_p_value_otpi(liste_montant_initial, data_loc, annee_min = annee_min, annee_max = annee_max, faire_tracer = TRUE, titre, titre_save, que_heritiers,que_proprio,  que_logit)
  }

  data_loc <- copy(data_pays)[VAGUE == num_vague]
  titre_save <- paste(pays,"_V",num_vague,"_pval_coeff_G_reg_Y_sur_G_toute_po_logit.pdf", sep = "")
  titre_save <- paste(repo_sorties, titre_save, sep ='/')
  titre <- paste("Résultats des régressions de Y sur G (", nom_pays, " & vague ",num_vague,")", sep = "")
  que_heritiers <- FALSE
  que_proprio <- FALSE
  que_logit <- TRUE
  if(!all(is.na(data_loc$Montant_heritage_1))){ #En Italie pour certaines vagues ce n'est pas renseigné...
    liste_chemins <- append(liste_chemins, titre_save)
    recherche_p_value_otpi(liste_montant_initial, data_loc, annee_min = annee_min, annee_max = annee_max, faire_tracer = TRUE, titre, titre_save, que_heritiers,que_proprio,  que_logit)
  }
}

##################################################### Sauvegarde des données dans le cas montant_heritage_min = 0

sous_data_loc <- data_pays[VAGUE == num_vague]
# On créé les groupes
sous_data_loc[, Reg_Y := as.numeric(DA1110I) - 1] # 1=Oui 0=Non
table(sous_data_loc$DA1110I)

sous_data_loc[, Reg_G :=as.numeric(DOINHERIT) - 1]
sous_data_loc[Reg_G > 1, Reg_G := 0]
# table(sous_data_loc$Reg_G)


# table(sous_data_loc$Reg_G, sous_data_loc$Reg_Y)

sous_data_loc <- ISCO_simplifie(sous_data_loc)


denyprobit <- glm(Reg_Y ~ Reg_G, 
                  family = binomial(link = "logit"), 
                  data = sous_data_loc)
dt_prep_probit <- as.data.table(summary(denyprobit)$coefficients, keep.rownames = TRUE)
setnames(dt_prep_probit, "Pr(>|z|)", "pvalue")
dt_prep_probit
beta_0 <- dt_prep_probit[rn == "(Intercept)"]$Estimate
beta_G <- dt_prep_probit[rn == "Reg_G"]$Estimate


Lambda <- function(x){return(1/(1+exp(-x)))}
dt_prep_probit$delta_ATE <- c(0, Lambda(beta_0 + beta_G) - Lambda(beta_0))
titre_save <- paste(pays,"_V",num_vague,"_Reg_logit_Y_G.xlsx", sep = "")
write.xlsx(dt_prep_probit, paste(repo_sorties,titre_save, sep = "/"))


denyprobit <- glm(Reg_G ~ DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE + DHEMPH1 + PE0300_simpl, 
                  family = binomial(link = "logit"), 
                  data = sous_data_loc)
dt_prep_probit <- as.data.table(summary(denyprobit)$coefficients, keep.rownames = TRUE)
setnames(dt_prep_probit, "Pr(>|z|)", "pvalue")
titre_save <- paste(pays,"_V",num_vague,"_Reg_logit_G_X.xlsx", sep = "")
write.xlsx(dt_prep_probit, paste(repo_sorties,titre_save, sep = "/"))


denyprobit <- glm(Reg_Y ~ Reg_G + DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE + DHEMPH1 + PE0300_simpl, 
                  family = binomial(link = "logit"), 
                  data = sous_data_loc)
dt_prep_probit <- as.data.table(summary(denyprobit)$coefficients, keep.rownames = TRUE)
setnames(dt_prep_probit, "Pr(>|z|)", "pvalue")
titre_save <- paste(pays,"_V",num_vague,"_Reg_logit_Y_G_X.xlsx", sep = "")
write.xlsx(dt_prep_probit, paste(repo_sorties,titre_save, sep = "/"))


########## On a trouvé une variable G qui n'est pas trop corrélée aux variables socio-éco : On régresse G sur la valeur de la HMR
## Ca chance pas grand chose qu'on passe par l'un ou par l'autre en vrai... (DA1110 & HB0900)

annee_min = 0
annee_max = 100

data_pays[(is.na(HB0700) & !is.na(Montant_heritage_1)), Annee_achat_heritage := - 99] # Pas d'achat mais un héritage
data_pays[(!is.na(HB0700) & is.na(Montant_heritage_1)), Annee_achat_heritage :=  99] # Achat mais pas d'héritage
data_pays[(!is.na(HB0700) & !is.na(Montant_heritage_1)), Annee_achat_heritage := HB0700 - Annee_heritage_1]

data_loc <- copy(data_pays[VAGUE == num_vague & DA1110I == '1']) # Que les proprio
titre_save <- paste(pays,"_V",num_vague,"_effet_heritage_val_HMR_HB0900.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
titre <- paste("Effet du fait de recevoir un don ou \nun héritage sur la valeur de la résidence principale (", nom_pays, " & vague ",num_vague,")", sep = "")
if(!all(is.na(data_loc$Montant_heritage_1))){
  liste_chemins <- append(liste_chemins, titre_save)
  effet_heritage_sur_valeur_HMR(data_loc, liste_montant_initial, titre, titre_save, caption_text, "HB0900", annee_min = annee_min, annee_max=annee_max)
}




# if(faire_tourner_recherche_pvalue_opti){
#   # D'abord avec les 3 régressions
#   # liste_montant_initial <- lseq(3000, 500000, nb_points_recherche)
#   data_loc <- copy(data_pays)[VAGUE == num_vague]
#   titre_save <- paste(pays,"_V",num_vague,"_pval_coeff_G_reg_Y_sur_G_heritier.pdf", sep = "")
#   titre_save <- paste(repo_sorties, titre_save, sep ='/')
#   titre <- paste("Résultats des régressions de Y sur G\nen ne considérant comme population initiale que les héritiers (", nom_pays, " & vague ",num_vague,")", sep = "")
#   liste_chemins <- append(liste_chemins, titre_save)
#   que_heritiers <- TRUE
#   que_proprio <- FALSE
#   que_logit <- FALSE
#   recherche_p_value_otpi(liste_montant_initial, data_loc, annee_min = annee_min, annee_max = annee_max, faire_tracer = TRUE, titre, titre_save, que_heritiers,que_proprio,  que_logit)
#   
#   # liste_montant_initial <- lseq(3000, 500000, nb_points_recherche)
#   data_loc <- copy(data_pays)[VAGUE == num_vague]
#   titre_save <- paste(pays,"_V",num_vague,"_pval_coeff_G_reg_Y_sur_G_proprios.pdf", sep = "")
#   titre_save <- paste(repo_sorties, titre_save, sep ='/')
#   titre <- paste("Résultats des régressions de Y sur G\nen ne considérant comme population initiale que les proprios (", nom_pays, " & vague ",num_vague,")", sep = "")
#   liste_chemins <- append(liste_chemins, titre_save)
#   que_heritiers <- FALSE
#   que_proprio <- TRUE
#   que_logit <- FALSE
#   recherche_p_value_otpi(liste_montant_initial, data_loc, annee_min = annee_min, annee_max = annee_max, faire_tracer = TRUE, titre, titre_save, que_heritiers,que_proprio,  que_logit)
#   
#   # liste_montant_initial <- lseq(3000, 500000, nb_points_recherche)
#   data_loc <- copy(data_pays)[VAGUE == num_vague]
#   titre_save <- paste(pays,"_V",num_vague,"_pval_coeff_G_reg_Y_sur_G_toute_po.pdf", sep = "")
#   titre_save <- paste(repo_sorties, titre_save, sep ='/')
#   titre <- paste("Résultats des régressions de Y sur G\nen considérant comme population initiale toute la population (", nom_pays, " & vague ",num_vague,")", sep = "")
#   liste_chemins <- append(liste_chemins, titre_save)
#   que_heritiers <- FALSE
#   que_proprio <- FALSE
#   que_logit <- FALSE
#   recherche_p_value_otpi(liste_montant_initial, data_loc, annee_min = annee_min, annee_max = annee_max, faire_tracer = TRUE, titre, titre_save, que_heritiers,que_proprio,  que_logit)
# }
# 
# 
# 
# 
# if(faire_tourner_recherche_pvalue_opti){
#   # D'abord avec les 3 régressions
#   # liste_montant_initial <- lseq(3000, 500000, nb_points_recherche)
#   data_loc <- copy(data_pays)[VAGUE == num_vague]
#   # data_loc <- copy(data_pays)[is.na(HH030B_1) | HH030B_1 == "2"]
#   # data_loc <- copy(data_pays)[HH030A_1 == '1' | is.na(HH030B_1)]
#   
#   titre_save <- paste(pays,"_V",num_vague,"_pval_coeff_G_reg_Y_sur_G_heritier_logit.pdf", sep = "")
#   titre_save <- paste(repo_sorties, titre_save, sep ='/')
#   titre <- paste("Résultats des régressions de Y sur G\nen ne considérant comme population initiale que les héritiers (", nom_pays, " & vague ",num_vague,")", sep = "")
#   liste_chemins <- append(liste_chemins, titre_save)
#   que_heritiers <- TRUE
#   que_proprio <- FALSE
#   que_logit <- TRUE
#   recherche_p_value_otpi(liste_montant_initial, data_loc, annee_min = annee_min, annee_max = annee_max, faire_tracer = TRUE, titre, titre_save, que_heritiers,que_proprio,  que_logit)
#   
#   # liste_montant_initial <- lseq(3000, 500000, nb_points_recherche)
#   data_loc <- copy(data_pays)[VAGUE == num_vague]
#   # data_loc <- copy(data_pays)[is.na(HH030B_1) | HH030B_1 == "2"]
#   # data_loc <- copy(data_pays)[HH030A_1 == '1' | is.na(HH030B_1)]
#   
#   titre_save <- paste(pays,"_V",num_vague,"_pval_coeff_G_reg_Y_sur_G_proprios_logit.pdf", sep = "")
#   titre_save <- paste(repo_sorties, titre_save, sep ='/')
#   titre <- paste("Résultats des régressions de Y sur G\nen ne considérant comme population initiale que les proprios (", nom_pays, " & vague ",num_vague,")", sep = "")
#   liste_chemins <- append(liste_chemins, titre_save)
#   que_heritiers <- FALSE
#   que_proprio <- TRUE
#   que_logit <- TRUE
#   recherche_p_value_otpi(liste_montant_initial, data_loc, annee_min = annee_min, annee_max = annee_max, faire_tracer = TRUE, titre, titre_save, que_heritiers,que_proprio,  que_logit)
#   
#   # liste_montant_initial <- lseq(3000, 500000, nb_points_recherche)
#   # data_loc <- copy(data_pays)[is.na(HH030B_1) | HH030B_1 == "2"]
#   data_loc <- copy(data_pays)[VAGUE == num_vague]
#   # data_loc <- copy(data_pays)[HH030A_1 == '1' | is.na(HH030B_1)]
#   
#   titre_save <- paste(pays,"_V",num_vague,"_pval_coeff_G_reg_Y_sur_G_proprios_her_logit.pdf", sep = "")
#   titre_save <- paste(repo_sorties, titre_save, sep ='/')
#   titre <- paste("Résultats des régressions de Y sur G\nen ne considérant comme population initiale que les proprios et les héritiers (", nom_pays, " & vague ",num_vague,")", sep = "")
#   liste_chemins <- append(liste_chemins, titre_save)
#   que_heritiers <- TRUE
#   que_proprio <- TRUE
#   que_logit <- TRUE
#   recherche_p_value_otpi(liste_montant_initial, data_loc, annee_min = annee_min, annee_max = annee_max, faire_tracer = TRUE, titre, titre_save, que_heritiers,que_proprio,  que_logit)
#   
#   
#   # liste_montant_initial <- lseq(3000, 500000, nb_points_recherche)
#   data_loc <- copy(data_pays)[VAGUE == num_vague]
#   # data_loc <- copy(data_pays)[is.na(HH030B_1) | HH030B_1 == "2"]
#   # data_loc <- copy(data_pays)[HH030A_1 == '1' | is.na(HH030B_1)]
#   
#   titre_save <- paste(pays,"_V",num_vague,"_pval_coeff_G_reg_Y_sur_G_toute_po_logit.pdf", sep = "")
#   titre_save <- paste(repo_sorties, titre_save, sep ='/')
#   titre <- paste("Résultats des régressions de Y sur G\nen considérant comme population initiale toute la population (", nom_pays, " & vague ",num_vague,")", sep = "")
#   liste_chemins <- append(liste_chemins, titre_save)
#   que_heritiers <- FALSE
#   que_proprio <- FALSE
#   que_logit <- TRUE
#   recherche_p_value_otpi(liste_montant_initial, data_loc, annee_min = annee_min, annee_max = annee_max, faire_tracer = TRUE, titre, titre_save, que_heritiers,que_proprio,  que_logit)
# }


# nrow(data_pays[VAGUE == num_vague])
# nrow(data_pays)
# nrow(data_pays[is.na(HH030B_1) | HH030B_1 == "2"])
# nrow(data_pays[HH030A_1 == '1' | is.na(HH030B_1)])

############ Recherche + précise du montant optimal
# 
# recherche_sous_df_opti(que_heritiers = FALSE,
#                        que_proprio = FALSE,
#                        data_loc = copy(data_pays)[VAGUE == 4],
#                        nb_var_expl_max = 0)
# 
# 
# recherche_sous_df_opti <- function(que_heritiers, que_proprio, data_loc, nb_var_expl_max=0){
#   dt_precis <- recherche_p_value_otpi(lseq(500, 500000, 150), data_loc, annee_min = annee_min, annee_max = annee_max, faire_tracer = FALSE, titre, titre_save, que_heritiers,que_proprio,  que_logit)
#   casted <- dcast(dt_precis[label_variable == "Logit"], Montant_initial ~ Statistique)
#   setnames(casted, "G sur X : Nombre de modalités significatives à 1%", "Nb_mod_sign_G_X")
#   setnames(casted, "Y sur (X,G) : Coefficiant associé à G", "Coeff_Y_X")
#   setnames(casted, "Y sur G : Coefficiant", "Coeff_Y_G")
#   setnames(casted, "Y sur G : log(|coeff|)", "log_coeff_Y_G")
#   setnames(casted, "Y sur G : log(pvalue)", "log_pval")
#   setnames(casted, "Y sur G : pvalue", "pval")
#   coeff_max <- max(casted[Nb_mod_sign_G_X <= nb_var_expl_max & pval <= max(0.015, min(casted$pval))]$Coeff_Y_G)
#   
#   casted_opti <- casted[Nb_mod_sign_G_X <= nb_var_expl_max & pval <= max(0.015, min(casted$pval)) & Coeff_Y_G == coeff_max]
#   
#   casted_opti$Odd_ratio <- exp(casted_opti$Coeff_Y_G)
#   
#   liste_cols <- c("Montant_initial", "Coeff_Y_G", "pval", "Odd_ratio", "Nb_mod_sign_G_X")
#   
#   return(casted_opti[,..liste_cols])
# }


######## Pour compter le nb de ménages dans chaque catégorie
# # data_loc <- data_pays[VAGUE == 3 & Annee_achat_heritage %in% -98:98]
# # data_loc <- data_pays[VAGUE == num_vague & Annee_achat_heritage < 98] # Que les héritiers
# sous_data_proprio <- data_pays[VAGUE == num_vague & Annee_achat_heritage > -98] # Que les proprio
# montant_ini_loc <- 155000
# 
# # On créé les groupes
# sous_data_proprio[, Reg_Y := 0]
# sous_data_proprio[Annee_achat_heritage %in% annee_min:annee_max, Reg_Y := 1] # Ont acheté qq années après
# table(sous_data_proprio$Reg_Y)
# 
# sous_data_proprio[, Reg_G := 0]
# sous_data_proprio[Montant_heritage_1 >= montant_ini_loc, Reg_G := 1] # Reçu un héritage conséquant
# table(sous_data_proprio$Reg_G)



# data_loc <- copy(data_pays[VAGUE == num_vague & Annee_achat_heritage > - 98]) # Que les proprio
# liste_montant_initial <- lseq(10, 500000, 250)
# titre_save <- paste(pays,"_V",num_vague,"_effet_heritage_val_HMR_DA1110.pdf", sep = "")
# titre_save <- paste(repo_sorties, titre_save, sep ='/')
# titre <- paste("Effet du fait de recevoir un don ou \nun héritage sur la valeur de la résidence principale (", nom_pays, " & vague ",num_vague,")", sep = "")
# liste_chemins <- append(liste_chemins, titre_save)
# effet_heritage_sur_valeur_HMR(data_loc, liste_montant_initial, titre, titre_save, caption_text, "DA1110")
# 
# data_loc <- copy(data_pays[VAGUE == num_vague & Annee_achat_heritage > - 98]) # Que les proprio
# titre_save <- paste(pays,"_V",num_vague,"_effet_heritage_val_HMR_HB0900.pdf", sep = "")
# titre_save <- paste(repo_sorties, titre_save, sep ='/')
# titre <- paste("Effet du fait de recevoir un don ou \nun héritage sur la valeur de la résidence principale (", nom_pays, " & vague ",num_vague,")", sep = "")
# if(!all(is.na(data_loc$Montant_heritage_1))){
#   liste_chemins <- append(liste_chemins, titre_save)
#   effet_heritage_sur_valeur_HMR(data_loc, liste_montant_initial, titre, titre_save, caption_text, "HB0900", annee_min = annee_min, annee_max=annee_max)
# }






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
              # "DLTOP10" = "Décile de dette",
              "DNTOP10" = "Décile de patrimoine net",
              "DOEINHERIT" = "S'attend à hériter",
              "DOINHERIT" = "A hérité",
              "DHEDUH1" = "Niveau d'éducation",
              "DHAGEH1" = "Tranche d'âge",
              "DATOP10" = "Décile de patrimoine brut",
              "DHGENDERH1" = 'Sexe',
              "DA1120I" = "Propriétaire d'autres biens immobiliers",
              "DHEMPH1" = "Statut professionel",
              "PE0300_simpl" = "Type de poste")


var_diff_loc = "DA1110I"
liste_legendes_loc = c("Non_prop" = "Non propriétaires", "Prop" = "Propriétaires","Total" = "Total")
data_loc <- data_pays[VAGUE == num_vague]
titre <- paste("Distribution de différentes variables socio-économiques \npour les ménages qui sont propriétaires de leur résidence principale, \net ceux qui ne le sont pas (", nom_pays, " & vague ",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Differences_prop_non_prop.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
drop_inactifs <- TRUE
if(!all(is.na(data_loc[[var_diff_loc]])) & !all(data_loc[[var_diff_loc]] == "NAN")){
  prop_non_prop <- trace_distribution_X_non_X(data_loc, liste_variables_loc, titre, titre_save, num_vague, var_diff_loc, liste_legendes_loc, drop_inactifs, retourner_base=TRUE)
  if(any(class(prop_non_prop) == "data.frame")){ # Si le tracé a pu se faire
    liste_chemins <- append(liste_chemins, titre_save)
  }
}


# Et le revenu qui est continu
data_loc <- data_pays[VAGUE == num_vague]
data_loc <- nettoyage_DA1110I(data_loc)
x <- "DI2000"
fill <- "label_DA1110I"
xlabel <- "Revenu net annuel du ménage"
ylabel <- "Nombre de ménages"
filllabel <- "Ménage propriétaire\nde sa résidence\nprincipale"
titre <- paste("Distribution des revenus net annuels du ménage,\npour les ménages propriétaires de leur résidence principale et pour les non propriétaires (", nom_pays, " & vague ",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Differences_prop_non_prop_distrib_revenu.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
if(!all(is.na(data_loc[[fill]]))){
  trace_distrib_simple(data_loc, x, fill, titre, titre_save, xlabel, ylabel, filllabel)
  liste_chemins <- append(liste_chemins, titre_save)
  
}

# table(data_loc$DA1110I)
# all(is.na(data_loc$DA1110I))


liste_variables_loc <-c(
                        # "DITOP10" = "Décile de revenu",
                        # "DLTOP10" = "Décile de dette",
                        "DNTOP10" = "Décile de patrimoine net",
                        "DOEINHERIT" = "S'attend à hériter",
                        # "DOINHERIT" = "A hérité",
                        "DHEDUH1" = "Niveau d'éducation",
                        "DHAGEH1" = "Tranche d'âge",
                        "DATOP10" = "Décile de patrimoine brut",
                        "DHGENDERH1" = 'Sexe',
                        "DA1110I" = "Est propriétaire de sa résidence principale",
                        "DA1120I" = "Propriétaire d'autres biens immobiliers",
                        "DHEMPH1" = "Statut professionel",
                        "PE0300_simpl" = "Type de poste")


var_diff_loc = "DOINHERIT"
liste_legendes_loc = c("Non_prop" = "Non héritier", "Prop" = "Héritier","Total" = "Total")
data_loc <- data_pays[VAGUE == num_vague]
titre <- paste("Distribution de différentes variables socio-économiques \npour les ménages qui ont eu un héritage, \net ceux qui n'en ont pas eu (", nom_pays, " & vague ",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Differences_heritiers_non_heritiers.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
drop_inactifs <- TRUE
if(!all(is.na(data_loc[[fill]]))){
  heritier_non_heritier <- trace_distribution_X_non_X(data_loc, liste_variables_loc, titre, titre_save, num_vague, var_diff_loc, liste_legendes_loc, drop_inactifs, retourner_base=TRUE)
  if(any(class(prop_non_prop) == "data.frame")){ # Si le tracé a pu se faire
    liste_chemins <- append(liste_chemins, titre_save)
  }
}

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
if(!all(is.na(data_loc[[fill]]))){
  liste_chemins <- append(liste_chemins, titre_save)
  trace_distrib_simple(data_loc, x, fill, titre, titre_save, xlabel, ylabel, filllabel)
}




liste_variables_loc <-c(
                        # "DITOP10" = "Décile de revenu",
                        # "DLTOP10" = "Décile de dette",
                        "DNTOP10" = "Décile de patrimoine net",
                        # "DOEINHERIT" = "S'attend à hériter",
                        "DOINHERIT" = "A hérité",
                        "DHEDUH1" = "Niveau d'éducation",
                        "DHAGEH1" = "Tranche d'âge",
                        "DATOP10" = "Décile de patrimoine brut",
                        "DHGENDERH1" = 'Sexe',
                        "DA1110I" = "Est propriétaire de sa résidence principale",
                        "DA1120I" = "Propriétaire d'autres biens immobiliers",
                        "DHEMPH1" = "Statut professionel",
                        "PE0300_simpl" = "Type de poste")

var_diff_loc = "DOEINHERIT"
liste_legendes_loc = c("Non_prop" = "Ne s'attend pas à hériter", "Prop" = "S'attend à hériter","Total" = "Total")
data_loc <- data_pays[VAGUE == num_vague]
titre <- paste("Distribution de différentes variables socio-économiques \npour les ménages qui attendent un héritage, \net ceux qui n'en attendent pas (", nom_pays, " & vague ",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Differences_attendent_her_non_attendent_her.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
drop_inactifs <- TRUE
if(!all(is.na(data_loc[[fill]]))){
  trace_distribution_X_non_X(data_loc, liste_variables_loc, titre, titre_save, num_vague, var_diff_loc, liste_legendes_loc, drop_inactifs)
  if(any(class(prop_non_prop) == "data.frame")){ # Si le tracé a pu se faire
    liste_chemins <- append(liste_chemins, titre_save)
  }
}



# Pour les variables continues
data_loc <- data_pays[VAGUE == num_vague]
data_loc <- nettoyage_DOEINHERIT(data_loc)
x <- "DI2000"
fill <- "label_DOEINHERIT"
xlabel <- "Revenu net annuel du ménage"
ylabel <- "Nombre de ménages"
filllabel <- "Ménage qui s'attend\nà recevoir\nhéritage ou don"
titre <- paste("Distribution des revenus net annuels du ménage,\npour les ménages qui attendent un héritage, \net ceux qui n'en attendent pas (", nom_pays, " & vague ",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Differences_attendent_her_non_attendent_her_distrib_revenu.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
if(!all(is.na(data_loc[[fill]]))){
  liste_chemins <- append(liste_chemins, titre_save)
  trace_distrib_simple(data_loc, x, fill, titre, titre_save, xlabel, ylabel, filllabel)
}


# Pour les variables discrètes
liste_variables_loc <-c(
                        # "DITOP10" = "Décile de revenu",
                        # "DLTOP10" = "Décile de dette",
                        "DNTOP10" = "Décile de patrimoine net",
                        "DOEINHERIT" = "S'attend à hériter",
                        "DOINHERIT" = "A hérité",
                        "DHEDUH1" = "Niveau d'éducation",
                        "DHAGEH1" = "Tranche d'âge",
                        "DATOP10" = "Décile de patrimoine brut",
                        "DHGENDERH1" = 'Sexe',
                        "DA1110I" = "Est propriétaire de sa résidence principale",
                        "DHEMPH1" = "Statut professionel",
                        "PE0300_simpl" = "Type de poste")

var_diff_loc = "DA1120I"
liste_legendes_loc = c("Non_prop" = "Non possédants", "Prop" = "Possédants","Total" = "Total")
data_loc <- data_pays[VAGUE == num_vague]
titre <- paste("Distribution de différentes variables socio-économiques \npour les ménages qui possèdent d'autres biens immobiliers que leur HMR,\net les ménages qui n'en possèdent pas (", nom_pays, " & vague ",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Differences_prop_res_sec_non_prop.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
drop_inactifs <- TRUE
if(!all(is.na(data_loc[[fill]]))){
  trace_distribution_X_non_X(data_loc, liste_variables_loc, titre, titre_save, num_vague, var_diff_loc, liste_legendes_loc, drop_inactifs)
  if(any(class(prop_non_prop) == "data.frame")){ # Si le tracé a pu se faire
    liste_chemins <- append(liste_chemins, titre_save)
  }
}

# Pour les variables continues
data_loc <- data_pays[VAGUE == num_vague]
data_loc <- nettoyage_DA1120I(data_loc)
x <- "DI2000"
fill <- "label_DA1120I"
xlabel <- "Revenu net annuel du ménage"
ylabel <- "Nombre de ménages"
filllabel <- "Ménage propriétaire\nd'autres biens\nimmobiliers que leur\nrésidence principale"
titre <- paste("Distribution des revenus net annuels du ménage,\npour les ménages qui possèdent d'autres biens immobiliers que leur HMR,\net les ménages qui n'en possèdent pas (", nom_pays, " & vague ",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Differences_prop_res_sec_non_prop_distrib_revenu.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
if(!all(is.na(data_loc[[fill]]))){
  liste_chemins <- append(liste_chemins, titre_save)
  trace_distrib_simple(data_loc, x, fill, titre, titre_save, xlabel, ylabel, filllabel)
}



############################## Surcharge de logement ou non
data_loc <- data_pays[VAGUE == num_vague]
data_loc <- nettoyage_Surcharge(data_loc)
liste_variables_loc <-c(
  # "DITOP10" = "Décile de revenu",
  # "DLTOP10" = "Décile de dette",
  "DNTOP10" = "Décile de patrimoine net",
  "DOEINHERIT" = "S'attend à hériter",
  "DOINHERIT" = "A hérité",
  "DHEDUH1" = "Niveau d'éducation",
  "DHAGEH1" = "Tranche d'âge",
  "DATOP10" = "Décile de patrimoine brut",
  "DHGENDERH1" = 'Sexe',
  "DA1110I" = "Est propriétaire de sa résidence principale",
  "DHEMPH1" = "Statut professionel",
  "PE0300_simpl" = "Type de poste")

var_diff_loc = "Surcharge_logement"
liste_legendes_loc = c("Non_prop" = "Pas de surcharge", "Prop" = "Surcharge","Total" = "Total")
titre <- paste("Distribution de différentes variables socio-économiques \npour les ménages qui sont en surcharge de logement,\net les ménages qui ne sont pas en surcharge\nUn ménage est en surcharge si le coût final de son logement représente plus de 40% de ses revenus (Eurostat)\n(", nom_pays, " & vague ",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Differences_surcharge_non_surcharge.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
drop_inactifs <- TRUE
if(!all(is.na(data_loc[[fill]]))){
  trace_distribution_X_non_X(data_loc, liste_variables_loc, titre, titre_save, num_vague, var_diff_loc, liste_legendes_loc, drop_inactifs)
  if(any(class(prop_non_prop) == "data.frame")){ # Si le tracé a pu se faire
    liste_chemins <- append(liste_chemins, titre_save)
  }
}






############################## Coût du logement entre les propriétaires et les non propriétaires
data_loc <- data_pays[VAGUE == num_vague]
data_loc[Charge_logement == 0, Charge_logement:= 1]
data_loc <- nettoyage_DA1110I(data_loc)
x <- "Charge_logement"
fill <- "label_DA1110I"
xlabel <- "Charge du logement "
ylabel <- "Nombre de ménages"
filllabel <- "Ménage propriétaire\nde sa résidence\nprincipale"
titre <- paste("Distribution des dépenses de logement mensuels du ménage,\npour les ménages propriétaires de leur résidence principale et pour les non propriétaires\n(remboursement du prêt, ou loyer, les ménages dépensant 0 sont mis à 1 euros de dépense/mois)\n(", nom_pays, " & vague ",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Differences_prop_non_prop_distrib_depense_log.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
faire_tableau <- FALSE
if(!all(is.na(data_loc[[fill]]))){
  liste_chemins <- append(liste_chemins, titre_save)
  trace_distrib_simple(data_loc, x, fill, titre, titre_save, xlabel, ylabel, filllabel, trans="log10", faire_tableau)
}


data_loc <- data_pays[VAGUE == num_vague]
data_loc[, Charge_logement_salaire_modif := Charge_logement_salaire]
data_loc[Charge_logement_salaire_modif >= 100, Charge_logement_salaire_modif := 100]
data_loc <- nettoyage_DA1110I(data_loc)
x <- "Charge_logement_salaire_modif"
fill <- "label_DA1110I"
xlabel <- "Charge du logement en % du revenu net (borné à 100%)"
ylabel <- "Nombre de ménages"
filllabel <- "Ménage propriétaire\nde sa résidence\nprincipale"
titre <- paste("Distribution des dépenses de logement mensuels du ménage, en % du revenu net\npour les ménages propriétaires de leur résidence principale et pour les non propriétaires\n(remboursement du prêt, ou loyer)\n(", nom_pays, " & vague ",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Differences_prop_non_prop_distrib_depense_prct_revenu.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
faire_tableau <- FALSE
if(!all(is.na(data_loc[[fill]]))){
  liste_chemins <- append(liste_chemins, titre_save)
  trace_distrib_simple(data_loc, x, fill, titre, titre_save, xlabel, ylabel, filllabel, trans="identity", faire_tableau, suffix_x = " %", orientation_label=0)
}

## Puis le boxplot
data_loc <- data_complete[SA0100 %in% liste_pays_traces & VAGUE == num_vague]
data_loc <- nettoyage_SA0100(data_loc)
data_loc <- nettoyage_DA1110I(data_loc)
x <- "Charge_logement_salaire"
fill <- "label_SA0100"
titre <- paste("Distribution des dépenses de logement mensuels du ménage, en % du revenu net\npour les ménages propriétaires de leur résidence principale et pour les non propriétaires\n(remboursement du prêt, ou loyer)\n(vague ",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Boxplot_distrib_depense_prct_revenu.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
xlabel <- "% du revenu net dépensé pour se loger (bloqué à 100%)"
filllabel <- "Pays"
facet <- "label_DA1110I"

trace_boxplot(data_loc, x, fill, facet, titre, titre_save, xlabel, filllabel, xlim = c(0,100))
liste_chemins <- append(liste_chemins, titre_save)








##############################  La distribution des montants d'héritage entre les pays
data_loc <- data_complete[SA0100 %in%  liste_pays_traces & VAGUE == num_vague]
data_loc <- nettoyage_SA0100(data_loc)
x <- "Montant_heritage_1"
fill <- "label_SA0100"
limits_x <- 1000000
titre <- paste("Distribution du montant du premier héritage ou don reçu par les ménages\n Pour la vague ",num_vague, sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Distrib_montant_heritage.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
ylabel <- "Densité"
xlabel <- "Montant du premier héritage reçu chronologiquement par le ménage"
filllabel <- "Pays"
facet <- "label_SA0100"
nbins <- 75

if(!all(is.na(data_loc$Montant_heritage_1))){
  trace_distrib_normalise(data_loc, x, fill, titre, titre_save, xlabel, ylabel, filllabel,facet, trans="log10", nbins)
  liste_chemins <- append(liste_chemins, titre_save)
}




############################## La distribution de l'âge de répeption du premier héritage
data_loc <- data_complete[SA0100 %in%  liste_pays_traces & VAGUE == num_vague]
data_loc <- nettoyage_SA0100(data_loc)
data_loc <- nettoyage_DA1110I(data_loc)
data_loc$Cb_annee_depuis_heritage <- as.numeric(as.character(data_loc$SA0200)) - as.numeric(data_loc$Annee_heritage_1)
data_loc$Age_heritage_1 <- as.numeric(data_loc$DHAGEH1) - data_loc$Cb_annee_depuis_heritage
data_loc <- data_loc[Age_heritage_1 >= -1]

x <- "Age_heritage_1"
fill <- "label_SA0100"
limits_x <- 1000000
titre <- paste("Distribution de l'âge de la personne référence du ménage lors de la réception du premier héritage ou du premier don\n Pour la vague ",num_vague, sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Distrib_age_heritage.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
ylabel <- "Densité"
xlabel <- "Âge de la personne référence du ménage"
filllabel <- "Pays"
facet <- "label_SA0100"
nbins <- 20

## D'abord la distribution
if(!all(is.na(data_loc$Age_heritage_1))){
  trace_distrib_normalise(data_loc, x, fill, titre, titre_save, xlabel, ylabel, filllabel,facet, trans="identity", nbins, suffix_x = "")
  liste_chemins <- append(liste_chemins, titre_save)
}

## Puis le boxplot
x <- "Age_heritage_1"
fill <- "label_SA0100"
titre <- paste("Distribution de l'âge de la personne référence du ménage lors de la réception du premier héritage ou du premier don\npour les ménages propriétaires de leur résidence principale, et pour les non propriétaires\n Pour la vague ",num_vague, sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Boxplot_age_heritage.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
xlabel <- "Âge de la personne référence du ménage"
filllabel <- "Pays"
facet <- "label_DA1110I"

trace_boxplot(data_loc, x, fill, facet, titre, titre_save, xlabel, filllabel)
liste_chemins <- append(liste_chemins, titre_save)




############################## La distribution des patrimoines suivant le sexe
liste_type_patrimoines <- c("DA3001" = "Patrimoine brut",
                            "DA1000" = "Patrimoine physique",
                            "DA2100" = "Patrimoine financier",
                            "DL1000" = "Dettes",
                            "DN3001" = "Patrimoine net")

data_loc <- data_pays[VAGUE == num_vague]
data_loc <- nettoyage_sexe(data_loc)
data_loc <- melt(data_loc, 
                   id.vars = , c("HW0010", "Sexe"),
                   measure.vars  = names(liste_type_patrimoines),
                   variable.name = "patrimoine",
                   value.name    = "value_1")

data_loc <- nettoyage_patrimoine(data_loc)
x <- "value_1"
fill <- "label_patrimoine"
titre <- paste("Distribution de l'âge de la personne référence du ménage lors de la réception du premier héritage ou du premier don\npour les ménages propriétaires de leur résidence principale, et pour les non propriétaires\n Pour la vague ",num_vague, sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Boxplot_sexe_patrimoine.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
xlabel <- "Âge de la personne référence du ménage"
filllabel <- "Type de patrimoine"
facet <- "Sexe"

trace_boxplot(data_loc, x, fill, facet, titre, titre_save, xlabel, filllabel, xlim = c(-10000, 1000000), suffix_x = " €")
liste_chemins <- append(liste_chemins, titre_save)




############################################################################################################################### 
############################## GENERATION DE CARTES D'INDICES DE FINI EN EUROPE ############################################### 
############################################################################################################################### 


nb_quantiles <- 100
type_pat_loc <- "DN3001"
data_path <- "C:/Users/Benjamin/Desktop/IWEPS/Data/Data_intermediaire/Gini_carte.csv" #Là où on va stocker les données pour faire la carte
num_vague_loc <- 3 # La seule où on a des données pour la Hongrie et la Littuanie...

liste_type_patrimoines <- c("DA3001" = "Patrimoine brut",
                            "DA1000" = "Patrimoine physique",
                            "DA2100" = "Patrimoine financier",
                            "DL1000" = "Dettes",
                            "DN3001" = "Patrimoine net")


for(type_pat in names(liste_type_patrimoines)){
  # On récupère les indices de Gini pour tous les pays
  liste_pays <- levels(data_complete[VAGUE == num_vague_loc]$SA0100)
  liste_gini <- c()
  for(pays_loc in liste_pays){
    gini <- calcul_gini_pays(data_complete[VAGUE == num_vague_loc & SA0100 == pays_loc], type_pat)
    liste_gini <- append(liste_gini, gini)
  }

  # On les sauvegarde
  df <- as.data.frame(do.call(cbind, list(liste_pays, liste_gini)))
  setnames(df, c("V1", "V2"), c("Pays", "Gini"))
  
  paste(repo_data, "/Data_intermediaire/Gini_carte.csv", sep = "")
  write.csv(df, paste(repo_data, "/Data_intermediaire/Gini_carte.csv", sep = ""), row.names=FALSE)
  
  
  # Puis on appelle Python pour produire les cartes
  titre_save <- paste("Cartes/V",num_vague_loc,"_Carte_Gini_",type_pat,".pdf", sep = "")
  titre_save <- paste(repo_sorties_initial, titre_save, sep ='/')
  liste_chemins <- append(liste_chemins, titre_save)
  titre <- paste("Carte d'Europe des indices de Gini : ",liste_type_patrimoines[type_pat],"\n Pour la vague 3", sep = "")
  map_path <- "C:/Users/Benjamin/Desktop/IWEPS/Data/Data_intermediaire/world-administrative-boundaries.geojson" # A priori à ne pas toucher

  py_run_file(paste(repo_prgm, "/Map_Gini_EU_rstudio.py", sep = ""))
}
  





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
"achat_apres_heritage"

# data_pays$achat_apres_heritage
table(data_pays[VAGUE == num_vague]$DA1110I)
# colnames(data_pays)


var <- "Surcharge_logement"
data_loc <- data_complete[VAGUE == 2 & SA0100 == "BE"]
locvar <- tableau_binaire(var, data_loc, count_na = TRUE) # TOUJOURS 1 = Non, 2 = Oui
locvar




sous_data <- data_complete[SA0100 == "IT" & VAGUE == num_vague,]
dw <- svydesign(ids = ~1, data = sous_data, weights = ~ sous_data$HW0010)
dt <- as.data.table(100*svytable(~ DHHTYPE + DA1110I, dw)/sum(sous_data$HW0010))
dt_casted <- dcast(dt, DHHTYPE ~ DA1110I,)
setnames(dt_casted, "0", "Non")
setnames(dt_casted, "1", "Oui")

dt_casted$somme <- dt_casted$Non + dt_casted$Oui

dt_casted$DHHTYPE <- as.numeric(dt_casted$DHHTYPE)
summed <- as.data.frame(t(colSums(dt_casted)))
dt_casted <- rbindlist(list(dt_casted,summed))

dt_casted

# DHHTYPE == 51, "Adulte seul.e <= 64 ans",
# DHHTYPE == 52, "Adulte seul.e >= 65 ans",
# DHHTYPE == 6, "Couple <= 64 ans",
# DHHTYPE == 7, "Couple au moins un.e >= 65 ans",
# DHHTYPE == 8, ">= 3 adultes",
# DHHTYPE == 9, "Adulte seul.e avec enfant(s)",
# DHHTYPE == 10, "Couple avec 1 enfant",
# DHHTYPE == 11, "Couple avec 2 enfants",
# DHHTYPE == 12, "Couple avec >= 3 enfants",
# DHHTYPE == 13, ">= 3 adultes avec enfant(s)"


#   casted <- dcast(dt_precis[label_variable == "Logit"], Montant_initial ~ Statistique)


# summary(svytable(~ DHHTYPE + DA1110I, dw))


# 
# liste_type_patrimoines <- c("DA3001" = "Patrimoine brut",
#                             "DA1000" = "Patrimoine physique",
#                             "DA2100" = "Patrimoine financier",
#                             "DL1000" = "Dettes",
#                             "DN3001" = "Patrimoine net")
# 
# 
# data_complete$DHGENDERH1
# 
# data_complete[, sum(HW0010), by = ]
# 
# sous_data <- data_complete[VAGUE == num_vague & SA0100 == "BE",]
# dw <- svydesign(ids = ~1, data = sous_data, weights = ~ sous_data$HW0010)
# 
# 
# svyquantile(x = ~ DA3001, 
#             design = dw, 
#             na.rm = TRUE, 
#             quantiles = c(.01, .25, .50, .75, .99))
# 
# 
# svytable(~ DA3001 + DHGENDERH1, dw)
# 
# dt <- as.data.table(100*svytable(~ HB0300 + SA0100, dw)/sum(sous_data$HW0010))
# 


# HB0300
# SA0100

# sous_data <- data_complete[VAGUE == num_vague,]
# dw <- svydesign(ids = ~1, data = sous_data, weights = ~ sous_data$HW0010)
# dt <- as.data.table(100*svytable(~ HB0300 + SA0100, dw)/sum(sous_data$HW0010))
# dt_casted <- dcast(dt, SA0100 ~ HB0300,)
# setnames(dt_casted, "1", "Proprio_complet")
# setnames(dt_casted, "2", "Proprio_partiel")
# setnames(dt_casted, "3", "Loue")
# setnames(dt_casted, "4", "Usage_gratuit")
# 
# dt_casted$somme <- dt_casted$Proprio_complet +
#   dt_casted$Proprio_partiel +
#   dt_casted$Loue +
#   dt_casted$Usage_gratuit
# 
# dt_casted$SA0100 <- as.character(dt_casted$SA0100)
# liste_cols <- c("Proprio_complet", "Proprio_partiel", "Loue", "Usage_gratuit", "somme")
# summed <- as.data.frame(t(colSums(dt_casted[,..liste_cols])))
# dt_casted <- rbindlist(list(dt_casted,summed), fill = TRUE)
# 
# dt_casted
  


# DA1110I

# data_pays[]


# data_pays$DI2000




# liste_cols <- c("DI2000", "Charge_logement", "Charge_logement_salaire", "salaire_net_mensuel", "salaire_net_mensuel_corr") ## Tout est calculé à l'échelle du MENAGE
# data_pays[,..liste_cols]
# 
# 
# data_complete[ SA0100 == 'FR' & VAGUE == 2]
# 
# 
# data_complete[VAGUE == 2, 100*cor(Charge_logement_salaire, salaire_net_mensuel_corr, use="complete.obs"), by = SA0100]


# data_complete[VAGUE == 2, table(Surcharge_logement), by = SA0100]

# table(data_complete[VAGUE == 2 & SA0100 == "DE"]$Surcharge_logement, useNA ="ifany")
# 
# 
# 
# data_complete[VAGUE == 2 & SA0100 == "DE", sum(HW0010), by = Surcharge_logement]
# 
# data_complete$HW0010








# HH030xA = what kind of assets received ? Money
# HH030xB = what kind of assets received ? Dwelling
# HH030xC = what kind of assets received ? Use of a dwelling (under reserve or usufruct)
# HH030xD = what kind of assets received ? Land
# HH030xE = what kind of assets received ? Business
# HH030xF = what kind of assets received ? Securities, shares
# HH030xG = what kind of assets received ? Jewellery, furniture, artwork
# HH030xH = what kind of assets received ? Life insurance
# HH030xJ = what kind of assets received ? Car / vehicle I - Other assets (specify)

# data_pays$HH030A_cons
# 
# 
# var <- "HH030A_1"
# data_loc <- data_complete[VAGUE == 2 & SA0100 == "BE"]
# locvar <- tableau_binaire(var, data_loc, count_na = TRUE) # ICI 1 = Oui, 2 = Non
# locvar
# 
# 
# 
# var <- "HH030B_1"
# data_loc <- data_complete[VAGUE == 2 & SA0100 == "BE"]
# locvar <- tableau_binaire(var, data_loc, count_na = TRUE) # ICI 1 = Oui, 2 = Non
# locvar
# 
# 
# 
# 
# var <- "HH030C_1"
# data_loc <- data_complete[VAGUE == 2 & SA0100 == "BE"]
# locvar <- tableau_binaire(var, data_loc, count_na = TRUE) # ICI 1 = Oui, 2 = Non
# locvar
# 
# 
# 
# data_complete[VAGUE == 2 & SA0100 == "BE" & is.na(HH030A_1) & DOINHERIT == 1]
# 
# # HH030",lettre,"_cons
# 
# cor(as.numeric(data_complete[VAGUE == 2 & SA0100 == "BE"]$HH030B_1), data_complete[VAGUE == 2 & SA0100 == "BE"]$DA1120, use="complete.obs")
# cor(as.numeric(data_complete[VAGUE == 2 & SA0100 == "BE"]$HH030B_1), data_complete[VAGUE == 2 & SA0100 == "BE"]$DA1110, use="complete.obs")
# cor(as.numeric(data_complete[VAGUE == 2 & SA0100 == "BE"]$HH030B_1), as.numeric(data_complete[VAGUE == 2 & SA0100 == "BE"]$DA1110I), use="complete.obs")










# liste_cols_reg_poids <- c("HW0010", "Reg_G", "Reg_Y", "DHAGEH1B", "DHEDUH1", "DHGENDERH1", "DI2000", "DHHTYPE", "DHEMPH1", "PE0300_simpl")
# dw <- svydesign(ids = ~1, data = sous_data_loc[,..liste_cols_reg_poids], weights = ~ HW0010)
# svyciprop(Reg_Y ~ Reg_G, dw, method = "logit", level = 0.95)


# svyciprop(Reg_G ~ DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE + DHEMPH1 + PE0300_simpl, dw, method = "logit", level = 0.95)
# 
# table(sous_data_loc$Reg_Y)
# 
# # zelig(Reg_Y ~ Reg_G, model = "logit.survey", weights = sous_data_loc$HW0010, data = sous_data_loc[,..liste_cols_reg_poids])
# 
# zelig(Reg_Y ~ Reg_G, model = "ls", weights = sous_data_loc$HW0010, data = sous_data_loc[,..liste_cols_reg_poids])
# 
# Zout <- zelig(Reg_G ~ DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE + DHEMPH1 + PE0300_simpl, model = "logit.survey", weights = sous_data_loc$HW0010, data = sous_data_loc[,..liste_cols_reg_poids])
# Zout <- zelig(Reg_Y ~ Reg_G + DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE + DHEMPH1 + PE0300_simpl, model = "logit.survey", weights = sous_data_loc$HW0010, data = sous_data_loc[,..liste_cols_reg_poids])
# 





# data(api, package = "survey")
# 
# apistrat$yr.rnd.numeric <- as.numeric(apistrat$yr.rnd == "Yes")
# zelig(yr.rnd.numeric ~ meals + mobility, model = "logit.survey",
#       weights = apistrat$pw, data = apistrat)
# 
# 
# apistrat$meals
# apistrat$mobility
