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


num_vague_max <- 4 ### Le nombre de vague qu'on veut concaténer ATTENTION la dernière vague a des noms de colonnes en MAJUSCULE Ca pose pbm dans la concaténation...
pays <- "BE"

montant_heritage_min <- 10000 # Le montant d'héritage au delà duquel on considère l'héritage reçu comme conséquant. Pour la partie économétrie

faire_tourner_recherche_pvalue_opti <- FALSE


################################################################################
# ============================ 02 IMPORTATION ==================================
################################################################################
liste_var_continues <- c("HH0401", "HH0402", "HH0403", "HH0201", "HH0202", "HH0203", "HB0700","HW0010", "DA1000", "DA2100", "DA3001", "DI2000", "DI2100", "DL1000", "DN3001", "DNFPOS", "DNHW", "DOGIFTINHER", "DA1120")
liste_var_categorielles <- c("SA0100","DHHTYPE","DOINHERIT", "DA1110I","SA0110","SA0210", "SA0010" ,"DATOP10", "DHAGEH1", "DHEDUH1", "DHGENDERH1", "DHLIFESATIS", "DITOP10", "DLTOP10", "DNTOP10", "DOEINHERIT", "VAGUE", "SA0200", "DHAGEH1B")
liste_var_interet <- union(liste_var_continues, liste_var_categorielles)


source(paste(repo_prgm , "02_importation_data.R" , sep = "/"))
data_complete <- importation_toutes_vagues(num_table_loc = 1)
data_pays <- data_complete[SA0100 == pays]
nom_pays <- dico_pays[pays]


# data_vague_2 <- importation_une_vagues(num_vague_loc = 2)
# MIcombine(with(data_vague_2,svytotal(~DA3001)))
# MIcombine(with(data_vague_2,svyratio(~DA3001,~DH0001)))


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
# "HB0700 = year of property acquisition
# "HH0201 = year gift/inheritance received
# "HH0202"
# "HH0203"
# HH0401 = Value of gift
# HH0402
# HH0403
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
data_pays[, label_education := factor(
  fcase(
    DHEDUH1 == 1, "< Brevet",
    DHEDUH1 == 2, "Brevet",
    DHEDUH1 == 3, "Bac",
    DHEDUH1 == 5, "> Bac"
  )
)
]


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
num_vague <- 3


# Patrimoine net
var_decile <- "DNTOP10"
titre <- paste("Répartition du patrimoine net, normalisé par sexe (", nom_pays, "& vague",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Patrimoine_net_sexe.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
xlabel <-"Quantile de patrimoine net"
facet <- "DHHTYPE" # On facet par type de ménage
var_normalisation <- c("DHGENDERH1","DHHTYPE") #On va mettre fill et normalisation par sexe
data_loc <- data_pays[!is.na(get(var_decile)) & VAGUE == num_vague]
graphique_repartition_pat_quantile_sexe(data_loc, var_decile,var_normalisation, titre, titre_save, xlabel, facet, xlim_sup = 35)

# Patrimoine brut
var_decile <- "DATOP10"
titre <- paste("Répartition du patrimoine brut, normalisé par sexe (", nom_pays, " & vague",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Patrimoine_brut_sexe.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
xlabel <-"Quantile de patrimoine brut"
facet <- "DHHTYPE"
var_normalisation <- c("DHGENDERH1","DHHTYPE") #On va mettre fill et normalisation par sexe
data_loc <- data_pays[!is.na(get(var_decile)) & VAGUE == num_vague]
graphique_repartition_pat_quantile_sexe(data_loc, var_decile,var_normalisation, titre, titre_save, xlabel, facet, xlim_sup = 25)

# Dettes
var_decile <- "DLTOP10"
titre <- paste("Répartition des dettes, normalisé par sexe (", nom_pays, " & vague",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Patrimoine_dettes_sexe.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
xlabel <-"Quantile de dettes"
facet <- "DHHTYPE"
var_normalisation <- c("DHGENDERH1","DHHTYPE") #On va mettre fill et normalisation par sexe
data_loc <- data_pays[!is.na(get(var_decile)) & VAGUE == num_vague]
graphique_repartition_pat_quantile_sexe(data_loc, var_decile,var_normalisation, titre, titre_save, xlabel, facet, xlim_sup = 25)




######## On regarde la concentration des différents types de patrimoines #############
nb_quantiles <- 100
liste_type_patrimoines <- c("DA3001" = "Patrimoine brut",
                            "DA1000" = "Patrimoine physique",
                            "DA2100" = "Patrimoine financier",
                            "DL1000" = "Dettes",
                            "DN3001" = "Patrimoine net")

titre_fig <- paste("Fonction de répartition de la richesse détenue par les ménages (", nom_pays, " & vague",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_Concentration_patrimoine_par_type.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
data_loc <- data_pays[VAGUE == num_vague]

graphique_contration_patrimoine(data_pays ,nb_quantiles, liste_type_patrimoines, titre_fig, titre_save)



# # DA1000
# 
# data_loc[, quantiles_pat := 
#           hutils::weighted_ntile(DA1000, weights =  sum(HW0010), nb_quantiles)]
# data_for_plot <- data_loc[,
#                            lapply(.SD, sum, na.rm = TRUE), 
#                            by = .(quantiles_pat),
#                            .SDcols = names(data_loc) == "DA1000"][order(quantiles_pat)]
# data_for_plot[, cumsum_DA1000 := 100*cumsum(DA1000)/sum(DA1000, na.rm=TRUE)]
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # On initialise
# data_for_plot <- as.data.table(1:nb_quantiles)
# setnames(data_for_plot, 'V1', "Quantiles")
# data_for_plot$Quantiles <- as.numeric(data_for_plot$Quantiles)
# # data_for_plot
# 
# # On boucle sur les types de patrimoines
# for(type_pat in names(liste_type_patrimoines)){
#   
#   
#   data_loc[, Quantiles := 
#              hutils::weighted_ntile(get(type_pat), weights =  sum(HW0010), nb_quantiles)]
#   data_for_plot_loc <- data_loc[,
#                             lapply(.SD, sum, na.rm = TRUE), 
#                             by = .(Quantiles),
#                             .SDcols = names(data_loc) == type_pat][order(Quantiles)]
#   data_for_plot_loc[, cum_sum := 100*cumsum(get(type_pat))/sum(get(type_pat), na.rm=TRUE)]
#   setnames(data_for_plot_loc, "cum_sum", liste_type_patrimoines[[type_pat]])
#   
#   
#   # Récupération des quantiles
#   # data_loc[,Quantiles := cut(data_loc[[type_pat]],
#   #                            breaks=quantile(data_loc[[type_pat]], probs=seq(0, 1, by=1/nb_quantiles), na.rm=T),
#   #                            include.lowest= TRUE, labels=1:nb_quantiles)]
#   
#   # Traitement pour pouvoir merge
#   # data_for_plot_loc <- data_loc[, mean(get(type_pat)), by = Quantiles]
#   # setnames(data_for_plot_loc, "V1", liste_type_patrimoines[[type_pat]])
#   data_for_plot_loc$Quantiles <- as.numeric(data_for_plot_loc$Quantiles)
#   
#   # Merge
#   data_for_plot <- merge(data_for_plot, data_for_plot_loc, by = "Quantiles")
# }
# 
# 
# # Melt pour pouvoir tracer
# melted <- melt(data_for_plot, 
#                id.vars = "Quantiles", 
#                measure.vars  = as.data.frame(liste_type_patrimoines)$liste_type_patrimoines,
#                variable.name = "variable",
#                value.name    = "value")
# 
# 
# x <- "Quantiles"
# y <- "value"
# color <- "variable"
# xlabel <- "Part des ménages"
# ylabel <- "Richesse détenue (cumulatif)"
# colorlabel <- "Type de richesse"
# data_melted_loc <- melted
# 
# trace_concentration(data_melted_loc, x, y, color, xlabel, ylabel,colorlabel, titre_fig, titre_save)
# 
# 
# 
# 







############################## Dispersion du patrimoine en fonction de l'âge
liste_type_patrimoines <- c("DA3001" = "Patrimoine brut",
                            "DA1000" = "Patrimoine physique",
                            "DA2100" = "Patrimoine financier",
                            "DL1000" = "Dettes",
                            "DN3001" = "Patrimoine net")

data_loc <- data_pays[VAGUE == num_vague]
titre <- paste("Variance du patrimoine détenu par les ménages\nIntervalles de confiance à 95% (", nom_pays, " & vague",num_vague,")", sep = "")
titre_save <- paste(pays,"_V",num_vague,"_variance_patrimoine.pdf", sep = "")

graphique_variance_pat_age(data_loc, liste_type_patrimoines, titre, titre_save)




######################### Tracé : le date d'achat de la HMR - la date de l'héritage
titre <- paste("Distribution de la variable :\ndate d'aquisition de la résidence principale actuelle - date du premier don ou héritage reçu\n(", nom_pays, "& vague",num_vague,")", sep = "")
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
trace_distrib_variable(data_loc, x, fill, xlabel, ylabel,filllabel, titre, titre_save, liste_breaks_fill, liste_breaks_x, limits_x)


titre <- paste("Distribution de la variable :\ndate d'aquisition de la résidence principale actuelle - date du premier don ou héritage reçu\n (", nom_pays, " & vague",num_vague,")", sep = "")
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
data_vagues <- vague_123
faire_rang = TRUE
try(graphique_evolution_position_vagues(vague_123 ,nb_quantiles, liste_type_patrimoines, titre_fig, titre_save, faire_rang))
  

titre_fig <- paste("Evolution des patrimoines des ménages entre les différentes vagues (", nom_pays, ")", sep = "")
titre_save <- paste(pays,"_evolution_pat_appartenance.pdf", sep = "")
titre_save <- paste(repo_sorties, titre_save, sep ='/')
data_vagues <- vague_123
faire_rang = FALSE

try(graphique_evolution_position_vagues(vague_123 ,nb_quantiles, liste_type_patrimoines, titre_fig, titre_save, faire_rang))



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

try(graphique_evolution_pat_entre_vagues(vague_123, liste_type_patrimoines,liste_quantiles, titre, titre_save))

hist(vague_123$DA1000_V1)


################################################################################
# ========================= 05 ECONOMETRIE =====================================
################################################################################


############################################################################################################################### 
########################## DiD ESSAI = EFFET DU FAIT D'AVOIR RECU UN HERITAGE SUR LE FAIT D'ACHETER UNE HMR ###################
########################## G = 1   <====> Reçu un héritage à la vague 3 MAIS PAS à la vague 2  ################################
pop_initiale_tot <- copy(data_pays[VAGUE %in% c(2,3),])
nrow(pop_initiale_tot)

pop_initiale_tot[, Reg_Y := 0]
pop_initiale_tot[(DA1110I == 1 & VAGUE == 3) | (DA1110I == 1 & VAGUE == 2), Reg_Y := 1] ## La population qui ont une HMR
pop_initiale_tot$Reg_Y <- as.numeric(pop_initiale_tot$Reg_Y)
count(pop_initiale_tot[ Reg_Y == 1])


pop_initiale_tot[, Reg_G := 0]
SA0110_V3 <- vague_23[DOINHERIT_V3 == 1 & DOINHERIT_V2 == 0]$SA0110_V3 ## On récupère les identifiants ménages de ceux qui ont reçu un héritage entre la vague 2 et la vague 3
pop_initiale_tot[(SA0110 %in% SA0110_V3 & VAGUE == 3) | (SA0010 %in% SA0110_V3 & VAGUE == 2), Reg_G := 1]
pop_initiale_tot$Reg_G <- as.numeric(pop_initiale_tot$Reg_G)
count(pop_initiale_tot[ Reg_G == 1])


pop_initiale_tot[, Reg_T := 0]
pop_initiale_tot[VAGUE == 3, Reg_T := 1] ## La date
pop_initiale_tot$Reg_T <- as.numeric(pop_initiale_tot$Reg_T)
count(pop_initiale_tot[ Reg_T == 1])


pop_initiale_tot[, Reg_D := Reg_G * Reg_T]

liste_cols_reg <- c("Reg_Y", "Reg_G", "Reg_T", "Reg_D")
liste_cols_reg_poids <- c("Reg_Y", "Reg_G", "Reg_T", "Reg_D", "HW0010")
pop_initiale_tot[,..liste_cols_reg]

n <- count(pop_initiale_tot[Reg_G == 1])
if(n < length(nrow(pop_initiale_tot[Reg_G == 0]))){
  sous_pop_initiale <- pop_initiale_tot[Reg_G == 0][sample(1:nrow(pop_initiale_tot[Reg_G == 0]), n$n), ]
}else{
  sous_pop_initiale <- pop_initiale_tot[Reg_G == 0]
}
sous_pop_initiale <- rbindlist(list(sous_pop_initiale, pop_initiale_tot[Reg_G == 1]), fill=TRUE)

dw <- svydesign(ids = ~1, data = sous_pop_initiale[,..liste_cols_reg_poids], weights = ~ HW0010)
mysvyglm <- svyglm(formula = Reg_Y ~ Reg_G + Reg_D + Reg_T, design = dw)
summary(mysvyglm)

titre <- paste(pays,"_DD_2_heritage_achat.xlsx", sep = "")
titre <- paste(repo_sorties,titre, sep = "/")
write.xlsx(as.data.table(summary(mysvyglm)$coefficients, keep.rownames = TRUE), titre)


## Test de la common trend asumption sur les vagues 1 et 2
pop_test_hyp <- copy(data_pays[VAGUE %in% c(1,2),]) 

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
  liste_montant_initial <- lseq(100, 1000000, 250)
  data_loc <- copy(data_pays)
  recherche_p_value_otpi(liste_montant_initial, data_loc, annee_min = annee_min, annee_max = annee_max, faire_tracer = TRUE)
}


### Si on a le montant optimal

melted <- recherche_p_value_otpi(c(8000,9000,10000), copy(data_pays), annee_min = annee_min, annee_max = annee_max, faire_tracer = FALSE)
melted



############################################################################################################################### 
#################################### MATCHING ##################################

data_pays$DOINHERIT

liste_cols <- c("DOINHERIT", "DHAGEH1", "DHEDUH1", "DHGENDERH1", "DI2000", "DHHTYPE", "DA3001")
sous_data_loc <- data_pays[,..liste_cols]
setnames(sous_data_loc, "DA3001", "outcome")
setnames(sous_data_loc, "DOINHERIT", "treatment")

sous_data_loc$DHAGEH1 <- as.numeric(sous_data_loc$DHAGEH1)

# Sans matching
no_match <- matchit(treatment ~ DHAGEH1 + DHEDUH1 + DHGENDERH1+ DI2000 + DHHTYPE, data=sous_data_loc, method=NULL, distance = 'glm')
summary(no_match)



# Nearest neighbot matching
nearest_neighbor_match <- matchit(treatment ~ DHAGEH1 + DHEDUH1 + DHGENDERH1+ DI2000 + DHHTYPE, data=sous_data_loc, method="nearest", ratio=1, replace=F, distance = 'glm', caliper=0.2)
# Summary of nearest neighbor matching results
summary(nearest_neighbor_match, un = FALSE)
# Nearest neighbor matching is a type of greedy matching. It matches the nearest control at the moment, and remove the matched control from the rest of the matching.
# Nearest neighbor matching is fast but sensitive to the order of samples. It is not optimal for minimizing the total distance because the samples that are matched later in the process can only choose from the contorl units that have not been matched.




# Optimal matching ==> Marche beaucoup mieux mais prend du temps...
optimal_match <- matchit(treatment ~ DHAGEH1 + DHEDUH1 + DHGENDERH1+ DI2000 + DHHTYPE, data=sous_data_loc, method="optimal", ratio=1, replace=F, distance = 'glm')
# Summary of optimal matching results
summary(optimal_match, un = FALSE)
# Optimal matching is also called optimal pair matching. Different from greedy matching, optimal matching minimizes the total distance across all pairs.
# When there are not many close matches for the treatment group, optimal matching can be helpful for finding the best matches.




# Full matching
full_match <- matchit(treatment ~ DHAGEH1 + DHEDUH1 + DHGENDERH1+ DI2000 + DHHTYPE, data=sous_data_loc, method="full", ratio=1, replace=F, distance = 'glm')
# Summary of full matching results
summary(full_match, un = FALSE)
# It is called full matching because all the test and control units are assigned to a subclass and are utilized in the matching.
# It is optimal because the weighted average distances between the treated and control units in each subclass are minimized.
# Full matching outputs weights that are computed based on subclasses. The weights can work similar to propensity score weights and be used to estimate a weighted treatment effect.

full_match$match.matrix

plot(full_match, type = "jitter", interactive = FALSE)
plot(full_match, type = "density", interactive = FALSE,
     which.xs = ~  DI2000 + DHHTYPE)
plot(full_match, type = "density", interactive = FALSE,
     which.xs = ~  DHAGEH1)


plot(summary(full_match))


full_match_data <- match.data(full_match)
nrow(sous_data_loc) - nrow(full_match_data) ### Il ne manque que les unmatched !

# Quid de l'effet du traitement ?
fit <- lm(outcome ~ treatment * (DHAGEH1 + DHEDUH1 + DHGENDERH1+ DI2000 + DHHTYPE), data = full_match_data, weights = weights)

avg_comparisons(fit,
                variables = "treatment",
                vcov = ~subclass,
                newdata = subset(full_match_data, treatment == 1),
                wts = "weights")



# Genetic matching
generic_match <- matchit(treatment ~ DHAGEH1 + DHEDUH1 + DHGENDERH1+ DI2000 + DHHTYPE, data=sous_data_loc, method="genetic", pop.size=20)
# Summary of genetic matching
summary(generic_match, un = FALSE)
# Genetic matching uses a generic search algorithm to find weights for each covariate to achieve optimal balance. The current matching is with replacement and the balance is evaluated using t-tests and Kolmogorov-Smirnov tests.







# ############## On se place au montant minimum, et on regarde si on a des pbms de biais de sélection
# data_loc <- copy(data_pays)
# 
# montant_heritage_min <- 8000
# 
# sous_data <- data_loc[Annee_achat_heritage <= 98] ## Uniquement les ménages qui ONT reçu un héritage
# 
# sous_data[, Reg_Y := 0]
# sous_data[Annee_achat_heritage %in% annee_min:annee_max, Reg_Y := 1] # Ont acheté qq années après
# table(sous_data$Reg_Y)
# 
# 
# sous_data[, Reg_G := 0]
# sous_data[Montant_heritage_1 >= montant_heritage_min, Reg_G := 1] # Reçu un héritage conséquant
# table(sous_data$Reg_G)
# 
# 
# # DHAGEH1B = Tranche d'âge de la personne de référence
# # DHEDUH1 = Education de la personne de référence
# # DHGENDERH1 = Genre de la personne de référence
# # DI2000 = Revenu total du ménage
# # DHHTYPE = Type du ménage
# 
# liste_cols_reg_poids <- c("HW0010", "Reg_G", "DHAGEH1B", "DHEDUH1", "DHGENDERH1", "DI2000", "DHHTYPE")
# dw <- svydesign(ids = ~1, data = sous_data[,..liste_cols_reg_poids], weights = ~ HW0010)
# 
# # Régression linéaire
# mysvyglm <- svyglm(formula = Reg_G ~ DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE, design = dw)
# titre <- paste(pays,"_DD_3_preparation_heritage_consequant_achat_reg_lin_",as.character(montant_heritage_min),".xlsx", sep = "")
# titre <- paste(repo_sorties,titre, sep = "/")
# write.xlsx(as.data.table(summary(mysvyglm)$coefficients, keep.rownames = TRUE), titre)
# 
# # Logit
# denylogit <- glm(Reg_G ~ DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE, 
#                   family = binomial(link = "logit"), 
#                   data = sous_data)
# summary(denylogit)
# titre <- paste(pays,"_DD_3_preparation_heritage_consequant_achat_logit_",as.character(montant_heritage_min),".xlsx", sep = "")
# titre <- paste(repo_sorties,titre, sep = "/")
# write.xlsx(as.data.table(summary(denylogit)$coefficients, keep.rownames = TRUE), titre)
# 
# # Probit
# denyprobit <- glm(Reg_G ~ DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE, 
#                   family = binomial(link = "probit"), 
#                   data = sous_data)
# summary(denyprobit)
# titre <- paste(pays,"_DD_3_preparation_heritage_consequant_achat_probit_",as.character(montant_heritage_min),".xlsx", sep = "")
# titre <- paste(repo_sorties,titre, sep = "/")
# write.xlsx(as.data.table(summary(denyprobit)$coefficients, keep.rownames = TRUE), titre)
# 
# 
# 
# dt_reg_lin <- as.data.table(summary(mysvyglm)$coefficients, keep.rownames = TRUE)
# setnames(dt_reg_lin, "Pr(>|t|)", "pvalue")
# dt_logit <- as.data.table(summary(denylogit)$coefficients, keep.rownames = TRUE)
# setnames(dt_logit, "Pr(>|z|)", "pvalue")
# dt_probit <- as.data.table(summary(denyprobit)$coefficients, keep.rownames = TRUE)
# setnames(dt_probit, "Pr(>|z|)", "pvalue")
# 
# count(dt_reg_lin[pvalue < 0.01 & abs(Estimate) > 0.1])
# count(dt_logit[pvalue < 0.01 & abs(Estimate) > 0])
# count(dt_probit[pvalue < 0.01 & abs(Estimate) > 0])
# 
# 








# ### La clé a l'air d'être l'héritage
# data_loc <- copy(data_pays)
# 
# data_loc[(is.na(HB0700) & !is.na(Montant_heritage_1)), Annee_achat_heritage := - 99] # Pas d'achat mais un héritage
# data_loc[(!is.na(HB0700) & is.na(Montant_heritage_1)), Annee_achat_heritage :=  99] # Achat mais pas d'héritage
# data_loc[(!is.na(HB0700) & !is.na(Montant_heritage_1)), Annee_achat_heritage := HB0700 - Annee_heritage_1]
# 
# sous_data <- data_loc[Annee_achat_heritage <= 98] ## Uniquement les ménages qui ONT reçu un héritage
# 
# sous_data[, Reg_Y := 0]
# sous_data[Annee_achat_heritage %in% -1:3, Reg_Y := 1] # Ont acheté qq années après
# table(sous_data$Reg_Y)
# 
# 
# sous_data[, Reg_G := 0]
# sous_data[Montant_heritage_1 >= montant_heritage_min, Reg_G := 1] # Reçu un héritage conséquant
# table(sous_data$Reg_G)
# 
# ##### Est-ce qu'on a un problème de biais de sélection des traités ????
# 
# 
# # DHAGEH1B = Tranche d'âge de la personne de référence
# # DHEDUH1 = Education de la personne de référence
# # DHGENDERH1 = Genre de la personne de référence
# # DI2000 = Revenu total du ménage
# # DHHTYPE = Type du ménage
# # lm_her <- lm(Reg_G ~ DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE, data = sous_data)
# # summary(lm_her) ### A priori à part le salaire pas beaucoup de variables ne jouent sur le fait d'être traité ou non. Sauf sur le niveau d'éducation...
# 
# liste_cols_reg_poids <- c("HW0010", "Reg_G", "DHAGEH1B", "DHEDUH1", "DHGENDERH1", "DI2000", "DHHTYPE")
# dw <- svydesign(ids = ~1, data = sous_data[,..liste_cols_reg_poids], weights = ~ HW0010)
# mysvyglm <- svyglm(formula = Reg_G ~ DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE, design = dw)
# summary(mysvyglm)
# 
# titre <- paste(pays,"_DD_3_preparation_heritage_consequant_achat_",as.character(montant_heritage_min),".xlsx", sep = "")
# titre <- paste(repo_sorties,titre, sep = "/")
# write.xlsx(as.data.table(summary(mysvyglm)$coefficients, keep.rownames = TRUE), titre)
# 
# 
# # Mêmes conclusions avec logit et probit...
# denyprobit <- glm(Reg_G ~ DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE, 
#                   family = binomial(link = "logit"), 
#                   data = sous_data)
# summary(denyprobit)
# 
# 
# 
# ###### Est-ce que G est significatif pour prévoir Y ?
# 
# # lm_her <- lm(Reg_Y ~ Reg_G, data = sous_data)
# # summary(lm_her)
# 
# liste_cols_reg_poids <- c("HW0010", "Reg_Y", "Reg_G")
# dw <- svydesign(ids = ~1, data = sous_data[,..liste_cols_reg_poids], weights = ~ HW0010)
# mysvyglm <- svyglm(formula = Reg_Y ~ Reg_G, design = dw)
# summary(mysvyglm)
# 
# titre <- paste(pays,"_DD_3_reg_lin_heritage_consequant_achat_",as.character(montant_heritage_min),".xlsx", sep = "")
# titre <- paste(repo_sorties,titre, sep = "/")
# write.xlsx(as.data.table(summary(mysvyglm)$coefficients, keep.rownames = TRUE), titre)
# 
# denyprobit <- glm(Reg_Y ~ Reg_G, 
#                   family = binomial(link = "probit"), 
#                   data = sous_data)
# summary(denyprobit)
# 
# titre <- paste(pays,"_DD_3_probit_heritage_consequant_achat_",as.character(montant_heritage_min),".xlsx", sep = "")
# titre <- paste(repo_sorties,titre, sep = "/")
# write.xlsx(as.data.table(summary(denyprobit)$coefficients, keep.rownames = TRUE), titre)
# beta_0 <- as.numeric(denyprobit$coefficients[1])
# beta_g <- as.numeric(denyprobit$coefficients[2])
# 
# dnorm(beta_0 + beta_g, mean=1, sd=1)*beta_g
# dnorm(beta_0, mean=1, sd=1)*beta_g
# 
# 
# denyprobit <- glm(Reg_Y ~ Reg_G, 
#                   family = binomial(link = "logit"), 
#                   data = sous_data)
# summary(denyprobit)
# 
# titre <- paste(pays,"_DD_3_logit_heritage_consequant_achat_",as.character(montant_heritage_min),".xlsx", sep = "")
# titre <- paste(repo_sorties,titre, sep = "/")
# write.xlsx(as.data.table(summary(denyprobit)$coefficients, keep.rownames = TRUE), titre)
# beta_0 <- as.numeric(denyprobit$coefficients[1])
# beta_g <- as.numeric(denyprobit$coefficients[2])
# 
# f <- function(x){return(exp(-x)/(1+exp(-x))^2)}
# f(beta_0 + beta_g)*beta_g
# f(beta_0)*beta_g
# 
# 
# #### Conclusion :
# # G est significatif pour prédire Y
# # Seuls 2 modalités sur les 4 de la variable d'éducation sont significative pour prédire G à partir de variables socio-éco du ménage
# # Donc on peut se dire qu'on a identifié un effet causal ???
# 
# # Quid d'un éventuel biais de sélection ?
# 
# 











