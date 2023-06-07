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


num_table <- 1 ### Change les poids assignés par eurostat
num_vague_max <- 4 ### Le nombre de vague qu'on veut concaténer ATTENTION la dernière vague a des noms de colonnes en MAJUSCULE Ca pose pbm dans la concaténation...

################################################################################
# ============================ 02 IMPORTATION ==================================
################################################################################


source(paste(repo_prgm , "02_importation_data.R" , sep = "/"))
data_belgique <- data_complete[SA0100 == "BE"]


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


liste_var_continues <- c("HH0401", "HH0402", "HH0403", "HH0201", "HH0202", "HH0203", "HB0700","HW0010", "DA1000", "DA2100", "DA3001", "DI2000", "DI2000EQ", "DI2100", "DL1000", "DN3001", "DNFPOS", "DNHW", "DOGIFTINHER", "DA1120")
liste_var_categorielles <- c("DHHTYPE","DOINHERIT", "DA1110I","SA0110","SA0210", "SA0010" ,"DATOP10", "DHAGEH1", "DHEDUH1", "DHGENDERH1", "DHLIFESATIS", "DITOP10", "DLTOP10", "DNTOP10", "DOEINHERIT", "VAGUE", "SA0200", "DHAGEH1B")
liste_var_interet <- union(liste_var_continues, liste_var_categorielles)

intersection <- intersect(liste_var_interet, colnames(data_belgique))
if(length(setdiff(liste_var_interet, intersection)) > 0){
  print(paste("Attention les variables : ", setdiff(liste_var_interet, intersection), "ont été sélectionnées mais ne sont pas présentent dans la table initiale", sep = " "))
}
sous_data_belgique <- data_belgique[,..liste_var_interet]
sous_data_belgique <- sous_data_belgique %>% mutate_at(liste_var_continues, as.numeric)
sous_data_belgique <- sous_data_belgique %>% mutate_at(liste_var_categorielles, as.factor)

################################################################################
# ====================== 04 STAT DES & GRAPHIQUES ==============================
################################################################################
source(paste(repo_prgm , "04_graphiques.R" , sep = "/"))


# Patrimoine net
var_decile <- "DNTOP10"
titre <- "Répartition du patrimoine net des Belges, normalisé par sexe"
titre_save <- "Patrimoine_net_sexe.pdf"
titre_save <- paste(repo_sorties, titre_save, sep ='/')
xlabel <-"Quantile de patrimoine net"
facet <- "DHHTYPE" # On facet par type de ménage
var_normalisation <- c("DHGENDERH1","DHHTYPE") #On va mettre fill et normalisation par sexe
graphique_repartition_pat_quantile_sexe(sous_data_belgique[!is.na(get(var_decile))], var_decile,var_normalisation, titre, titre_save, xlabel, facet, xlim_sup = 25)

# Patrimoine brut
var_decile <- "DATOP10"
titre <- "Répartition du patrimoine brut des Belges, normalisé par sexe"
titre_save <- "Patrimoine_brut_sexe.pdf"
titre_save <- paste(repo_sorties, titre_save, sep ='/')
xlabel <-"Quantile de patrimoine brut"
facet <- "DHHTYPE"
var_normalisation <- c("DHGENDERH1","DHHTYPE") #On va mettre fill et normalisation par sexe
graphique_repartition_pat_quantile_sexe(sous_data_belgique[!is.na(get(var_decile))], var_decile,var_normalisation, titre, titre_save, xlabel, facet, xlim_sup = 25)

# Dettes
var_decile <- "DLTOP10"
titre <- "Répartition des dettes des Belges, normalisé par sexe"
titre_save <- "Patrimoine_dettes_sexe.pdf"
titre_save <- paste(repo_sorties, titre_save, sep ='/')
xlabel <-"Quantile de dettes"
facet <- "DHHTYPE"
var_normalisation <- c("DHGENDERH1","DHHTYPE") #On va mettre fill et normalisation par sexe
graphique_repartition_pat_quantile_sexe(sous_data_belgique[!is.na(get(var_decile))], var_decile,var_normalisation, titre, titre_save, xlabel, facet, xlim_sup = 25)




######## On regarde la concentration des différents types de patrimoines #############
nb_quantiles <- 100
liste_type_patrimoines <- c("DA3001" = "Patrimoine brut",
                            "DA1000" = "Patrimoine physique",
                            "DA2100" = "Patrimoine financier",
                            "DL1000" = "Dettes",
                            "DN3001" = "Patrimoine net")

titre_fig <- "Fonction de répartition de la richesse détenue par les ménages Belges"
titre_save <- "Concentration_patrimoine_par_type.pdf"
titre_save <- paste(repo_sorties, titre_save, sep ='/')


graphique_contration_patrimoine(sous_data_belgique ,nb_quantiles, liste_type_patrimoines, titre_fig, titre_save)
  



############################## Dispersion du patrimoine en fonction de l'âge
num_vague <- 2
liste_type_patrimoines <- c("DA3001" = "Patrimoine brut",
                            "DA1000" = "Patrimoine physique",
                            "DA2100" = "Patrimoine financier",
                            "DL1000" = "Dettes",
                            "DN3001" = "Patrimoine net")

data_loc <- sous_data_belgique[VAGUE == num_vague]
titre <- "Variance du patrimoine détenu par les ménages Belges\nIntervalles de confiance à 95%"
titre_save <- "variance_patrimoine.pdf"

graphique_variance_pat_age(data_loc, liste_type_patrimoines, titre, titre_save)




######################### Tracé : le date d'achat de la HMR - la date de l'héritage
sous_data_belgique <- Ajout_premier_heritage(sous_data_belgique)
# En fait on ne veut pas le premier héritage obtenu mais le premier héritage CONSEQUANT obtenu
montant_heritage_min <- 10000
sous_data_belgique <- Ajout_premier_heritage_cons(sous_data_belgique, montant_heritage_min)
sous_data_belgique[(!is.na(HB0700) & !is.na(Montant_heritage_1)), Annee_achat_heritage := HB0700 - Annee_heritage_1]


## Petit histogramme pour visualiser
sous_data_belgique[, label_education := factor(
  fcase(
    DHEDUH1 == 1, "< Brevet",
    DHEDUH1 == 2, "Brevet",
    DHEDUH1 == 3, "Bac",
    DHEDUH1 == 5, "> Bac"
  )
)
]



titre <- "Distribution de la variable :\ndate d'aquisition de la résidence principale actuelle - date du premier don ou héritage reçu"
xlabel <- "Année (coupé à 50) "
ylabel <- "Nombre d'occurence"
filllabel <- "Niveau d'éducation\nde la personne de\nréférence du ménage"
titre_save <- "Distrib_diff_annees_heritage_achat_detaille.pdf"
titre_save <- paste(repo_sorties, titre_save, sep ='/')
x <- "Annee_achat_heritage"
fill <- "label_education"
liste_breaks_fill <- c('< Brevet', 'Brevet', 'Bac', '> Bac')
data_loc <- sous_data_belgique
liste_breaks_x <- seq(-50, 50, 2)
limits_x <- c(-50,50)
trace_distrib_variable(data_loc, x, fill, xlabel, ylabel,filllabel, titre, titre_save, liste_breaks_fill, liste_breaks_x, limits_x)


titre <- "Distribution de la variable :\ndate d'aquisition de la résidence principale actuelle - date du premier don ou héritage reçu"
xlabel <- "Année (coupé à 50) "
ylabel <- "Nombre d'occurence"
filllabel <- NaN
titre_save <- "Distrib_diff_annees_heritage_achat_non_detaille.pdf"
titre_save <- paste(repo_sorties, titre_save, sep ='/')
x <- "Annee_achat_heritage"
fill <- NaN
liste_breaks_fill <- NaN
data_loc <- sous_data_belgique
liste_breaks_x <- seq(-50, 50, 2)
limits_x <- c(-50,50)
trace_distrib_variable(data_loc, x, NaN, xlabel, ylabel,NaN, titre, titre_save, liste_breaks_fill, liste_breaks_x, limits_x)





######### A-t-on bien des données de panel ?
list_output <- Creation_donnees_panel(sous_data_belgique)
vague_12 <- as.data.table(list_output[1])
vague_23 <- as.data.table(list_output[2])
vague_34 <- as.data.table(list_output[3])
vague_123 <- as.data.table(list_output[4])
vague_234 <- as.data.table(list_output[5])
vague_1234 <- as.data.table(list_output[6])


####### Positions dans la distribution du patrimoine
liste_type_patrimoines <- c("DA3001" = "Patrimoine brut",
                            "DN3001" = "Patrimoine net")
nb_quantiles <- 100

titre_fig <- "Evolution des rangs d'appartenance des ménages Belges entre les différentes vagues, en patrimoine net"
titre_save <- "evolution_rang_appartenance.pdf"
titre_save <- paste(repo_sorties, titre_save, sep ='/')


graphique_evolution_position_vagues(vague_123 ,nb_quantiles, liste_type_patrimoines, titre_fig, titre_save)
  


######### Evolution du patrimoine des ménages entre les vagues ##############
liste_type_patrimoines <- c("DA3001" = "Patrimoine brut",
                            "DA1000" = "Patrimoine physique",
                            "DA2100" = "Patrimoine financier",
                            "DL1000" = "Dettes",
                            "DN3001" = "Patrimoine net")

liste_quantiles <- seq(0.2, 0.8, 0.2)
titre <- "Quantiles de l'évolution du patrimoine des Belges entre les vagues"
titre_save <- "quantiles_evolution_richesse_panel.pdf"
titre_save <- paste(repo_sorties, titre_save, sep ='/')


graphique_evolution_pat_entre_vagues(vague_123, liste_type_patrimoines,liste_quantiles, titre, titre_save)


########################## DiD ESSAI N°1 = EFFET DU FAIT D'AVOIR RECU UN HERITAGE SUR LE FAIT D'ACHETER UNE HMR ###############
pop_initiale_tot <- copy(sous_data_belgique[VAGUE %in% c(2,3),]) ## On se place sur les vagues 2 et 3 pour pouvoir tester la common trend asump. sur vagues 1 et 2
nrow(pop_initiale_tot)

pop_initiale_tot[, Reg_Y := 0]
pop_initiale_tot[DA1110I == 1, Reg_Y := 1] ## La population qui ont une HMR
pop_initiale_tot$Reg_Y <- as.numeric(pop_initiale_tot$Reg_Y)

pop_initiale_tot[, Reg_G := 0]
pop_initiale_tot[DOINHERIT == 1, Reg_G := 1] ## La population reçoit le traitement = un héritage
pop_initiale_tot$Reg_G <- as.numeric(pop_initiale_tot$Reg_G)


pop_initiale_tot[, Reg_T := 0]
pop_initiale_tot[VAGUE == 3, Reg_T := 1] ## La date
pop_initiale_tot$Reg_T <- as.numeric(pop_initiale_tot$Reg_T)


pop_initiale_tot[, Reg_D := Reg_G * Reg_T]

liste_cols_reg <- c("Reg_Y", "Reg_G", "Reg_T", "Reg_D")
liste_cols_reg_poids <- c("Reg_Y", "Reg_G", "Reg_T", "Reg_D", "HW0010")
pop_initiale_tot[,..liste_cols_reg]

n <- count(pop_initiale_tot[Reg_G == 1])
# sous_pop_initiale <- as.data.table(sapply(pop_initiale_tot[Reg_G == 0], sample, n$n))
sous_pop_initiale <- pop_initiale_tot[Reg_G == 0][sample(1:nrow(pop_initiale_tot[Reg_G == 0]), n$n), ]
sous_pop_initiale <- rbindlist(list(sous_pop_initiale, pop_initiale_tot[Reg_G == 1]), fill=TRUE)

dw <- svydesign(ids = ~1, data = sous_pop_initiale[,..liste_cols_reg_poids], weights = ~ HW0010)

# dw <- svydesign(ids = ~1, data = pop_initiale_tot[,..liste_cols_reg_poids], weights = ~ HW0010)
mysvyglm <- svyglm(formula = Reg_Y ~ Reg_G + Reg_D + Reg_T, design = dw)
summary(mysvyglm) 

## Test de la common trend asumption sur les vagues 1 et 2
pop_test_hyp <- copy(sous_data_belgique[VAGUE %in% c(1,2),]) 

pop_test_hyp[, Reg_Y := 0]
pop_test_hyp[DA1110I == 1, Reg_Y := 1] ## La population qui ont une HMR
pop_test_hyp$Reg_Y <- as.numeric(pop_test_hyp$Reg_Y)

pop_test_hyp[, Reg_G := 0]
pop_test_hyp[DOINHERIT == 1, Reg_G := 1] ## La population reçoit le traitement = un héritage
pop_test_hyp$Reg_G <- as.numeric(pop_test_hyp$Reg_G)


pop_test_hyp[, Reg_T := -1]
pop_test_hyp[VAGUE == 2, Reg_T := 0] ## La date
pop_test_hyp$Reg_T <- as.numeric(pop_test_hyp$Reg_T)

dw <- svydesign(ids = ~1, data = pop_test_hyp, weights = ~ HW0010)


sous_dw <- subset(dw, Reg_G == 1 & Reg_T == 0) # Un sous-échantillon
# svymean(~Reg_Y, subset(dw, Reg_G == 1 & Reg_T == 0))[1]

svymean(~Reg_Y, subset(dw, Reg_G == 1 & Reg_T == 0))[1] - svymean(~Reg_Y, subset(dw, Reg_G == 1 & Reg_T == -1))[1]
svymean(~Reg_Y, subset(dw, Reg_G == 0 & Reg_T == 0))[1] - svymean(~Reg_Y, subset(dw, Reg_G == 0 & Reg_T == -1))[1]

table(pop_initiale_tot$Reg_D)



############################################################################################################################### 
########################## DiD ESSAI N°2 = EFFET DU FAIT D'AVOIR RECU UN HERITAGE SUR LE FAIT D'ACHETER UNE HMR ###############
########################## G = 1   <====> Reçu un héritage à la vague 3 MAIS PAS à la vague 2  ################################
pop_initiale_tot <- copy(sous_data_belgique[VAGUE %in% c(2,3),])
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
sous_pop_initiale <- pop_initiale_tot[Reg_G == 0][sample(1:nrow(pop_initiale_tot[Reg_G == 0]), n$n), ]
sous_pop_initiale <- rbindlist(list(sous_pop_initiale, pop_initiale_tot[Reg_G == 1]), fill=TRUE)

dw <- svydesign(ids = ~1, data = sous_pop_initiale[,..liste_cols_reg_poids], weights = ~ HW0010)
mysvyglm <- svyglm(formula = Reg_Y ~ Reg_G + Reg_D + Reg_T, design = dw)
summary(mysvyglm) 


## Test de la common trend asumption sur les vagues 1 et 2
pop_test_hyp <- copy(sous_data_belgique[VAGUE %in% c(1,2),]) 

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
### La clé a l'air d'être l'héritage
data_loc <- copy(sous_data_belgique)

data_loc[(is.na(HB0700) & !is.na(Montant_heritage_1)), Annee_achat_heritage := - 99] # Pas d'achat mais un héritage
data_loc[(!is.na(HB0700) & is.na(Montant_heritage_1)), Annee_achat_heritage :=  99] # Achat mais pas d'héritage
data_loc[(!is.na(HB0700) & !is.na(Montant_heritage_1)), Annee_achat_heritage := HB0700 - Annee_heritage_1]

sous_data <- data_loc[Annee_achat_heritage <= 98] ## Uniquement les ménages qui ONT reçu un héritage

sous_data[, Reg_Y := 0]
sous_data[Annee_achat_heritage %in% -1:5, Reg_Y := 1] # Ont acheté qq années après


Montant_minimal <- 35000
sous_data[, Reg_G := 0]
sous_data[Montant_heritage_1 >= Montant_minimal, Reg_G := 1] # Reçu un héritage conséquant

table(sous_data$Reg_G)
table(sous_data$Reg_Y)

##### Est-ce qu'on a un problème de biais de sélection des traités ????


# DHAGEH1B = Tranche d'âge de la personne de référence
# DHEDUH1 = Education de la personne de référence
# DHGENDERH1 = Genre de la personne de référence
# DI2000 = Revenu total du ménage
# DHHTYPE = Type du ménage
lm_her <- lm(Reg_G ~ DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE, data = sous_data)
summary(lm_her) ### A priori à part le salaire pas beaucoup de variables ne jouent sur le fait d'être traité ou non. Sauf sur le niveau d'éducation...

# Mêmes conclusions avec logit et probit...
denyprobit <- glm(Reg_G ~ DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE, 
                  family = binomial(link = "logit"), 
                  data = sous_data)
summary(denyprobit)



###### Est-ce que G est significatif pour prévoir Y ?

lm_her <- lm(Reg_Y ~ Reg_G, data = sous_data)
summary(lm_her)

liste_cols_reg_poids <- c("HW0010", "Reg_Y", "Reg_G")
dw <- svydesign(ids = ~1, data = sous_data[,..liste_cols_reg_poids], weights = ~ HW0010)
mysvyglm <- svyglm(formula = Reg_Y ~ Reg_G, design = dw)
summary(mysvyglm) 

denyprobit <- glm(Reg_Y ~ Reg_G, 
                  family = binomial(link = "probit"), 
                  data = sous_data)
summary(denyprobit)
beta_0 <- as.numeric(denyprobit$coefficients[1])
beta_g <- as.numeric(denyprobit$coefficients[2])

dnorm(beta_0 + beta_g, mean=1, sd=1)*beta_g
dnorm(beta_0, mean=1, sd=1)*beta_g


denyprobit <- glm(Reg_Y ~ Reg_G, 
                  family = binomial(link = "logit"), 
                  data = sous_data)
summary(denyprobit)
beta_0 <- as.numeric(denyprobit$coefficients[1])
beta_g <- as.numeric(denyprobit$coefficients[2])

f <- function(x){return(exp(-x)/(1+exp(-x))^2)}
f(beta_0 + beta_g)*beta_g
f(beta_0)*beta_g


#### Conclusion :
# G est significatif pour prédire Y
# Seuls 2 modalités sur les 4 de la variable d'éducation sont significative pour prédire G à partir de variables socio-éco du ménage
# Donc on peut se dire qu'on a identifié un effet causal ???

# Quid d'un éventuel biais de sélection ?










hist(sous_data_belgique$HB0700 - sous_data_belgique$Annee_heritage_1_cons, breaks=50)
### La clé a l'air d'être l'héritage
data_loc <- copy(sous_data_belgique)

data_loc[(is.na(HB0700) & !is.na(Annee_heritage_1_cons)), Annee_achat_heritage := - 99] # Pas d'achat mais un héritage
data_loc[(!is.na(HB0700) & is.na(Annee_heritage_1_cons)), Annee_achat_heritage :=  99] # Achat mais pas d'héritage
data_loc[(!is.na(HB0700) & !is.na(Annee_heritage_1_cons)), Annee_achat_heritage := HB0700 - Annee_heritage_1_cons]

table(data_loc$Annee_achat_heritage)
hist(data_loc$Annee_achat_heritage, breaks = 50)


data_loc[, Reg_G := 0]
data_loc[Annee_achat_heritage %in% -1:4, Reg_G := 1]
n <- count(data_loc[Reg_G == 1])
table(data_loc[Reg_G == 0]$DHAGEH1B)

sous_pop_initiale <- data_loc[Reg_G == 0][sample(1:nrow(data_loc[Reg_G == 0]), n$n), ]
sous_pop_initiale <- rbindlist(list(sous_pop_initiale, data_loc[Reg_G == 1]), fill=TRUE)


# DHAGEH1B = Tranche d'âge de la personne de référence
# DHEDUH1 = Education de la personne de référence
# DHGENDERH1 = Genre de la personne de référence
# DI2000 = Revenu total du ménage
# DHHTYPE = Type du ménage
lm_her <- lm(DOINHERIT ~ DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE, data = data_loc)
summary(lm_her) ### A priorià part le salaire pas beaucoup de variables ne jouent sur le fait d'être traité ou non ==> On a peu de biais de variables ommises ???


# 
# liste_cols_importantes <- c("DHAGEH1B", "DHEDUH1", "DHGENDERH1", "DI2000", "DHHTYPE")
# X_tot <- sous_pop_initiale[,..liste_cols_importantes]
# Y_tot <- sous_pop_initiale$Reg_G
# 
# 
# 
# data_loc$DOINHERIT <- as.numeric(data_loc$DOINHERIT)
# data_loc$DA1110I <- as.numeric(data_loc$DA1110I)
# 
# lm_her <- lm(DOINHERIT ~ DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE, data = data_loc)
# summary(lm_her) ### A priorià part le salaire pas beaucoup de variables ne jouent sur le fait d'être traité ou non ==> On a peu de biais de variables ommises ???
# 
# 
# # DA1110I= Fait d'être proprio
# # DOINHERIT = Fait d'avoir hérité
# 
# lm_caus <- lm(DA1110I ~ DOINHERIT, data = data_loc)
# summary(lm_caus)
# 
# 
# 
# dtrain <- xgb.DMatrix(data = sous_pop_initiale[,..liste_cols_importantes], label = sous_pop_initiale$Reg_G)
# bstSparse <- xgboost(data = X_tot, label = Y_tot, max.depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")
# 




# sous_data_belgique$Annee_heritage_1_cons
# 
# sous_data_belgique[is.na(HH0202) & is.na(HH0203), Annee_heritage_1_cons := HH0201] # S'il n'y a qu'un ou aucun héritage, alors on ne met que lui, ou on met NAN
# sous_data_belgique[ pmax(HH0401, HH0402, HH0403, na.rm = TRUE) < montant_heritage_min , Annee_heritage_1_cons := pmin(HH0201, HH0202, HH0203, na.rm = TRUE)] # Si aucun héritage ne dépasse le montant mini alors on met le premier héritage dans le temps
# # A partir de là ça veut dire qu'il existe au moins un héritage qui dépasse le montant !
# 
# 
# 
# sous_data_belgique[pmin(HH0201, HH0202, HH0203, na.rm = TRUE) == HH0201 & HH0401 >= montant_heritage_min, Annee_heritage_1_cons := HH0201] # Si le premier héritage premier alors on met l'année du premier héritage
# sous_data_belgique[pmin(HH0201, HH0202, HH0203, na.rm = TRUE) == HH0202 & HH0402 >= montant_heritage_min, Annee_heritage_1_cons := HH0202] # Si le plus grand héritage est le deuxième...
# sous_data_belgique[pmin(HH0201, HH0202, HH0203, na.rm = TRUE) == HH0203 & HH0403 >= montant_heritage_min, Annee_heritage_1_cons := HH0203] # Si le plus grand héritage est le troisième...
# 
# 
# 
# 
# sous_data_belgique[ pmax(HH0401, HH0402, HH0403, na.rm = TRUE) == HH0402 & HH0402 >= montant_heritage_min , Annee_heritage_1_cons := HH0202] # Si le plus grand héritage est le deuxième alors on met l'année du deuxième héritage
# sous_data_belgique[ pmax(HH0401, HH0402, HH0403, na.rm = TRUE) == HH0403 & HH0403 >= montant_heritage_min , Annee_heritage_1_cons := HH0203] # Si le plus grand héritage est le troisième alors on met l'année du troisième héritage
# count(sous_data_belgique[ pmax(HH0401, HH0402, HH0403, na.rm = TRUE) == HH0401 & HH0401 >= montant_heritage_min,])
# count(sous_data_belgique[ pmax(HH0401, HH0402, HH0403, na.rm = TRUE) == HH0402 & HH0402 >= montant_heritage_min,])
# count(sous_data_belgique[ pmax(HH0401, HH0402, HH0403, na.rm = TRUE) == HH0403 & HH0403 >= montant_heritage_min,])
# 
# sous_data_belgique[, Montant_heritage_1_cons := factor(
#   fcase(
#     Annee_heritage_1_cons == HH0201, HH0401,
#     Annee_heritage_1_cons == HH0202, HH0402,
#     Annee_heritage_1_cons == HH0203, HH0403
#   )
# )
# ]
# sous_data_belgique$Montant_heritage_1_cons <- as.numeric(as.character(sous_data_belgique$Montant_heritage_1_cons)) ### ATTENTION laisser le as.character sinon ça bug je sais pas pourquoi...
# hist(log10(sous_data_belgique$Montant_heritage_1_cons), breaks = 50)
# 
# table(sous_data_belgique$Montant_heritage_1_cons)
# 
# all(Montant_heritage_1_cons_old == sous_data_belgique$Montant_heritage_1_cons, na.rm = TRUE)
# 
# Montant_heritage_1_cons_old <- sous_data_belgique$Montant_heritage_1_cons
# 
# sous_data_belgique[]
# 
# head(sous_data_belgique$Heritage_1, 30)
# head(sous_data_belgique$HH0201, 30)
# head(sous_data_belgique$HH0202, 30)
# head(sous_data_belgique$HH0201, 30)
# 
# 
# head(sous_data_belgique[sous_data_belgique$Heritage_1 != sous_data_belgique$HH0201]$Heritage_1, 10)
# head(sous_data_belgique[sous_data_belgique$Heritage_1 != sous_data_belgique$HH0201]$HH0201, 10)
# head(sous_data_belgique[sous_data_belgique$Heritage_1 != sous_data_belgique$HH0201]$HH0401, 10)
# head(sous_data_belgique[sous_data_belgique$Heritage_1 != sous_data_belgique$HH0201]$HH0202, 10)
# head(sous_data_belgique[sous_data_belgique$Heritage_1 != sous_data_belgique$HH0201]$HH0402, 10)
# 
# sous_data_belgique[, Heritage_1 := sort(HH0201, HH0202, HH0203, na.rm = TRUE, partial = 3)]
# 
# 
# min(sous_data_belgique$HH0201, sous_data_belgique$HH0202, sous_data_belgique$HH0203, na.rm = TRUE)
# 
# 
# sum(table(sous_data_belgique$HH0201))
# sum(table(sous_data_belgique$HH0202))
# sum(table(sous_data_belgique$HH0203))
# 
# hist(sous_data_belgique$HB0700 - sous_data_belgique$HH0201, breaks=50)
# hist(sous_data_belgique$HB0700 - sous_data_belgique$Heritage_1, breaks=50)
# table(sous_data_belgique$HB0700 - sous_data_belgique$HH0201)
# 
# 
# hist(log10(sous_data_belgique$DOGIFTINHER), breaks = 50)

# names(data_for_plot)[names(data_for_plot) %like% "1-2"]
# names(data_for_plot)[names(data_for_plot) %like% "2-3"]

# vague_123$DA1000_V12 <- vague_123$DA1000_V2 - vague_123$DA1000_V1
# vague_123$DA1000_V23 <- vague_123$DA1000_V3 - vague_123$DA1000_V2

# 
# length(diff_V12)
# 
# 
# liste_valeurs_quantiles <- as.data.frame(diff_DA1000_V12)
# 
# diff_DA1000_V12 <- quantile(vague_123$DA1000_V2 - vague_123$DA1000_V1, probs = liste_quantiles)
# diff_DA1000_V23 <- quantile(vague_123$DA1000_V3 - vague_123$DA1000_V2, probs = liste_quantiles)
# 
# 
# diff_DA1000_V12[[2]]
# 






# diff_V12 <- as.data.table(quantile(vague_123[['DA1000_V2']] - vague_123[['DA1000_V1']], probs = liste_quantiles))
# setnames(diff_V12, "V1", "DA1000_V12")
# diff_V12[, liste_quantiles := liste_quantiles]
# 
# diff_V23 <- as.data.table(quantile(vague_123[['DA1000_V3']] - vague_123[['DA1000_V2']], probs = liste_quantiles))
# setnames(diff_V23, "V1", "DA1000_V23")
# diff_V23[, liste_quantiles := liste_quantiles]
# 
# merge(data_for_plot, diff_V12, by = "liste_quantiles")



### BROUILLON

# data_for_plot
# 
# 
# sous_sous_data_belgique <- copy(sous_data_belgique)
# sous_sous_data_belgique[is.na(DA1120)]$DA1120 <- 0
# 
# sous_sous_data_belgique[,Quantiles := cut(DA1120,
#                                      breaks= unique(quantile(DA1120, probs=seq(0, 1, by=1/100), na.rm=T)),
#                                      include.lowest= TRUE)]
# 
# longueur <- length(table(sous_sous_data_belgique$Quantiles))
# 
# sous_sous_data_belgique[,Quantiles := cut(DA1120,
#                                           breaks= unique(quantile(DA1120, probs=seq(0, 1, by=1/100), na.rm=T)),
#                                           include.lowest= TRUE)]
# 
# sous_sous_data_belgique
# 
# setorder(sous_sous_data_belgique, Quantiles)
# length(table(sous_sous_data_belgique$Quantiles))



# age <- 16
# data_loc <- sous_data_belgique[DHAGEH1B == age,]
# view(data_loc)
# dw_loc <- svydesign(ids = ~1, data = data_loc, weights = ~ data_loc$HW0010)
# svyvar(~DA3001, design = dw_loc)
# nrow(data_loc)

################################################## Un peu d'héritage...

hist(log(sous_data_belgique$DOGIFTINHER), breaks=50, col="red")

sous_data_belgique

sous_data_belgique[, cor(DA3001, DI2000), by = VAGUE]


dw <- svydesign(ids = ~1, data = data_belgique, weights = ~ data_belgique$HW0010)

correlation_vague <- 1:num_vague_max
for(num_vague in 1:num_vague_max){
  data_loc <- sous_data_belgique[VAGUE == num_vague,]
  dw_loc <- svydesign(ids = ~1, data = data_loc, weights = ~ data_loc$HW0010)
  correlation_vague[num_vague] <- svycor(~DA3001 + DI2000, design = dw_loc)$cors[1,2]
}

correlation_vague
sous_data_belgique[, cor(DA3001, DI2000), by = VAGUE]



class(svycor(~DA3001 + DI2000, design = dw))

svycor(~DA3001 + DI2000, design = dw)$cors[1,2]



################################################## Exploration avec le paclage 
# 
# dw$variables$DN3001 # Pour retrouver le contenu d'une colonne
# 
# svymean(~DN3001, dw) # Sur une variable continue
# svyquantile(~DN3001, dw, quantile = c(0.25, 0.5, 0.75), ci = TRUE) # Quantiles + leurs IC_a sur une variable continue
# 
# svytable(~DHLIFESATIS, dw) # Sur une variable catégorielle
# 
# svytable(~ DHLIFESATIS + DHGENDERH1, dw) # Sur deux variables catégorielles
# 
# tab <- svytable(~DHLIFESATIS + DHGENDERH1, dw) # Pour avoir une fréquence sur une variable
# lprop(tab, total = TRUE) #Pour avoir les %
# 
# 
# ggplot(dw$variables) +
#   aes(weight = weights(dw), x = DHGENDERH1, fill = DOEINHERIT) +
#   geom_bar(position = "fill")
# 
# ggplot(data_belgique) +
#   aes(x = DHGENDERH1, fill = DOEINHERIT) +
#   geom_bar(position = "fill")
# 
# table(data_belgique$DOEINHERIT)


################################################## Pour étudier les différences entre les différentes versions
# 
# data_complete_1 <- copy(data_complete)
# 
# num_table <- 2 ### Change les poids assignés par eurostat
# source(paste(repo_prgm , "02_importation_data.R" , sep = "/"))
# data_complete_2 <- copy(data_complete)
# 
# 
# num_table <- 1 ### Change les poids assignés par eurostat
# source(paste(repo_prgm , "02_importation_data.R" , sep = "/"))
# head(data_complete$HW0010,100)
# 
# table(data_complete_1[data_complete_1$DN3001 != data_complete_2$DN3001])
# 
# data_complete_1[data_complete_1$SA0010  != data_complete_2$SA0010]
# 
# table(data_complete_1  == data_complete_2)
# 
# head(data_complete_1[data_complete_1$DOEINHERIT  != data_complete_2$DOEINHERIT ]$DOEINHERIT , 10)
# head(data_complete_2[data_complete_1$DOEINHERIT  != data_complete_2$DOEINHERIT ]$DOEINHERIT , 10)
# 
# nrow(data_complete_1)*length(colnames(data_complete_1))
# 2277054 + 37663527




########## A discuter : 
# Les changements entre les fichiers ne viennent pas des poids mais de certaines valeurs. J'imagine les valeurs imputées ?
# En tout cas elles ont l'air de relativement peu changer entre les différents fichiers, de l'ordre de qq % en général
# Etats des lieux : Il existe au moins une différence entre les deux premières imputations dans 2277054/59424064 = 4% des cases où il y a du changement
# Pour obtenir ce chiffre = table(data_complete_1  == data_complete_2) effectifs FALSE/nrow(data_complete_1)*length(colnames(data_complete_1))



















