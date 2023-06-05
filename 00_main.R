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
num_vague_max <- 4 ### Le nombre de vague qu'on veut concaténer ATTENTION la dernière vague a des noms de colonnes en MINUSCULE. Ca pose pbm dans la concaténation...

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


liste_var_continues <- c("HH0201", "HH0202", "HH0203", "HB0700","HW0010", "DA1000", "DA2100", "DA3001", "DI2000", "DI2000EQ", "DI2100", "DL1000", "DN3001", "DNFPOS", "DNHW", "DOGIFTINHER", "DA1120")
liste_var_categorielles <- c("DOINHERIT", "DA1110I","SA0110","SA0210", "SA0010" ,"DATOP10", "DHAGEH1", "DHEDUH1", "DHGENDERH1", "DHLIFESATIS", "DITOP10", "DLTOP10", "DNTOP10", "DOEINHERIT", "VAGUE", "SA0200", "DHAGEH1B")
liste_var_interet <- union(liste_var_continues, liste_var_categorielles)

intersection <- intersect(liste_var_interet, colnames(data_belgique))
if(length(setdiff(liste_var_interet, intersection)) > 0){
  print(paste("Attention les variables : ", setdiff(liste_var_interet, intersection), "ont été sélectionnées mais ne sont pas présentent dans la table initiale", sep = " "))
}
sous_data_belgique <- data_belgique[,..liste_var_interet]
sous_data_belgique <- sous_data_belgique %>% mutate_at(liste_var_continues, as.numeric)
sous_data_belgique <- sous_data_belgique %>% mutate_at(liste_var_categorielles, as.factor)

# sous_data_belgique
# summary(sous_data_belgique$HW0010)

################################################################################
# ====================== 04 STAT DES & GRAPHIQUES ==============================
################################################################################
source(paste(repo_prgm , "04_graphiques.R" , sep = "/"))



liste_var_groupby <- c("DHGENDERH1", "DATOP10")
var_normalisation <- 'DHGENDERH1' # Par quelle variable est-ce qu'on normalise ? Généralement le sexe

dots <- lapply(var_normalisation, as.symbol) #Penser à bien convertir pour ne pas avoir de problèmes...
data_for_plot <- sous_data_belgique[, sum(HW0010), by = liste_var_groupby] #On calcule les effectifs
data_for_plot <- data_for_plot %>% group_by(.dots = dots) %>% mutate(new = 100*V1/sum(V1)) # Pour la normalisation il faut faire attention à grouper par sexe
data_for_plot <- as.data.table(data_for_plot)
data_for_plot

data_for_plot[, Sexe:= factor(
  fcase(
    DHGENDERH1 == 1, "Homme",
    DHGENDERH1 == 2, "Femme"
  )
)
]

### Puis le tracé
titre <- "Répartition du patrimoine total des Belges, normalisé par sexe"
titre_save <- "Patrimoine_sexe.pdf"
titre_save <- paste(repo_sorties, titre_save, sep ='/')
x <-"DATOP10"
sortby_x <- "DATOP10"
y <- "new"
fill <- "Sexe"
xlabel <-"Quantile de patrimoine brut"
ylabel <-"% de la population belge"
data_loc <- data_for_plot
xlim_sup <- 15 #La limite en % sur l'axe x

trace_barplot(data_loc, x, sortby_x, y, fill, xlabel, ylabel, titre, titre_save, xlim_sup)
  


# ### Exploration supplémentaire
# data_for_plot <- sous_data_belgique[, sum(HW0010), by = DHGENDERH1]
# data_for_plot

######## On regarde la concentration des différents types de patrimoines #############

nb_quantiles <- 100
liste_type_patrimoines <- c("DA3001" = "Total patrimoine",
                            "DA1000" = "Patrimoine physique",
                            "DA2100" = "Patrimoine financier",
                            "DL1000" = "Dettes",
                            "DN3001" = "Richesse totale")

# liste_labels as.data.frame(liste_type_patrimoines)$liste_type_patrimoines

# On initialise
data_for_plot <- as.data.table(1:nb_quantiles)
setnames(data_for_plot, 'V1', "Quantiles")
data_for_plot$Quantiles <- as.numeric(data_for_plot$Quantiles)
data_for_plot

# On boucle sur les types de patrimoines
for(type_pat in names(liste_type_patrimoines)){
  
  # Récupération des quantiles
  sous_data_belgique[,Quantiles := cut(sous_data_belgique[[type_pat]],
                                       breaks=quantile(sous_data_belgique[[type_pat]], probs=seq(0, 1, by=1/nb_quantiles), na.rm=T),
                                       include.lowest= TRUE, labels=1:nb_quantiles)]
  
  # Traitement pour pouvoir merge
  data_for_plot_loc <- sous_data_belgique[, mean(get(type_pat)), by = Quantiles]
  setnames(data_for_plot_loc, "V1", liste_type_patrimoines[[type_pat]])
  data_for_plot_loc$Quantiles <- as.numeric(data_for_plot_loc$Quantiles)
  
  # Merge
  data_for_plot <- merge(data_for_plot, data_for_plot_loc, by = "Quantiles")
}

# Calcul de la cumsum
for(type_pat in liste_type_patrimoines){
  data_for_plot[, eval(type_pat) := 100*cumsum(get(type_pat))/sum(get(type_pat))]
  
}

# Melt pour pouvoir tracer
melted <- melt(data_for_plot, 
               id.vars = "Quantiles", 
               measure.vars  = as.data.frame(liste_type_patrimoines)$liste_type_patrimoines,
               variable.name = "variable",
               value.name    = "value")


titre_fig <- "Fonction de répartition de la richesse détenue par les ménages Belges"
titre_save <- "Concentration_patrimoine_par_type.pdf"
titre_save <- paste(repo_sorties, titre_save, sep ='/')
x <- "Quantiles"
y <- "value"
color <- "variable"
xlabel <- "Part des ménages"
ylabel <- "Richesse détenue (cumulatif)"
colorlabel <- "Type de richesse"
data_melted_loc <- melted

trace_concentration(data_melted_loc, x, y, color, xlabel, ylabel,colorlabel, titre_fig, titre_save)



############### Dispersion du patrimoine en fonction de l'âge

liste_type_patrimoines <- c("DA3001" = "Total patrimoine",
                            "DA1000" = "Patrimoine physique",
                            "DA2100" = "Patrimoine financier",
                            "DL1000" = "Dettes",
                            "DN3001" = "Richesse totale")

table(sous_data_belgique$DHAGEH1B) #C'est la borne inférieure de l'âge qui est indiquée

data_for_plot <- sous_data_belgique[, sum(HW0010), by = DHAGEH1B]
setnames(data_for_plot, "DHAGEH1B", "liste_ages")
setnames(data_for_plot, "V1", "Effectifs")
for(type_pat in names(liste_type_patrimoines)){
  liste_ages <- sort(unique(sous_data_belgique$DHAGEH1B))
  liste_variances <- 1:length(liste_ages)
  
  for(num_age in 1:length(liste_ages)){
    age <- liste_ages[num_age]
    data_loc <- sous_data_belgique[DHAGEH1B == age & VAGUE != 4,]
    if(nrow(data_loc) >= 2){
      dw_loc <- svydesign(ids = ~1, data = data_loc, weights = ~ data_loc$HW0010)
      liste_variances[num_age] <- svyvar(~get(type_pat), design = dw_loc, na.rm=TRUE)[1]
    }else{
      liste_variances[num_age] <- 0
    }
  }
  
  data_for_plot_loc <- data.frame(liste_ages,liste_variances)
  data_for_plot_loc <- as.data.table(data_for_plot_loc)
  setnames(data_for_plot_loc, "liste_variances", liste_type_patrimoines[type_pat])
  data_for_plot <- merge(data_for_plot_loc, data_for_plot, by = "liste_ages")
}



# Melt pour pouvoir tracer
melted <- melt(data_for_plot, 
               id.vars = "liste_ages", 
               measure.vars  = as.data.frame(liste_type_patrimoines)$liste_type_patrimoines,
               variable.name = "variable",
               value.name    = "value")

titre <- "Variance de la richesse détenue par les ménages Belges"
titre_save <- "variance_patrimoine.pdf"
titre_save <- paste(repo_sorties, titre_save, sep ='/')
x <-"liste_ages"
sortby_x <- "liste_ages"
y <- "value"
fill <- "variable"
xlabel <-"Tranche d'âge de la personne de référence du ménage"
ylabel <-"Variance de la richesse (échelle log)"
filllabel <- "Type de richesse"
data_loc <- melted[variable != "Effectifs"]

trace_barplot_log(data_loc, x, y, fill, xlabel, ylabel,filllabel, titre, titre_save)
  

######### A-t-on bien des données de panel ?
# SA0200 = Survey vintage
# SA0010 = household identification number
# SA0210 = Vintage of last interview (household)
# SA0110 = Past household ID
### Pour faire l'évolution il faut commencer par mettre à 0 les patrimoines NaN ################## A REMONTER LUNDI ###################
sous_data_belgique[is.na(DA1000), DA1000 := 0]
sous_data_belgique[is.na(DA2100), DA2100 := 0]
sous_data_belgique[is.na(DL1000), DL1000 := 0]


vague_1 <- sous_data_belgique[VAGUE == 1,] # On récupère les vagues
vague_2 <- sous_data_belgique[VAGUE == 2,]
vague_3 <- sous_data_belgique[VAGUE == 3,]
vague_4 <- sous_data_belgique[VAGUE == 4,]

colnames(vague_1) <- paste(colnames(vague_1),"V1",sep="_") # On renome pour ne pas avoir de conflits
colnames(vague_2) <- paste(colnames(vague_2),"V2",sep="_")
colnames(vague_3) <- paste(colnames(vague_3),"V3",sep="_")
colnames(vague_4) <- paste(colnames(vague_4),"V4",sep="_")


vague_12 <- merge(x = vague_2, y = vague_1, by.x = 'SA0110_V2',by.y = 'SA0010_V1')
vague_123 <- merge(x = vague_3, y = vague_12, by.x = 'SA0110_V3',by.y = 'SA0010_V2') ## On s'arrête là parce qu'aucun ménage n'est enquêté 4x
vague_1234 <- merge(x = vague_4, y = vague_123, by.x = 'SA0110_V4',by.y = 'SA0010_V3')

vague_23 <- merge(x = vague_3, y = vague_2, by.x = 'SA0110_V3',by.y = 'SA0010_V2')
vague_234 <- merge(x = vague_4, y = vague_23, by.x = 'SA0110_V4',by.y = 'SA0010_V3') ## PERSONNE N'EST EN PANEL A LA VAGUE 4 ?????


### Pour vérifier on regarde les différences d'âges de la personne de référence du ménage
## 2010      2014      2017 les vagues
diff_12 <- as.numeric(vague_123$DHAGEH1_V2) - as.numeric(vague_123$DHAGEH1_V1)
diff_23 <- as.numeric(vague_123$DHAGEH1_V3) - as.numeric(vague_123$DHAGEH1_V2)


table(diff_12)
table(diff_23)


######### Evolution du patrimoine des ménages entre les vagues ##############
liste_type_patrimoines <- c("DA3001" = "Total patrimoine",
                            "DA1000" = "Patrimoine physique",
                            "DA2100" = "Patrimoine financier",
                            "DL1000" = "Dettes",
                            "DN3001" = "Richesse totale")

liste_quantiles <- seq(0.2, 0.8, 0.2)

data_for_plot <- as.data.table(liste_quantiles)

for(type_pat in names(liste_type_patrimoines)){
  diff_V12 <- as.data.table(quantile(vague_123[[paste(type_pat, "V2", sep = "_")]] - vague_123[[paste(type_pat, "V1", sep = "_")]], probs = liste_quantiles))
  setnames(diff_V12, "V1", "Vagues 1-2")
  diff_V12[, liste_quantiles := liste_quantiles]
  
  diff_V23 <- as.data.table(quantile(vague_123[[paste(type_pat, "V3", sep = "_")]] - vague_123[[paste(type_pat, "V2", sep = "_")]], probs = liste_quantiles))
  setnames(diff_V23, "V1", "Vagues 2-3")
  diff_V23[, liste_quantiles := liste_quantiles]
  
  data_for_plot_loc <- merge(diff_V12, diff_V23, by = "liste_quantiles")
  
  data_for_plot_loc <- melt(data_for_plot_loc, 
                 id.vars = "liste_quantiles", 
                 measure.vars  = c("Vagues 2-3", "Vagues 1-2"),
                 variable.name = "Vague",
                 value.name    = "Difference")

  data_for_plot_loc[, Type_patrimoine := liste_type_patrimoines[type_pat]]
  
  data_for_plot <- rbindlist(list(data_for_plot,
                                  data_for_plot_loc), fill=TRUE)
}

data_for_plot <- na.omit(data_for_plot)
data_for_plot[Vague == "Vagues 2-3", Numero_vague := 2]
data_for_plot[Vague == "Vagues 1-2", Numero_vague := 1]

data_for_plot$Type_patrimoine <- as.factor(data_for_plot$Type_patrimoine)
data_for_plot$Vague <- as.factor(data_for_plot$Vague)
data_for_plot$liste_quantiles <- as.factor(data_for_plot$liste_quantiles)


data_for_plot <- ff_interaction(data_for_plot, Type_patrimoine, liste_quantiles)


titre <- "Quantiles de l'évolution de la richesse des Belges entre les vagues"
titre_save <- "quantiles_evolution_richesse_panel.pdf"
titre_save <- paste(repo_sorties, titre_save, sep ='/')
x <-"Vague"
sortby_x <- "Numero_vague"
y <- "Difference"
xlabel <-"Vague"
ylabel <-"Différence entre les vagues"
data_loc <- data_for_plot
fill <- "Type_patrimoine"
ligne <- "Type_patrimoine_liste_quantiles"
shape <- "liste_quantiles"

p <- ggplot(data = data_loc, aes(x = reorder(.data[[x]], .data[[sortby_x]]), y = .data[[y]], color = .data[[fill]], shape = .data[[shape]], group = .data[[ligne]])) +
  geom_point(size=2) +
  geom_line() +
  labs(title=titre,
       x= xlabel,
       y= ylabel) 

ggsave(titre_save, p ,  width = 297, height = 210, units = "mm")



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
pop_initiale_tot[,..liste_cols_reg]


did_reg <- lm(Reg_Y  ~ Reg_G + Reg_D + Reg_T , data = pop_initiale_tot[,..liste_cols_reg])

summary(did_reg)

summary(did_reg)$coefficients["Reg_D", "Pr(>|t|)"] #Pour récupérer la pvalue du coeff


############ Ajout de poids
liste_cols_reg <- c("Reg_Y", "Reg_G", "Reg_T", "Reg_D", "HW0010")
pop_initiale_tot[,..liste_cols_reg]
dw <- svydesign(ids = ~1, data = pop_initiale_tot[,..liste_cols_reg], weights = ~ HW0010)

mysvyglm <- svyglm(formula = Reg_Y ~ Reg_G + Reg_D + Reg_T, design = dw)
summary(mysvyglm) # Ca change pas grand chose... Ca dégrade même les résultats mais on va utiliser ça pour la suite




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
svymean(~Reg_Y, subset(dw, Reg_G == 1 & Reg_T == 0))[1]


svymean(~Reg_Y, subset(dw, Reg_G == 1 & Reg_T == 0))[1] - svymean(~Reg_Y, subset(dw, Reg_G == 1 & Reg_T == -1))[1]
svymean(~Reg_Y, subset(dw, Reg_G == 0 & Reg_T == 0))[1] - svymean(~Reg_Y, subset(dw, Reg_G == 0 & Reg_T == -1))[1]


table(pop_initiale_tot$Reg_D)


######## Tracé : le date d'achat de la HMR - la date de l'héritage
# "HB0700 = year of property acquisition
# "HH0201 = year gift/inheritance received
# "HH0202"
# "HH0203"

sum(table(sous_data_belgique$HH0201))
sum(table(sous_data_belgique$HH0202))
sum(table(sous_data_belgique$HH0203))

hist(sous_data_belgique$HB0700 - sous_data_belgique$HH0201, breaks=50)
table(sous_data_belgique$HB0700 - sous_data_belgique$HH0201)


hist(log10(sous_data_belgique$DOGIFTINHER), breaks = 50)

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



















