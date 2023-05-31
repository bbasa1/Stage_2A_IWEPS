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
source(paste(repo_prgm , "03_nettoyage_data.R" , sep = "/"))


############### Variables d'intérêt
# HW0010 = Le poids
# DA1000 = Total real assets (+ ventilations plus précises)
# DA2100 = Total financial assets (+ ventilations plus précises)
# DA3001 = DA1000 + DA2100 = Total assets
# DATOP10 = Le décile de gross wealth/richesse brute au sein du pays
# DHAGEH1 = Âge de la personne de référence
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
# DOINHERIT = Substantial inheritance/gift received ==> OSEF non ?
# VAGUE = La vague
# SA0200 = L'année
# DA1120 = La valeur de la résidence principale

liste_var_continues <- c("HW0010", "DA1000", "DA2100", "DA3001", "DI2000", "DI2000EQ", "DI2100", "DL1000", "DN3001", "DNFPOS", "DNHW", "DOGIFTINHER", "DA1120")
liste_var_categorielles <- c("DATOP10", "DHAGEH1", "DHEDUH1", "DHGENDERH1", "DHLIFESATIS", "DITOP10", "DLTOP10", "DNTOP10", "DOEINHERIT", "VAGUE", "SA0200")
liste_var_interet <- union(liste_var_continues, liste_var_categorielles)
sous_data_belgique <- data_belgique[,..liste_var_interet]
sous_data_belgique <- sous_data_belgique %>% mutate_at(liste_var_continues, as.numeric)
sous_data_belgique <- sous_data_belgique %>% mutate_at(liste_var_categorielles, as.factor)

sous_data_belgique

# summary(sous_data_belgique$HW0010)

################################################################################
# ====================== 04 STAT DES & GRAPHIQUES ==============================
################################################################################
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


titre <- "Patrimoine normalisé par sexe"
titre_save <- "Patrimoine_sexe"
titre_save <- paste(repo_sorties, titre_save, sep ='/')
x <-"DATOP10"
sortby_x <- "DATOP10"
y <- "new"
fill <- "Sexe"
xlabel <-"Quantile de patrimoine brut"
ylabel <-"% de la population belge"

ggplot(data = data_for_plot, aes(x = reorder(.data[[x]], .data[[sortby_x]]), y = .data[[y]], fill = .data[[fill]])) +
  geom_bar(stat="identity", position=position_dodge()) + 
  labs(title=titre,
       x= xlabel,
       y= ylabel) + 
  scale_y_continuous(limits = c(0, 15), labels = function(y) format(y, scientific = FALSE)) + 
  scale_fill_discrete() +
  scale_color_viridis() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))



######## On regarde la concentration des différents types de patrimoines #############

nb_quantiles <- 100
liste_type_patrimoines <- c("DA3001" = "Total",
                            "DA1000" = "Physique",
                            "DA2100" = "Financier")

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




data_for_plot

# Melt pour pouvoir tracer
melted <- melt(data_for_plot, 
               id.vars = "Quantiles", 
               measure.vars  = c("Total", "Physique", "Financier"),
               variable.name = "variable",
               value.name    = "value")


ggplot(melted) +
  geom_line(aes(x = Quantiles, y= value, color = variable)) +
  scale_color_viridis_d() +
  labs(
    x = "Part des ménages", 
    y = "Patrimoine détenu (cumulatif)",
    color = "Type de patrimoine",
    titre = "Fonction de répartition du patrimoine des ménages Belges, par type de patrimoine"
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(
      prefix = "",
      suffix = " %",
      big.mark = " ",
      decimal.mark = ","), 
    expand = c(0, 0)
  ) + 
  scale_x_continuous(
    labels = scales::dollar_format(
      prefix = "",
      suffix = " %",
      big.mark = " ",
      decimal.mark = ","),
    expand = c(0, 0)
  ) +
  theme(legend.text = element_text(angle = 0, vjust = 0.7, hjust = 0),
        axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = "bottom")




########### OLD VERSION 

# On calcule tous les quantiles et les patrimoines moyens par quantiles
# sous_data_belgique[,Quantiles := cut(DA3001,
#                                             breaks=quantile(DA3001, probs=seq(0, 1, by=1/nb_quantiles), na.rm=T),
#                                             include.lowest= TRUE, labels=1:nb_quantiles)]
# data_for_plot <- sous_data_belgique[, mean(DA3001), by = Quantiles]
# setnames(data_for_plot, "V1", "Total")
# 
# sous_data_belgique[,Quantiles := cut(DA1000,
#                                             breaks=quantile(DA1000, probs=seq(0, 1, by=1/nb_quantiles), na.rm=T),
#                                             include.lowest= TRUE, labels=1:nb_quantiles)]
# data_for_plot_1 <- sous_data_belgique[, mean(DA1000), by = Quantiles]
# setnames(data_for_plot_1, "V1", "Physique")
# 
# sous_data_belgique[,Quantiles := cut(DA2100,
#                                      breaks=quantile(DA2100, probs=seq(0, 1, by=1/nb_quantiles), na.rm=T),
#                                      include.lowest= TRUE, labels=1:nb_quantiles)]
# data_for_plot_2 <- sous_data_belgique[, mean(DA2100), by = Quantiles]
# setnames(data_for_plot_2, "V1", "Financier")
# 
# # On merge tout ça pour avoir 1 colonne/type de patrimoine
# data_for_plot <- merge(data_for_plot, data_for_plot_1, by = "Quantiles")
# data_for_plot <- merge(data_for_plot, data_for_plot_2, by = "Quantiles")
# 
# # On fait les cumsum
# data_for_plot[, Total := 100*cumsum(Total)/sum(Total)]
# data_for_plot[, Physique := 100*cumsum(Physique)/sum(Physique)]
# data_for_plot[, Financier := 100*cumsum(Financier)/sum(Financier)]





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



################################################## Exploration avec le paclage survey

dw$variables$DN3001 # Pour retrouver le contenu d'une colonne

svymean(~DN3001, dw) # Sur une variable continue
svyquantile(~DN3001, dw, quantile = c(0.25, 0.5, 0.75), ci = TRUE) # Quantiles + leurs IC_a sur une variable continue

svytable(~DHLIFESATIS, dw) # Sur une variable catégorielle

svytable(~ DHLIFESATIS + DHGENDERH1, dw) # Sur deux variables catégorielles

tab <- svytable(~DHLIFESATIS + DHGENDERH1, dw) # Pour avoir une fréquence sur une variable
lprop(tab, total = TRUE) #Pour avoir les %


ggplot(dw$variables) +
  aes(weight = weights(dw), x = DHGENDERH1, fill = DOEINHERIT) +
  geom_bar(position = "fill")

ggplot(data_belgique) +
  aes(x = DHGENDERH1, fill = DOEINHERIT) +
  geom_bar(position = "fill")

table(data_belgique$DOEINHERIT)


################################################## Pour étudier les différences entre les différentes versions

data_complete_1 <- copy(data_complete)

num_table <- 2 ### Change les poids assignés par eurostat
source(paste(repo_prgm , "02_importation_data.R" , sep = "/"))
data_complete_2 <- copy(data_complete)


num_table <- 1 ### Change les poids assignés par eurostat
source(paste(repo_prgm , "02_importation_data.R" , sep = "/"))
head(data_complete$HW0010,100)

table(data_complete_1[data_complete_1$DN3001 != data_complete_2$DN3001])

data_complete_1[data_complete_1$SA0010  != data_complete_2$SA0010]

table(data_complete_1  == data_complete_2)

head(data_complete_1[data_complete_1$DOEINHERIT  != data_complete_2$DOEINHERIT ]$DOEINHERIT , 10)
head(data_complete_2[data_complete_1$DOEINHERIT  != data_complete_2$DOEINHERIT ]$DOEINHERIT , 10)

nrow(data_complete_1)*length(colnames(data_complete_1))
2277054 + 37663527




########## A discuter : 
# Les changements entre les fichiers ne viennent pas des poids mais de certaines valeurs. J'imagine les valeurs imputées ?
# En tout cas elles ont l'air de relativement peu changer entre les différents fichiers, de l'ordre de qq % en général
# Etats des lieux : Il existe au moins une différence entre les deux premières imputations dans 2277054/59424064 = 4% des cases où il y a du changement
# Pour obtenir ce chiffre = table(data_complete_1  == data_complete_2) effectifs FALSE/nrow(data_complete_1)*length(colnames(data_complete_1))



















