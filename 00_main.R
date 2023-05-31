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



table(data_complete$SA0200)
table(data_complete$SA0100)
table(data_complete$VAGUE)
colnames(data_complete)

length(colnames(data_complete))

table(data_complete[SA0100 == "BE"]$DATOP10)
head(data_complete[SA0100 == "BE"]$DATOP10, 20)

hist(data_complete[SA0100 == "BE"]$DATOP10, breaks=13, col="red")

sous_data <- data_complete[SA0100 == "BE", sum(HW0010), by = DITOP10]
setorder(sous_data, DITOP10)
sous_data


############### Variables d'intérêt
# HW0010 = Le poids
# DA1000 = Total real assets (+ ventilations plus précises)
# DA2100 = Total financial assets (+ ventilations plus précises)
# DA3001 = DA1000 + DA2100 = Total assets
# DATOP10 = Le décile de gross wealth/richesse brut au sein du pays
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

sapply(data_belgique, function(x) sum(is.na(x)))


data_belgique$HW0010 <- as.numeric(data_belgique$HW0010)
data_belgique$DA1000 <- as.numeric(data_belgique$DA1000)
data_belgique$DA2100 <- as.numeric(data_belgique$DA2100)
data_belgique$DA3001 <- as.numeric(data_belgique$DA3001)
data_belgique$DI2000 <- as.numeric(data_belgique$DI2000)
data_belgique$DI2000eq <- as.numeric(data_belgique$DI2000eq)
data_belgique$DI2100 <- as.numeric(data_belgique$DI2100)
data_belgique$DL1000 <- as.numeric(data_belgique$DL1000)
data_belgique$DN3001 <- as.numeric(data_belgique$DN3001)
data_belgique$DNFPOS <- as.numeric(data_belgique$DNFPOS)
data_belgique$DNHW <- as.numeric(data_belgique$DNHW)
data_belgique$DOGIFTINHER <- as.numeric(data_belgique$DOGIFTINHER)

data_belgique$DATOP10 <- as.factor(data_belgique$DATOP10)
data_belgique$DHAGEH1 <- as.factor(data_belgique$DHAGEH1)
data_belgique$DHEDUH1 <- as.factor(data_belgique$DHEDUH1)
data_belgique$DHGENDERH1 <- as.factor(data_belgique$DHGENDERH1)
data_belgique$DHLIFESATIS <- as.factor(data_belgique$DHLIFESATIS)
data_belgique$DITOP10 <- as.factor(data_belgique$DITOP10)
data_belgique$DLTOP10 <- as.factor(data_belgique$DLTOP10)
data_belgique$DNTOP10 <- as.factor(data_belgique$DNTOP10)
data_belgique$DOEINHERIT <- as.factor(data_belgique$DOEINHERIT)



################################################## Un peu d'héritage...

hist(log(data_belgique$DOGIFTINHER), breaks=50, col="red")




################################################## Exploration avec le paclage survey
dw <- svydesign(ids = ~1, data = data_belgique, weights = ~ data_belgique$HW0010)

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



















