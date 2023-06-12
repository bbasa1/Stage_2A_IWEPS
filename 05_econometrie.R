################################################################################
# =================== 05 FONCTIONS D'ECONOMETRIE ===============================
################################################################################
# Ici toutes les fonctions qui permettent de sortir des résultats d'économétrie


recherche_p_value_otpi <- function(liste_montant_initial, data_loc, annee_min = -1, annee_max = 3, faire_tracer = TRUE){
  ## Produit la courbe de pvalue et coeff associés à G dans la régression Y sur G pour trouver la valeur du montant minimal d'héritage optimal
  ## Utilise les deux fonctions ci-dessous
  
  sous_data_loc <- data_loc[Annee_achat_heritage <= 98] ## Uniquement les ménages qui ONT reçu un héritage

  ### On récupère les données
  dt_tot_reg <- data.table(Reg_lin_pval = numeric(),
                           Logit_pval = numeric(),
                           Probit_pval = numeric(),
                           Montant_initial =  numeric(),
                           Reg_lin_coeff = numeric(),
                           Logit_coeff = numeric(),
                           Probit_coeff = numeric())
  for(montant_ini_loc in liste_montant_initial){
    # print(montant_ini_loc)
    dt_loc <- recuperation_pval(sous_data_loc, montant_ini_loc, annee_min = annee_min, annee_max = annee_max)
    dt_tot_reg <- rbindlist(list(dt_tot_reg, dt_loc), fill=TRUE)
  }
  
  ### On met en forme
  melted_pval <- melt(dt_tot_reg,
                      id.vars = "Montant_initial",
                      measure.vars = c("Reg_lin_pval", "Logit_pval", "Probit_pval"),
                      variable.name = "variable",
                      value.name    = "value")
  melted_pval$Statistique <- "pvalue"
  
  melted_coeff <- melt(dt_tot_reg,
                       id.vars = "Montant_initial",
                       measure.vars = c("Reg_lin_coeff", "Logit_coeff", "Probit_coeff"),
                       variable.name = "variable",
                       value.name    = "value")
  melted_coeff$Statistique <- "Coefficiant"
  
  melted_count <- melt(dt_tot_reg,
                       id.vars = "Montant_initial",
                       measure.vars = c("Reg_lin_count", "Logit_count", "Probit_count"),
                       variable.name = "variable",
                       value.name    = "value")
  melted_count$Statistique <- "Nombre modalités significatives"
  
  melted_final <- rbindlist(list(melted_pval, melted_coeff, melted_count), fill=TRUE)
  melted_final$Statistique <- as.factor(melted_final$Statistique)
  
  ### On prépare le graphique
  melted_final[, label_variable := factor(
    fcase(
      variable == "Reg_lin_pval", "Linéaire",
      variable == "Logit_pval", "Logit",
      variable == "Probit_pval", "Probit",
      variable == "Reg_lin_coeff", "Linéaire",
      variable == "Logit_coeff", "Logit",
      variable == "Probit_coeff", "Probit",
      variable == "Reg_lin_count", "Linéaire",
      variable == "Logit_count", "Logit",
      variable == "Probit_count", "Probit"
    )
  )
  ]
  
  if(faire_tracer){
    ### on trace
    # titre <- "Résultats des régressions de Y sur G"
    titre <- paste("Résultats des régressions de Y sur G (", nom_pays, ")", sep = "")
    xlabel <- "Montant d'héritage minimal pour être considéré comme conséquant"
    ylabel <- ""
    titre_save <- paste(pays,"_pval_coeff_G_reg_Y_sur_G.pdf", sep = "")
    titre_save <- paste(repo_sorties, titre_save, sep ='/')
    melted_loc <- melted_final
    x <- "Montant_initial"
    y <- 'value'
    color <- "label_variable"
    colorlabel <- "Régression"
    facet <- "Statistique"
    
    trace_courbes(melted_loc, x, y, color, facet, xlabel, ylabel, colorlabel, titre, titre_save)
  }else{
    return(melted_final)
  }
}



recuperation_pval <- function(sous_data_loc, montant_ini_loc, annee_min, annee_max){
  # Utilise la fonction ci-dessous, permet de sortir sous une forme propre la pvalue et la valeur du coefficiant associés G dans la régression de Y sur G
  output <- dependance_montant_heritage_min(sous_data_loc, montant_ini_loc, annee_min = annee_min, annee_max = annee_max)
  dico_sortie <- output[1]
  dt_reg_lin <- as.data.table(output[2])
  dt_probit <- as.data.table(output[3])
  dt_logit <- as.data.table(output[4])
  dt_prep_reg_lin <- as.data.table(output[5])
  dt_prep_probit <- as.data.table(output[6])
  dt_prep_logit <- as.data.table(output[7])

  
  dt_loc_pval <- as.data.table(list(dt_reg_lin[rn == "Reg_G" ]$pvalue,dt_logit[rn == "Reg_G" ]$pvalue, dt_probit[rn == "Reg_G" ]$pvalue))
  dt_loc_pval$Montant_initial <- montant_ini_loc
  setnames(dt_loc_pval, "V1", "Reg_lin_pval")
  setnames(dt_loc_pval, "V2", "Logit_pval")
  setnames(dt_loc_pval, "V3", "Probit_pval")
  
  dt_loc_coeff <- as.data.table(list(dt_reg_lin[rn == "Reg_G" ]$Estimate ,dt_logit[rn == "Reg_G" ]$Estimate , dt_probit[rn == "Reg_G" ]$Estimate ))
  dt_loc_coeff$Montant_initial <- montant_ini_loc
  setnames(dt_loc_coeff, "V1", "Reg_lin_coeff")
  setnames(dt_loc_coeff, "V2", "Logit_coeff")
  setnames(dt_loc_coeff, "V3", "Probit_coeff")
  
  dt_loc_count <- as.data.table(list(count(dt_prep_reg_lin[pvalue < 0.01 & abs(Estimate) > 0.1]), count(dt_prep_logit[pvalue < 0.01 & abs(Estimate) > 0.01]), count(dt_prep_probit[pvalue < 0.01 & abs(Estimate) > 0.01])))
  dt_loc_count$Montant_initial <- montant_ini_loc
  setnames(dt_loc_count, "n", "Reg_lin_count")
  setnames(dt_loc_count, "n.1", "Logit_count")
  setnames(dt_loc_count, "n.2", "Probit_count")
  
  merge_1 <- merge(dt_loc_pval, dt_loc_coeff, by = "Montant_initial")
  return(merge(merge_1, dt_loc_count, by = "Montant_initial"))
}

dependance_montant_heritage_min <- function(sous_data_loc, montant_ini_loc, annee_min, annee_max){
  # Effectue la régression de Y sur G. C'est la plus profonde des sous-fonctions
  
  dico_sortie <- c()
  
  # On créé les groupes
  sous_data_loc[, Reg_Y := 0]
  sous_data_loc[Annee_achat_heritage %in% annee_min:annee_max, Reg_Y := 1] # Ont acheté qq années après
  dico_sortie["Effectif_Y_0"] <- table(sous_data_loc$Reg_Y)[1]
  dico_sortie["Effectif_Y_1"] <- table(sous_data_loc$Reg_Y)[2]
  
  sous_data_loc[, Reg_G := 0]
  sous_data_loc[Montant_heritage_1 >= montant_ini_loc, Reg_G := 1] # Reçu un héritage conséquant
  dico_sortie["Effectif_G_0"] <- table(sous_data_loc$Reg_G)[1]
  dico_sortie["Effectif_G_1"] <- table(sous_data_loc$Reg_G)[2]  
  
  # Etude préparatoire : éventuel problème de biais de sélection des traités
  liste_cols_reg_poids <- c("HW0010", "Reg_G", "DHAGEH1B", "DHEDUH1", "DHGENDERH1", "DI2000", "DHHTYPE")
  dw <- svydesign(ids = ~1, data = sous_data_loc[,..liste_cols_reg_poids], weights = ~ HW0010)
  mysvyglm <- svyglm(formula = Reg_G ~ DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE, design = dw)
  dt_prep_reg_lin <- as.data.table(summary(mysvyglm)$coefficients, keep.rownames = TRUE)  
  setnames(dt_prep_reg_lin, "Pr(>|t|)", "pvalue")
  
  denylogit <- glm(Reg_G ~ DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE, 
                   family = binomial(link = "logit"), 
                   data = sous_data_loc)
  dt_prep_logit <- as.data.table(summary(denylogit)$coefficients, keep.rownames = TRUE)
  setnames(dt_prep_logit, "Pr(>|z|)", "pvalue")

  denyprobit <- glm(Reg_G ~ DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE, 
                    family = binomial(link = "probit"), 
                    data = sous_data_loc)
  dt_prep_probit <- as.data.table(summary(denyprobit)$coefficients, keep.rownames = TRUE)
  setnames(dt_prep_probit, "Pr(>|z|)", "pvalue")
  
  
  # Est-ce que G est significatif pour prévoir Y ?
  liste_cols_reg_poids <- c("HW0010", "Reg_Y", "Reg_G")
  dw <- svydesign(ids = ~1, data = sous_data_loc[,..liste_cols_reg_poids], weights = ~ HW0010)
  mysvyglm <- svyglm(formula = Reg_Y ~ Reg_G, design = dw)
  dt_reg_lin <- as.data.table(summary(mysvyglm)$coefficients, keep.rownames = TRUE)  
  setnames(dt_reg_lin, "Pr(>|t|)", "pvalue")
  
  
  denyprobit <- glm(Reg_Y ~ Reg_G, 
                    family = binomial(link = "probit"), 
                    data = sous_data_loc)
  dt_probit <- as.data.table(summary(denyprobit)$coefficients, keep.rownames = TRUE)  
  setnames(dt_probit, "Pr(>|z|)", "pvalue")
  
  
  denyprobit <- glm(Reg_Y ~ Reg_G, 
                    family = binomial(link = "logit"), 
                    data = sous_data_loc)
  dt_logit <- as.data.table(summary(denyprobit)$coefficients, keep.rownames = TRUE) 
  setnames(dt_logit, "Pr(>|z|)", "pvalue")
  
  return(list(dico_sortie, dt_reg_lin, dt_probit, dt_logit, dt_prep_reg_lin, dt_prep_probit, dt_prep_logit))
}

