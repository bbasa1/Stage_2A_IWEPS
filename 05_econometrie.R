################################################################################
# =================== 05 FONCTIONS D'ECONOMETRIE ===============================
################################################################################
# Ici toutes les fonctions qui permettent de sortir des résultats d'économétrie


recherche_p_value_otpi <- function(liste_montant_initial, data_loc){
  ## Produit la courbe de pvalue et coeff associés à G dans la régression Y sur G pour trouver la valeur du montant minimal d'héritage optimal
  ## Utilise les deux fonctions ci-dessous
  data_loc[(is.na(HB0700) & !is.na(Montant_heritage_1)), Annee_achat_heritage := - 99] # Pas d'achat mais un héritage
  data_loc[(!is.na(HB0700) & is.na(Montant_heritage_1)), Annee_achat_heritage :=  99] # Achat mais pas d'héritage
  data_loc[(!is.na(HB0700) & !is.na(Montant_heritage_1)), Annee_achat_heritage := HB0700 - Annee_heritage_1]
  sous_data <- data_loc[Annee_achat_heritage <= 98] ## Uniquement les ménages qui ONT reçu un héritage
  sous_data_loc <- sous_data
  
  
  ### On récupère les données
  dt_tot_reg <- data.table(Reg_lin_pval = numeric(),
                           Logit_pval = numeric(),
                           Probit_pval = numeric(),
                           Montant_initial =  numeric(),
                           Reg_lin_coeff = numeric(),
                           Logit_coeff = numeric(),
                           Probit_coeff = numeric())
  for(Montant_initial in liste_montant_initial){
    dt_loc <- recuperation_pval(sous_data_loc, Montant_initial)
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
  
  melted_final <- rbindlist(list(melted_pval, melted_coeff), fill=TRUE)
  melted_final$Statistique <- as.factor(melted_final$Statistique)
  
  ### On prépare le graphique
  melted_final[, label_variable := factor(
    fcase(
      variable == "Reg_lin_pval", "Linéaire",
      variable == "Logit_pval", "Logit",
      variable == "Probit_pval", "Probit",
      variable == "Reg_lin_coeff", "Linéaire",
      variable == "Logit_coeff", "Logit",
      variable == "Probit_coeff", "Probit"
    )
  )
  ]
  
  ### on trace
  titre <- "Résultats des régressions de Y sur G"
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
}



recuperation_pval <- function(sous_data_loc, Montant_minimal){
  # Utilise la fonction ci-dessous, permet de sortir sous une forme propre la pvalue et la valeur du coefficiant associés G dans la régression de Y sur G
  output <- dependance_montant_heritage_min(sous_data_loc, Montant_minimal)
  dico_sortie <- output[1]
  dt_preparation <- as.data.table(output[2])
  dt_reg_lin <- as.data.table(output[3])
  dt_probit <- as.data.table(output[4])
  dt_logit <- as.data.table(output[5])
  
  dt_loc_pval <- as.data.table(list(dt_reg_lin[rn == "Reg_G" ]$pvalue,dt_logit[rn == "Reg_G" ]$pvalue, dt_probit[rn == "Reg_G" ]$pvalue))
  dt_loc_pval$Montant_initial <- Montant_initial
  setnames(dt_loc_pval, "V1", "Reg_lin_pval")
  setnames(dt_loc_pval, "V2", "Logit_pval")
  setnames(dt_loc_pval, "V3", "Probit_pval")
  
  dt_loc_coeff <- as.data.table(list(dt_reg_lin[rn == "Reg_G" ]$Estimate ,dt_logit[rn == "Reg_G" ]$Estimate , dt_probit[rn == "Reg_G" ]$Estimate ))
  dt_loc_coeff$Montant_initial <- Montant_initial
  setnames(dt_loc_coeff, "V1", "Reg_lin_coeff")
  setnames(dt_loc_coeff, "V2", "Logit_coeff")
  setnames(dt_loc_coeff, "V3", "Probit_coeff")
  
  return(merge(dt_loc_pval, dt_loc_coeff, by = "Montant_initial"))
}

dependance_montant_heritage_min <- function(sous_data_loc, Montant_minimal){
  # Effectue la régression de Y sur G
  
  dico_sortie <- c()
  
  # On créé les groupes
  sous_data_loc[, Reg_Y := 0]
  sous_data_loc[Annee_achat_heritage %in% -1:3, Reg_Y := 1] # Ont acheté qq années après
  dico_sortie["Effectif_Y_0"] <- table(sous_data_loc$Reg_Y)[1]
  dico_sortie["Effectif_Y_1"] <- table(sous_data_loc$Reg_Y)[2]
  
  sous_data_loc[, Reg_G := 0]
  sous_data_loc[Montant_heritage_1 >= Montant_minimal, Reg_G := 1] # Reçu un héritage conséquant
  dico_sortie["Effectif_G_0"] <- table(sous_data_loc$Reg_G)[1]
  dico_sortie["Effectif_G_1"] <- table(sous_data_loc$Reg_G)[2]  
  
  # Etude préparatoire : éventuel problème de biais de sélection des traités
  liste_cols_reg_poids <- c("HW0010", "Reg_G", "DHAGEH1B", "DHEDUH1", "DHGENDERH1", "DI2000", "DHHTYPE")
  dw <- svydesign(ids = ~1, data = sous_data_loc[,..liste_cols_reg_poids], weights = ~ HW0010)
  mysvyglm <- svyglm(formula = Reg_G ~ DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE, design = dw)
  dt_preparation <- as.data.table(summary(mysvyglm)$coefficients, keep.rownames = TRUE)  
  setnames(dt_preparation, "Pr(>|t|)", "pvalue")
  
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
  
  return(list(dico_sortie, dt_preparation, dt_reg_lin, dt_probit, dt_logit))
}
