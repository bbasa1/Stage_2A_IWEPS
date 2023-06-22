################################################################################
# =================== 05 FONCTIONS D'ECONOMETRIE ===============================
################################################################################
# Ici toutes les fonctions qui permettent de sortir des résultats d'économétrie


recherche_p_value_otpi <- function(liste_montant_initial, data_loc, annee_min = -1, annee_max = 3, faire_tracer = TRUE, titre, titre_save, que_heritiers = TRUE, que_proprio = TRUE, que_logit = FALSE){
  ## Produit la courbe de pvalue et coeff associés à G dans la régression Y sur G pour trouver la valeur du montant minimal d'héritage optimal
  ## Utilise les deux fonctions ci-dessous
  sous_data_loc <- copy(data_loc)
  
  
  if(que_heritiers){
    sous_data_loc <- data_loc[Annee_achat_heritage <= 98] ## Uniquement les ménages qui ONT reçu un héritage
  }
  
  if(que_proprio){
    sous_data_loc <- sous_data_loc[Annee_achat_heritage >= -98] ## Uniquement les ménages qui SONT proprios
  }
  
  if(nrow(sous_data_loc) == 0){
    print("Table avec 0 lignes : on passe")
    return()
  }
  
  ### On récupère les données
  dt_tot_reg <- data.table(Reg_lin_pval = numeric(),
                           Logit_pval = numeric(),
                           Probit_pval = numeric(),
                           Montant_initial =  numeric(),
                           Reg_lin_coeff = numeric(),
                           Logit_coeff = numeric(),
                           Probit_coeff = numeric(),
                           Reg_lin_coeff_X_Y = numeric(),
                           Logit_coeff_X_Y = numeric(),
                           Probit_coeff_X_Y = numeric()
                           )
  for(montant_ini_loc in liste_montant_initial){
    # print(montant_ini_loc)
    dt_loc <- recuperation_pval(sous_data_loc, montant_ini_loc, annee_min = annee_min, annee_max = annee_max, que_logit=que_logit)
    dt_tot_reg <- rbindlist(list(dt_tot_reg, dt_loc), fill=TRUE)
  }
  
  ### On met en forme
  melted_pval <- melt(dt_tot_reg,
                      id.vars = "Montant_initial",
                      measure.vars = c("Reg_lin_pval", "Logit_pval", "Probit_pval"),
                      variable.name = "variable",
                      value.name    = "value")
  melted_pval$Statistique <- "Y sur G : pvalue"
  
  melted_pval_log <- copy(melted_pval)
  melted_pval_log$value <- log10(melted_pval_log$value)
  melted_pval_log$Statistique <- "Y sur G : log(pvalue)"
  melted_pval_log$variable <- paste(melted_pval_log$variable, "_log", sep = "")
  
  melted_coeff <- melt(dt_tot_reg,
                       id.vars = "Montant_initial",
                       measure.vars = c("Reg_lin_coeff", "Logit_coeff", "Probit_coeff"),
                       variable.name = "variable",
                       value.name    = "value")
  melted_coeff$Statistique <- "Y sur G : Coefficiant"
  
  melted_coeff_log <- copy(melted_coeff)
  melted_coeff_log$value <- exp(melted_coeff_log$value)
    # log10(abs(melted_coeff_log$value))
  melted_coeff_log$Statistique <- "Y sur G : Odd ratio"
  melted_coeff_log$variable <- paste(melted_coeff_log$variable, "_exp", sep = "")

  
  melted_count <- melt(dt_tot_reg,
                       id.vars = "Montant_initial",
                       measure.vars = c("Reg_lin_count", "Logit_count", "Probit_count"),
                       variable.name = "variable",
                       value.name    = "value")
  melted_count$Statistique <- "G sur X : Nombre de modalités significatives à 1%"

  
  melted_coeff_X_Y <- melt(dt_tot_reg,
                       id.vars = "Montant_initial",
                       measure.vars = c("Reg_lin_coeff_X_Y", "Logit_coeff_X_Y", "Probit_coeff_X_Y"),
                       variable.name = "variable",
                       value.name    = "value")
  melted_coeff_X_Y$Statistique <- "Y sur (X,G) : Coefficiant associé à G"
  
  
  
  melted_final <- rbindlist(list(melted_pval, melted_coeff, melted_count, melted_pval_log, melted_coeff_log, melted_coeff_X_Y), fill=TRUE)
  melted_final$Statistique <- as.factor(melted_final$Statistique)
  
  table(melted_final$variable)
  table(melted_final$label_variable)
  
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
      variable == "Probit_count", "Probit",
      variable == "Reg_lin_pval_log", "Linéaire",
      variable == "Logit_pval_log", "Logit",
      variable == "Probit_pval_log", "Probit",
      variable == "Reg_lin_coeff_exp", "Linéaire",
      variable == "Logit_coeff_exp", "Logit",
      variable == "Probit_coeff_exp", "Probit",
      variable == "Logit_coeff_X_Y ", "Logit",
      variable == "Reg_lin_coeff_X_Y", "Linéaire",
      variable == "Probit_coeff_X_Y ", "Probit"
    )
  )
  ]
  
  #### A corriger : Pour une raison que je ne m'explique pas il ne veut pas changer le label pour ces dernières lignes il faut le faire à la main...
  melted_final[variable == "Reg_lin_coeff_X_Y", label_variable := "Linéaire"]
  melted_final[variable == "Logit_coeff_X_Y", label_variable := "Logit"]
  melted_final[variable == "Probit_coeff_X_Y", label_variable := "Probit"]
  

  if(faire_tracer){
    ### on trace
    # titre <- "Résultats des régressions de Y sur G"
    xlabel <- "Montant d'héritage minimal pour être considéré comme conséquant"
    ylabel <- ""

    melted_loc <- melted_final
    x <- "Montant_initial"
    y <- 'value'
    color <- "label_variable"
    colorlabel <- "Régression"
    facet <- "Statistique"
    ordre_facet <- "ordre_facet"
    
    caption_text <- "Y = Fait d'être propriétaire au moment de réception d'un héritage ou d'un don\n
                      G = Fait de recevoir un héritage ou un don supérieur au montant minimal\n
                      X = Ensemble de variables socio-économiques du ménage"
    
    if(que_logit){
      trace_courbes(melted_loc[label_variable == "Logit"], x, y, color, facet, xlabel, ylabel, colorlabel, titre, titre_save, caption_text)
    }else{
      trace_courbes(melted_loc, x, y, color, facet, xlabel, ylabel, colorlabel, titre, titre_save, caption_text)
      
    }
    
  }else{
    return(melted_final)
  }
}



recuperation_pval <- function(sous_data_loc, montant_ini_loc, annee_min, annee_max, que_logit){
  # Utilise la fonction ci-dessous, permet de sortir sous une forme propre la pvalue et la valeur du coefficiant associés G dans la régression de Y sur G
  output <- dependance_montant_heritage_min(sous_data_loc, montant_ini_loc, annee_min = annee_min, annee_max = annee_max, que_logit=que_logit)
  dico_sortie <- output[1]
  dt_reg_lin <- as.data.table(output[2])
  dt_probit <- as.data.table(output[3])
  dt_logit <- as.data.table(output[4])
  dt_prep_reg_lin <- as.data.table(output[5])
  dt_prep_probit <- as.data.table(output[6])
  dt_prep_logit <- as.data.table(output[7])
  dt_reg_lin_X_Y <- as.data.table(output[8])
  dt_probit_X_Y <- as.data.table(output[9])
  dt_logit_X_Y <- as.data.table(output[10])
  
  
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
  
  dt_loc_count <- as.data.table(list(count(dt_prep_reg_lin[pvalue < 0.01]), count(dt_prep_logit[pvalue < 0.01]), count(dt_prep_probit[pvalue < 0.01])))
  dt_loc_count$Montant_initial <- montant_ini_loc
  setnames(dt_loc_count, "n", "Reg_lin_count")
  setnames(dt_loc_count, "n.1", "Logit_count")
  setnames(dt_loc_count, "n.2", "Probit_count")
  
  
  dt_loc_coeff_Y_X <- as.data.table(list(dt_reg_lin_X_Y[rn == "Reg_G" ]$Estimate ,dt_logit_X_Y[rn == "Reg_G" ]$Estimate , dt_probit_X_Y[rn == "Reg_G" ]$Estimate ))
  dt_loc_coeff_Y_X$Montant_initial <- montant_ini_loc
  setnames(dt_loc_coeff_Y_X, "V1", "Reg_lin_coeff_X_Y")
  setnames(dt_loc_coeff_Y_X, "V2", "Logit_coeff_X_Y")
  setnames(dt_loc_coeff_Y_X, "V3", "Probit_coeff_X_Y")
  
  merge_1 <- merge(dt_loc_pval, dt_loc_coeff, by = "Montant_initial")
  merge_2 <- merge(merge_1, dt_loc_count, by = "Montant_initial")
  merge_3 <- merge(merge_2, dt_loc_coeff_Y_X, by = "Montant_initial")
  return(merge_3)
}

dependance_montant_heritage_min <- function(sous_data_loc, montant_ini_loc, annee_min, annee_max, que_logit){
  # Effectue toutes les régressions : Y/G, G/X et Y/(G,X). C'est la plus profonde des sous-fonctions
  
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
  
  for (i in names(sous_data_loc)){
    try(sous_data_loc[[i]] <- droplevels(sous_data_loc[[i]]), silent = TRUE)
  }
  
  # Etude préparatoire : éventuel problème de biais de sélection des traités
  if(!que_logit){
    liste_cols_reg_poids <- c("HW0010", "Reg_G", "DHAGEH1B", "DHEDUH1", "DHGENDERH1", "DI2000", "DHHTYPE", "DHEMPH1", "PE0300_simpl")
    dw <- svydesign(ids = ~1, data = sous_data_loc[,..liste_cols_reg_poids], weights = ~ HW0010)
    mysvyglm <- svyglm(formula = Reg_G ~ DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE + DHEMPH1 + PE0300_simpl, design = dw)
    dt_prep_reg_lin <- as.data.table(summary(mysvyglm)$coefficients, keep.rownames = TRUE)  
    setnames(dt_prep_reg_lin, "Pr(>|t|)", "pvalue")
    
    denyprobit <- glm(Reg_G ~ DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE + DHEMPH1 + PE0300_simpl, 
                    family = binomial(link = "probit"), 
                    data = sous_data_loc)
    dt_prep_probit <- as.data.table(summary(denyprobit)$coefficients, keep.rownames = TRUE)
    setnames(dt_prep_probit, "Pr(>|z|)", "pvalue")
  }else{
    dt_prep_reg_lin <- data.table(rn = numeric(),
                                  Estimate = numeric(),
                                  std_err = numeric(),
                                  tval =  numeric(),
                                  pvalue = numeric())
    setnames(dt_prep_reg_lin, 'std_err', "Std. Error")
    setnames(dt_prep_reg_lin, 'tval', "t value")
    
    dt_prep_probit <- data.table(rn = numeric(),
                                  Estimate = numeric(),
                                  std_err = numeric(),
                                  tval =  numeric(),
                                  pvalue = numeric())
    setnames(dt_prep_probit, 'std_err', "Std. Error")
    setnames(dt_prep_probit, 'tval', "t value")
  }
  
  
  denylogit <- glm(Reg_G ~ DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE + DHEMPH1 + PE0300_simpl, 
                   family = binomial(link = "logit"), 
                   data = sous_data_loc)
  dt_prep_logit <- as.data.table(summary(denylogit)$coefficients, keep.rownames = TRUE)
  setnames(dt_prep_logit, "Pr(>|z|)", "pvalue")


  
  # Est-ce que G est significatif pour prévoir Y ?
  if(!que_logit){
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
  }else{
    dt_reg_lin <- data.table(rn = numeric(),
                               Estimate = numeric(),
                               std_err = numeric(),
                               tval =  numeric(),
                               pvalue = numeric())
    setnames(dt_reg_lin, 'std_err', "Std. Error")
    setnames(dt_reg_lin, 'tval', "t value")
    
    dt_probit <- data.table(rn = numeric(),
                             Estimate = numeric(),
                             std_err = numeric(),
                             tval =  numeric(),
                             pvalue = numeric())
    setnames(dt_probit, 'std_err', "Std. Error")
    setnames(dt_probit, 'tval', "t value")
      
  }
  denyprobit <- glm(Reg_Y ~ Reg_G, 
                    family = binomial(link = "logit"), 
                    data = sous_data_loc)
  dt_logit <- as.data.table(summary(denyprobit)$coefficients, keep.rownames = TRUE) 
  setnames(dt_logit, "Pr(>|z|)", "pvalue")
  
  
  
  
  # Est-ce que (X,G) est significatif pour prévoir Y ?
  if(!que_logit){
    liste_cols_reg_poids <- c("Reg_Y", "HW0010", "Reg_G", "DHAGEH1B", "DHEDUH1", "DHGENDERH1", "DI2000", "DHHTYPE", "DHEMPH1", "PE0300_simpl")
    dw <- svydesign(ids = ~1, data = sous_data_loc[,..liste_cols_reg_poids], weights = ~ HW0010)
    mysvyglm <- svyglm(formula = Reg_Y ~ Reg_G + DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE + DHEMPH1 + PE0300_simpl, design = dw)
    dt_reg_lin_X_Y <- as.data.table(summary(mysvyglm)$coefficients, keep.rownames = TRUE)  
    setnames(dt_reg_lin_X_Y, "Pr(>|t|)", "pvalue")
    
    denyprobit <- glm(Reg_Y ~ Reg_G + DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE + DHEMPH1 + PE0300_simpl, 
                      family = binomial(link = "probit"), 
                      data = sous_data_loc)
    dt_probit_X_Y <- as.data.table(summary(denyprobit)$coefficients, keep.rownames = TRUE)  
    setnames(dt_probit_X_Y, "Pr(>|z|)", "pvalue")
  }else{
    dt_reg_lin_X_Y <- data.table(rn = numeric(),
                             Estimate = numeric(),
                             std_err = numeric(),
                             tval =  numeric(),
                             pvalue = numeric())
    setnames(dt_reg_lin_X_Y, 'std_err', "Std. Error")
    setnames(dt_reg_lin_X_Y, 'tval', "t value")
    
    dt_probit_X_Y <- data.table(rn = numeric(),
                            Estimate = numeric(),
                            std_err = numeric(),
                            tval =  numeric(),
                            pvalue = numeric())
    setnames(dt_probit_X_Y, 'std_err', "Std. Error")
    setnames(dt_probit_X_Y, 'tval', "t value")
    
  }
  
  
  
  denyprobit <- glm(Reg_Y ~ Reg_G + DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE + DHEMPH1 + PE0300_simpl, 
                    family = binomial(link = "logit"), 
                    data = sous_data_loc)
  dt_logit_X_Y <- as.data.table(summary(denyprobit)$coefficients, keep.rownames = TRUE) 
  setnames(dt_logit_X_Y, "Pr(>|z|)", "pvalue")
  
  
  
  
  
  
  return(list(dico_sortie, dt_reg_lin, dt_probit, dt_logit, dt_prep_reg_lin, dt_prep_probit, dt_prep_logit, dt_reg_lin_X_Y, dt_probit_X_Y, dt_logit_X_Y))
}


##################################### Quid de la valeur du bien immobilier ? ######################


effet_heritage_sur_valeur_HMR <- function(data_loc, liste_montant_initial, titre, titre_save, caption_text, col_montant_bien = "HB0900"){
  # Après la régression sur le fait d'être propriétaire, on fait une régression sur la valeur de la propriété principale
  # Avoir reçu un héritage augmente-t-il la valeur de la résidence principale ?
  
  liste_pval <- c() #pvalue régression Y/G
  liste_coeff <- c() #coeff régression Y/G
  liste_count <- c() #Nb de modalités de variables socio-éco explicatives de G à plus de 1%
  liste_effectifs_1 <- c() #La fraction de ménages étant dans le groupe G=1
  ntot <- nrow(data_loc)
  for(montant_ini_loc in liste_montant_initial){
    sortie <- regression_heritage_valeur_hmr(data_loc, montant_ini_loc, col_montant_bien)
    liste_pval <- append(liste_pval, sortie$pvalue)
    liste_coeff <- append(liste_coeff, sortie$Estimate)
    n <- count(regression_G_heritage(data_loc, montant_ini_loc)[pvalue < 0.01])$n
    liste_count <- append(liste_count, n)
    
    data_loc[, Reg_G := 0]
    data_loc[Annee_achat_heritage >= 0 & Montant_heritage_1 >= montant_ini_loc, Reg_G := 1] # Reçu un héritage avant l'achat
    liste_effectifs_1 <- append(liste_effectifs_1, 100 * sum(data_loc$Reg_G)/ntot)
    
  }
  
  dt_for_plot <- as.data.table(list(liste_montant_initial, liste_pval, liste_coeff, liste_count, liste_effectifs_1))
  # dt_for_plot
  
  dt_for_plot[V2 <= 0.01, snignif := "1%"]
  dt_for_plot[V2 <= 0.02 & V2 > 0.01, snignif := "2%"]
  dt_for_plot[V2 <= 0.05 & V2 > 0.02, snignif := "5%"]
  dt_for_plot[V2 <= 0.10 & V2 > 0.05, snignif := "10%"]
  dt_for_plot[V2 > 0.10, snignif := "Non significatif"]
  
  # dt_for_plot$Nb_modalite <- as.factor(dt_for_plot$V4)
  # [, Nb_modalite := V4]
  # dt_for_plot[V2 <= 0.02 & V2 > 0.01, snignif := "2%"]
  # dt_for_plot[V2 <= 0.05 & V2 > 0.02, snignif := "5%"]
  # dt_for_plot[V2 <= 0.10 & V2 > 0.05, snignif := "10%"]
  # dt_for_plot[V2 > 0.10, snignif := "Non significatif"]
  
  # setnames(dt_for_plot, c("V1", "V2", "V3", "V4"), c("Montant_initial", "Y sur G : pvalue", "Y sur G : coefficiant", "G sur X : Nombre de variables dignificatives à 1%"))
  setnames(dt_for_plot, c("V1", "V2", "V3", "V4", "V5"), c("Montant_initial", "pvalue", "coefficiant", "Nb_modalite", "Effectifs"))
  
  dt_for_plot$Nb_modalite <- as.factor(dt_for_plot$Nb_modalite)
  
    
  x <- "Montant_initial"
  y <- "coefficiant"
  color <- "Nb_modalite"
  shape <- "snignif"
  shapelabel <- "Significativité du coefficiant"
  xlabel <- "Montant d'héritage minimal pouvant être considéré comme conséquant"
  ylabel <- ""
  colorlabel <- "Nombre de modalités de X\nexplicatives de G à 1%"
  caption_text <- "Y = Valeur en euro de la résidence principale\n
                      G = Fait de recevoir un héritage ou un don supérieur au montant minimal\n
                      X = Ensemble de variables socio-économiques du ménage"
  size <- "Effectifs"
  sizelabel <- "% de ménages\nayant reçu un héritage ou un don\nsignificatif"
  
  p <- ggplot(data = dt_for_plot, aes(x=dt_for_plot[[x]], y = dt_for_plot[[y]], color = dt_for_plot[[color]], shape = dt_for_plot[[shape]], size = dt_for_plot[[size]])) +
    geom_point() +
    labs(title=titre,
         x= xlabel,
         y= ylabel,
         color = colorlabel,
         shape=shapelabel,
         size=sizelabel) +
    scale_y_continuous(labels = function(y) format(y, scientific = FALSE)) +
    scale_x_continuous(trans='log10', labels = scales::dollar_format(
      prefix = "",
      suffix = " €",
      big.mark = " ",
      decimal.mark = ","), n.breaks = 10) +
    scale_color_viridis(discrete = TRUE) +
    theme(axis.text.x = element_text(angle = 22.5, vjust = 0.5, hjust=1)) +
    labs(caption = caption_text)
  


  # melted_loc <- melt(dt_for_plot,
  #                    id.vars = c("Montant_initial", "snignif", "Nb_modalite"),
  #                    measure.vars = c("Y sur G : pvalue", "Y sur G : coefficiant", "G sur X : Nombre de variables dignificatives à 1%"),
  #                    variable.name = "variable",
  #                    value.name    = "value")
  # 
  # 
  # x <- "Montant_initial"
  # y <- 'value'
  # shape <- "snignif"
  # shapelabel <- "Significativité du coefficiant"
  # color <- "Nb_modalite"
  # colorlabel <- "Nombre de modalités de X\nexplicatives de G à 1%"
  # facet <- "variable"
  # xlabel <- "Montant d'héritage minimal pouvant être considéré comme conséquant"
  # ylabel <- ""
  # caption_text <- "Y = Valeur en euro de la résidence principale\n
  #                     G = Fait de recevoir un héritage ou un don supérieur au montant minimal\n
  #                     X = Ensemble de variables socio-économiques du ménage"
  # 
  # 
  # 
  # 
  # p <- ggplot(data = melted_loc, aes(x=melted_loc[[x]], y = melted_loc[[y]], color = melted_loc[[color]], shape = melted_loc[[shape]])) +
  #   geom_point() +
  #   # geom_line() +
  #   # geom_smooth( method = 'gam', se = FALSE, span = 0.3) +
  #   labs(title=titre,
  #        x= xlabel,
  #        y= ylabel,
  #        color = colorlabel,
  #        shape=shapelabel) + 
  #   scale_y_continuous(labels = function(y) format(y, scientific = FALSE)) + 
  #   scale_x_continuous(trans='log10', labels = scales::dollar_format(
  #     prefix = "",
  #     suffix = " €",
  #     big.mark = " ",
  #     decimal.mark = ","), n.breaks = 10) + 
  #   scale_color_viridis(discrete = TRUE) +
  #   theme(axis.text.x = element_text(angle = 22.5, vjust = 0.5, hjust=1)) +
  #   facet_wrap(~factor(melted_loc[[facet]]),  scales = "free", ncol = 2) +
  #   labs(caption = caption_text)
  # 
  
  ggsave(titre_save, p ,  width = 297, height = 210, units = "mm")
  print(p)  
}

## Deux sous-fonctions qui font les régressions :
regression_heritage_valeur_hmr <- function(data_loc, montant_ini_loc, col_montant_bien){
  data_loc$Reg_Y <- data_loc[[col_montant_bien]]
  # data_loc[, Reg_Y := DA1110]
  data_loc[, Reg_G := 0]
  data_loc[Annee_achat_heritage >= 0 & Montant_heritage_1 >= montant_ini_loc, Reg_G := 1] # Reçu un héritage avant l'achat
  
  # data_loc$Reg_y
  
  liste_cols_reg_poids <- c("HW0010", "Reg_G", "Reg_Y")
  dw <- svydesign(ids = ~1, data = data_loc[,..liste_cols_reg_poids], weights = ~ HW0010)
  mysvyglm <- svyglm(formula = Reg_Y ~ Reg_G, design = dw)
  dt_prep_reg_lin <- as.data.table(summary(mysvyglm)$coefficients, keep.rownames = TRUE)  
  setnames(dt_prep_reg_lin, "Pr(>|t|)", "pvalue")
  return(dt_prep_reg_lin[rn == "Reg_G"])
}

regression_G_heritage <- function(data_loc, montant_ini_loc){
  data_loc[, Reg_G := 0]
  data_loc[Annee_achat_heritage >= 0 & Montant_heritage_1 >= montant_ini_loc, Reg_G := 1] # Reçu un héritage avant l'achat
  
  denylogit <- glm(Reg_G ~ DHAGEH1B + DHEDUH1 + DHGENDERH1 + DI2000 + DHHTYPE + DHEMPH1 + PE0300_simpl, 
                   family = binomial(link = "logit"), 
                   data = data_loc)
  dt_prep_logit <- as.data.table(summary(denylogit)$coefficients, keep.rownames = TRUE)
  setnames(dt_prep_logit, "Pr(>|z|)", "pvalue")
  return(dt_prep_logit)
}


