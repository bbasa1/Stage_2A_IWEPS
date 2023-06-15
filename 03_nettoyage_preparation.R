################################################################################
# ====================== 03 NETTOYAGE DONNEES ==================================
################################################################################
# Ici toutes les fonctions qui crééent les graphiques précis. Elles utilisent notamment le script 04_graphiques.R et sont appelées dans le script 00_main.R



################################################################################
# -------------------------- LES FONCTIONS COMPLETES  --------------------------
################################################################################
# Ces fonctions partent de la table initiale et produisent un graphique complet
# Généralement il y a deux étapes : 
  # - Préparation de la table
  # - Appel d'une fonction de 04.graphiques.R pour le tracé


graphique_repartition_pat_quantile_sexe <- function(data_loc, var_decile, var_normalisation, titre, titre_save, xlabel, facet, xlim_sup = 20){
  # Prépare la table pour les graphiques de répartition du patrimoine par quantile, puis trace le graphique associé

  # On prépare la table
  liste_var_groupby <- c(var_decile, var_normalisation)
  dots <- lapply(var_normalisation, as.symbol) #Penser à bien convertir pour ne pas avoir de problèmes...
  data_for_plot <- data_loc[, sum(HW0010), by = liste_var_groupby] #On calcule les effectifs
  data_for_plot <- data_for_plot %>% group_by(.dots = dots) %>% mutate(new = 100*V1/sum(V1)) # Pour la normalisation il faut faire attention à grouper par sexe
  data_for_plot <- as.data.table(data_for_plot)
  
  data_loc_effectifs <- data_loc[, .N, by = liste_var_groupby]
  
  data_for_plot <- merge(data_for_plot, data_loc_effectifs, by = liste_var_groupby)
  
  # Puis on nettoie
  data_for_plot <- nettoyage_sexe(data_for_plot)
  data_for_plot <- nettoyage_type_menage(data_for_plot, "N") # Pour faire des facets propres
  
  ### La partie tracé
  x <-var_decile
  sortby_x <- var_decile
  y <- "new"
  fill <- "Sexe"
  ylabel <-"% de la population"
  data_loc <- data_for_plot
  facet <- facet
  ordre_facet <- facet
  
  trace_barplot_avec_facet(data_loc, x, sortby_x, y, fill, xlabel, ylabel, titre, titre_save,facet, ordre_facet, xlim_sup=xlim_sup)
}



graphique_contration_patrimoine <- function(data_loc, nb_quantiles, liste_type_patrimoines, titre_fig, titre_save){
  # On initialise
  data_for_plot <- as.data.table(1:nb_quantiles)
  setnames(data_for_plot, 'V1', "Quantiles")
  data_for_plot$Quantiles <- as.numeric(data_for_plot$Quantiles)
  # data_for_plot
  
  # On boucle sur les types de patrimoines
  for(type_pat in names(liste_type_patrimoines)){
    # Récupération des quantiles
    data_loc[, Quantiles := 
               hutils::weighted_ntile(get(type_pat), weights =  sum(HW0010), nb_quantiles)]
    data_for_plot_loc <- data_loc[,
                                  lapply(.SD, sum, na.rm = TRUE), 
                                  by = .(Quantiles),
                                  .SDcols = names(data_loc) == type_pat][order(Quantiles)]
    data_for_plot_loc[, cum_sum := 100*cumsum(get(type_pat))/sum(get(type_pat), na.rm=TRUE)]
    setnames(data_for_plot_loc, "cum_sum", liste_type_patrimoines[[type_pat]])
    
    data_for_plot_loc$Quantiles <- as.numeric(data_for_plot_loc$Quantiles)
    
    # Merge
    data_for_plot <- merge(data_for_plot, data_for_plot_loc, by = "Quantiles")
  }
  
  
  # Melt pour pouvoir tracer
  melted <- melt(data_for_plot, 
                 id.vars = "Quantiles", 
                 measure.vars  = as.data.frame(liste_type_patrimoines)$liste_type_patrimoines,
                 variable.name = "variable",
                 value.name    = "value")
  
  
  x <- "Quantiles"
  y <- "value"
  color <- "variable"
  xlabel <- "Part des ménages"
  ylabel <- "Richesse détenue (cumulatif)"
  colorlabel <- "Type de richesse"
  data_melted_loc <- melted
  
  trace_concentration(data_melted_loc, x, y, color, xlabel, ylabel,colorlabel, titre_fig, titre_save)
}




graphique_variance_pat_age <- function(data_loc, liste_type_patrimoines_loc, titre, titre_save){
  ## Le graphique de variance du patrimoine en fonction de l'âge
  
  # Préparation de la table
  data_for_plot_initial <- nettoyage_classe_age(data_loc) # On prépare un data.table initial qu'on va augmenter pour le tracé
  data_for_plot <- data_for_plot_initial[, sum(HW0010), by = c("classe_age", "age_min")]
  setnames(data_for_plot, "V1", "Effectifs")
  for(type_pat in names(liste_type_patrimoines_loc)){
    classe_age <- unique(data_for_plot$classe_age) # On créé des listes temporaires pour stocker les valeurs
    liste_variances <- 1:length(classe_age)
    liste_conf_inter <- 1:length(classe_age)
    
    for(num_age in 1:length(classe_age)){
      age <- classe_age[num_age]
      data_loc <- data_for_plot_initial[classe_age == age] 
      if(nrow(data_loc) >= 2){
        dw_loc <- svydesign(ids = ~1, data = data_loc, weights = ~ data_loc$HW0010)
        variance <- svyvar(~get(type_pat), design = dw_loc, na.rm=TRUE)
        liste_variances[num_age] <- variance[1]
        liste_conf_inter[num_age] <- SE(variance)[1]
      }else{
        liste_variances[num_age] <- 0
        liste_conf_inter[num_age] <- 0
      }
    }
    
    data_for_plot_loc <- data.frame(classe_age,liste_variances, liste_conf_inter) #On ajoute ces listes temporaires au data.table initial
    data_for_plot_loc <- as.data.table(data_for_plot_loc)
    setnames(data_for_plot_loc, "liste_variances", liste_type_patrimoines_loc[type_pat])
    setnames(data_for_plot_loc, "liste_conf_inter", paste(liste_type_patrimoines_loc[type_pat], "_SE", sep = ""))
    data_for_plot <- merge(data_for_plot_loc, data_for_plot, by = "classe_age")
  }
  
  
  
  # Melt pour pouvoir tracer
  melted <- melt(data_for_plot, 
                 id.vars = c("classe_age", "age_min"), 
                 measure.vars  = as.data.frame(liste_type_patrimoines_loc)$liste_type_patrimoines_loc,
                 variable.name = "variable",
                 value.name    = "value")
  
  melted_SE <- melt(data_for_plot, 
                    id.vars = c("classe_age", "age_min"), 
                    measure.vars  = paste(as.data.frame(liste_type_patrimoines_loc)$liste_type_patrimoines_loc,"_SE", sep = ""),
                    variable.name = "variable",
                    value.name    = "value_SE")
  
  melted_SE$variable= gsub("_SE","",melted_SE$variable)
  merged_melted <- merge(melted, melted_SE, by = c("classe_age", "variable", "age_min"))
  merged_melted$value_SE <- 1.96*merged_melted$value_SE # Pour un interval à 95%
  merged_melted$ymin <- merged_melted$value - merged_melted$value_SE
  merged_melted$ymax <- merged_melted$value + merged_melted$value_SE
  
  
  x <-"classe_age"
  sortby_x <- "age_min"
  y <- "value"
  fill <- "variable"
  xlabel <-"Tranche d'âge de la personne de référence du ménage"
  ylabel <-"Variance du patrimoine (échelle log)"
  filllabel <- "Type de patrimoine"
  data_loc <- merged_melted[variable != "Effectifs"]
  
  trace_barplot_log(data_loc, x,sortby_x, y, fill, xlabel, ylabel,filllabel, titre, titre_save)
}


graphique_evolution_position_vagues <- function(data_vagues, nb_quantiles, liste_type_patrimoines, titre_fig, titre_save, faire_rang = TRUE, vagues_dispo = "123"){
  if(vagues_dispo == "123"){
    liste_cols <- apply(expand.grid(names(liste_type_patrimoines), c("_V1", "_V2", "_V3")), 1, paste, collapse="")
    liste_cols <- append(liste_cols, c("HW0010_V1"))
  }else{
    #### En France la vague 1 n'est pas panelisée...
    liste_cols <- apply(expand.grid(names(liste_type_patrimoines), c("_V2", "_V3")), 1, paste, collapse="")
    liste_cols <- append(liste_cols, c("HW0010_V2"))
  }
  
  data_for_plot_loc <- data_vagues[,..liste_cols]
  
  if(faire_rang){
    # On va tracer en fonction des rangs des ménages dans la distribution
    for(type_pat in names(liste_type_patrimoines)){
      # Rang de chaque ménage
      if(vagues_dispo == "123"){data_for_plot_loc[, RANG_V1 := rank(data_for_plot_loc[[paste(type_pat, "_V1", sep = "")]])]}
      data_for_plot_loc[, RANG_V2 := rank(data_for_plot_loc[[paste(type_pat, "_V2", sep = "")]])]
      data_for_plot_loc[, RANG_V3 := rank(data_for_plot_loc[[paste(type_pat, "_V3", sep = "")]])]
      
      if(vagues_dispo == "123"){setnames(data_for_plot_loc, "RANG_V1", paste("Rang_",type_pat,"_V1", sep = ""))}
      setnames(data_for_plot_loc, "RANG_V2", paste("Rang_",type_pat,"_V2", sep = ""))
      setnames(data_for_plot_loc, "RANG_V3", paste("Rang_",type_pat,"_V3", sep = ""))
      }
    }else{
      for(type_pat in names(liste_type_patrimoines)){
        if(vagues_dispo == "123"){setnames(data_for_plot_loc, paste(type_pat, "_V1", sep = ""), paste("Rang_",type_pat,"_V1", sep = ""))}
      setnames(data_for_plot_loc, paste(type_pat, "_V2", sep = ""), paste("Rang_",type_pat,"_V2", sep = ""))
      setnames(data_for_plot_loc, paste(type_pat, "_V3", sep = ""), paste("Rang_",type_pat,"_V3", sep = ""))
      }
    }

  
  # Mise en forme pour plot
  if(vagues_dispo == "123"){
    melted <- data.table(Rang_V1 = integer(),
                         Vague = factor(),
                         Quantile_vagues_suivantes  = integer(),
                         Type_patrimoine =  factor())
    
    for(type_pat in names(liste_type_patrimoines)){
      melted_loc <- melt(data_for_plot_loc, 
                         id.vars = c(paste("Rang_",type_pat,"_V1", sep = ""), "HW0010_V1"),
                         measure.vars  = names(data_for_plot_loc)[names(data_for_plot_loc) %in% c(paste("Rang_",type_pat,"_V2", sep = ""), paste("Rang_",type_pat,"_V3", sep = ""))],
                         variable.name = "Vague",
                         value.name    = "Quantile_vagues_suivantes")
      
      melted_loc[, Vague := factor(
        fcase(
          Vague == paste("Rang_",type_pat,"_V2", sep = ""), "Vague 2",
          Vague == paste("Rang_",type_pat,"_V3", sep = ""), "Vague 3"
        ))]
      
      melted_loc[, Type_patrimoine := liste_type_patrimoines[type_pat]]
      setnames(melted_loc, paste("Rang_",type_pat,"_V1", sep = ""), "Rang_V1")
      
      melted <- rbindlist(list(melted,melted_loc), fill=TRUE)
    }
  
    melted$HW0010_V1 <- as.numeric(melted$HW0010_V1)
  
  }else{
    melted <- data.table(Rang_V2 = integer(),
                         Vague = factor(),
                         Quantile_vagues_suivantes  = integer(),
                         Type_patrimoine =  factor())
    
    for(type_pat in names(liste_type_patrimoines)){
      melted_loc <- melt(data_for_plot_loc, 
                         id.vars = c(paste("Rang_",type_pat,"_V2", sep = ""), "HW0010_V2"),
                         measure.vars  = names(data_for_plot_loc)[names(data_for_plot_loc) %in% c(paste("Rang_",type_pat,"_V3", sep = ""))],
                         variable.name = "Vague",
                         value.name    = "Quantile_vagues_suivantes")
      
      melted_loc[, Vague := factor(
        fcase(
          # Vague == paste("Rang_",type_pat,"_V2", sep = ""), "Vague 2",
          Vague == paste("Rang_",type_pat,"_V3", sep = ""), "Vague 3"
        ))]
      
      melted_loc[, Type_patrimoine := liste_type_patrimoines[type_pat]]
      setnames(melted_loc, paste("Rang_",type_pat,"_V2", sep = ""), "Rang_V2")
      
      melted <- rbindlist(list(melted,melted_loc), fill=TRUE)
    }
    
    melted$HW0010_V2 <- as.numeric(melted$HW0010_V2)
  }
  
  ### La partie graphique
  if(vagues_dispo == "123"){
    x <- "Rang_V1"
      if(faire_rang){
      xlabel <- "Quantile de patrimoine à la vague 1"
      ylabel <- "Quantile de patrimoine aux vagues 2 et 3"
    }else{
      xlabel <- "Patrimoine à la vague 1"
      ylabel <- "Patrimoine aux vagues 2 et 3"
    }
    sizelabel <- "Poids du ménage à la vague 1"
    size <- "HW0010_V1"
  }else{
    x <- "Rang_V2"
    if(faire_rang){
      xlabel <- "Quantile de patrimoine à la vague 2"
      ylabel <- "Quantile de patrimoine à la vague 3"
    }else{
      xlabel <- "Patrimoine à la vague 2"
      ylabel <- "Patrimoine à la vague 3"
    }
    sizelabel <- "Poids du ménage à la vague 2"
    size <- "HW0010_V2"
  }

    
    
    
  y <- "Quantile_vagues_suivantes"
  color <- "Vague"
  scalelabel <- "Vague"
  facet <- "Type_patrimoine"
  colorlabel <- "Vague"
  
  if(faire_rang){
    # On peut tracer tel quel
    p <- ggplot(data = melted, aes(x = melted[[x]], y= melted[[y]], color = melted[[color]], size = melted[[size]])) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      labs(title=titre,
           x= xlabel,
           y= ylabel,
           color = colorlabel,
           size = sizelabel) +
      facet_wrap(~factor(.data[[facet]]), ncol = 2) +
      scale_size(range = c(1,3))
    }else{
      # Il faut passer en échelle log sinon c'est illisible
      if(vagues_dispo == "123"){
        melted[Rang_V1 <= 0, Rang_V1 := 1]
      }else{
        melted[Rang_V2 <= 0, Rang_V2 := 1]
      }
      melted[Quantile_vagues_suivantes <=  0, Quantile_vagues_suivantes := 1]
      
      p <- ggplot(data = melted, aes(x = melted[[x]], y= melted[[y]], color = melted[[color]], size = melted[[size]])) +
        geom_point() +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
        labs(title=titre,
             x= xlabel,
             y= ylabel,
             color = colorlabel,
             size = sizelabel) +
        facet_wrap(~factor(.data[[facet]]), ncol = 2) +
        scale_size(range = c(1,3)) +
        scale_x_continuous(trans='log10', labels = scales::dollar_format(
          prefix = "",
          suffix = " €",
          big.mark = " ",
          decimal.mark = ",")) +
        scale_y_continuous(trans='log10', labels = scales::dollar_format(
          prefix = "",
          suffix = " €",
          big.mark = " ",
          decimal.mark = ",")) +
        labs(caption = "Les ménages ayant un patrimoine négatif se sont vu assigner un patrimoine de 1€") +
        theme(legend.text = element_text(angle = 0, vjust = 0.7, hjust = 0),
              axis.text.x = element_text(angle = 45, vjust = 0.5),
              legend.position = "right")
      
      }
  
  ggsave(titre_save, p ,  width = 297, height = 210, units = "mm")
  print(p)
}







graphique_evolution_position_vagues_23 <- function(data_vagues, nb_quantiles, liste_type_patrimoines, titre_fig, titre_save, faire_rang = TRUE){
  #### En France la vague 1 n'est pas panelisée...
  liste_cols <- apply(expand.grid(names(liste_type_patrimoines), c("_V2", "_V3")), 1, paste, collapse="")
  liste_cols <- append(liste_cols, c("HW0010_V2"))
  data_for_plot_loc <- data_vagues[,..liste_cols]
  
  if(faire_rang){
    # On va tracer en fonction des rangs des ménages dans la distribution
    for(type_pat in names(liste_type_patrimoines)){
      # Rang de chaque ménage
      # data_for_plot_loc[, RANG_V1 := rank(data_for_plot_loc[[paste(type_pat, "_V1", sep = "")]])]
      data_for_plot_loc[, RANG_V2 := rank(data_for_plot_loc[[paste(type_pat, "_V2", sep = "")]])]
      data_for_plot_loc[, RANG_V3 := rank(data_for_plot_loc[[paste(type_pat, "_V3", sep = "")]])]
      
      # setnames(data_for_plot_loc, "RANG_V1", paste("Rang_",type_pat,"_V1", sep = ""))
      setnames(data_for_plot_loc, "RANG_V2", paste("Rang_",type_pat,"_V2", sep = ""))
      setnames(data_for_plot_loc, "RANG_V3", paste("Rang_",type_pat,"_V3", sep = ""))
    }
  }else{
    for(type_pat in names(liste_type_patrimoines)){
      # setnames(data_for_plot_loc, paste(type_pat, "_V1", sep = ""), paste("Rang_",type_pat,"_V1", sep = ""))
      setnames(data_for_plot_loc, paste(type_pat, "_V2", sep = ""), paste("Rang_",type_pat,"_V2", sep = ""))
      setnames(data_for_plot_loc, paste(type_pat, "_V3", sep = ""), paste("Rang_",type_pat,"_V3", sep = ""))
    }
  }
  
  
  # Mise en forme pour plot
  melted <- data.table(Rang_V2 = integer(),
                       Vague = factor(),
                       Quantile_vagues_suivantes  = integer(),
                       Type_patrimoine =  factor())
  
  for(type_pat in names(liste_type_patrimoines)){
    melted_loc <- melt(data_for_plot_loc, 
                       id.vars = c(paste("Rang_",type_pat,"_V2", sep = ""), "HW0010_V2"),
                       measure.vars  = names(data_for_plot_loc)[names(data_for_plot_loc) %in% c(paste("Rang_",type_pat,"_V3", sep = ""))],
                       variable.name = "Vague",
                       value.name    = "Quantile_vagues_suivantes")
    
    melted_loc[, Vague := factor(
      fcase(
        # Vague == paste("Rang_",type_pat,"_V2", sep = ""), "Vague 2",
        Vague == paste("Rang_",type_pat,"_V3", sep = ""), "Vague 3"
      ))]
    
    melted_loc[, Type_patrimoine := liste_type_patrimoines[type_pat]]
    setnames(melted_loc, paste("Rang_",type_pat,"_V2", sep = ""), "Rang_V2")
    
    melted <- rbindlist(list(melted,melted_loc), fill=TRUE)
  }
  
  melted$HW0010_V1 <- as.numeric(melted$HW0010_V1)
  
  ### La partie graphique
  x <- "Rang_V2"
  y <- "Quantile_vagues_suivantes"
  color <- "Vague"
  if(faire_rang){
    xlabel <- "Quantile de patrimoine à la vague 2"
    ylabel <- "Quantile de patrimoine à la vague 3"
  }else{
    xlabel <- "Patrimoine à la vague 2"
    ylabel <- "Patrimoine à la vague 3"
  }
  scalelabel <- "Vague"
  facet <- "Type_patrimoine"
  size <- "HW0010_V2"
  sizelabel <- "Poids du ménage à la vague 2"
  colorlabel <- "Vague"
  
  if(faire_rang){
    # On peut tracer tel quel
    p <- ggplot(data = melted, aes(x = melted[[x]], y= melted[[y]], color = melted[[color]], size = melted[[size]])) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      labs(title=titre,
           x= xlabel,
           y= ylabel,
           color = colorlabel,
           size = sizelabel) +
      facet_wrap(~factor(.data[[facet]]), ncol = 2) +
      scale_size(range = c(1,3))
  }else{
    # Il faut passer en échelle log sinon c'est illisible
    melted[Rang_V1 <= 0, Rang_V1 := 1]
    melted[Quantile_vagues_suivantes <=  0, Quantile_vagues_suivantes := 1]
    
    p <- ggplot(data = melted, aes(x = melted[[x]], y= melted[[y]], color = melted[[color]], size = melted[[size]])) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
      labs(title=titre,
           x= xlabel,
           y= ylabel,
           color = colorlabel,
           size = sizelabel) +
      facet_wrap(~factor(.data[[facet]]), ncol = 2) +
      scale_size(range = c(1,3)) +
      scale_x_continuous(trans='log10', labels = scales::dollar_format(
        prefix = "",
        suffix = " €",
        big.mark = " ",
        decimal.mark = ",")) +
      scale_y_continuous(trans='log10', labels = scales::dollar_format(
        prefix = "",
        suffix = " €",
        big.mark = " ",
        decimal.mark = ",")) +
      labs(caption = "Les ménages ayant un patrimoine négatif se sont vu assigner un patrimoine de 1€") +
      theme(legend.text = element_text(angle = 0, vjust = 0.7, hjust = 0),
            axis.text.x = element_text(angle = 45, vjust = 0.5),
            legend.position = "right")
    
  }
  
  ggsave(titre_save, p ,  width = 297, height = 210, units = "mm")
  print(p)
}






graphique_evolution_pat_entre_vagues <- function(data_loc, liste_type_patrimoines,liste_quantiles, titre, titre_save){
  # Trace les quantiles de l'évolution de patrimoine entre les vagues pour les ménages qui ont été suivis sur trois vagues
  
  data_for_plot <- as.data.table(liste_quantiles)
  
  for(type_pat in names(liste_type_patrimoines)){
    diff_V12 <- as.data.table(quantile(data_loc[[paste(type_pat, "V2", sep = "_")]] - data_loc[[paste(type_pat, "V1", sep = "_")]], probs = liste_quantiles))
    setnames(diff_V12, "V1", "Vagues 1-2")
    diff_V12[, liste_quantiles := liste_quantiles]
    
    diff_V23 <- as.data.table(quantile(data_loc[[paste(type_pat, "V3", sep = "_")]] - data_loc[[paste(type_pat, "V2", sep = "_")]], probs = liste_quantiles))
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
  print(p)
  ggsave(titre_save, p ,  width = 297, height = 210, units = "mm")
}



trace_distribution_X_non_X <- function(data_loc, liste_variables_loc, titre, titre_save, num_vague_loc, var_diff_loc = "DA1110I", liste_legendes_loc = c("Non_prop" = "Non propriétaires", "Prop" = "Propriétaires","Total" = "Total")){
  # Pour une variable binaire (propriétaire/non propriétaire, ou héritier/non héritier par exemple) trace la distribution des populations concernées pour pouvoir les comparer
  # Nettoyage préalable pour éviter les modalités en trop
  f_dowle2 <- function(DT){
    for (i in names(DT))
      DT[is.na(get(i)), (i):= "NAN"]
  }
  
  f_dowle2(data_loc)
  
  try(data_loc[DOEINHERIT == "A", DOEINHERIT := "NAN"], silent = TRUE)

  # On prépare les tables d'effectifs de ménages
  dw_V2 <- svydesign(ids = ~1, data = data_loc[VAGUE == 2], weights = ~ HW0010)
  dw_V3 <- svydesign(ids = ~1, data = data_loc[VAGUE == 3], weights = ~ HW0010)
  dw_V4 <- svydesign(ids = ~1, data = data_loc[VAGUE == 4], weights = ~ HW0010)
  
  
  # On empile tout
  dt_tableau <- data.table(Valeur = factor(),
                           Non_prop = numeric(),
                           Prop = numeric(),
                           # N.total = numeric(),
                           Variable = factor(),
                           Vague = factor())
  for(i in 2:4){
    for(var in names(liste_variables_loc)){
      txt <- paste("as.data.table(svytable(~ ",var_diff_loc," +",var,", dw_V",i,"))", sep = "")
      dt_loc <- eval(parse(text = txt))
      dt_loc <- reshape(dt_loc, idvar = var, timevar = var_diff_loc, direction = "wide")
      setnames(dt_loc, "N.0", "Non_prop")
      setnames(dt_loc, "N.1", "Prop")
      setnames(dt_loc, var, "Valeur")
      dt_loc[, Variable := var]
      dt_loc[, Vague := i]
      
      dt_tableau <- rbindlist(list(dt_tableau, dt_loc), fill=TRUE)
    }
  }
  dt_tableau[, Total := Non_prop + Prop]
  
  # On melt pour avoir le bon format
  melted <- melt(dt_tableau, 
                 id.vars = c("Valeur", "Variable", "Vague"),
                 measure.vars  = c("Non_prop", "Prop", "Total"),
                 variable.name = "Type",
                 value.name    = "Effectifs")
  
  melted[,Valeur_num := as.numeric(as.character(Valeur))]
  
  melted[Variable == "DHAGEH1" & Valeur_num <= 24, Valeur := "16 - 24 ans"]
  melted[Variable == "DHAGEH1" & Valeur_num %in% 25:34, Valeur := "25 - 34 ans"]
  melted[Variable == "DHAGEH1" & Valeur_num %in% 35:44, Valeur := "35 - 44 ans"]
  melted[Variable == "DHAGEH1" & Valeur_num %in% 45:54, Valeur := "45 - 54 ans"]
  melted[Variable == "DHAGEH1" & Valeur_num %in% 55:64, Valeur := "55 - 64 ans"]
  melted[Variable == "DHAGEH1" & Valeur_num >= 65, Valeur := "+ 65 ans"]
  
  
  melted[Variable == "DHGENDERH1" & Valeur_num == 1, Valeur := "Homme"]
  melted[Variable == "DHGENDERH1" & Valeur_num == 2, Valeur := "Femme"]
  
  melted[Variable == "DHEDUH1" & Valeur_num == 1, Valeur := "< Brevet"]
  melted[Variable == "DHEDUH1" & Valeur_num == 2, Valeur := "Brevet"]
  melted[Variable == "DHEDUH1" & Valeur_num == 3, Valeur := "Bac"]
  melted[Variable == "DHEDUH1" & Valeur_num == 5, Valeur := "> Bac"]
  
  melted[Variable == "DOEINHERIT" & Valeur_num == 0, Valeur := "Non"]
  melted[Variable == "DOEINHERIT" & Valeur_num == 1, Valeur := "Oui"]
  
  melted[Variable == "DOINHERIT" & Valeur_num == 0, Valeur := "Non"]
  melted[Variable == "DOINHERIT" & Valeur_num == 1, Valeur := "Oui"]

  melted[Variable == "DHEMPH1" & Valeur_num == 1, Valeur := "Employé.e"]
  melted[Variable == "DHEMPH1" & Valeur_num == 2, Valeur := "Indép-\nendant.e"]
  melted[Variable == "DHEMPH1" & Valeur_num == 3, Valeur := "Inactif.ve/\nchomeur.se"]
  melted[Variable == "DHEMPH1" & Valeur_num == 4, Valeur := "Retraité.e"]
  melted[Variable == "DHEMPH1" & Valeur_num == 5, Valeur := "Autre"]
  
  
  
  
  
  # melted[is.na(Valeur_num)]
  
  # On renome pour le facet
  for(var in names(liste_variables_loc)){
    melted[Variable == var, Variable := liste_variables_loc[var]]
  }
  
  for(var in names(liste_legendes_loc)){
    melted[Type == var, Type := liste_legendes_loc[var]]
  }
  
  setnames(melted, "Type", "Type de ménage")
  
  
  # Et on trace
  data_pour_plot <- melted[Vague == num_vague_loc]
  x <- "Valeur"
  sortby_x <- "Valeur_num"
  y <- "Effectifs"
  fill <- "Type de ménage"
  facet <- "Variable"
  xlabel <- "Valeur"
  ylabel <- "Effectifs"
  
  
  p <- ggplot(data = data_pour_plot, aes(x = reorder(.data[[x]], .data[[sortby_x]]), y = .data[[y]], fill = .data[[fill]])) +
    geom_bar(stat="identity", position=position_dodge()) + 
    labs(title=titre,
         x= xlabel,
         y= ylabel) + 
    scale_y_continuous(labels = scales::dollar_format(
      prefix = "",
      suffix = "",
      big.mark = " ",
      decimal.mark = ",")) + 
    scale_fill_viridis(discrete = TRUE) +
    # theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
    facet_wrap(~factor(.data[[facet]]), ncol = 2, scales="free") +
    labs(caption = "")
  
  print(p)
  
  ggsave(titre_save, p ,  width = 297, height = 210, units = "mm")
  
}





################################################################################
# -------------------------- LES SOUS-FONCTIONS  -------------------------------
################################################################################
# Ces fonctions sont des sous-fonctions appelées plus haut ou dans le script 00_main.R


nettoyage_classe_age <- function(data_loc){ # Créé des classes d'âge + larges que celles présentes dans DHAGEH1B
  data_loc$DHAGEH1 = as.numeric(data_loc$DHAGEH1)
  data_loc[, classe_age := factor(
    fcase(
      DHAGEH1 <= 24, "16 - 24 ans",
      DHAGEH1 %in% 25:34, "25 - 34 ans",
      DHAGEH1 %in% 35:44, "35 - 44 ans",
      DHAGEH1 %in% 45:54, "45 - 54 ans",
      DHAGEH1 %in% 55:64, "55 - 64 ans",
      DHAGEH1 >= 65, "+ 65 ans"
    )
  )
  ]
  data_loc[, age_min := factor( #Permet de trier ensuite par âge croissant
    fcase(
      DHAGEH1 <= 24, 16,
      DHAGEH1 %in% 25:34, 25,
      DHAGEH1 %in% 35:44, 35,
      DHAGEH1 %in% 45:54, 45,
      DHAGEH1 %in% 55:64, 55,
      DHAGEH1 >= 65, 65
    )
  )
  ]
  data_loc$age_min = as.numeric(data_loc$age_min)
  return(data_loc)
}

nettoyage_sexe <- function(data_loc){ # Renome les modalités de DHGENDERH1
  data_loc[, Sexe:= factor(
    fcase(
      DHGENDERH1 == 1, "Homme",
      DHGENDERH1 == 2, "Femme"
    )
  )
  ]
  return(data_loc)
}


nettoyage_education <- function(data_loc){
  data_loc[, label_education := factor(
    fcase(
      DHEDUH1 == 1, "< Brevet",
      DHEDUH1 == 2, "Brevet",
      DHEDUH1 == 3, "Bac",
      DHEDUH1 == 5, "> Bac"
    )
  )]
  return(data_loc)
}

nettoyage_DOINHERIT <- function(data_loc){
  data_loc[, label_DOINHERIT := factor(
    fcase(
      DOINHERIT == 0, "Non",
      DOINHERIT == 1, "Oui"
    )
  )]
  return(data_loc)
}

nettoyage_DOEINHERIT <- function(data_loc){
  data_loc[, label_DOEINHERIT := factor(
    fcase(
      DOEINHERIT == 0, "Non",
      DOEINHERIT == 1, "Oui"
    )
  )]
  return(data_loc)
}

nettoyage_DA1110I <- function(data_loc){
  data_loc[, label_DA1110I := factor(
    fcase(
      DA1110I == 0, "Non",
      DA1110I == 1, "Oui"
    )
  )]
  return(data_loc)
}

nettoyage_DA1120I <- function(data_loc){
  data_loc[, label_DA1120I := factor(
    fcase(
      DA1120I == 0, "Non",
      DA1120I == 1, "Oui"
    )
  )]
  return(data_loc)
}

nettoyage_type_menage <- function(data_for_plot_loc, var_sum){ # Renome proprement les modalités de DHHTYPE pour en faire une appélation compréhensible avec le nb de ménages concernés
  data_for_plot_loc[, DHHTYPE:= factor( # On renome proprement
    fcase(
      DHHTYPE == 51, "Adulte seul.e <= 64 ans",
      DHHTYPE == 52, "Adulte seul.e >= 65 ans",
      DHHTYPE == 6, "Couple <= 64 ans",
      DHHTYPE == 7, "Couple au moins un.e >= 65 ans",
      DHHTYPE == 8, ">= 3 adultes",
      DHHTYPE == 9, "Adulte seul.e avec enfant(s)",
      DHHTYPE == 10, "Couple avec 2 enfants",
      DHHTYPE == 11, "Couple avec 2 enfants",
      DHHTYPE == 12, "Couple avec >= 3 enfants",
      DHHTYPE == 13, ">= 3 adultes avec enfant(s)"
    )
  )
  ]
  data_for_plot_loc[, Nb_personnes_conc := sum(get(var_sum)), by = DHHTYPE] #On ajoute le nb de personnes concernées
  data_for_plot_loc$Nb_personnes_conc <- paste("\n(",round(data_for_plot_loc$Nb_personnes_conc), " ménages concernés)", sep = "")
  data_for_plot_loc$DHHTYPE <- paste(data_for_plot_loc$DHHTYPE, data_for_plot_loc$Nb_personnes_conc)
  return(data_for_plot_loc)
}



tableau_binaire <- function(var, data_loc){
  ## Renvoie un tableau avec les effectifs par variables
  
  loc <- copy(data_loc)
  loc[[var]] <- as.numeric(loc[[var]])
  loc[is.na(get(var)),  eval(var) := 1]
  loc[ get(var) == 3, eval(var):= 1]
  loc[ get(var) == 4, eval(var):= 1]
  
  loc1 <- loc[, sum(HW0010), by = var]
  loc2 <- loc[, .N, by = var]
  loc1$prop <- 100*loc1$V1/sum(data_pays[VAGUE == num_vague]$HW0010)
  merged <- merge(loc1, loc2, by = var)
  summed <- as.data.frame(t(colSums(merge(loc1, loc2, by = var))))
  
  
  loc3 <- rbindlist(list(merged,summed))
  
  loc3[[var]] <- as.character(loc3[[var]])
  loc3[round(prop) == 100, eval(var) := "Somme"]
  
  setnames(loc3, "V1", "Nombre ménages")
  setnames(loc3, "N", "Nombre observations")
  setnames(loc3, "prop", "% ménages")
  return(loc3)
}




Ajout_premier_heritage <- function(data_loc){
  # "HB0700 = year of property acquisition
  # "HH0201 = year gift/inheritance received
  # "HH0202"
  # "HH0203"
  
  ### Cette fonction créé une colonne qui indique l'année, et une autre qui indique le montant du PREMIER héritage obtenu dans le temps
  data_loc[, Annee_heritage_1 := pmin(HH0201, HH0202, HH0203, na.rm = TRUE)] ## C'est le premier héritage obtenu
  data_loc[, Montant_heritage_1 := factor(
    fcase(
      Annee_heritage_1 == HH0201, HH0401,
      Annee_heritage_1 == HH0202, HH0402,
      Annee_heritage_1 == HH0203, HH0403
    )
  )
  ]
  data_loc$Montant_heritage_1 <- as.numeric(as.character(data_loc$Montant_heritage_1)) ### ATTENTION laisser le as.character sinon ça bug je sais pas pourquoi...
  
  
  
  # Puis les objets qui ont été hérités
  for(lettre in c("A","B","C","D","E","F","G","H","I","J")){
    # Pour se protéger des modalités manquantes
    for(chiffre in 1:3){
      if(is.null(data_loc[[paste("HH030",chiffre, lettre, sep = "")]])){
        data_loc[[paste("HH030",chiffre, lettre, sep = "")]] <- as.factor(2)
      }
    }
    txt1 <- paste("levels(data_loc$HH0301",lettre,") <- c(1,2)", sep = "")
    txt2 <- paste("levels(data_loc$HH0302",lettre,") <- c(1,2)", sep = "")
    txt3 <- paste("levels(data_loc$HH0303",lettre,") <- c(1,2)", sep = "")
    eval(parse(text = txt1))
    eval(parse(text = txt2))
    eval(parse(text = txt3))
    
    
    txt <- paste("data_loc[, HH030",lettre,"_1 := factor(fcase(
        Annee_heritage_1 == HH0201, HH0301",lettre,",
        Annee_heritage_1 == HH0202, HH0302",lettre,",
        Annee_heritage_1 == HH0203, HH0303",lettre,"
      )
    )
    ]", sep = "")
    # print(txt)
    eval(parse(text = txt))
  }
  
  
  
  return(data_loc)
}

Ajout_premier_heritage_cons <- function(data_loc, montant_heritage_min){
  ### Cette fonction créé une colonne qui indique l'année, et une autre qui indique le montant du PREMIER héritage obtenu CONSEQUENT dans le temps
  data_loc[HH0401 >= montant_heritage_min, HH0201_cons := HH0201] #On recréé les colonnes des dates d'héritages, mais que si l'héritage est conséquent
  data_loc[HH0402 >= montant_heritage_min, HH0202_cons := HH0202]
  data_loc[HH0403 >= montant_heritage_min, HH0203_cons := HH0203]
  
  data_loc[, Annee_heritage_1_cons := pmin(HH0201_cons, HH0202_cons, HH0203_cons, na.rm = TRUE)] # Pour les ménages qui ont un premier héritage conséquent on met celui-là
  data_loc[is.na(Annee_heritage_1_cons), Annee_heritage_1_cons := Annee_heritage_1] # Pour les autres on peut mettre le premier héritage obtenu même s'il est faible
  data_loc[, Montant_heritage_1_cons := factor( #Puis on peut mettre le montant de l'héritage associé
    fcase(
      Annee_heritage_1_cons == HH0201, HH0401,
      Annee_heritage_1_cons == HH0202, HH0402,
      Annee_heritage_1_cons == HH0203, HH0403
    )
  )
  ]
  
  
  # Puis les objets qui ont été hérités
  for(lettre in c("A","B","C","D","E","F","G","H","I","J")){
    # Pour se protéger des modalités manquantes
    for(chiffre in 1:3){
      if(is.null(data_loc[[paste("HH030",chiffre, lettre, sep = "")]])){
        data_loc[[paste("HH030",chiffre, lettre, sep = "")]] <- as.factor(2)
      }
    }
    txt1 <- paste("levels(data_loc$HH0301",lettre,") <- c(1,2)", sep = "")
    txt2 <- paste("levels(data_loc$HH0302",lettre,") <- c(1,2)", sep = "")
    txt3 <- paste("levels(data_loc$HH0303",lettre,") <- c(1,2)", sep = "")
    eval(parse(text = txt1))
    eval(parse(text = txt2))
    eval(parse(text = txt3))
    
    
    txt <- paste("data_loc[, HH030",lettre,"_cons := factor(fcase(
        Annee_heritage_1_cons == HH0201, HH0301",lettre,",
        Annee_heritage_1_cons == HH0202, HH0302",lettre,",
        Annee_heritage_1_cons == HH0203, HH0303",lettre,"
      )
    )
    ]", sep = "")
    # print(txt)
    eval(parse(text = txt))
  }
  
  data_loc$Montant_heritage_1_cons <- as.numeric(as.character(data_loc$Montant_heritage_1_cons)) ### ATTENTION laisser le as.character sinon ça bug je sais pas pourquoi...
  return(data_loc)
}



Creation_donnees_panel <- function(data_loc){
  ###### Cette fonction met sous format long les données pannélisées
  
  # SA0200 = Survey vintage
  # SA0010 = household identification number
  # SA0210 = Vintage of last interview (household)
  # SA0110 = Past household ID
  
  # Pour faire l'évolution il faut commencer par mettre à 0 les patrimoines NaN
  data_loc[is.na(DA1000), DA1000 := 0]
  data_loc[is.na(DA2100), DA2100 := 0]
  data_loc[is.na(DL1000), DL1000 := 0]
  
  vague_1 <- data_loc[VAGUE == 1,] # On récupère les vagues
  vague_2 <- data_loc[VAGUE == 2,]
  vague_3 <- data_loc[VAGUE == 3,]
  vague_4 <- data_loc[VAGUE == 4,]
  
  colnames(vague_1) <- paste(colnames(vague_1),"V1",sep="_") # On renome pour ne pas avoir de conflits
  colnames(vague_2) <- paste(colnames(vague_2),"V2",sep="_")
  colnames(vague_3) <- paste(colnames(vague_3),"V3",sep="_")
  colnames(vague_4) <- paste(colnames(vague_4),"V4",sep="_")
  
  
  vague_12 <- merge(x = vague_2, y = vague_1, by.x = 'SA0110_V2',by.y = 'SA0010_V1')
  vague_123 <- merge(x = vague_3, y = vague_12, by.x = 'SA0110_V3',by.y = 'SA0010_V2') ## On s'arrête là parce qu'aucun ménage n'est enquêté 4x
  vague_1234 <- merge(x = vague_4, y = vague_123, by.x = 'SA0110_V4',by.y = 'SA0010_V3')
  
  vague_23 <- merge(x = vague_3, y = vague_2, by.x = 'SA0110_V3',by.y = 'SA0010_V2')
  vague_234 <- merge(x = vague_4, y = vague_23, by.x = 'SA0110_V4',by.y = 'SA0010_V3') ## PERSONNE N'EST EN PANEL A LA VAGUE 4 ?????
  
  vague_34 <- merge(x = vague_4, y = vague_3, by.x = 'SA0110_V4',by.y = 'SA0010_V3')
  
  return(list(vague_12, vague_23, vague_34, vague_123, vague_234, vague_1234))
}
