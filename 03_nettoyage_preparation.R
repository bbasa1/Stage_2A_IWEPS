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


graphique_repartition_pat_quantile_sexe <- function(data_loc, var_decile, var_normalisation, titre, titre_save, xlabel, facet, xlim_sup = 15){
  # Prépare la table pour les graphiques de répartition du patrimoine par quantile, puis trace le graphique associé

  # On prépare la table
  liste_var_groupby <- c(var_decile, var_normalisation)
  dots <- lapply(var_normalisation, as.symbol) #Penser à bien convertir pour ne pas avoir de problèmes...
  data_for_plot <- data_loc[, sum(HW0010), by = liste_var_groupby] #On calcule les effectifs
  data_for_plot <- data_for_plot %>% group_by(.dots = dots) %>% mutate(new = 100*V1/sum(V1)) # Pour la normalisation il faut faire attention à grouper par sexe
  data_for_plot <- as.data.table(data_for_plot)
  
  # Puis on nettoie
  data_for_plot <- nettoyage_sexe(data_for_plot)
  data_for_plot <- nettoyage_type_menage(data_for_plot, "V1") # Pour faire des facets propres
  
  ### La partie tracé
  x <-var_decile
  sortby_x <- var_decile
  y <- "new"
  fill <- "Sexe"
  ylabel <-"% de la population belge"
  data_loc <- data_for_plot
  facet <- facet
  ordre_facet <- facet
  
  trace_barplot_avec_facet(data_loc, x, sortby_x, y, fill, xlabel, ylabel, titre, titre_save,facet, ordre_facet, xlim_sup=xlim_sup)
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
  
  
  # titre <- "Variance du patrimoine détenu par les ménages Belges\nIntervalles de confiance à 95%"
  # titre_save <- "variance_patrimoine.pdf"
  titre_save <- paste(repo_sorties, titre_save, sep ='/')
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


nettoyage_type_menage <- function(data_loc, var_sum){ # Renome proprement les modalités de DHHTYPE pour en faire une appélation compréhensible avec le nb de ménages concernés
  data_loc[, DHHTYPE:= factor( # On renome proprement
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
  data_loc[, Nb_personnes_conc := sum(get(var_sum)), by = DHHTYPE] #On ajoute le nb de personnes concernées
  data_loc$Nb_personnes_conc <- paste("\n(",round(data_loc$Nb_personnes_conc), " personnes concernées)", sep = "")
  data_loc$DHHTYPE <- paste(data_loc$DHHTYPE, data_loc$Nb_personnes_conc)
  return(data_loc)
}
