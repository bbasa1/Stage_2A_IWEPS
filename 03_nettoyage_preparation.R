################################################################################
# ====================== 03 NETTOYAGE DONNEES ==================================
################################################################################
# Ici toutes les fonctions qui crééent les graphiques précis. Elles utilisent notamment le script 04_graphiques.R

graphique_repartition_pat_quantile_sexe <- function(data_loc, var_decile, var_normalisation, titre, titre_save, xlabel, facet, xlim_sup = 15){
  # Prépare la table pour les graphiques de répartition du patrimoine par quantile, puis trace le graphique associé

  
  liste_var_groupby <- c(var_decile, var_normalisation)
  dots <- lapply(var_normalisation, as.symbol) #Penser à bien convertir pour ne pas avoir de problèmes...
  data_for_plot <- data_loc[, sum(HW0010), by = liste_var_groupby] #On calcule les effectifs
  data_for_plot <- data_for_plot %>% group_by(.dots = dots) %>% mutate(new = 100*V1/sum(V1)) # Pour la normalisation il faut faire attention à grouper par sexe
  data_for_plot <- as.data.table(data_for_plot)
  data_for_plot
  
  
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
  # data_loc <- data_for_plot
  
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
