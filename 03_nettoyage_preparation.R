################################################################################
# ====================== 03 NETTOYAGE DONNEES ==================================
################################################################################



graphique_repartition_pat_quantile_sexe <- function(data_loc, var_decile, titre, titre_save, xlabel, xlim_sup = 15){
  # Prépare la table pour les graphiques de répartition du patrimoine par quantile, puis trace le graphique associé
  
  var_normalisation <- "DHGENDERH1" #On va mettre fill et normalisation par sexe
  liste_var_groupby <- c(var_decile, var_normalisation)
  dots <- lapply(var_normalisation, as.symbol) #Penser à bien convertir pour ne pas avoir de problèmes...
  data_for_plot <- data_loc[, sum(HW0010), by = liste_var_groupby] #On calcule les effectifs
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
  
  ### La partie tracé
  x <-var_decile
  sortby_x <- var_decile
  y <- "new"
  fill <- "Sexe"
  ylabel <-"% de la population belge"
  data_loc <- data_for_plot
  
  trace_barplot(data_loc, x, sortby_x, y, fill, xlabel, ylabel, titre, titre_save, xlim_sup)
}
