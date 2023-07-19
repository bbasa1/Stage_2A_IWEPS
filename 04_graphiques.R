################################################################################
# ====================== 04 FONCTIONS GRAPHIQUES ===============================
################################################################################
# Ici uniquement les fonctions graphiques

trace_barplot <- function(data_loc, x, sortby_x, y, fill, xlabel, ylabel, titre, titre_save, xlim_sup=100){
  p <- ggplot(data = data_loc, aes(x = reorder(.data[[x]], .data[[sortby_x]]), y = .data[[y]], fill = .data[[fill]])) +
    geom_bar(stat="identity", position=position_dodge()) + 
    labs(title=titre,
         x= xlabel,
         y= ylabel) + 
    scale_y_continuous(limits = c(0, xlim_sup), labels = function(y) format(y, scientific = FALSE)) + 
    scale_fill_viridis(discrete = TRUE) +
    scale_color_viridis() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
          text = element_text(size = 25))
  
  # p
  ggsave(titre_save, p ,  width = 297, height = 210, units = "mm")
  print(p)
}

trace_barplot_avec_facet <- function(data_loc, x, sortby_x, y, fill, xlabel, ylabel, titre, titre_save,facet, ordre_facet, xlim_sup=100){
  if (length(ordre_facet) > 2){
    p <- ggplot(data = data_loc, aes(x = reorder(.data[[x]], .data[[sortby_x]]), y = .data[[y]], fill = .data[[fill]])) +
      geom_bar(stat="identity", position=position_dodge()) + 
      labs(title=titre,
           x= xlabel,
           y= ylabel) + 
      scale_y_continuous(limits = c(0, xlim_sup), labels = function(y) format(y, scientific = FALSE)) + 
      scale_fill_viridis(discrete = TRUE) +
      scale_color_viridis() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
            text = element_text(size = 20)) +
      facet_grid(~factor(.data[[facet]], levels = ordre_facet))
  }else{
    p <- ggplot(data = data_loc, aes(x = reorder(.data[[x]], .data[[sortby_x]]), y = .data[[y]], fill = .data[[fill]])) +
      geom_bar(stat="identity", position=position_dodge()) + 
      labs(title=titre,
           x= xlabel,
           y= ylabel) + 
      scale_y_continuous(limits = c(0, xlim_sup), labels = function(y) format(y, scientific = FALSE)) + 
      scale_fill_viridis(discrete = TRUE) +
      scale_color_viridis() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
            text = element_text(size = 20)) +
      facet_wrap(~factor(.data[[facet]]), ncol = 2)
  }
  
  # p
  ggsave(titre_save, p ,  width = 297, height = 210, units = "mm")
  print(p)
}


trace_barplot_log <- function(data_loc, x,sortby_x, y, fill, xlabel, ylabel,filllabel, titre, titre_save){
    p <- ggplot(data = data_loc, aes(x = reorder(.data[[x]], .data[[sortby_x]]), y = data_loc[[y]], fill = data_loc[[fill]], ymin = ymin, ymax = ymax)) +
      geom_bar(stat="identity", position=position_dodge()) + 
      geom_errorbar(position=position_dodge(width=0.9), color = "purple", linetype = 1) +
      labs(title=titre,
           x= xlabel,
           y= ylabel,
           fill = filllabel) + 
      scale_y_continuous(trans='log10', labels = function(y) format(y, scientific = TRUE)) + 
      scale_fill_viridis(discrete = TRUE) +
      scale_color_viridis(discrete = TRUE) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), 
            text = element_text(size = 22))
    
  ggsave(titre_save, p ,  width = 297, height = 210, units = "mm")
  print(p)
}




trace_concentration <- function(data_melted_loc, x, y, color, xlabel, ylabel,colorlabel, titre_fig, titre_save, x_table){
  p <- ggplot(data_melted_loc) +
    geom_line(aes(x = data_melted_loc[[x]], y= data_melted_loc[[y]], color = data_melted_loc[[color]])) +
    labs(
      x = xlabel, 
      y = ylabel,
      color = colorlabel,
      title = titre_fig
    ) +
    scale_y_continuous(
      labels = scales::dollar_format(
        prefix = "",
        suffix = " %",
        big.mark = " ",
        decimal.mark = ",")
      # expand = c(0, 0)
    ) + 
    scale_x_continuous(
      labels = scales::dollar_format(
        prefix = "",
        suffix = " %",
        big.mark = " ",
        decimal.mark = ","),
      expand = c(0, 0)
    ) +
    scale_color_viridis_d() +
    theme(legend.text = element_text(angle = 0, vjust = 0.7, hjust = 0),
          axis.text.x = element_text(angle = 0, vjust = 0.5),
          legend.position = "right", 
          # legend.text = element_text(size=30),
          # legend.title = element_text(size=30),
          # legend.key.size = unit(2, 'cm')
          text = element_text(size = 25)
          ) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    annotate(geom = "table", x = 12.5, y = 100, label = list(x_table), 
             vjust = 1, hjust = 0)
  
  print(p)
  
  ggsave(titre_save, p ,  width = 297, height = 210, units = "mm")
}





trace_distrib_variable <- function(data_loc, x, fill, xlabel, ylabel,filllabel, titre, titre_save, liste_breaks_fill, liste_breaks_x, limits_x){
  ### Trace l'histogramme d'une variable. Avec ou sans sous-décomposition fill
  
  if(!is.na(fill)){
    p <- ggplot(data_loc, aes(x = data_loc[[x]], fill = data_loc[[fill]], weight = HW0010)) + 
      geom_histogram(binwidth=2, color="black", alpha = 0.75) +
      scale_fill_viridis(discrete = TRUE, breaks=liste_breaks_fill) +
      labs(title=titre,
           x= xlabel,
           y= ylabel,
           fill = filllabel) +
      scale_x_continuous(breaks = liste_breaks_x, limits = limits_x) +
      scale_y_continuous(labels = scales::dollar_format(
        prefix = "",
        suffix = "",
        big.mark = " ",
        decimal.mark = ",")) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            text = element_text(size = 25)) + 
      annotate("text", x=-25, y = Inf, vjust = 15, label= "Achat immobilier plus ancien", size=5) +
      annotate("text", x=25, y = Inf, vjust = 15, label= "Réception de l'héritage plus ancien", size=5)
    
    
    # annotate("text", x=-25, y=80, label= "Achat immobilier plus ancien", size=5) +
    # annotate("text", x=25, y=80, label= "Réception de l'héritage plus ancien", size=5)
  }else{
    p <- ggplot(data_loc, aes(x = data_loc[[x]], weight = HW0010)) + 
      geom_histogram(binwidth=2, color="black", alpha = 0.75) +
      labs(title=titre,
           x= xlabel,
           y= ylabel) +
      scale_x_continuous(breaks = liste_breaks_x, limits = limits_x) +
      scale_y_continuous(labels = scales::dollar_format(
        prefix = "",
        suffix = "",
        big.mark = " ",
        decimal.mark = ",")) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            text = element_text(size = 25)) +
      annotate("text", x=-25, y = Inf, vjust = 15, label= "Achat immobilier plus ancien", size=5) +
      annotate("text", x=25, y = Inf, vjust = 15, label= "Réception de l'héritage plus ancien", size=5)
  }
  
  ggsave(titre_save, p ,  width = 297, height = 210, units = "mm")
  print(p) 
}


trace_distrib_simple <- function(data_loc, x, fill, titre, titre_save, xlabel, ylabel, filllabel, trans="log10", faire_tableau = TRUE, suffix_x = " €",orientation_label=45){
  # On commence par récupérer la médiane pour pouvoir plot
  med <- data_loc[, median(get(x), na.rm = TRUE), by = get(fill)]
  setnames(med, "V1", paste(xlabel, "\n(Médiane)", sep =))
  setnames(med, "get", filllabel)
  
  # Puis il va falloir placer le tableau proprement...
  x_table <- max(min(data_loc[[x]], na.rm = TRUE), 1)*10
  nbins <- 50
  
  # Pour y on récupère le max de l'histogramme total, puis on multiplie par le coeff moyen pour avoir une idée du max, et on divise par un facteur
  h <- hist(data_loc[[x]], breaks  = nbins)
  coeffs <- data_loc$HW0010
  y_max <- max(h$counts)*max(coeffs)/50
  
  if(!faire_tableau){
    med <- NaN
  }
  
  p <- ggplot(data = data_loc,
           mapping = aes(data_loc[[x]], weight = HW0010, fill = data_loc[[fill]], weight = HW0010)) +
      geom_histogram(color="black", alpha=0.6, position="identity", bins=nbins) +
      scale_x_continuous(trans=trans, labels = scales::dollar_format(
        prefix = "",
        suffix = suffix_x,
        big.mark = " ",
        decimal.mark = ","), n.breaks = 30) +
      scale_color_brewer(palette="Dark2") +
      labs(title=titre,
           x= xlabel,
           y= ylabel,
           fill = filllabel) +
      scale_y_continuous(labels = scales::dollar_format(
        prefix = "",
        suffix = "",
        big.mark = " ",
        decimal.mark = ",")) +
    theme(axis.text.x = element_text(angle = orientation_label, vjust = 1, hjust=1),
          text = element_text(size = 20)) +
    annotate(geom = "table", x = x_table, y = y_max, label = list(med), 
             vjust = 1, hjust = 0)
  
    p
  
  ggsave(titre_save, p ,  width = 297, height = 210, units = "mm")
  print(p) 
}



trace_distrib_normalise <- function(data_loc, x, fill, titre, titre_save, xlabel, ylabel, filllabel,facet, trans="log10", nbins = 100, suffix_x = " €", orientation_label=45, xlim = c()){
  #Trace la distribution de la variable, mais normalisée pour sommer à 1
  
  p <- ggplot(data = data_loc,
              mapping = aes(data_loc[[x]], weight = HW0010, fill = data_loc[[fill]], weight = HW0010)) +
    geom_histogram(aes(y=..density..), color="black", alpha=0.75, position="dodge", bins=nbins) +
    scale_fill_viridis(discrete = TRUE) +
    labs(title=titre,
         x= xlabel,
         y= ylabel,
         fill = filllabel) +
    scale_y_continuous(labels = scales::dollar_format(
      prefix = "",
      suffix = "",
      big.mark = " ",
      decimal.mark = ",")) +
    theme(axis.text.x = element_text(angle = orientation_label, vjust = 1, hjust=1),
          text = element_text(size = 20)) +
    facet_wrap(~factor(.data[[facet]]), ncol = 2)
  
  if(length(xlim) == 2){
    p <- p +
      scale_x_continuous(trans=trans, limits = xlim, labels = scales::dollar_format(
        prefix = "",
        suffix = suffix_x,
        big.mark = " ",
        decimal.mark = ","), n.breaks = 20)
  }else{
    p <- p +
      scale_x_continuous(trans=trans, labels = scales::dollar_format(
        prefix = "",
        suffix = suffix_x,
        big.mark = " ",
        decimal.mark = ","), n.breaks = 20)
  }
  
  ggsave(titre_save, p ,  width = 297, height = 210, units = "mm")
  print(p) 
  
}


trace_courbes <- function(melted_loc, x, y, color, facet, xlabel, ylabel, colorlabel, titre, titre_save, caption_text = "", transformation_x="log10"){
  ### Trace les points contenus dans melted_loc
  
  p <- ggplot(data = melted_loc, aes(x=melted_loc[[x]], y = melted_loc[[y]], color = melted_loc[[color]])) +
    geom_point() +
    geom_line() +
    # geom_smooth( method = 'gam', se = FALSE, span = 0.3) +
    labs(title=titre,
         x= xlabel,
         y= ylabel,
         color = colorlabel) + 
    scale_y_continuous(labels = function(y) format(y, scientific = FALSE)) + 
    scale_x_continuous(trans=transformation_x, labels = scales::dollar_format(
      prefix = "",
      suffix = " €",
      big.mark = " ",
      decimal.mark = ","), n.breaks = 10) + 
    scale_color_viridis(discrete = TRUE) +
    theme(axis.text.x = element_text(angle = 22.5, vjust = 0.5, hjust=1),
          text = element_text(size = 17)) +
    facet_wrap(~factor(melted_loc[[facet]]),  scales = "free", ncol = 2) +
    labs(caption = caption_text)
  
  
  ggsave(titre_save, p ,  width = 297, height = 210, units = "mm")
  print(p)
}


trace_boxplot <- function(data_loc, x, fill, facet, titre, titre_save, xlabel, filllabel, xlim = c(0,70), suffix_x = ""){
  # Trace un boxplot : Carré = Q25, Q50 et Q75
  
  if(str_length(facet) > 0){# Alors on facet
    
    # On commence par préparer une case "Ensemble" 
    data_loc$Ensemble <- "Ensemble"
    melted_loc <- melt(data_loc, 
                       id.vars = c(x, fill, "HW0010"), 
                       measure.vars  = c(facet, "Ensemble"),
                       variable.name = "variable",
                       value.name    = "value")
    facet_trace <- "value"
  
        p <- ggplot(melted_loc, aes(x = melted_loc[[x]], fill = melted_loc[[fill]], weight = HW0010)) +
    geom_boxplot(color="black", outlier.alpha = 0.35)+
    scale_fill_viridis(discrete=TRUE) +
    labs(title=titre,
         x= xlabel,
         fill = filllabel) +
    scale_x_continuous(n.breaks = 20, limits = xlim,
                       labels = scales::dollar_format(
                         prefix = "",
                         suffix = suffix_x,
                         big.mark = " ",
                         decimal.mark = ",")) +
    facet_wrap(~factor(.data[[facet_trace]]), scales='free') +
    coord_flip() +
    theme(axis.text.x=element_blank(),
          text = element_text(size = 25))  #remove x axis labels
  }else{
    data_loc[[x]] <- as.numeric(data_loc[[x]] )
    p <- ggplot(data_loc, aes(x = data_loc[[x]], fill = data_loc[[fill]], weight = HW0010)) +
      geom_boxplot(color="black", outlier.alpha = 0.35)+
      scale_fill_viridis(discrete=TRUE) +
      labs(title=titre,
           x= xlabel,
           fill = filllabel) +
      scale_x_continuous(n.breaks = 20, limits = xlim,
                         labels = scales::dollar_format(
                           prefix = "",
                           suffix = suffix_x,
                           big.mark = " ",
                           decimal.mark = ",")) +
      coord_flip() +
      theme(axis.text.x=element_blank(),
            text = element_text(size = 25))  #remove x axis labels
  }
  
  ggsave(titre_save, p ,  width = 297, height = 210, units = "mm")
  print(p) 
}



trace_decomposition_fnt_decile <- function(data_loc, nom_decile, titre_save, titre, filllabel, xlabel){
  # Cette fonction trace la décomposition suivant une variable fill et une variable x
 
  fill <- 'HB2410_corr'
  data_for_plot <- data_loc[, sum(HW0010), by = c("HB2410_corr", nom_decile)]
  data_for_plot[[fill]] <- as.factor(data_for_plot[[fill]])
  tot_weight_decile <- data_loc[, sum(HW0010), by = eval(nom_decile)]
  setnames(tot_weight_decile, "V1", 'tot_decile')
  data_for_plot <- merge(data_for_plot, tot_weight_decile, on = nom_decile)
  data_for_plot[, V1_corr := 100*V1/tot_decile]
  
  p <- ggplot(data = data_for_plot, aes(x = .data[[nom_decile]], y = V1_corr, fill = .data[[fill]])) +
    geom_bar(stat="identity") + 
    labs(title=titre,
         x= xlabel,
         y= ylabel,
         fill = filllabel) + 
    scale_y_continuous(
      labels = scales::dollar_format(
        prefix = "",
        suffix = " %",
        big.mark = " ",
        decimal.mark = ",")
    ) +
    scale_fill_viridis(discrete = TRUE) +
    scale_color_viridis() +
    theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1),
          text = element_text(size = 25))
  
  ggsave(titre_save, p ,  width = 297, height = 210, units = "mm")
  print(p) 
}
