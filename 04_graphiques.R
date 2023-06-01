################################################################################
# ====================== 04 FONCTIONS GRAPHIQUES ===============================
################################################################################


trace_barplot <- function(data_loc, x, sortby_x, y, fill, xlabel, ylabel, titre, titre_save, xlim_sup=100){
  p <- ggplot(data = data_loc, aes(x = reorder(.data[[x]], .data[[sortby_x]]), y = .data[[y]], fill = .data[[fill]])) +
    geom_bar(stat="identity", position=position_dodge()) + 
    labs(title=titre,
         x= xlabel,
         y= ylabel) + 
    scale_y_continuous(limits = c(0, xlim_sup), labels = function(y) format(y, scientific = FALSE)) + 
    scale_fill_viridis(discrete = TRUE) +
    scale_color_viridis() +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
  
  # p
  ggsave(titre_save, p ,  width = 297, height = 210, units = "mm")
  print(p)
}

trace_barplot_log <- function(data_loc, x, y, fill, xlabel, ylabel,filllabel, titre, titre_save){
    p <- ggplot(data = data_loc, aes(x = data_loc[[x]], y = data_loc[[y]], fill = data_loc[[fill]])) +
      geom_bar(stat="identity", position=position_dodge()) + 
      labs(title=titre,
           x= xlabel,
           y= ylabel,
           fill = filllabel) + 
      scale_y_continuous(trans='log10', labels = function(y) format(y, scientific = TRUE)) + 
      scale_fill_viridis(discrete = TRUE) +
      scale_color_viridis() +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
    
  ggsave(titre_save, p ,  width = 297, height = 210, units = "mm")
  print(p)
}




trace_concentration <- function(data_melted_loc, x, y, color, xlabel, ylabel,colorlabel, titre_fig, titre_save){
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
    scale_color_viridis_d() +
    theme(legend.text = element_text(angle = 0, vjust = 0.7, hjust = 0),
          axis.text.x = element_text(angle = 45, vjust = 0.5),
          legend.position = "right") +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed")
  
  print(p)
  
  ggsave(titre_save, p ,  width = 297, height = 210, units = "mm")
}
