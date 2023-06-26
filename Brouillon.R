
library(rgdal)
library(mapdata)
library(mapproj)
library(maps)
library(ggplot2)
library(ggrepel)
# library(legendMap)
# install.packages("legendMap", dependencies=TRUE)

library(dplyr)

mySHP = "C:/Users/Benjamin/Desktop/Carte_UE"
myFile = readOGR(mySHP, layer = "CNTR_BN_60M_2020_3035", stringsAsFactors = FALSE)

head(myFile@data, 6)
colnames(myFile@data)

# myFile@data$CNTR_CODE

myDF = fortify(myFile, region = "CNTR_CODE")

# myDF <- as.data.table(myFile)


head(myDF, 6)


myDF = rename(myDF, Longitude = long, Latitude = lat)

dt_gini <- as.data.table(list(liste_pays, liste_gini))
setnames(dt_gini, "V1", "id")
dt_gini$id <- as.character(dt_gini$id)

plotData <- left_join(myDF, dt_gini)














as.data.table(myFile)





data_complete <- data_complete %>% mutate_at(liste_var_continues, as.numeric)
data_complete <- data_complete %>% mutate_at(liste_var_categorielles, as.factor)



calcul_gini_pays <- function(data_loc){
  data_for_plot <- as.data.table(1:nb_quantiles)
  setnames(data_for_plot, 'V1', "Quantiles")
  data_for_plot$Quantiles <- as.numeric(data_for_plot$Quantiles)
  data_loc[, Quantiles := 
             hutils::weighted_ntile(get(type_pat), weights =  sum(HW0010), nb_quantiles)]
  data_for_plot_loc <- data_loc[,
                                lapply(.SD, sum, na.rm = TRUE), 
                                by = .(Quantiles),
                                .SDcols = names(data_loc) == type_pat][order(Quantiles)]
  data_for_plot_loc[, cum_sum := 100*cumsum(get(type_pat))/sum(get(type_pat), na.rm=TRUE)]
  data_for_plot_loc$Quantiles <- as.numeric(data_for_plot_loc$Quantiles)
  data_for_plot <- merge(data_for_plot, data_for_plot_loc, by = "Quantiles")
  return(calcul_gini(data_for_plot$cum_sum))
}


nb_quantiles <- 100
type_pat <- "DN3001"

liste_pays <- levels(data_complete[VAGUE == num_vague]$SA0100)
liste_gini <- c()
for(pays in liste_pays){
  gini <- calcul_gini_pays(data_complete[VAGUE == num_vague & SA0100 == pays])
  liste_gini <- append(liste_gini, gini)
}



df <- as.data.frame(do.call(cbind, list(liste_pays, liste_gini)))
setnames(df, c("V1", "V2"), c("Pays", "Gini"))
df

paste(repo_data, "/Data_intermediaire/Gini_carte.csv", sep = "")
write.csv(df, paste(repo_data, "/Data_intermediaire/Gini_carte.csv", sep = ""), row.names=FALSE)













library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("rgeos")
library("ggspatial")
library("ggrepel")
library("tidyverse")


library(rnaturalearth)
library(sf)
library(wbstats)

world <- ne_countries(scale="medium", returnclass="sf") %>% filter(admin != "Antartica")

# CHange World map projections
target_crs <- "+proj=moll"
world_moll <- world %>% st_transform(crs=target_crs)


dt_gini$name_sort <- dico_pays_anglais[dt_gini$id]


dt_world_moll <- as.data.table(world)
dt_gini <- as.data.table(dt_gini)

merged <- merge(dt_world_moll, dt_gini, how = 'left', on = "name_sort")

merged <- merged %>% st_transform(crs=target_crs)



world_moll %>%
  left_join(dt_gini, by = c('name_sort' = 'nom_pays'))


world_moll.join(dt_gini, on = "name_sort")

world_moll %>%
  left_join(dt_gini, by = "name_sort")

left_join(world_moll, dt_gini, by.x="name_sort", by.y="nom_pays")

class(world_moll)
$name_sort)
class(dt_gini$nom_pays)

  ggeplot()

world_moll$name_sort

nrow(world_moll)





# titre = "Carte d'Europe des indices de Gini sur les patrimoines net"
# 
# map_path = "C:/Users/Benjamin/Desktop/IWEPS/Data/Data_intermediaire/" + "world-administrative-boundaries.geojson"
# data_path = "C:/Users/Benjamin/Desktop/IWEPS/Data/Data_intermediaire/Gini_carte.csv"
# 
# def generation_map_Gini_UE(data_path, titre, titre_save = "C:/Users/Benjamin/Desktop/IWEPS/Sorties/Carte_Gini_pat_net.pdf") :



library(reticulate)



py$titre <- "Carte d'Europe des indices de Gini sur les patrimoines net"
py$map_path <- "C:/Users/Benjamin/Desktop/IWEPS/Data/Data_intermediaire/world-administrative-boundaries.geojson"
py$data_path <- "C:/Users/Benjamin/Desktop/IWEPS/Data/Data_intermediaire/Gini_carte.csv"
py$titre_save <- "C:/Users/Benjamin/Desktop/IWEPS/Sorties/Carte_Gini_pat_net.pdf"

# def generation_map_Gini_UE(data_path, titre, titre_save = "C:/Users/Benjamin/Desktop/IWEPS/Sorties/Carte_Gini_pat_net.pdf") :



# a <- 1

# py$a <- 1


py_run_file(paste(repo_prgm, "/Map_Gini_EU.py", sep = ""))



# inspect <- import("inspect")

converted_func <- r_to_py(generation_map_Gini_UE(data_path, titre, titre_save))

repo_prgm


