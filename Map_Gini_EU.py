import requests
import pandas as pd
import io

import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import plotly
import plotly.express as px
from IPython.display import HTML

import geopandas as gpd
import contextily as ctx

import urllib

from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler

import plotly.graph_objects as go
from plotly.subplots import make_subplots


import warnings
from math import isnan

# warnings.filterwarnings("ignore")

# titre = "Carte d'Europe des indices de Gini sur les patrimoines net"
# map_path = "C:/Users/Benjamin/Desktop/IWEPS/Data/Data_intermediaire/" + "world-administrative-boundaries.geojson"
# data_path = "C:/Users/Benjamin/Desktop/IWEPS/Data/Data_intermediaire/Gini_carte.csv"
# titre_save = "C:/Users/Benjamin/Desktop/IWEPS/Sorties/Carte_Gini_pat_net.pdf"


# print(a)

# def generation_map_Gini_UE(data_path, titre, titre_save = "C:/Users/Benjamin/Desktop/IWEPS/Sorties/Carte_Gini_pat_net.pdf") :
df = pd.read_csv(filepath_or_buffer=data_path)
sf = gpd.read_file(map_path)
sf = sf[(sf["continent"] == "Europe") & (sf["status"] == "Member State") & (sf["name"] != "Russian Federation")]
sf = sf.rename(columns = {"iso_3166_1_alpha_2_codes" : "id" })
df_pays = sf.merge(df, how = "left", left_on = "id", right_on = "Pays")
df_pays_simple = df_pays.loc[(df_pays["id"] != 'RU')]
df_pays_simple[['Gini_modif']] = df_pays_simple[['Gini']].fillna(0)
df_pays_simple["centre_pays"] = df_pays_simple.centroid


fig,ax = plt.subplots(figsize=(25, 25))
# fix, ax = plt.subplots(figsize=(8.27, 11.69))
df_pays_simple.plot(ax = ax, column = "Gini",
             legend=True,
            figsize=(20,20),
            missing_kwds={
                "color": "lightgrey",
                "edgecolor": "red",
                "hatch": "///",
                "label": "Missing values",
            },
             colormap = "viridis",
            vmin = 0, vmax = 1,
            legend_kwds={"label": "1 = Inégalité totale, 0 = Egalité parfaite", "orientation": "vertical"}
                   )


for num_ligne in range(len(df_pays_simple)) :
    ligne = df_pays_simple.iloc[num_ligne] # On répupère la ligne
    if not isnan(ligne["Gini"]) : #Uniquement si l'indice de Gini n'est pas NAN
        x = ligne['centre_pays'].x
        y = ligne['centre_pays'].y
        ax.annotate(np.round(ligne["Gini"], 2), xy = (x, y), xytext = (0, 0),
                    textcoords="offset points", ha='center', color='black', weight='bold', fontsize=20)

ax.set_axis_off()
# plt.colorbar(scatter, ax=ax, shrink=0.5)

# plt.legend(fontsize = 100)

plt.title(titre, fontsize = 40)
# ax.tick_params(labelsize=1)

# ax

plt.savefig(titre_save, format="pdf", bbox_inches="tight", papertype = 'a4', orientation = 'portrait')
# savefig("foo.eps", papertype = 'a4', orientation = 'portrait', format = 'eps')

# plt.show()