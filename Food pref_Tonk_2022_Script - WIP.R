############################################
######    Food preference - Script    ######
######    Tonk - Mars Avril 2022    ######
############################################

## Charger les packages n?cessaires
library(EloRating)
library(dplyr)
library(ggplot2)

## Definir le repertoire de travail
# = depend du pc

## Definir les donnees a utiliser
# On a notre fichier "raw", on le convertit en format utilisable
Data_Abricot_raw <- read_excel("C:/Users/chtid/Desktop/Stage DPZ/data/Data_Abricot_raw.xlsx")
foodpref <- Data_Abricot_raw
#on recode pour les formats
foodpref$Date <- as.Date(foodpref$Date)
foodpref$Gauche <- as.character(foodpref$Gauche)
foodpref$Droite <- as.character(foodpref$Droite)
foodpref$Choix <- as.character(foodpref$Choix)

# delete unnecessary columns, rename Winner for elo and add looser
foodpref <- foodpref[,-c(2,3)]
foodpref$Winner <-foodpref$Choix
foodpref <-foodpref[,-4]
foodpref$Loser <- NA

#Cree la colonne winner/looser en fonction du choix de l'individu
for(i in 1:nrow(foodpref)){
  # On ecrit le choix pour faire le winner et looser
  if(foodpref$Winner[i] == foodpref$Droite[i]) {
    foodpref$Loser[i] <- foodpref$Gauche[i]
  }
  else(foodpref$Loser[i] <- foodpref$Droite[i])
  }

#on enlève gauche-droite
foodpref <- foodpref[,-c(2,3)]

# On a notre tableau foodpref sur lequel on run le code

## La fonction elo.seq = Calculate Elo ratings from a sequence of dominance interactions
res_foodpref <- elo.seq(winner=foodpref$Winner, loser=foodpref$Loser, Date=foodpref$Date, runcheck=TRUE)

## On fait un diagnostic des data pour voir si le elorating va pouvoir etre fait sans probleme
seqcheck(winner=foodpref$Winner, loser=foodpref$Loser, Date=foodpref$Date)

## On fait un resume du resultat
summary(res_foodpref)

## On fait la representation graphique des donnees
eloplot(res_foodpref)

## On demande les valeurs des scores individuels
extract_elo(res_foodpref)

## On demande une valeur de la stabilite de la hierarchie entre deux dates
# ATTENTION: pour la date de debut il faut mettre j+1 car les calculs commencent entre j1 et J2 
# et ne sont donc disponibles qu'a partir de J2
stab_elo(res_foodpref,from="2022-03-23", to="2022-04-05")



###################
### Création de la matrice de dominance

# directement à partir du fichier .txt
matrice_foodpref <- creatematrix(winners = foodpref$Winner, losers = foodpref$Loser)

# ou à partir de l'objet elo res_Mous cree avant avec elo.seq
matrice_foodpref <- creatematrix(res_foodpref)

# Pour obtenir l'indice de linearite h et h', et p-value ("p right")
h.index(matrice_foodpref, loops = 1000)

# Pour obtenir les incoherences
incontable(matrice_foodpref)

extract_elo(res_foodpref)
####### JEN SUIS LA              
#On crer un tableau du elo
write.table(dfelo, file = "Data_Abricot.csv", sep = ",", row.names = F, quote = F)



#On peut faire un beau Barplot
ggplot(data=eloseq_eric, aes(x = reorder(Fruit,-Elo), y=Elo, fill = Fruit))+geom_bar(stat="identity")

#ESSAI GITHUB UPDATE