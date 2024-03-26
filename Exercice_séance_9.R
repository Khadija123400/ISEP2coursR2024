#' Il a été demandé de calculer les quantitiés achetées,
#'  de calculer les prix unitaires,
#'  d'estimer les dépenses de consommation
#'  et de corriger les valeurs aberrantes



# Importing libraries
library("tidyverse")
library("haven")
library("questionr")
library("readxl")
library("data.table")
# Importing dataframe------------------------------------------------------------------------------
cereales <-read_dta("cereales.dta")

## Renaming the variables
colnames(cereales)[4:14] <- c("AutresCereales","Qtty_cons",
                              "Unite_cons","Taille_cons",
                              "AutoCons","AutresProv",
                              "DernierAchat","Qtty_achat",
                              "Unite_achat","Taille_achat",
                              "Value_achat")

table_conv <- read_xlsx("Table de conversion phase 2.xlsx", sheet = "nationale")
cereales <- merge(cereales,table_conv, 
                  by.x = c("cereales__id","Unite_cons","Taille_cons"),
                  by.y = c("produitID","uniteID","tailleID"),
                  all.x = T)

cereales <- cereales[!(is.na(cereales$Qtty_cons)),] # On enlève les NA

setnames(cereales,"poids", "poids_cons")

cereales <- data.table(cereales)  # Converting it into a data table

# Quantités totales consommées
cereales[, Qtty_cons_tt:= Qtty_cons*as.numeric(poids_cons)/1000] 


### _________________________________________HomeWork______________________________________________________

# calculer la quantite achete en kg; ------------------------------------------------------------------------

# We merge using cereales__id, Unite_achat et Taille_achat

cereales <- left_join(cereales,table_conv,
                  by =c("cereales__id"= "produitID",
                  "Unite_achat"="uniteID",
                  "Taille_achat"="tailleID"))

# Renaming
setnames(cereales,"poids", "poids_achat")

cereales[, Qtty_achat_tt:= Qtty_achat*as.numeric(poids_achat)/1000] 

# calculer le prix unitaire (d'un kg);-------------------------------------------------------------------------------

cereales[, Unit_price:= Value_achat/Qtty_achat_tt]

# Calculer les depenses de consommation : Prix unitaire* Qtty_cons_tt -------------------------------------------------------------------

cereales[, dpses_cons := Unit_price*Qtty_cons_tt]

# Correction des valeurs aberrantes (On reconduit ce qu a été fait la dernière fois)---------------------------------------

# Pour tout ce qui est riz, identifions les valeurs aberrantes

# Summary
cereales[cereales__id == 1 | cereales__id == 2 | cereales__id == 3 |cereales__id == 4 ,  summary(Qtty_cons_tt)]

# Slice_max, pour voir les 5 plus grandes valeurs
cereales [cereales__id == 1 | cereales__id == 2 | cereales__id == 3 | cereales__id == 4 ,
          slice_max(cereales ,Qtty_cons_tt, n= 5)%>% 
            select(Qtty_cons, Qtty_cons_tt, cereales__id)] %>%  
  View()

# Boxplot, représentation graphique
cereales [cereales__id == 1 | cereales__id == 2 | cereales__id == 3 |cereales__id == 4,
          boxplot(Qtty_cons_tt)]

#'On considère alors la valeur aberrante qui est le maximum.
#'Cette aberrance est en fait due à la quantité consommée, on va donc agir à ce niveau
#'Puis nous allons récréer notre variable Qtty_cons_tt  

cereales_imp <- cereales # Agissons sur un autre data table

# Récupération de la ligne de cette valeur
ind_max <-which.max(cereales_imp[cereales__id == 1 | cereales__id == 2 | cereales__id == 3 |cereales__id ==4,
                                 Qtty_cons_tt ])

# "Imputation par la moyenne" : On affecte à la valeur max, la partie entière de la moyenne des autres 

cereales_imp[ind_max, "Qtty_cons"] <- floor(mean(cereales_imp[cereales__id %in% c(1,2,3,4), Qtty_cons[-ind_max]], na.rm = T))

cereales_imp[, "Qtty_cons_tt"] <- NULL

cereales_imp[, Qtty_cons_tt := as.numeric(poids)*Qtty_cons/1000] # On recalcule;


# Vérification...

cereales_imp[ind_max, "Qtty_cons_tt"]
cereales_imp[cereales__id %in% c(1,2,3,4),
             boxplot(Qtty_cons_tt)]

#'La valeur abtenue (650) est toujours une valeur aberrante...
#'Et si on faisait une imputation par la médiane ?

median(cereales_imp[cereales__id %in% c(1,2,3,4), Qtty_cons[-ind_max]], na.rm = T)

#'La valeur de la médiane ci-dessus est de 13.5 contre 13.38078 pour la moyenne.
#'Ainsi, une imputation par la médiane donnerait un résultat similaire


