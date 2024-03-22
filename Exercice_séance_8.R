
# This work has been done with the help of Alioune Abdou Salam Kane


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

cereales <- cereales[!(is.na(cereales$Qtty_cons)),] # Je n avais pas enlevé les NA

View(cereales)
### HomeWork____________________


# calculer la quantite achete en kg; ------------------------------------------------------------------------


 cereales <- data.table(cereales)
 cereales[, Qtty_achat_tt:= Qtty_achat*as.numeric(poids)/1000] # Comme dans le cours...

 
# calculer le prix unitaire ;-------------------------------------------------------------------------------

 
cereales[, Unit_price:= Value_achat/Qtty_achat]

 
# Calculer les depenses de consommation : Prix unitaire* qtty_cons -------------------------------------------------------------------

 
 cereales[, dpses_cons := Unit_price*Qtty_cons]
 
 
# Valeurs aberrantes :: corrections ; ------------------------------------------------------------------------

 cereales[, Qtty_cons_tt := as.numeric(poids)*Qtty_cons/1000]
 
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
#'Cette aberrance est en fait due à la quantité consommée (21), on va donc agir à ce niveau
#'Puis nous allons récréer notre variable qtty_cons_tt  

cereales_imp <- cereales # Agissons sur un autre data table

# Récupération de l'index
ind_max <-which.max(cereales_imp[cereales__id == 1 | cereales__id == 2 | cereales__id == 3 |cereales__id ==4,
                                 Qtty_cons_tt ])

# "Imputation par la moyenne" : On affecte à la valeur max, la moyenne des autres 

cereales_imp[ind_max, "Qtty_cons"] <- floor(mean(cereales_imp[cereales__id %in% c(1,2,3,4), Qtty_cons[-ind_max]], na.rm = T))

cereales_imp[, "Qtty_cons_tt"] <- NULL

cereales_imp[, Qtty_cons_tt := as.numeric(poids)*Qtty_cons/1000]

# Vérification...
cereales_imp[ind_max, "Qtty_cons_tt"]
cereales_imp[cereales__id %in% c(1,2,3,4),
          boxplot(Qtty_cons_tt)]


#'La valeur abtenue (650) est toujours une valeur aberrante...
#' Et si on faisait une imputation par la médiane ?

median(cereales_imp[cereales__id %in% c(1,2,3,4), Qtty_cons[-ind_max]], na.rm = T)

#'La valeur de la médiane ci-dessus est de 13.5, valeur proche de la moyenne
#'Ainsi, une imputation par la médiane donnerait un résultat similaire



##------------------------------------------FIN_FIN_FIN--------------------------------------------------------------





