
# Il a été demandé de continuer le travail entamé en cours.

# Importing libraries
library("tidyverse")
library("haven")
library("questionr")
library("readxl")

# Importing dataframe------------------------------------------------------------------------------
cereales <-read_dta("cereales.dta")
View(cereales)
str(cereales)

## Renaming the variables
colnames(cereales)[4:14] <- c("AutresCereales","Qtty_cons",
                              "Unite_cons","Taille_cons",
                              "AutoCons","AutresProv",
                              "DernierAchat","Qtty_achat",
                              "Unite_achat","Taille_achat",
                              "Value_achat")




# Les variables factor crées la dernière fois (dans le homework passé) avaient un système d'étiquettage inverse à celui demandé

# Ainsi, nous allons reprendre cette partie-------------------------------------------------------------


## Renommer, créer, labeliser, recoder les variables

# On regroupe les variables à recoder dans une liste
# To rec sera  donc la liste des variables à recoder
to_rec <- list()
for (i in names(cereales)){
  if(is.null(attr(cereales[[i]], "labels"))== 0){ # On met deux crochets pour préciser que c'est la colonne que l'on veut
    to_rec <- c(to_rec, i)
  }
}

to_rec 

# Création des variables recodées

noms <- list()
for (i in to_rec ){

  
  nom <- paste0("Var_rec_", i) # nom_variable
  
  
  # Creation des variables
                cereales$A <- factor(cereales[[i]], labels= names(attr(cereales[[i]], "labels")), 
                                levels=unname(attr(cereales[[i]], "labels")))
  
  
 
  attr(cereales$A, "label") <- paste0(attr(cereales$Unite_achat, "label"),"_recoded") # label des variables
  noms[[nom]] <- cereales$A # Ajout à la liste
}


# noms est une liste de vecteurs, nos nouvelles variables
noms

# On supprime A
cereales$A <- NULL

# Transformation en dataframe puis merging with cereales
noms <- as.data.frame(noms)
cereales <- cbind.data.frame(cereales, noms)
View(cereales)



# Nous avions déjà fait une jointure des deux bases avec left_join
# Nous allons donc le faire ici avec merge.-----------------------------------------------------------


table_conv <- read_xlsx("Table de conversion phase 2.xlsx", sheet = "nationale")
names(table_conv)
# On renomme les colonnes d'abord
names(table_conv) <- c("produitID",  "Var_rec_cereales__id" ,"uniteID" ,  "Var_rec_Unite_cons"  , "tailleID"  , "Var_rec_Taille_cons" , "poids" , "...8" ,     
                        "...9")
# On fusionne les bases;

cereales <- merge(cereales, table_conv, by= c("Var_rec_cereales__id", "Var_rec_Unite_cons", "Var_rec_Taille_cons"), all.x= TRUE)
# A noter que la variable poids génère 8425 NA avec cet code...
View(cereales)


