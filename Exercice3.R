
# Ce travail a été fait en collaboration avec Alioune Abdou Salam Kane.

# Voici notre bas de données
Base <- data.frame(col1=c("+221","+223","+222","+224","+261","+237"), col2=c("Dakar","Bamako","Nouakchott","Conakry","Antananarivo","Douala"), col3=c(18, 16.7, 4.5, 13.1, 27.7, 26.5), col4=factor(c("Low","Low","Low","Med","Low","High"), levels=c("High","Med","Low")), col5=c(14,19, 13, 8, 22, 10), col6=c(19,13,13,7,13,3))

#Créer une matrice à partir données et renommenr les lignes et les colonnes
Matr_base <- as.matrix(Base)
rownames(Matr_base) <- c("Pays1","Pays2","Pays3","Pays4","Pays5", "Pays6")
colnames(Matr_base) <- c("Identif-phone","Centre_Activités","Pop (millions)","Niveau_vie","Nb_régions", "First_letter")
View(Matr_base)
View(Base)


#Ajout d'observations

for (i in 1:42) {
  Pays7 <- c("+228", "Yamoussoukro",27, "High", 12, 25)
  Matr_base <- rbind(Matr_base, Pays7)
  
  Pays8 <- c(paste("+", 230+i), "Dakar", 18,"Low", 10, 5)
  Matr_base <- rbind(Matr_base, Pays8)
  
  Pays9 <- c(paste("+", 230+i), "Yaoundé",17,"High", 8, 7)
  Matr_base <- rbind(Matr_base, Pays9)
  
  Pays10 <- c(paste("+", 230+i),"Porto_Novo",18,"Medium",9,15)
  Matr_base <- rbind(Matr_base,  Pays10)
  
  Pays11 <- c(paste("+", 230+i),"Douala", 11,"High", 10, 19)
  Matr_base <- rbind(Matr_base,Pays11)
  
  Pays12 <- c(paste("+", 230+i), "New York", 15,"Low",17, 23)
  Matr_base <- rbind(Matr_base, Pays12)
  
  Pays13 <- c(paste("+", 230+i),"Luxelbourg", 18, "High", 18, 20)
  Matr_base <- rbind(Matr_base, Pays13)
  
}

# Vérifications

View(Matr_base)
dim(Matr_base)

# Ajout de colonnes...

Var7 <- rep(c("A","A","D","B","C","D","E","E","B","C"), 30)
Matr_base <- cbind(Matr_base, Var7)

IDH <- rep(c(0.4,0.6,0.5,1,0.9,0.3,0.7,0.6,0.5,0.7), 30)
Matr_base <- cbind(Matr_base, IDH)

Dette <- rep(c("Endetté", "Peu endetté","Très endetté","Très endetté","Peu endetté","Endetté"),50)
Matr_base <- cbind(Matr_base, Dette)

Secteur_Dominant <- rep(c("Primaire","Secondaire","Tertiaire","Secondaire","Primaire","Tertiaire","Tertiaire","Secondaire","Primaire", "Secondaire"),30)
Matr_base <- cbind(Matr_base, Secteur_Dominant)

Situation <- rep(c("Nord", "Sud","Equateur","Nord", "Sud","Equateur", "Sud", "Equateur", "Sud","Nord"),30)
Matr_base <- cbind(Matr_base, Situation)

Sexe_Dominant <- rep(c("H","F","F","H","H","F"), 50)
Matr_base <- cbind(Matr_base, Sexe_Dominant)

Solde_Commercial_millions <- rep(c(-100, -150,-87,-30,-20,-10), 50)
Matr_base <- cbind(Matr_base, Solde_Commercial_millions)

Etat_BC <- rep(c("Equilibrée","Déficitaire", "Excédentaire"), 100)
Matr_base <- cbind(Matr_base, Etat_BC)
View(Matr_base)

Temps_doublement_ans <- rep(c(8,7,10,14,19,20), 50)
Matr_base <- cbind(Matr_base, Temps_doublement_ans)
View(Matr_base)

# Matr_base <- Matr_base[,-10]

#_____On renomme les lignes

rownames(Matr_base) <- c(paste0("Pays", 1:300))

#_____________________ Exportons et importons...

#install.packages("writexl") (package déjà installé)

My_data <- as.data.frame(Matr_base)
My_data

# Exportation

library(writexl)
write_xlsx( My_data, "My_data.xlsx",col_names = TRUE, format_headers = TRUE)

# Importation

#install.packages("readxl")
library(readxl)
Ma_Base <- read_excel("My_data.xlsx")
View(Ma_Base)
My_data <-data.frame(Ma_Base)

# Quelques statistiques descriptives...

mean(IDH)
mean(Temps_doublement_ans)

max(IDH)
min(Solde_Commercial_millions)
max(Solde_Commercial_millions)

quantile(Solde_Commercial_millions, probs=c(0.25,0.5, 0.75))
quantile(Temps_doublement_ans, probs=c(0.25,0.5, 0.75))



#_________________________________Implémentation manuelle du KHI-2________________________________________________________


# On extrait une base pour faire le test de Khi-2

Sub_frame <- Matr_base[,c(14,15)]
View(Sub_frame)

# On croise les variables 

table <- xtabs(~Situation + Secteur_Dominant, data=Sub_frame)

# Transformation du tableau en matrice

table <- as.matrix(table)
table

# Ajout des lignes et colonnes sommes

lines <- rowSums(table)
table <- cbind(table,lines)

columns <- colSums(table)
table <- rbind(table,columns)

table

# Calcul des effectifs théoriques

eff_theor <- matrix(nrow=3,ncol=3)

for(i in 1:3){
  for(j in 1:3){
    eff_theor[i,j]= table[i,4]*table[4,j]/table[4,4]
  }
}

eff_theor

# Calcule des éléments du Khi-2
S <-0

for(i in 1:3){
  for(j in 1:3){
    S= S+((eff_theor[i,j]-table[i,j])^2)/eff_theor[i,j]
  }
}
# Affichage du résultat

paste("Le Khi-2 entre La Situation Géographique et le Secteur dominant dans un pays est ",S)

# ___________________________FIN_________FIN_________FIN_________________________________
