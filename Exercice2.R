## Créer une base de données fictive avec au moins 5 variables _ 7 pour nous
# Prenons 5 variables

Base <- data.frame(col1=c("+221","+223","+222","+224","+261","+237"), col2=c("Dakar","Bamako","Nouakchott","Conakry","Antananarivo","Douala"), col3=c(18, 16.7, 4.5, 13.1, 27.7, 26.5), col4=factor(c("Low","Low","Low","Med","Low","High"), levels=c("High","Med","Low")), col5=c(14,19, 13, 8, 22, 10), col6=c(19,13,13,7,13,3))

#Créer une matrice à partir données et renommenr les lignes et les colonnes
Matr_base <- as.matrix(Base)
rownames(Matr_base) <- c("Sénégal","Mali","Mauritanie","Guinée","Madagascar", "Cameroun")
colnames(Matr_base) <- c("Identif-phone","Capitale","Pop (millions)","Niveau_vie","Nb_régions", "First_letter")
View(Matr_base)
View(Base)

#Faire des statistiques descriptives : mean, max, quartiles

mean_Pop <- mean(Base$col3)
mean_Pop

max(Base$col3)

max_regions <- max(Base$col5)
max_regions


quart_Pop <- quantile(Base$col3, probs=c(0.25,0.5, 0.75))
quart_Pop

#Graphiques: Histogramme, camembert

hist( Base$col6, main="Histogramme", xlab="Valeurs", ylab="Fréquence", col="lightblue", border="black")

plot(Base$col4)

pie(Base$col6, labels=seq_along(Base), main="Camembert")
pie(Base$col3, labels=seq_along(Base), main="Camembert")


#Résoudre manuellement le problème d'optimisation

#_ _ __________________________________________________________________
#exemple de donnée
#vecteur de coût
c <- c(-8, -4, 0, 0, 0)

#Matrices de restriction
A<-matrix(nrow=3,ncol=5)
A[1,] <- c(5, -2, 1, 0, 0)
A[2,] <- c(8, -2, 0, 1, 0)
A[3,] <- c(8,  1, 0, 0, 1)

# côtés
b<-c(0,1,2)

#solution initiales
B<-matrix(nrow=3,ncol=3)
B[1,] <- c(1, 0, 0)
B[2,] <- c(0, 1, 0)
B[3,] <- c(0, 0, 1)
solIndexes <- c(3,4,5)

#intialisation
simplex <- function(c,A,b,B,solIndexes){
  i = 0
  j = 1
  sum = 0
  max = -1
  min = 1000000
  entryVariable = -1
  exitVariable = -1
  entryVariable.relative = -1
  exitVariable.relative = -1
  cb <- c()
  entryCriterion <- c()
  
  #Etape 1: initialisation
  invB=solve(B)               #inversion de la matrice
  xb <- invB %*% b            #tableau de solution initiale
  for(i in solIndexes){       #tableau 
    cb <- c(cb, c[i])
  }
  cb[is.na(cb)] <- 0
  
  noSolIndexes <- c()         #indexes des variables candidats
  for(i in 1:5){
    if(!i %in% solIndexes){
      noSolIndexes <- c(noSolIndexes,i)
    }
  }
  
  #itération par l'algorithme
  while(TRUE){
    #Étape 2 : critère d'entrée 
    for(i in noSolIndexes){     #on obtient le critère pour décider quelle variable va entrer dans la solution
      ac <- A[,i]
      y  <- invB %*% ac
      
      candidateVariableCost = c[i]
      if(is.na(candidateVariableCost))  candidateVariableCost = 0
      entryCriterion <- c(entryCriterion, cb %*% y - candidateVariableCost)
    }
    
    for(i in entryCriterion){  #maximum (la variable qui va entrer est obtenue)
      if(i<=0){
        sum = sum+1
      }
      else if(i > max){
        max = i
        entryVariable.relative = j
      }
      j = j + 1
    }
    
    if(sum == length(entryCriterion)){ #une solution optimale a été trouvée
      print("[ Optimal solution ]")
      break
    }
    
    entryVariable = noSolIndexes[entryVariable.relative] #l'index de la variable d'entrée est obtenu
    
    
    
    #Étape 3 : critère de sortie
    y <- c()
    sum = 0
    j=1
    y <- invB %*% A[,entryVariable]
    for(i in y){
      if(i <= 0){
        sum = sum + 1
      }else if(xb[j]/i < min){
        min = xb[j]/i
        exitVariable.relative = j
      }
      j = j + 1
    }
    
    exitVariable = solIndexes[exitVariable.relative]
    
    
    if(sum == length(A[,entryVariable])){
      return("[ Unbounded problem ]")
    }
    
    
    #Étape 4 : la solution est recalculée
    B[,exitVariable.relative] = A[,entryVariable]
    
    invB=solve(B)               #inverse de la matrice B
    xb <- invB %*% b            #la solution est obtenue
    solIndexes[exitVariable.relative] = entryVariable 
    noSolIndexes[which(noSolIndexes==entryVariable)] = exitVariable
    cb[exitVariable.relative] = c[entryVariable]
    if(is.na(cb[exitVariable.relative]))  cb[exitVariable.relative] = 0
    
    #les variables temporaires sont nettoyées
    i = 0
    j = 1
    sum = 0
    max = -1
    min = 1000000
    entryVariable = -1
    exitVariable = -1
    entryVariable.relative = -1
    exitVariable.relative = -1
    entryCriterion <- c()
  }
  
  #retour des valeurs
  z = cb[i]%*%xb[i]
  return(list("Valeur des variables" = xb, "coûts minimal" = z, "Base" = solIndexes))
}
#vérification
simplex(c,A,b,B,solIndexes)








