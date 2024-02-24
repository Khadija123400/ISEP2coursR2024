# My do file, code 

#Création d'un vecteur
 
x<- c(1:16)
print(x)

y=c("Bousso","Coulibaly","Diaw", "Diakhaté")
y

## Aide-help

?print
help("print")
help(print) #Same

## Install a new package
install.packages("tidyr")

?list

class <- list(x,y, "ISEP2-2024")


#Data frame
classbis <- data.frame(ordre=c(1:16), nom=rep(y,4), "KC_GURL")

# Case sensitive
View(classbis)


##To access to the elements (columns) of the table

nometudiant <- classbis$nom

print (nometudiant)

attach(classbis)
nameEtud <- nom
detach(classbis)
print (nameEtud)

# Opérateurs
1+1
1/3
5*3
sqrt(16)
2^3
### Avoir : cos(
kc <- list(y,"Khadija")
print(kc)
##Exercice
