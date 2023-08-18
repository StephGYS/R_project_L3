###################################### projet ######################################################
######## Les bénéficiaires de la prime d'excellence scientifique de 1993 à 2012 ##################
####### Problématique: le programme d'investissement IDEX depend-il des disciplines des laureats########################
-------------------------------Pré-taitement----------------------------------------------
source(file="C:/Users/ELEVE/Music/tp R/ressources/outils_stat_desc.R")
proj <- read.csv("C:/Users/ELEVE/Downloads/fr-esr-pes-pedr-beneficiaires.csv", encoding="UTF-8", sep=";")
View(proj)
str(proj)
summary(proj)
library("DescTools") 
proj<-proj[,(-19),drop=FALSE]
proj<-proj[,(-16),drop=FALSE]
proj<-proj[,(-14),drop=FALSE]
proj<-proj[,(-12),drop=FALSE]
proj<-proj[,(-2),drop=FALSE]
proj<-proj[,(-3),drop=FALSE]
proj<-proj[,(-4),drop=FALSE]
proj<-proj[,(-5),drop=FALSE]
proj<-proj[,(-6),drop=FALSE]

proj$distance_univ <- character(nrow(proj))
proj$distance_univ <- ifelse(proj$pres == 'Hors PRES', 'Hors PRES', 'PRES')
proj<-proj[,(-8),drop=FALSE]

proj$ideX <- character(nrow(proj))
proj$ideX <- ifelse(proj$idex == 'Hors IDEX', 'Hors IDEX', 'IDEX')
proj<-proj[,(-8),drop=FALSE]

View(proj)
str(proj)
summary(proj)
PlotMiss(proj, main = "Données manquantes")

------------------------Etude univarié-------------------------------------
############### annee ################### 
#L'année d'obtention de la prime.
str(proj$annee)
summary(proj$annee)
Desc(proj$annee)
psych::describe(proj$annee)
#l'année avec le plus de bénéficiares est 1994 à Université Paris 6.
subset(proj, subset = (beneficiaires == max(proj$beneficiaires)))
#boite moustaches
par(mar = c(1.1, 4.1, 3.1, 1.1))
boxplot(x = proj$annee,main = 'Distribution des années', 
ylab = 'Année', outcol = 2, outpch = 4) 
points(x = 1, y = mean( proj$annee, na.rm = TRUE),
       col = 4, pch = 3, lwd = 2)

############### sexe ###############
#Le sexe des bénéficaires.
str(proj$sexe)
summary(proj$sexe)
summary(factor(proj$sexe))
Desc(proj$sexe)
#Il y'a 62.9% hommes et 37.1% femmes.
proj_femme<-subset(proj, subset = (sexe == "Femmes"))
subset(proj_femme, subset = (beneficiaires == max(proj_femme$beneficiaires)))
# Les années avec le plus de bénéficiares femmes sont 1994 et 2009
#diagramme circulaire.
eff_sexe <- table(proj$sexe) 
my_pie(freq = eff_sexe, cex = 0.7)
#Il y'a 62.9% hommes et 37.1% femmes.

############### seteur_disciplinaire ###############
#c'est secteur de section CNU(Conseil national des universités).
str(proj$secteur_disciplinaire)
summary((proj$secteur_disciplinaire))
summary(factor(proj$secteur_disciplinaire))
x11()
Desc(proj$secteur_disciplinaire)
#Les candidats qui participent le plus sont en sciences, 
#aprés c'est ceux de Lettres et sciences humaines.
eff_secteur <- table(proj$secteur_disciplinaire) 
#diagramme en barre
my_barplot(eff_secteur,cex=0.7)
#my_barplot(freq=eff_secteur,stack = TRUE)

############### groupe de corps ###############
#la profession des participants,il y'a:
#Professeurs et assimilés.
#Maîtres de conférences et assimilés.
#particuliers.
str(proj$groupe_de_corps)
summary((proj$groupe_de_corps))
summary(factor(proj$groupe_de_corps))
Desc(proj$groupe_de_corps)
#il y'a 63.8% Professeurs et assimilés.
#il y'a 36.0% Maîtres de conférences et assimilés.
#0.2% de particuliers.
eff_group <- table(proj$groupe_de_corps) 
#diagramme en bande
#par(mar = c(2.1, 2.1, 1.1, 1.1), cex = 0.5)
my_barplot(freq=eff_group,stack = TRUE,cex=0.7)

################ region ###############
#la région des participants.
str(proj$region)
summary((proj$region))
summary(factor(proj$region))
Desc(proj$region)
#La région la plus répresenté est l'Ile de de france, puis Rhone-alpes.
eff_region <- table(proj$region) 
#diagramme en barres horizentale
my_barplot(eff_region, pareto = TRUE,cex=0.7)

############### beneficiare ###############
#le nombre de beneficiaires par universite.
str(proj$beneficiaires)
summary((proj$beneficiaires))
Desc(proj$beneficiaires)
#l'année avec le plus de bénéficiares est 1994 avec 148 bénéficiares de l'Université Paris 6.
subset(proj, subset = (beneficiaires == max(proj$beneficiaires)))
psych::describe(proj$beneficiaires)
#barplot(proj$beneficiaires)

############### distance_univ ###############
#Libellé du pôle de recherche et d’enseignement supérieur.
#pres: pole de recherche est dans l'etablissement.
#hors pres: pole de recherche est en dehors l'etablissement.
str(proj$distance_univ)
summary((proj$distance_univ))
summary(factor(proj$distance_univ))
Desc(proj$distance_univ)
#il y'a 84.7% pres.
#il y'a 15.3% hors pres.
eff_dist <- table(proj$distance_univ)     
my_pie(eff_dist,cex=0.7)

#### idex ####
#l’initiative d’excellence est un programmes d'investissement de 
#l'État français dont le but est de créer en France des ensembles
#pluridisciplinaires d'enseignement supérieur et de recherche qui 
#soient de rang mondial. 
str(proj$ideX)
summary((proj$ideX))
summary(factor(proj$ideX))
Desc(proj$ideX)
#il y'a 75.4% Hors IDEX.
#il y'a 24.6% IDEX.
eff_idex <- table(proj$ideX)     
my_pie(eff_idex)

------------------------------Etude bivarié-------------------------------------

############# Sexe et groupe de corps___2 var qualitatives
eff_sexe_group <- table(proj$sexe, proj$groupe_de_corps)
eff_sexe_group
## Profils lignes 
profl <- prop.table(eff_sexe_group, margin = 1)
t(apply(X = profl*100, MARGIN = 1, FUN = round_perc, digits = 1))
## Profils colonnes
profc <- prop.table(eff_sexe_group, margin = 2)
t(apply(X = profc*100, MARGIN = 1, FUN = round_perc, digits = 1))
## Représentation des profils lignes et colonnes
my_barplot(freq = eff_sexe_group, margin = 1,cex=0.5) # Profils lignes
my_barplot(freq = eff_sexe_group, margin = 2,cex=0.5) # Profils colonnes
# Interprétation
# Il ne semble pas émerger de tendance de dépendance entre le sexe et la profeession metier.
   # H1=indépendance entre les variables (p_value> seuil)
   # H0=dépendance entre les variables (p_value< seuil)
## Test du khi-deux
chisq.test(eff_sexe_group)
# Interprétation :
# p-value = 1.994e-11 < 0.05.
# Au niveau de significativité de 5 %, on décèle une relation de 
#dépendance significative entre les variables sexe et le groupe de 
#corps.
res.test <-chisq.test((eff_sexe_group))
res.test$expected #Le test chi2 est bien applicables (effectifs espérés < 5).
res.test$residuals
##Visualisation des résidus associés aux données
x11()
mosaicplot(sexe ~ groupe_de_corps, data = proj,
           color = TRUE, shade = TRUE, type = "pearson",
           main = "",
           xlab = "Statut femme/homme",
           ylab = "groupe") #Profils lignes
#Interpretation
#les principales contributions à l’inertie sont dûes aux couples (femmes, particuliers),(hommes, Maîtres de conférences et assimilés), (femmes, Professeurs et assimilés)
#et (Hommes,particuliers),(Femmes,Maîtres de conférences et assimilés),(Hommes,Professeurs et assimilés)
#respectivement sous-représentés et sur-représentés par rapport à la situation théorique de dépendance.


############# idex et beneficiaires_1 var qual(2 modalites) et 1 var quat
moy_idex_beneficiaire<-aggregate(beneficiaires ~ ideX, data = proj,mean, na.rm =TRUE)
moy_idex_beneficiaire
total_idex_beneficiaire<-aggregate(beneficiaires ~ ideX, data = proj,sum, na.rm =TRUE)
total_idex_beneficiaire
#commentaire 
options(scipen = 1000) 
Desc(beneficiaires ~ ideX, data = proj, plotit = FALSE) 
#Il semble avoir plus de primés hors IDEX que de primés IDEX.
describeBy(x = proj$beneficiaires, group = proj$ideX)
##boite de moutaches
par(mar = c(2.1, 2.1, 1.1, 1.1), cex = 0.7)
#x11()
boxplot(beneficiaires ~ ideX, data = proj,
        outcol = 2, outpch = 3,ylim = c(0, 20))
points(1:2, c(moy_idex_beneficiaire[1,2],moy_idex_beneficiaire[2,2]), pch = 4, col = 4) 
legend("topleft", horiz = TRUE,
       legend = c("Outliers", "Moyennes des primés"),
       lty = c(NA, NA, 2), col = c(2, 4, 4), pch = c(3,4, NA))
#commentaire  
##vérification des variances
var_idex_beneficiaire<-aggregate(beneficiaires ~ ideX, data = proj,sd, na.rm =TRUE)
var_idex_beneficiaire
#différence notable de variances au sein des sous-populations.
## Test student
#H0:pas différence significative entre les moyennes des deux groupes.
#H1:différence significative.
var.test(beneficiaires ~ ideX, data = proj)
#La p-value < seuil, on conclut qu’il n'y a pas une différence
#significative de variance entre les deux populations.
#On s’oriente donc vers le test de Welch pour comparer les moyennes des deux échantillons.
t.test(beneficiaires ~ ideX, data = proj, alternative = "greater", var.equal = FALSE)
#interpretation
#La p-value > 0,05, on accepte H0.
#il n'y a pas de différence significative entre les moyennes.

############# sexe et beneficiaires_ 1 var qual(2 modalites) et 1 var quat
moy_sexe_beneficiaire<-aggregate(beneficiaires ~ sexe, data = proj,mean, na.rm =TRUE)
moy_sexe_beneficiaire
options(scipen = 1000) 
library("DescTools") 
Desc(beneficiaires ~ sexe, data = proj, plotit = FALSE) 
#Il semble avoir plus d'hommes primés que de femmes.
library('psych')
describeBy(x = proj$beneficiaires, group = proj$sexe)
#le nombre max de femmes primées en une année est 34.
#le nombre max de hommes primés en une année est 147.
#boite de moutaches
par(mar = c(2.1, 2.1, 1.1, 1.1), cex = 0.7)
#x11()
boxplot(beneficiaires ~ sexe, data = proj,
        outcol = 2, outpch = 3,ylim = c(0, 20))
points(1:2, c(moy_sexe_beneficiaire[1,2],moy_sexe_beneficiaire[2,2]), pch = 4, col = 4) 
legend("topleft", horiz = TRUE,
       legend = c("Outliers", "Moyennes des primés"),
       lty = c(NA, NA, 2), col = c(2, 4, 4), pch = c(3,4, NA))
#commentaire de boite de moutaches 

var_sex_beneficiaire<-aggregate(beneficiaires ~ sexe, data = proj,sd, na.rm =TRUE)
var_sex_beneficiaire
#différence notable de variances au sein des sous-populations.
##test student
#H0:pas différence significative entre les moyennes des deux groupes.
#H1:différence significative.
var.test(beneficiaires ~ sexe, data = proj)
#La p-value étant inférieure au seuil de significativité, 
#on conclut qu’il y a une différence significative de variance
#entre les deux populations. 
#On s’oriente donc vers le test de Welch pour comparer les moyennes des deux échantillons.
t.test(beneficiaires ~ sexe, data = proj, alternative = "greater", var.equal = FALSE)
#interpretation
#La p-value > 0,05, on accepte H0.
#il n'y a pas de différence significative entre les moyennes.


############# secteur de disciplinaire et groupe de corps___2 var qualitatives
eff_secteur_group <- table(proj$secteur_disciplinaire, proj$groupe_de_corps)
eff_secteur_group
## Profils lignes 
profl <- prop.table(eff_secteur_group, margin = 1)
t(apply(X = profl*100, MARGIN = 1, FUN = round_perc, digits = 1))
## Profils colonnes
profc <- prop.table(eff_secteur_group, margin = 2)
t(apply(X = profc*100, MARGIN = 1, FUN = round_perc, digits = 1))
## Représentation des profils lignes et colonnes
my_barplot(freq = eff_secteur_group, margin = 1,cex=0.7) # Profils lignes
my_barplot(freq = eff_secteur_group, margin = 2,cex=0.7) # Profils colonnes
# Interprétation
# Il semble  émerger de tendance de dépendance entre la discipline
#et le groupe de corps.
    # H1=indépendance entre les variables (p_value> seuil).
    # H0=dépendance entre les variables (p_value< seuil).
## Test du khi-deux
chisq.test(eff_secteur_group)
# Interprétation :
# p-value = p-value < 2.2e-16 <0.05
# Au niveau de significativité de 5 %, on décèle une relation de dépendance significative entre les variables secteur et le groupe de corps.
res.test <-chisq.test((eff_secteur_group))
res.test$expected # Le test chi2 n'est pas applicables (effectifs espérés < 5).


############# Idex et distance_univ_ 2 var qualitatives
eff_idex_dist <- table(proj$ideX, proj$distance_univ)
eff_idex_dist
## Profils lignes 
profl <- prop.table(eff_idex_dist, margin = 1)
t(apply(X = profl*100, MARGIN = 1, FUN = round_perc, digits = 1))
## Profils colonnes
profc <- prop.table(eff_idex_dist, margin = 2)
t(apply(X = profc*100, MARGIN = 1, FUN = round_perc, digits = 1))
## Représentation des profils lignes et colonnes
my_barplot(freq = eff_idex_dist, margin = 1,cex=0.7) # Profils lignes
my_barplot(freq = eff_idex_dist, margin = 2,cex=0.7) # Profils colonnes
# Interprétation
# Il ne semble pas émerger de tendance de dépendance entre 
#idex et le distance_univ.
      # H1=indépendance entre les variables (p_value> seuil).
      # H0=dépendance entre les variables (p_value< seuil).
## Test d'indépendance du khi-deux
chisq.test(eff_idex_dist)
# Interprétation :
# p-value < 2.2e-16 < 0.05.
# Au niveau de significativité de 5 %, on décèle une relation 
#de dépendance significative entre les variables idex et 
#la distance_univ .
res.test <-chisq.test((eff_idex_dist))
res.test$expected #le test de khi est appliquable.
res.test$residuals
##Visualisation des résidus associés aux données
par(mar = c(2.1, 2.1, 1.1, 5.1), cex = 0.7)
mosaicplot(ideX ~ distance_univ, data = proj,
           color = TRUE, shade = TRUE, type = "pearson",
           main = "",
           xlab = "Statut hors idex/idex",
           ylab = "distance_univ_domicile") #Profils lignes
#Interpretation
#les principales contributions à l’inertie sont dûes aux couples (HORS IDEX, Hors PRES),
#(IDEX,PRES)et (IDEX,HORS PRES ) respectivement 
#sous-représenté et sur-représenté par rapport à la situation théorique de dépendance.

############# Idex et secteur_disciplinaire___2 var qualitatives
eff_idex_secteur <- table(proj$ideX, proj$secteur_disciplinaire)
eff_idex_secteur
## Profils lignes 
profl <- prop.table(eff_idex_secteur, margin = 1)
t(apply(X = profl*100, MARGIN = 1, FUN = round_perc, digits = 1))
## Profils colonnes
profc <- prop.table(eff_idex_secteur, margin = 2)
t(apply(X = profc*100, MARGIN = 1, FUN = round_perc, digits = 1))
## Représentation des profils lignes et colonnes
x11()
my_barplot(freq = eff_idex_secteur, margin = 1,cex=0.7) # Profils lignes
my_barplot(freq = eff_idex_secteur, margin = 2,cex=0.7) # Profils colonnes
# Interprétation
# Il ne semble pas émerger de tendance de dépendance entre idex et le secteur_discipline.
# H1=indépendance entre les variables (p_value> seuil).
# H0=dépendance entre les variables (p_value< seuil).
## Test du khi-deux
chisq.test(eff_idex_secteur)
# Interprétation :
# p-value = 6.495e-06 < 0.05
# Au niveau de significativité de 5 %, on décèle une relation 
#de dépendance significative entre les variables idex et le secteur_disciplinaires.
res.test <-chisq.test((eff_idex_secteur))
res.test$expected  #Le test chi2 est bien applicables (effectifs espérés < 5).
res.test$residuals
#Interpretation: 
##Visualisation des résidus associés aux données
par(mar = c(2.1, 2.1, 1.1, 3.1), cex = 0.7)
#x11()
mosaicplot(ideX ~ secteur_disciplinaire, data = proj,
           color = TRUE, shade = TRUE, type = "pearson",
           main = "",
           xlab = "Statut hors idex/idex",
           ylab = "secteur_disciplinaire") #Profils lignes
#Interpretation
#les principales contributions à l’inertie sont dûes aux couples (IDEX, Sciences)
#et (IDEX,Santé),(IDEX,Droit et sciences économiques) respectivement 
#sous-représenté et sur-représenté par rapport à la situation théorique de dépendance.



############ secteur_disciplinaire et beneficiaires_ 1 var qual(5 modalites) et 1 var quat
moy_sect_beneficiaire<-aggregate(beneficiaires ~ secteur_disciplinaire, data = proj,sd, na.rm =TRUE)
moy_sect_beneficiaire
total_sect_beneficiaire<-aggregate(beneficiaires ~ secteur_disciplinaire, data = proj,sum, na.rm =TRUE)
total_sect_beneficiaire
options(scipen = 1000) 
Desc(beneficiaires ~ secteur_disciplinaire, data = proj, plotit = FALSE) 
#Il semble avoir plus de primés en sciences.
describeBy(x = proj$beneficiaires, group = proj$secteur_disciplinaire)
   ##boite de moutaches
par(mar = c(2.1, 2.1, 1.1, 1.1), cex = 0.7)
#x11()
boxplot(beneficiaires ~ secteur_disciplinaire, data = proj,
        outcol = 2, outpch = 3,ylim = c(0, 20))  #
points(1:5, c(moy_sect_beneficiaire[1,5],moy_sect_beneficiaire[2,5],moy_sect_beneficiaire[3,5],moy_sect_beneficiaire[4,5],moy_sect_beneficiaire[5,5]), pch = 4, col = 4) 
legend("topleft", horiz = TRUE,
       legend = c("Outliers", "Moyennes des primés"),
       lty = c(NA, NA, 2), col = c(2, 4, 4), pch = c(3,4, NA))
##diagramme en bande croisé
par(mar = c(2.1, 2.1, 1.1, 1.1), cex = 0.7, cex.axis = 0.5)
my_barplot(c(total_sect_beneficiaire[1,2],total_sect_beneficiaire[2,2],total_sect_beneficiaire[3,2],total_sect_beneficiaire[4,2],total_sect_beneficiaire[5,2]), 
           cex=0.7)
#Le secteur de la science est a proiri le plus primé.
##test ANOVA
var_sect_beneficiaire<-aggregate(beneficiaires ~ secteur_disciplinaire, data = proj,sd, na.rm =TRUE)
var_sect_beneficiaire
#différence notable de variances au sein des sous-populations.
#Test de comparaison des variances de Bartlett
bartlett.test(beneficiaires ~ secteur_disciplinaire, data = proj) #différence significative
#La p-value < seuil, on conclut qu’il y a une différence
#significative de variance entre les deux populations.
oneway.test(beneficiaires ~ secteur_disciplinaire, data = proj, var.equal = FALSE) #Différence(s) significative(s)
#interpretation
#La p-value < 0,05, on rejecte H0
#il y'a différence significative entre les moyennes
pairwise.t.test(x = proj$beneficiaires, g = proj$secteur_disciplinaire)
#on met en évidence des différences significatives 
#entre 'Droit et sciences économiques' et les deux autres secteur_disciplinaire.

############## année et beneficiaires_ 2 var quat 
cor(proj$annee, proj$beneficiaires, use = "complete.obs")
# coefficient de correlation proche de 0, on ne donc pas se prononcer avec certutide
#sur l'indépendance des variables.

