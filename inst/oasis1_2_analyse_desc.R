################################################################################
#                Analyses descriptives OASIS 1 et OASIS 2                      #
################################################################################

####################################################
# Préambule : variables contenues dans les bases
####################################################

#M.F : Homme/Femme
#Hand : Doitier/Gaucher : l'ensemble des individus est droitier
#Age : âge du patient lors de la visite
#Educ : niveau de formation
#1: < diplome d'études secondaires
#2. diplôme d'études secondaires
#3. "some college" : études post-bac/universitaires
#4. "college grad" : diplôme post-bac/universitaire
#5. beyonds college : doctorat
#SES : statut socio-économique 1 (highest status) to 5 (lowest status)
#MMSE : score obtenu à l'examen clinique De 0 (plus mauvais score) à 30 (meilleur score)
#Any score greater than or equal to 24 points (out of 30) indicates a normal cognition.
#Below this, scores can indicate severe (≤9 points), moderate (10–18 points) or mild (19–23 points)
#cognitive impairment. The raw score may also need to be corrected for educational attainment and age.
#CDR : niveau de démence
#0. Non dément
#0.5. Démence très bénigne
#1 : démence bénigne
#2. Démence modérée
#Ce CDR est diagnostiqué avec une certaine probabilité
#eTIV : estimation du volume intercranien en mm3
#ASF : Atlas scaling factor
#nWBV : Normalized whole brain


######################
#1) Import des tables
######################

oasis1<-read.table("C:/Users/Klara/Documents/Datascientist/dementiaproject/inst/extdata/oasis1/oasis_cross-sectional.csv",sep=",",header=TRUE,row.names=1,na.strings="")
summary(oasis1)

oasis2<-read.table("C:/Users/Klara/Documents/Datascientist/dementiaproject/inst/extdata/oasis2/oasis_longitudinal.csv",sep=",",header=TRUE,na.strings="")
summary(oasis2)

#Attention, dans Oasis2, on a plusieurs lignes pour un même patient, puisqu'on a une ligne pour chaque visite médicale.
#On crée une table oasis2_patient ne contenant que les variables permettant de caractériser le patient
#On retient Subject.ID, Group, M.F, Age (on retiendra l'âge lors de la première visite), EDUC, SES.
#On garde aussi les valeurs des variables de santé (MMSE, CDR, ETIV, ASF, nWBV) mesurées lors de la première visite
#La table est triée par patient x visite

oasis2$Hand<-NULL

oasis2_patient<-oasis2
oasis2_patient$Hand<-NULL
oasis2_patient$MRI.ID<-NULL
oasis2_patient$Visit<-NULL
oasis2_patient$MR.Delay<-NULL

#On supprime les autres visites, on ne garde que la première

library(dplyr)
oasis2_patient <-oasis2_patient %>% slice(1) %>% ## on sélectionne la première ligne
bind_rows(oasis2_patient %>% filter(oasis2_patient$Subject.ID!=lag(oasis2_patient$Subject.ID))) ## on ajoute toutes les lignes lorsque col1 change

summary(oasis2_patient)

############################
# 2) Traitement des bases
############################

#############
#2.1 OASIS 1



#On remplace les valeurs N/A par NA

oasis1$Delay[oasis1$Delay=="N/A"]<-""

#On transforme le type de la variable Delay en integer

oasis1$Delay<-as.numeric(oasis1$Delay)

summary(oasis1)

#On supprime la variable Hand puisque la base de données ne contient que des droitiers

oasis1$Hand<-NULL
summary(oasis1)

#On convertit les variable Educ, SES, MMSE, CDR en facteurs
oasis1$Educ<-as.factor(oasis1$Educ)
oasis1$SES<-as.factor(oasis1$SES)
oasis1$CDR<-as.factor(oasis1$CDR)
oasis1$M.F<-as.factor(oasis1$M.F)

summary(oasis1)

#On crée une variable classe d'âge appelée clage

oasis1$clage <- cut(oasis1$Age, breaks = c(18, 29, 39, 49, 59,69,79,89,99), include.lowest = TRUE)
oasis1$clage

#On a des valeurs manquantes pour les variables :
#- Delay remplie seulement pour 20 individus pour lesquels on dispose d'images prises dans les 90 jours suivant la première analyse
#- Educ : niveau d'éducation
#- SES : statut socio économique
# - MMSE
# -CDR

#On crée une indicatrice démence/pas démence

oasis1$demence[oasis1$CDR=="0"]<- "Sain"
oasis1$demence[oasis1$CDR=="0.5"]<- "Dément"
oasis1$demence[oasis1$CDR=="1"]<- "Dément"
oasis1$demence[oasis1$CDR=="2"]<- "Dément"
oasis1$demence<-as.factor(oasis1$demence)


oasis1_sans_na$demence[oasis1_sans_na$CDR=="0"]<- "Sain"
oasis1_sans_na$demence[oasis1_sans_na$CDR=="0.5"]<- "Dément"
oasis1_sans_na$demence[oasis1_sans_na$CDR=="1"]<- "Dément"
oasis1_sans_na$demence[oasis1_sans_na$CDR=="2"]<- "Dément"
oasis1_sans_na$demence<-as.factor(oasis1_sans_na$demence)

#############
#2.2 OASIS 2


#On convertit les variable SES, MMSE, CDR en facteurs
oasis2$SES<-as.factor(oasis2$SES)
oasis2$CDR<-as.factor(oasis2$CDR)
oasis2$M.F<-as.factor(oasis2$M.F)

oasis2_patient$SES<-as.factor(oasis2_patient$SES)
oasis2_patient$CDR<-as.factor(oasis2_patient$CDR)
oasis2_patient$M.F<-as.factor(oasis2_patient$M.F)


#Attention : EDUC ne contient pas les mêmes modalités que dans la table OASIS 1 => à étudier
#Attention 2 : on a quelques valeurs manquantes dans la base OASIS2 aussi.
#Elles ne concernent que la variable SES dans la table oasis2_patient => voir comment les traiter, il y en a 8 dans la tableau oasis2_patient
#Elles concernent les variables SES et MMSE dans la table OASIS2 (2 valeurs manquantes pour MMSE et 19 pour la variable SES)


#On crée des classes d'âge : attention, tous les individus ont 60 ans et plus

oasis2_patient$clage <- cut(oasis2_patient$Age, breaks = c(60, 69, 79, 89, 99), include.lowest = TRUE)

#On crée une indicatrice démence/pas démence

oasis2$demence[oasis2$CDR=="0"]<- "Sain"
oasis2$demence[oasis2$CDR=="0.5"]<- "Dément"
oasis2$demence[oasis2$CDR=="1"]<- "Dément"
oasis2$demence[oasis2$CDR=="2"]<- "Dément"
oasis2$demence<-as.factor(oasis2$demence)


oasis2_patient$demence[oasis2_patient$CDR=="0"]<- "Sain"
oasis2_patient$demence[oasis2_patient$CDR=="0.5"]<- "Dément"
oasis2_patient$demence[oasis2_patient$CDR=="1"]<- "Dément"
oasis2_patient$demence[oasis2_patient$CDR=="2"]<- "Dément"
oasis2_patient$demence<-as.factor(oasis2_patient$demence)



#######################################
# 3 - Statistiques descriptives
#######################################

#############
#3.1 OASIS 1

#Base complète

summary(oasis1)


#################################################################
#Profil des personnes pour lesquelles on a des valeurs manquantes

#On crée une table qui ne contient que les individus présentant des valeurs manquantes sur CDR notamment puisqu'il s'agit
#de la variable d'intérêt permettant de savoir si le patient est atteint ou non d'une démence

CDR_na<- is.na(oasis1$CDR)
oasis1_na<-oasis1[CDR_na,]

#table sans valeur manquante

oasis1_sans_na<-oasis1[!CDR_na,]
summary(oasis1_sans_na)


#Base contenant les individus pour lesquels CDR=vide

summary(oasis1_na)

#Base excluant les valeurs manquantes

summary(oasis1_sans_na)


#On regarde le lien entre les différentes variables et la variable d'intérêt, ie la variable CDR

plot(demence~M.F,data=oasis1_sans_na,xlab="Sexe du patient",ylab="Démence")
plot(demence~SES,data=oasis1_sans_na,xlab="Statut socio-économique",ylab="Démence")
plot(demence~Educ,data=oasis1_sans_na,xlab="Niveau de formation",ylab="Démence")
plot(demence~clage,data=oasis1_sans_na,xlab="Classe d'âge",ylab="Démence")


plot(Age~demence,data=oasis1_sans_na,xlab="Démence",ylab="Age du patient")
plot(MMSE~demence,data=oasis1_sans_na,xlab="Démence",ylab="score à l'examen clinique")
plot(eTIV~demence,data=oasis1_sans_na,xlab="Démence",ylab="volume intercranien")
plot(ASF~demence,data=oasis1_sans_na,xlab="Démence",ylab="Atlas scaling factor")
plot(nWBV~demence,data=oasis1_sans_na,xlab="Démence",ylab="Normalized whole Brain")



#############
#3.2 OASIS 2


summary(oasis2_patient)

#On regarde le lien entre les différentes variables et la variable d'intérêt, ie la variable

plot(demence~M.F,data=oasis2,xlab="Sexe du patient",ylab="Démence")
plot(demence~SES,data=oasis2,xlab="Statut socio-économique",ylab="Démence")
#plot(demence~Educ,data=oasis2,xlab="Niveau de formation",ylab="Démence")
plot(demence~clage,data=oasis2,xlab="Classe d'âge",ylab="Démence")

plot(M.F~demence,data=oasis2,xlab="Démence",ylab="Sexe du patient")
plot(Age~demence,data=oasis2,xlab="Démence",ylab="Age du patient")
plot(MMSE~demence,data=oasis2,xlab="Démence",ylab="score à l'examen clinique")
plot(eTIV~demence,data=oasis2,xlab="Démence",ylab="volume intercranien")
plot(ASF~demence,data=oasis2,xlab="Démence",ylab="Atlas scaling factor")
plot(nWBV~demence,data=oasis2,xlab="Démence",ylab="Normalized whole Brain")

