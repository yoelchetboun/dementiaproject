######################################################################
# Programme 1 - statistiques descriptives sur les principales tables
######################################################################

#Tables disponibles :
#oasis3_sujets_complete : table contenant l'ensemble des participants à l'étude
#Table oasis3_freesurfer_complete : table contenant une ligne par diagnostic et l'ensemble des variables issues de l'imagerie médicale
#Table oasis3_diagnostic_complete : table contenant l'ensemble des variables "patient" : une ligne par diagnostic


library(data.table)
library(purrr)#permet de faire des boucles plus rapidement
library(ggplot2)
library(dplyr)


#chemin demon répertoire projet
#path_root <- "~/GENERIC/dementiaproject/"
path_root <-"C:/Users/Klara/Documents/Datascientist/dementiaproject"
#base de données brute
#path_data <- "/srv/OASIS_DATA/oasis3/"
path_data <- "C:/Users/Klara/Documents/Datascientist/dementiaproject/donnees irm patients/oasis3"


##### ---- Subjects data ---- ######

subjects_data <- read.csv2(file.path(path_root, "/inst/extdata/oasis3/bases/sujets/finale/oasis3_sujets_complete.csv"), header = TRUE, sep = ",")#table sujets avec toutes les variables pour chaque patient
setDT(subjects_data)#conversion en datatable
str(subjects_data)#structure de la table
#subjects_data <- subjects_data[,which(unlist(lapply(subjects_data, function(x)!all(is.na(x))))),with=F] #suppression des colonnes vides
subjects_data$M.F <- as.factor(subjects_data$M.F)
subjects_data$Hand <- as.factor(subjects_data$Hand)
subjects_data$Race <- as.factor(subjects_data$Race)
subjects_data$Ethnicity <- as.factor(subjects_data$Ethnicity)


##### ---- Diagnostics data ---- ######
diag_data <- read.csv2(file.path(path_root, "/inst/extdata/oasis3/bases/diagnostics/finale/oasis3_diagnostic_complete.csv"), header = TRUE, sep = ",")#table sujets avec toutes les variables pour chaque patient
setDT(diag_data)#conversion en datatable
str(diag_data)#structure de la table


##### ---- Freesurfer data ---- ######
freesurfer_data <- read.csv2(file.path(path_root, "/inst/extdata/oasis3/bases/freesurfer/finale/oasis3_freesurfer_complete.csv"), header = TRUE, sep = ",")#table sujets avec toutes les variables pour chaque patient
setDT(freesurfer_data)#conversion en datatable
str(freesurfer_data)#structure de la table


#########################################################
# Préparation de la table diagnostics
#########################################################

#On a beaucoup de valeurs manquantes dans cette table => il faut étudier chacune des variables, voir si elle a un lien avec
#la variable cible, combien elle présente de valeurs manquantes et décider de ce qu'on en fait.

summary(diag_data)

#On transforme le CDR en facteurs
diag_data$cdr <- as.factor(diag_data$cdr)

#On crée une variable Dement 0/1
levels(diag_data$cdr)
diag_data$dementia<-factor(diag_data$cdr,label=c("0","1","1","1","1"))


# On crée une variable CDR3 qui regroupe le CDR en 3 classes :
# 0
# 0,5 et 1
# 2 et 3

diag_data$CDR3<-factor(diag_data$cdr,label=c("0","1","1","2","2"))


#On crée une variable CDR4 qui regroupe le CDR en 4 classes au lieu de 5 : on regroupe CDR = 2 et 3

diag_data$CDR4<-factor(diag_data$cdr,label=c("0","1","2","3","3"))

##################################################################################################

# 1) MMSE : il s'agit du score obtenu à l'examen clinique permettant de diagnostiquer une démence
#Mini-Mental State Examination score
apply(is.na(diag_data),2,sum)
#On a 13 valeurs manquantes pour le MMSE

#Lien entre MMSE et CDR
#ggplot(data=matable,aes= #ce qu'il y aura dans mon graphique comme variables #fill permet de remplir le barplot avec une couleur différente selon droitier ou gaucher),
# + le graphique que je veux geom_bar ou geom_line ou geom_boxplot... # + pour ajouter de s choses
ggplot(diag_data, aes(fill = cdr, y= mmse)) + geom_boxplot() + ggtitle("Distribution du MMSE par groupe diagnostic") + scale_fill_discrete(name = "CDR") + labs(x = "Clinical Dementia Rating [0 à 3]", y = "Mini-Mental State Examination score [0 à 30]")+ theme_minimal() + theme(axis.text.x = element_blank())

ggplot(diag_data, aes(fill = dementia, y= mmse)) + geom_boxplot() + ggtitle("Distribution du MMSE - Dément/Non dément") + scale_fill_discrete(name = "Dementia") + labs(x = "Clinical Dementia Rating [0 ou 1]", y = "Mini-Mental State Examination score [0 à 30]")+ theme_minimal() + theme(axis.text.x = element_blank())

ggplot(diag_data, aes(fill = CDR3, y= mmse)) + geom_boxplot() + ggtitle("Distribution du MMSE - par groupe de diagnostic") + scale_fill_discrete(name = "CDR3") + labs(x = "Clinical Dementia Rating [0 à 2]", y = "Mini-Mental State Examination score [0 à 30]")+ theme_minimal() + theme(axis.text.x = element_blank())

ggplot(diag_data, aes(fill = CDR4, y= mmse)) + geom_boxplot() + ggtitle("Distribution du MMSE - par groupe de diagnostic") + scale_fill_discrete(name = "CDR4") + labs(x = "Clinical Dementia Rating [0 à 3]", y = "Mini-Mental State Examination score [0 à 30]")+ theme_minimal() + theme(axis.text.x = element_blank())

#Il faut garder la variable -> On va imputer les 13 valeurs manquantes
#Quel est le CDR des individus ayant un MMSE à vide ?

select<-is.na(diag_data$mmse)
which(select)
#[1]  325 1437 1556 1928 2151 2320 2328 2771 2830 2831 2832 2907 3973
diag_data[select]

#Le MMSE étant très lié au CDR, on va imputer la valeur moyenne du MMSE par classe pour chaque diagnostic ayant un MMSE manquant

diag_data<-diag_data[is.na(mmse),mmse:=mean(diag_data$mmse,na.rm =TRUE,by=cdr)]
#diag_data[,mean_mmse:=mean(mmse),by=cdr]
#diag_data<-diag_data[is.na(mmse),mmse:=mean_cdr]


apply(is.na(diag_data),2,sum)

##########################################################
#Age à l'entrée dans l'étude

select<-is.na(diag_data$ageAtEntry)
which(select)
#Aucune valeur manquante
#On calcule l'âge à chaque visite
#permet de sélectionner les visites initiales
diag_data[grepl(Session, pattern = "d0000")]
#permet d'extraire le nb de jours
diag_data[, nb_days_since_entry := sub(pattern = "d", replacement = "", strsplit(Session, split = "_")[[1]][[2]]), by = "Session"]
#conversion en numérique
diag_data[, nb_days_since_entry := as.numeric(nb_days_since_entry)]
#on sort l'âge au moment du diagnostic (à partir de l'âge au début du processus)
diag_data[, age_at_diagnosis := as.numeric(ageAtEntry) + nb_days_since_entry / 365]

#age au moment du diag
ggplot(diag_data[, .(cdr , age_at_diagnosis )], aes(fill = cdr, y= age_at_diagnosis)) + geom_boxplot() + ggtitle("Box plot de l'âge au moment du diagnostic") + scale_fill_discrete(name = "CDR") + labs(x = "Clinical Dementia Rating [0 à 3]", y = "Age") + theme_minimal() + theme(axis.text.x = element_blank())


################################################################################
# Table CDR
#
# La CDR est une échelle permettant la stratification des patients selon la sévérité de la démence puisqu'elle permet d'évaluer les fonctions cognitives et globales.
#
# Un examinateur doit évaluer 6 axes de fonctionnement de façon la plus indépendante possible, en utilisant toutes les informations disponibles.
#
# Ces facteurs sont obtenus à partir d'interrogatoires libres.
#
# Trois facteurs reflètent les aptitudes cognitives : mémoire, orientation et jugement.
#Les trois autres évaluent les actes de la vie courante : participation à la vie collective, occupations au foyer et hobbies, soins personnels.
#
# Un examinateur attribue un score de sévérité à chaque facteur
#
# 0 - en l'absence de trouble ; 0,5 - pour des troubles incertains et 1, 2 ou 3 pour, respectivement, des troubles bénins, modérés ou sévères.

#On a dans la base le détail pour chaque axe

# 1) COMMUN : participation à la vie collective

select<-is.na(diag_data$commun)
which(select)
#pas de valeur manquante
diag_data$CDR_VIECOLL <- diag_data$commun
diag_data$commun<-NULL

# 2) HOMEHOBB : occupations au foyer et hobbies

select<-is.na(diag_data$homehob)
which(select)
#pas de valeur manquante
diag_data$CDR_OCC_HOB <- diag_data$homehobb
diag_data$homehobb<-NULL

# 3) PERSCARE : soins personnels

select<-is.na(diag_data$perscare)
which(select)
#pas de valeur manquante
diag_data$CDR_SOINS_PERS <- diag_data$perscare
diag_data$perscare<-NULL

#4) judgment : jugement

select<-is.na(diag_data$judgment)
which(select)
#pas de valeur manquante
diag_data$CDR_JUGEMENT <- diag_data$judgment
diag_data$judgment<-NULL

#5) MEMORY : mémoire

select<-is.na(diag_data$memory)
which(select)
#pas de valeur manquante
diag_data$CDR_MEMOIRE <- diag_data$memory
diag_data$memory<-NULL

#6) ORIENT : orientation

select<-is.na(diag_data$orient)
which(select)
#pas de valeur manquante
diag_data$CDR_ORIENT <- diag_data$orient
diag_data$orient<-NULL

# 7) CDRSUM : CDR standard : somme des différents axes

select<-is.na(diag_data$sumbox)
which(select)
#pas de valeur manquante
diag_data$CDR_SOMME <- diag_data$sumbox
diag_data$sumbox<-NULL

#Attention, par construction, les variables CDR_ sont dnc très liées à la variable cible CDR. Elles l'expliquent toutes ensemble.

#####################################################################################
# Les diagnostics dx
# dx1 : Diagnostic impression intake and interview culminating with a coded dementia diagnosis that is
# recorded in the OASIS datatype “ADRC Clinical Data” dx1-dx5.
# Diagnoses for this variable include “cognitively normal”, “AD dementia”, “vascular dementia”
# and contributing factors such as vitamin deficiency, alcoholism, and mood disorders

#On supprime ces variables de diagnostic et on utilise les variables indicatrices présentes dans les différentes base, plus
#facilement exploitables

diag_data$dx1<-NULL
diag_data$dx2<-NULL
diag_data$dx3<-NULL
diag_data$dx4<-NULL
diag_data$dx5<-NULL

######################################################################################
# Les facteurs génétiques

#Variable APOE : The apolipoprotein E gene (APOE) has been identified for its influence on Alzheimer’s disease.
# The APOE ε4 allele of apolipoprotein E gene (APOE ε4) has been linked to increased risk for Alzheimer’s disease19
# while the ε2 allele (APOE ε2) may provide protection from Alzheimer’s disease

select<-is.na(diag_data$apoe)
which(select)
#19 valeurs manquantes

#On supprime les diagnostics pour lesquels on a une valeur manquantes
diag_data<-diag_data[!select,]

#Genotype
ggplot(na.omit(diag_data[, .(cdr , apoe )]), aes(fill = cdr, x= cdr)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~apoe) + ggtitle("Nombre de diagnostics vs Genotype") +
  scale_fill_discrete(name = "CDR") + labs(x = "Genotype", y = "Nombre de diagnostics")

#####################################################################################
# B1 : Evaluation Form


#1) HEIGHT : La taille du patient en inches
apply(is.na(diag_data),2,sum)

#valeurs manquantes : 192
#on renomme
diag_data$TAILLE <- diag_data$height
diag_data$height<-NULL
#On convertit la taille en cm
diag_data$TAILLE_cm<-as.numeric(diag_data$TAILLE)/0.394
diag_data$TAILLE <-diag_data$TAILLE_cm
diag_data$TAILLE_cm<-NULL

#On remplace les valeurs manquantes par la taille moyenne
diag_data<-diag_data[is.na(TAILLE),TAILLE:=mean(diag_data$TAILLE,na.rm =TRUE)]
summary(diag_data$TAILLE)

ggplot(diag_data[, .(cdr , TAILLE)], aes(fill = cdr, y= TAILLE)) + geom_boxplot() + ggtitle("Box plot de la taille du patient") + scale_fill_discrete(name = "CDR") + labs(x = "Clinical Dementia Rating [0 à 3]", y = "Taille en cm") + theme_minimal() + theme(axis.text.x = element_blank())

#2) WEIGHT : Le poids du patient en lbs
#183 valeurs manquantes
diag_data$POIDS<-as.numeric(diag_data$weight)*0.454
diag_data$weight<-NULL

#On remplace les valeurs manquantes par le poids moyen
diag_data<-diag_data[is.na(POIDS),POIDS:=mean(diag_data$POIDS,na.rm =TRUE)]
summary(diag_data$POIDS)

ggplot(diag_data[, .(cdr , POIDS)], aes(fill = cdr, y= POIDS)) + geom_boxplot() + ggtitle("Box plot de la taille du patient") + scale_fill_discrete(name = "CDR") + labs(x = "Clinical Dementia Rating [0 à 3]", y = "Poids en kg") + theme_minimal() + theme(axis.text.x = element_blank())


################################################################################################
# Table B2 : Evaluation Form : HIS and CVD : Cerebrovascular health and stroke history

#ABRUPT : Apparition soudaine
# 20 Valeurs manquantes

#STEPWISE : Détérioration progressive
# 20 Valeurs manquantes

#SOMATIC : plaintes somatiques
# 21 valeurs manquantes

#EMOT : incontinence émotionnelle
#19 valeurs manquantes

#HXHYPER : historique d'une présence d'hypertension
#20 valeurs manquantes

#HXSTROKE : historique d'AVC
#19 valeurs manquantes

#FOCLSYM : symptômes neurologiques focaux
#19 valeurs manquantes

#FOCLSIGN : Signes neurologiques focaux
#19 valeurs manquantes

# HACHIN : Hachinski Ischemic score
#19 valeurs manquantes

#On supprime les individus ayant des valeurs manquantes pour SOMATIC, ce qui revient à gérer le problème pour les autres variables
#Seulement 21 individus

select<-is.na(diag_data$SOMATIC)
diag_data<-diag_data[!select,]

#Encore 1 individu présentant des valeurs manquantes
select<-is.na(diag_data$STEPWISE)
diag_data<-diag_data[!select,]

#Encore 1 individu présentant des valeurs manquantes
select<-is.na(diag_data$HXHYPER)
diag_data<-diag_data[!select,]

#Encore 1 individu présentant des valeurs manquantes
select<-is.na(diag_data$HACHIN)
diag_data<-diag_data[!select,]

apply(is.na(diag_data),2,sum)
#Ok : on a supprimé 24 individus


#CVDCOG : maladie cérébrovasculaire contribuant à une perte cognitive
#720 valeurs manquantes : on supprime
diag_data$CVDCOG<-NULL

#STROKCOG : relation entre l'AVC et la perte cognitive
#721 valeurs manquantes : on supprime
diag_data$STROKCOG<-NULL

#CVDIMAG : mise en évidence par imagerie d'un lien entre l'atteinte cérébrovasculaire et la perte cognitive
#722 valeurs manquantes : on supprime
diag_data$CVDIMAG<-NULL

#CVDIMAG1: Si oui, l'imagerie met-elle en évidence une simple rupture ?
#4049 valeurs manquantes : on supprime
diag_data$CVDIMAG1<-NULL

#CVDIMAG2: Si oui, l'imagerie met-elle en évidence de multiples ruptures ?
#4049 valeurs manquantes : on supprime
diag_data$CVDIMAG2<-NULL

#CVDIMAG3: Si oui, l'imagerie met-elle en évidence une augmentation de la matière blanche ?
#4049 valeurs manquantes : on supprime
diag_data$CVDIMAG3<-NULL

#CVDIMAG4: Si oui, y a-t-il d'autres choses que l'imagerie met en évidence ?
#4049 valeurs manquantes : on supprime
diag_data$CVDIMAG4<-NULL


#######################################################################
# Table B3 - Evaluation FORM UPDRS
#Unified Parkinson’s Disease Rating Scale
#Il s'agit d'un examen optionnel que le médecin choisit de faire ou non => beaucoup de valeurs manquantes

ggplot(na.omit(diag_data[, .(cdr , PDNORMAL )]), aes(fill = cdr, x= cdr)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~PDNORMAL) + ggtitle("Nombre de diagnostics vs UPDRS") +
  scale_fill_discrete(name = "CDR") + labs(x = "UPDRS", y = "Nombre de diagnostics")

#Il y a un lien... On crée une variable à partir de PDNORMAL qui permet de savoir si :
# 1) L'examen n'a pas été passé : PDNORMAL=vide -> 9
# 2) L'examen a été passé et est normal  : PDNORMAL = 1 -> 1
# 3) L'examen a été passé et n'est pas normal : PDNORMAL= 0 -> 0

select<-is.na(diag_data$PDNORMAL)

diag_data<-diag_data[is.na(PDNORMAL),PDNORMAL:=9]
diag_data$EXAM_PARKINSON<-as.factor(diag_data$PDNORMAL)
levels(diag_data$EXAM_PARKINSON)

ggplot(na.omit(diag_data[, .(cdr , EXAM_PARKINSON )]), aes(fill = cdr, x= cdr)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~EXAM_PARKINSON) + ggtitle("Nombre de diagnostics vs UPDRS") +
  scale_fill_discrete(name = "CDR") + labs(x = "UPDRS", y = "Nombre de diagnostics")

#On supprime toutes les autres variables qui ne sont remplies que si l'examen n'était pas normal

diag_data$SPEECH<-NULL
diag_data$FACEXP<-NULL
diag_data$TRESTFAC<-NULL
diag_data$TRESTRHD<-NULL
diag_data$TRESTLHD<-NULL
diag_data$TRESTRFT<-NULL
diag_data$TRESTLFT<-NULL
diag_data$TRACTRHD<-NULL
diag_data$TRACTLHD<-NULL
diag_data$RIGDNECK<-NULL
diag_data$RIGDUPRT<-NULL
diag_data$RIGDUPLF<-NULL
diag_data$RIGDLORT<-NULL
diag_data$RIGDLOLF<-NULL
diag_data$TAPSRT<-NULL
diag_data$TAPSLF<-NULL
diag_data$HANDMOVR<-NULL
diag_data$HANDMOVL<-NULL
diag_data$HANDALTR<-NULL
diag_data$HANDALTL<-NULL
diag_data$LEGRT<-NULL
diag_data$LEGLF<-NULL
diag_data$ARISING<-NULL
diag_data$POSTURE<-NULL
diag_data$GAIT<-NULL
diag_data$POSSTAB<-NULL
diag_data$BRADYKIN<-NULL

#################################################################
# Table B5 - Behavioral Assessment - NPI-Q

#B5 records the participant’s neuropsychiatric history

apply(is.na(diag_data),2,sum)

#NPIQINF : personne proche qui informe

ggplot(na.omit(diag_data[, .(CDR3 , NPIQINF )]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~NPIQINF) + ggtitle("Nombre de diagnostics vs Informant") +
  scale_fill_discrete(name = "CDR") + labs(x = "Informant", y = "nombre de diagnostics")

#On a 18 valeurs manquantes et la variable n'est pas informative ->on la supprime
diag_data$NPIQINF<-NULL

#DEL : illusion : le patient pense-t-il que les autres veulent le voler ou le blesser ?
#15 valeurs manquantes
diag_data$DEL<-as.factor(diag_data$DEL)
summary(diag_data$DEL)

select<-is.na(diag_data$DEL)
diag_data$cdr[select]

ggplot(na.omit(diag_data[, .(CDR3 , DEL )]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~DEL) + ggtitle("Nombre de diagnostics vs illusion") +
  scale_fill_discrete(name = "CDR") + labs(x = "Illusion", y = "nombre de diagnostics")
#On impute les 15 valeurs à 0

diag_data<-diag_data[is.na(DEL),DEL:="0"]
diag_data$PARANO<-diag_data$DEL
levels(diag_data$PARANO)
diag_data$DEL<-NULL

#DELSEV : sévérité de la paranoïa
#La question n'est posée que si la réponse à la question précédente est 1 -> énormément de valeurs manquantes
#3885 valeurs manquantes

ggplot(na.omit(diag_data[, .(CDR3 , DELSEV )]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~DELSEV) + ggtitle("Nombre de diagnostics vs illusion") +
  scale_fill_discrete(name = "CDR") + labs(x = "Illusion", y = "nombre de diagnostics")

#On supprime la variable
diag_data$DELSEV<-NULL

#HALL : Hallucinations
#Le patient agit-il comme s'il entendait des voix ou comme s'il s'adressait à quelqu'un qui n'est pas présent ?
#14 valeurs manquantes
select<-is.na(diag_data$HALL)
diag_data$cdr[select]
ggplot(na.omit(diag_data[, .(CDR3 , HALL)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~HALL) + ggtitle("Nombre de diagnostics vs hallucinations") +
  scale_fill_discrete(name = "CDR") + labs(x = "Hallucinations", y = "nombre de diagnostics")
#On impute les 14 valeurs à 0
diag_data<-diag_data[is.na(HALL),HALL:="0"]
diag_data$HALLUCINATION<-as.factor(diag_data$HALL)
levels(diag_data$HALLUCINATION)
diag_data$HALL<-NULL

#HALLSEV : Sévérité des hallucinations
#N'est posée que si HALL=1 -> 3970 valeurs manquantes

#On supprime la variable
diag_data$HALLSEV<-NULL

#AGIT : Le patient est-il têtu et refuse-t-il l'aide des autres ?
#14 valeurs manquantes
select<-is.na(diag_data$AGIT)
diag_data$cdr[select]
ggplot(na.omit(diag_data[, .(CDR3 , AGIT)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~AGIT) + ggtitle("Nombre de diagnostics vs Entêtement") +
  scale_fill_discrete(name = "CDR") + labs(x = "Entêtement", y = "nombre de diagnostics")
#On impute les 14 valeurs à 0
diag_data<-diag_data[is.na(AGIT),AGIT:="0"]
diag_data$ENTETEMENT<-as.factor(diag_data$AGIT)
levels(diag_data$ENTETEMENT)
diag_data$AGIT<-NULL

#AGITSEV : Sévérité de l'entêtement
#  3422 valeurs manquantes

#On supprime la variable
diag_data$AGITSEV<-NULL

#DEPD : Dépression ou dysphorie
#15 valeurs manquantes
select<-is.na(diag_data$DEPD)
diag_data$cdr[select]
ggplot(na.omit(diag_data[, .(CDR3 , DEPD)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~DEPD) + ggtitle("Nombre de diagnostics vs Dépression") +
  scale_fill_discrete(name = "CDR") + labs(x = "Dépression", y = "nombre de diagnostics")
#On impute les 15 valeurs à 0
diag_data<-diag_data[is.na(DEPD),DEPD:=0]
diag_data$DEPRESS<-as.factor(diag_data$DEPD)
levels(diag_data$DEPRESS)
diag_data$DEPD<-NULL

#DEPDSEV : sévérité de la dépression ou de la dysphorie
#3408 valeurs manquantes
#On supprime la variable
diag_data$DEPDSEV<-NULL

#ANX : Anxiété
# 15 valeurs manquantes
select<-is.na(diag_data$ANX)
diag_data$cdr[select]
ggplot(na.omit(diag_data[, .(CDR3 , ANX)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~ANX) + ggtitle("Nombre de diagnostics vs Anxiété") +
  scale_fill_discrete(name = "CDR") + labs(x = "Anxiété", y = "nombre de diagnostics")
#On impute les 15 valeurs à 0
diag_data<-diag_data[is.na(ANX),ANX:=0]
diag_data$ANXIETE<-as.factor(diag_data$ANX)
levels(diag_data$ANXIETE)
diag_data$ANX<-NULL

#ANXSEV : Sévérité de l'anxiété
#3587 valeurs manquantes
diag_data$ANXSEV<-NULL

#ELAT : Allégresse ou euphorie
#15 valeurs manquantes
select<-is.na(diag_data$ELAT)
diag_data$cdr[select]
ggplot(na.omit(diag_data[, .(CDR3 , ELAT)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~ELAT) + ggtitle("Nombre de diagnostics vs Euphorie") +
  scale_fill_discrete(name = "CDR") + labs(x = "Euphorie", y = "nombre de diagnostics")
#On impute les 15 valeurs à 0
diag_data<-diag_data[is.na(ELAT),ELAT:=0]
diag_data$EUPHORIE<-as.factor(diag_data$ELAT)
levels(diag_data$EUPHORIE)
summary(diag_data$EUPHORIE)
#On a un diagnostic pour lequel on a une euphorie à 2 -> on recode en 0
diag_data<-diag_data[EUPHORIE=="2",EUPHORIE:="0"]
levels(diag_data$EUPHORIE)
summary(diag_data$EUPHORIE)
levels(diag_data$EUPHORIE)<-c("0","1","0")
diag_data$ELAT<-NULL

#ELATSEV : Sévérité de l'euphorie
#Posée si ELAT=1
#3917 valeurs manquantes

diag_data$ELATSEV<-NULL

#APA : Apathie ou indifférence
#14 valeurs manquantes
select<-is.na(diag_data$APA)
diag_data$cdr[select]
ggplot(na.omit(diag_data[, .(CDR3 , APA)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~APA) + ggtitle("Nombre de diagnostics vs Apathie") +
  scale_fill_discrete(name = "CDR") + labs(x = "Apathie", y = "nombre de diagnostics")
#On impute les 14 valeurs à 0
diag_data<-diag_data[is.na(APA),APA:=0]
diag_data$APATHIE<-as.factor(diag_data$APA)
levels(diag_data$APATHIE)
summary(diag_data$APATHIE)
diag_data$APA<-NULL

#APASEV : Sévérité de l'apathie
#Posée uniquement si APA=1
#3511 valeurs manquantes
diag_data$APASEV<-NULL

#DISN : Disinhibition
#14 valeurs manquantes
select<-is.na(diag_data$DISN)
diag_data$cdr[select]
ggplot(na.omit(diag_data[, .(CDR3 , DISN)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~DISN) + ggtitle("Nombre de diagnostics vs Disinhibition") +
  scale_fill_discrete(name = "CDR") + labs(x = "Disinhibition", y = "nombre de diagnostics")
#On impute les 14 valeurs à 0
diag_data<-diag_data[is.na(DISN),DISN:=0]
diag_data$DISINHIB<-as.factor(diag_data$DISN)
levels(diag_data$DISINHIB)
summary(diag_data$DISINHIB)
diag_data$DISN<-NULL

#DISNSEV : Sévérité de l'apathie
#Posée uniquement si DISN=1
# 3674 valeurs manquantes
diag_data$DISNSEV<-NULL


#IRR : Irritabilité
#13 valeurs manquantes
select<-is.na(diag_data$IRR)
diag_data$cdr[select]
ggplot(na.omit(diag_data[, .(CDR3 , IRR)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~IRR) + ggtitle("Nombre de diagnostics vs Irritabilité") +
  scale_fill_discrete(name = "CDR") + labs(x = "Irritabilité", y = "nombre de diagnostics")
#On impute les 13 valeurs à 0
diag_data<-diag_data[is.na(IRR),IRR:=0]
diag_data$IRRITAB<-as.factor(diag_data$IRR)
levels(diag_data$IRRITAB)
summary(diag_data$IRRITAB)
diag_data$IRR<-NULL

#IRRSEV : Sévérité de l'irritabilité
#Posée uniquement si IRR=1
# 3198 valeurs manquantes
diag_data$IRRSEV<-NULL


#MOT : Perturbation motrice : fait de répéter certains mouvements, certaines actions
#13 valeurs manquantes
select<-is.na(diag_data$MOT)
diag_data$cdr[select]
ggplot(na.omit(diag_data[, .(CDR3 , MOT)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~MOT) + ggtitle("Nombre de diagnostics vs Perturbation motrice") +
  scale_fill_discrete(name = "CDR") + labs(x = "Perturbation motrice", y = "nombre de diagnostics")
#On impute les 13 valeurs à 0
diag_data<-diag_data[is.na(MOT),MOT:=0]
diag_data$MOTEUR<-as.factor(diag_data$MOT)
levels(diag_data$MOTEUR)
summary(diag_data$MOTEUR)
diag_data$MOT<-NULL

#MOTSEV : Sévérité des perturbations motrices
#Posée uniquement si MOT=1
# 3813 valeurs manquantes
diag_data$MOTSEV<-NULL



#NITE : Activités nocturnes
#17 valeurs manquantes
select<-is.na(diag_data$NITE)
diag_data$cdr[select]
ggplot(na.omit(diag_data[, .(CDR3 , NITE)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~NITE) + ggtitle("Nombre de diagnostics vs Activités nocturnes") +
  scale_fill_discrete(name = "CDR") + labs(x = "Activité nocturne", y = "nombre de diagnostics")
#On impute les 17 valeurs à 0
diag_data<-diag_data[is.na(NITE),NITE:=0]
diag_data$ACTNOCT<-as.factor(diag_data$NITE)
levels(diag_data$ACTNOCT)
summary(diag_data$ACTNOCT)
#Attention, on a 3 diagnosics pour lesquels on a un NITE à 9 inconnu.
#On les recode à 0.
diag_data<-diag_data[ACTNOCT=="9",ACTNOCT:="0"]
levels(diag_data$ACTNOCT)
summary(diag_data$ACTNOCT)
levels(diag_data$ACTNOCT)<-c("0","1","0")
diag_data$NITE<-NULL


#NITESEV : Intensité des activités nocturnes
#Posée uniquement si NITE=1
# 3587 valeurs manquantes
diag_data$NITESEV<-NULL


#APP :  Appétit : le patient a-t-il pris ou perdu du poids ; ses goûts ou sa façon de manger ont-ils changé ?
#13 valeurs manquantes
select<-is.na(diag_data$APP)
diag_data$cdr[select]
ggplot(na.omit(diag_data[, .(CDR3 , APP)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~APP) + ggtitle("Nombre de diagnostics vs Appétit") +
  scale_fill_discrete(name = "CDR") + labs(x = "Appétit", y = "nombre de diagnostics")
#On impute les 13 valeurs à 0
diag_data<-diag_data[is.na(APP),APP:=0]
diag_data$APPETIT<-as.factor(diag_data$APP)
levels(diag_data$APPETIT)
summary(diag_data$APPETIT)
#Attention, on a 1 diagnosic pour lesquel on a un NITE à 9 inconnu.
#On le recode à 0.
diag_data<-diag_data[APPETIT=="9",APPETIT:="0"]
levels(diag_data$APPETIT)
summary(diag_data$APPETIT)
levels(diag_data$APPETIT)<-c("0","1","0")
diag_data$APP<-NULL


#APPSEV : Intensité de la modification de l'appétit
#Posée uniquement si APP=1
# 3539 valeurs manquantes
diag_data$APPSEV<-NULL

########################################################################################
# Table B6 - Behavorial Assessment - GDS
#Evaluation comportementale
#On n'a qu'une seule variable de cette table : la variable GDS qui permet de comptabiliser
#le nombre de cases cochées par le patient sur la façon dont il se sent
# GDS : échelle de dépression gériatrique
#L’EDG a été conçue pour évaluer la présence de sentiments dépressifs et d’intentions
#suicidaires chez les personnes âgées (Cheng et coll., 2010). C’est essentiellement un test de dépistage
#de l’atteinte dépressive ou de l’état dépressif

summary(diag_data$GDS)
#On a 3 valeurs manquantes et la variable est numérique
ggplot(diag_data[, .(cdr , GDS)], aes(fill = cdr, y=GDS)) + geom_boxplot() + ggtitle("Box plot de l'échelle de dépression gériatrique") + scale_fill_discrete(name = "CDR") + labs(x = "Clinical Dementia Rating [0 à 3]", y = "GDS") + theme_minimal() + theme(axis.text.x = element_blank())

select<-diag_data[GDS<20,]

ggplot(select[, .(cdr , GDS)], aes(fill = cdr, y=GDS)) + geom_boxplot() + ggtitle("Box plot de l'échelle de dépression gériatrique") + scale_fill_discrete(name = "CDR") + labs(x = "Clinical Dementia Rating [0 à 3]", y = "GDS") + theme_minimal() + theme(axis.text.x = element_blank())

#On a un lien... Il y a beaucoup de valeurs extrêmes, on regarde la distribution et on fait des tranches

hist(select$GDS)

#On découpe

diag_data$EDG <- cut(diag_data$GDS, breaks = c(0, 0.9, 1, 3, 5, 7, 10, 88), include.lowest = TRUE)

plot(diag_data$EDG)
levels(diag_data$EDG)<-c("0","1","2","3","4","5","6")
summary(diag_data$EDG)

#On a 3 valeurs manquantes
#On regarde le CDR
select<-is.na(diag_data$EDG)
diag_data$cdr[select]
#0 -> On impute le EDG à 0 pour ces 3 valeurs
diag_data<-diag_data[is.na(EDG),EDG:="0"]
levels(diag_data$EDG)
summary(diag_data$EDG)
diag_data$GDS<-NULL



##################################################################################
# Table B7 - Functional Assessment - FAQ
# Cette table contient des informations sur les activités quotidiennes

#BILLS : Dans les 4 dernières semaines, le patient a-t-il eu des difficultés ou besoin d'aide pour écrire un chèque,
#payer avec des billets... ?
summary(diag_data$BILLS)
#19 valeurs manquantes
select<-is.na(diag_data$BILLS)
diag_data$cdr[select]
ggplot(na.omit(diag_data[, .(CDR3 , BILLS)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~BILLS) + ggtitle("Nombre de diagnostics vs Gestion de l'argent") +
  scale_fill_discrete(name = "CDR") + labs(x = "Argent", y = "nombre de diagnostics")
#On impute les 19 valeurs à 0
diag_data<-diag_data[is.na(BILLS),BILLS:=0]
diag_data$ARGENT<-as.factor(diag_data$BILLS)
levels(diag_data$ARGENT)
summary(diag_data$ARGENT)
#Attention, on a deux valeurs à 9
diag_data<-diag_data[ARGENT=="9",APPETIT:="0"]
levels(diag_data$ARGENT)
summary(diag_data$ARGENT)
levels(diag_data$ARGENT)<-c("0","1","2","3","8","0")
diag_data$BILLS<-NULL


#TAXES : Dans les 4 dernières semaines, le patient a-t-il eu des difficultés pour gérer ses papiers, payer ses factures, etc ?
summary(diag_data$TAXES)
#17 valeurs manquantes
select<-is.na(diag_data$TAXES)
diag_data$cdr[select]
ggplot(na.omit(diag_data[, .(CDR3 , TAXES)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~TAXES) + ggtitle("Nombre de diagnostics vs Factures") +
  scale_fill_discrete(name = "CDR") + labs(x = "Factures", y = "nombre de diagnostics")
#On impute les 17 valeurs à 0
diag_data<-diag_data[is.na(TAXES),TAXES:=0]
diag_data$FACTURES<-as.factor(diag_data$TAXES)
levels(diag_data$FACTURES)
summary(diag_data$FACTURES)
#Attention, on a 1 valeur à 9 et 1 valeur à 5 alors que la modalité n'existe pas
diag_data<-diag_data[FACTURES=="9",FACTURES:="0"]
diag_data<-diag_data[FACTURES=="5",FACTURES:="0"]
levels(diag_data$FACTURES)
summary(diag_data$FACTURES)
diag_data$TAXES<-NULL


#SHOPPING : Dans les 4 dernières semaines, le patient a-t-il eu des difficultés pour faire ses courses ?
summary(diag_data$SHOPPING)
#17 valeurs manquantes
select<-is.na(diag_data$SHOPPING)
diag_data$cdr[select]
ggplot(na.omit(diag_data[, .(CDR3 , SHOPPING)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~SHOPPING) + ggtitle("Nombre de diagnostics vs Shopping") +
  scale_fill_discrete(name = "CDR") + labs(x = "Shopping", y = "nombre de diagnostics")
#On impute les 17 valeurs à 0
diag_data<-diag_data[is.na(SHOPPING),SHOPPING:=0]
diag_data$SHOPPING<-as.factor(diag_data$SHOPPING)
levels(diag_data$SHOPPING)
summary(diag_data$SHOPPING)


#GAMES : Dans les 4 dernières semaines, le patient a-t-il rencontré des difficultés ou eu besoin d'aide pour jouer
#à un jeu de réflexion tel que le bridge ou les échecs ?
summary(diag_data$GAMES)
#20 valeurs manquantes
select<-is.na(diag_data$GAMES)
diag_data$cdr[select]
ggplot(na.omit(diag_data[, .(CDR3 , GAMES)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~GAMES) + ggtitle("Nombre de diagnostics vs Jeu de réflexion") +
  scale_fill_discrete(name = "CDR") + labs(x = "Jeu de réflexion", y = "nombre de diagnostics")
#On impute les 20 valeurs à 0
diag_data<-diag_data[is.na(GAMES),GAMES:=0]
diag_data$JEU<-as.factor(diag_data$GAMES)
levels(diag_data$JEU)
summary(diag_data$JEU)
#Attention, on a 1 valeur à 9
diag_data<-diag_data[JEU=="9",JEU:="0"]
levels(diag_data$JEU)
summary(diag_data$JEU)
levels(diag_data$JEU)<-c("0","1","2","3","8","0")
diag_data$GAMES<-NULL

#STOVE : Dans les 4 dernières semaines, le patient a-t-il eu des difficultés avec l'eau chaude, l'utilisation d'une poêle ou lors
#de la préparation d'une tasse de café ?

summary(diag_data$STOVE)
#19 valeurs manquantes
select<-is.na(diag_data$STOVE)
diag_data$cdr[select]
ggplot(na.omit(diag_data[, .(CDR3 , STOVE)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~STOVE) + ggtitle("Nombre de diagnostics vs Cuisine") +
  scale_fill_discrete(name = "CDR") + labs(x = "Cuisine", y = "nombre de diagnostics")
#On impute les 19 valeurs à 0
diag_data<-diag_data[is.na(STOVE),STOVE:=0]
diag_data$CUISINE<-as.factor(diag_data$STOVE)
levels(diag_data$CUISINE)
summary(diag_data$CUISINE)

diag_data$STOVE<-NULL

#MEALPREP : Durant les 4 dernières semaines, le patient a-t-il eu des difficultés lors de la préparation d'un repas équilibré ?
summary(diag_data$MEALPREP)
#22 valeurs manquantes
select<-is.na(diag_data$MEALPREP)
diag_data$cdr[select]
ggplot(na.omit(diag_data[, .(CDR3 , MEALPREP)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~MEALPREP) + ggtitle("Nombre de diagnostics vs Préparation du repas") +
  scale_fill_discrete(name = "CDR") + labs(x = "Préparation du repas", y = "nombre de diagnostics")
#On impute les 22 valeurs à 0
diag_data<-diag_data[is.na(MEALPREP),MEALPREP:=0]
diag_data$REPAS<-as.factor(diag_data$MEALPREP)
levels(diag_data$REPAS)
summary(diag_data$REPAS)

diag_data$MEALPREP<-NULL


#EVENTS : durant les 4 dernières semaines, le patient a-t-il eu des difficultés à se rappeler d'événements courants ?
summary(diag_data$EVENTS)
#23 valeurs manquantes
select<-is.na(diag_data$EVENTS)
diag_data$cdr[select]
ggplot(na.omit(diag_data[, .(CDR3 , EVENTS)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~EVENTS) + ggtitle("Nombre de diagnostics vs Souvenir d'événements courants") +
  scale_fill_discrete(name = "CDR") + labs(x = "Souvenir d'événement courant", y = "nombre de diagnostics")
#On impute les 23 valeurs à 0
diag_data<-diag_data[is.na(EVENTS),EVENTS:=0]
diag_data$SOUV_EVENT<-as.factor(diag_data$EVENTS)
levels(diag_data$SOUV_EVENT)
summary(diag_data$SOUV_EVENT)
diag_data$EVENTS<-NULL


#PAYATTN : durant les 4 dernières semaines, le patient a-t-il eu des difficultés à se concentrer et à comprendre
#un programme TV, un livre ou un magazine ?
summary(diag_data$PAYATTN)
#22 valeurs manquantes
select<-is.na(diag_data$PAYATTN)
diag_data$cdr[select]
ggplot(na.omit(diag_data[, .(CDR3 , PAYATTN)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~PAYATTN) + ggtitle("Nombre de diagnostics vs Concentration") +
  scale_fill_discrete(name = "CDR") + labs(x = "Concentration", y = "nombre de diagnostics")
#On impute les 22 valeurs à 0
diag_data<-diag_data[is.na(PAYATTN),PAYATTN:=0]
diag_data$CONCENTRATION<-as.factor(diag_data$PAYATTN)
levels(diag_data$CONCENTRATION)
summary(diag_data$CONCENTRATION)
diag_data$PAYATTN<-NULL



#REMDATES : durant les 4 dernières semaines, le patient a-t-il eu des difficultés à se souvenir de dates ?
summary(diag_data$REMDATES)
#17 valeurs manquantes
select<-is.na(diag_data$REMDATES)
diag_data$cdr[select]
ggplot(na.omit(diag_data[, .(CDR3 , REMDATES)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~REMDATES) + ggtitle("Nombre de diagnostics vs Souvenir des dates") +
  scale_fill_discrete(name = "CDR") + labs(x = "Souvenir des dates", y = "nombre de diagnostics")
#On impute les 17 valeurs à 0
diag_data<-diag_data[is.na(REMDATES),REMDATES:=0]
diag_data$SOUV_DATES<-as.factor(diag_data$REMDATES)
levels(diag_data$SOUV_DATES)
summary(diag_data$SOUV_DATES)
diag_data$REMDATES<-NULL


#REMDATES : durant les 4 dernières semaines, le patient a-t-il eu des difficultés à se souvenir de dates ?
summary(diag_data$REMDATES)
#17 valeurs manquantes
select<-is.na(diag_data$REMDATES)
diag_data$cdr[select]
ggplot(na.omit(diag_data[, .(CDR3 , REMDATES)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~REMDATES) + ggtitle("Nombre de diagnostics vs Souvenir des dates") +
  scale_fill_discrete(name = "CDR") + labs(x = "Souvenir des dates", y = "nombre de diagnostics")
#On impute les 17 valeurs à 0
diag_data<-diag_data[is.na(REMDATES),REMDATES:=0]
diag_data$SOUV_DATES<-as.factor(diag_data$REMDATES)
levels(diag_data$SOUV_DATES)
summary(diag_data$SOUV_DATES)
diag_data$REMDATES<-NULL


#TRAVEL : Durant les 4 dernières semaines, le patient a-t-il eu des difficultés à se déplacer en dehors de son quartier, à conduire,
# ou à prendre les transports en commun ?
summary(diag_data$TRAVEL)
#18 valeurs manquantes
select<-is.na(diag_data$TRAVEL)
diag_data$cdr[select]
ggplot(na.omit(diag_data[, .(CDR3 , TRAVEL)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~TRAVEL) + ggtitle("Nombre de diagnostics vs Déplacement") +
  scale_fill_discrete(name = "CDR") + labs(x = "Déplacement", y = "nombre de diagnostics")
#On impute les 18 valeurs à 0
diag_data<-diag_data[is.na(TRAVEL),TRAVEL:=0]
diag_data$DEPLACEMENT<-as.factor(diag_data$TRAVEL)
levels(diag_data$DEPLACEMENT)
summary(diag_data$DEPLACEMENT)
diag_data$TRAVEL<-NULL

######################################################################
# Table B8 - Physical/NEurological Exam findings


# NORMAL : Examen normal (ou normal compte tenu de l'âge)
summary(diag_data$NORMAL)
#48 valeurs manquantes
select<-is.na(diag_data$NORMAL)
diag_data$cdr[select]
#Attention, on a un peu de tout... Donc, on recode les valeurs manquantes en 9 : Unknown
ggplot(na.omit(diag_data[, .(CDR3 , NORMAL)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~NORMAL) + ggtitle("Nombre de diagnostics vs Examen physique/neurologique") +
  scale_fill_discrete(name = "CDR") + labs(x = "Examen", y = "nombre de diagnostics")
#On impute les 48 valeurs à 9
diag_data<-diag_data[is.na(NORMAL),NORMAL:=9]
#Attention, on a aussi la modalité 4 qui apparaît alors qu'elle ne veut rien dire
diag_data<-diag_data[NORMAL==4,NORMAL:=9]
diag_data$EXAMEN<-as.factor(diag_data$NORMAL)
levels(diag_data$EXAMEN)
summary(diag_data$EXAMEN)
diag_data$NORMAL<-NULL

#FOCLDEF : Est-ce que le déficit focal traduit un trouble du système nerveux ?
summary(diag_data$FOCLDEF)
#49 valeurs manquantes
select<-is.na(diag_data$FOCLDEF)
diag_data$cdr[select]
#Attention, on a un peu de tout... Donc, on recode les valeurs manquantes en 9 : Unknown
ggplot(na.omit(diag_data[, .(CDR3 , FOCLDEF)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~FOCLDEF) + ggtitle("Nombre de diagnostics vs Déficit focal") +
  scale_fill_discrete(name = "CDR") + labs(x = "Déficit focal", y = "nombre de diagnostics")
#On impute les 49 valeurs manquantes à 9
diag_data<-diag_data[is.na(FOCLDEF),FOCLDEF:=9]
#Attention, on a aussi la modalité 4 qui apparaît alors qu'elle ne veut rien dire
diag_data$DEFICIT_FOCAL<-as.factor(diag_data$FOCLDEF)
levels(diag_data$DEFICIT_FOCAL)
summary(diag_data$DEFICIT_FOCAL)
diag_data$FOCLDEF<-NULL


#GAITDIS : Est-ce que la démarche du patient traduit un trouble du système nerveux ?
summary(diag_data$GAITDIS)
#50 valeurs manquantes
select<-is.na(diag_data$GAITDIS)
diag_data$cdr[select]
#Attention, on a un peu de tout... Donc, on recode les valeurs manquantes en 9 : Unknown
ggplot(na.omit(diag_data[, .(CDR3 , GAITDIS)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~GAITDIS) + ggtitle("Nombre de diagnostics vs Démarche du patient") +
  scale_fill_discrete(name = "CDR") + labs(x = "Démarche du patient", y = "nombre de diagnostics")
#On impute les 50 valeurs manquantes à 9
diag_data<-diag_data[is.na(GAITDIS),GAITDIS:=9]
#Attention, on a aussi la modalité 4 qui apparaît alors qu'elle ne veut rien dire
diag_data$DEFICIT_DEMARCHE<-as.factor(diag_data$GAITDIS)
levels(diag_data$DEFICIT_DEMARCHE)
summary(diag_data$DEFICIT_DEMARCHE)
diag_data$GAITDIS<-NULL

#EYEMOVE : Les mouvements des yeux laissent-il penser qu'il existe un déficit du système nerveux ?
summary(diag_data$EYEMOVE)
#49 valeurs manquantes
select<-is.na(diag_data$EYEMOVE)
diag_data$cdr[select]
#Attention, on a un peu de tout... Donc, on recode les valeurs manquantes en 9 : Unknown
ggplot(na.omit(diag_data[, .(CDR3 , EYEMOVE)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~EYEMOVE) + ggtitle("Nombre de diagnostics vs Mouvement des yeux") +
  scale_fill_discrete(name = "CDR") + labs(x = "Mouvement des yeux", y = "nombre de diagnostics")
#On impute les 49 valeurs manquantes à 9
diag_data<-diag_data[is.na(EYEMOVE),EYEMOVE:=9]
diag_data$MVMT_YEUX<-as.factor(diag_data$EYEMOVE)
levels(diag_data$MVMT_YEUX)
summary(diag_data$MVMT_YEUX)
diag_data$EYEMOVE<-NULL


####################################################################################
# Table B9 - Clinician Judgment of symptoms

#DECSUB : Le patient rapporte-il un déclin de sa mémoire par rapport à ses capacités antérieures ?
summary(diag_data$DECSUB)
#262 valeurs manquantes
select<-is.na(diag_data$DECSUB)
diag_data$cdr[select]
#Attention, on a un peu de tout... Donc, on recode les valeurs manquantes en 9 : Unknown
ggplot(na.omit(diag_data[, .(CDR3 , DECSUB)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~DECSUB) + ggtitle("Nombre de diagnostics vs Déclin de la mémoire") +
  scale_fill_discrete(name = "CDR") + labs(x = "Déclin de la mémoire", y = "nombre de diagnostics")
#La variable est liée au CDR, mais on ne peut pas imputer facilement car les CDR sont tous présents parmi les diagnostics
#présentant des valeurs manquantes
#On impute les 262 valeurs manquantes à 9
diag_data<-diag_data[is.na(DECSUB),DECSUB:=9]
#On impute également pour les modalités 8 et 3 qui ne sont pas censées exister
diag_data<-diag_data[DECSUB==3,DECSUB:=9]
diag_data<-diag_data[DECSUB==8,DECSUB:=9]

diag_data$DECLIN_MEMOIRE<-as.factor(diag_data$DECSUB)
levels(diag_data$DECLIN_MEMOIRE)
summary(diag_data$DECLIN_MEMOIRE)
diag_data$DECSUB<-NULL

#DECIN : Le proche rapporte-il un déclin de la mémoire du patient comparativement à ses capacités antérieures ?
summary(diag_data$DECIN)
#263 valeurs manquantes
select<-is.na(diag_data$DECIN)
diag_data$cdr[select]
#Attention, on a un peu de tout... Donc, on recode les valeurs manquantes en 9 : Unknown
ggplot(na.omit(diag_data[, .(CDR3 , DECIN)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~DECIN) + ggtitle("Nombre de diagnostics vs Déclin de la mémoire informant") +
  scale_fill_discrete(name = "CDR") + labs(x = "Déclin de la mémoire", y = "nombre de diagnostics")
#La variable est liée au CDR, mais on ne peut pas imputer facilement car les CDR sont tous présents parmi les diagnostics
#présentant des valeurs manquantes
#On impute les 263 valeurs manquantes à 9
diag_data<-diag_data[is.na(DECIN),DECIN:=9]
#On impute également pour la modalité 8 qui ne sont pas censées exister
diag_data<-diag_data[DECIN==8,DECIN:=9]

diag_data$DECLIN_MEM_INF<-as.factor(diag_data$DECIN)
levels(diag_data$DECLIN_MEM_INF)
summary(diag_data$DECLIN_MEM_INF)
diag_data$DECIN<-NULL

#DECCLIN : Le clinicien considère-t-il qu'il y a un déclin significatif du patient ?
summary(diag_data$DECCLIN)
#262 valeurs manquantes
select<-is.na(diag_data$DECCLIN)
diag_data$cdr[select]
#Attention, on a un peu de tout... Donc, on recode les valeurs manquantes en 9 : Unknown
ggplot(na.omit(diag_data[, .(CDR3 , DECCLIN)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~DECCLIN) + ggtitle("Nombre de diagnostics vs Déclin clinicien") +
  scale_fill_discrete(name = "CDR") + labs(x = "Déclin clinicien", y = "nombre de diagnostics")
#La variable est liée au CDR, mais on ne peut pas imputer facilement car les CDR sont tous présents parmi les diagnostics
#présentant des valeurs manquantes
#On impute les 262 valeurs manquantes à 9
diag_data<-diag_data[is.na(DECCLIN),DECCLIN:=9]
#On impute également pour la modalité 8 qui ne sont pas censées exister
diag_data<-diag_data[DECCLIN==8,DECCLIN:=9]

diag_data$DECLIN_CLINICIEN<-as.factor(diag_data$DECCLIN)
levels(diag_data$DECLIN_CLINICIEN)
summary(diag_data$DECLIN_CLINICIEN)
diag_data$DECCLIN<-NULL

#Les autres variables de la table ne sont posées que si le clinicien a signifié un déclin.
#On ne les conserve pas ici
diag_data$DECSUB<-NULL
diag_data$DECIN<-NULL
diag_data$DECCLIN<-NULL
diag_data$DECAGE<-NULL
diag_data$COGMEM<-NULL
diag_data$COGJUDG<-NULL
diag_data$COGLANG<-NULL
diag_data$COGVIS<-NULL
diag_data$COGATTN<-NULL
diag_data$COGFLUC<-NULL
diag_data$COGOTHR<-NULL
diag_data$COGFRST<-NULL
diag_data$COGMODE<-NULL
diag_data$BEAPATHY<-NULL
diag_data$BEDEP<-NULL
diag_data$BEVHALL<-NULL
diag_data$BEVWELL<-NULL
diag_data$BEAHALL<-NULL
diag_data$BEDEL<-NULL
diag_data$BEDISIN<-NULL
diag_data$BEIRRIT<-NULL
diag_data$BEAGIT<-NULL
diag_data$BEPERCH<-NULL
diag_data$BEREM<-NULL
diag_data$BEOTHR<-NULL
diag_data$BEFRST<-NULL
diag_data$BEMODE<-NULL
diag_data$MOGAIT<-NULL
diag_data$MOFALLS<-NULL
diag_data$MOTREM<-NULL
diag_data$MOSLOW<-NULL
diag_data$MOFRST<-NULL
diag_data$MOMODE<-NULL
diag_data$MOMOPARK<-NULL
diag_data$COURSE<-NULL
diag_data$FRSTCHG<-NULL

#######################################################################################
# Table A5 - Subject Health History - Dossier médical

#CVHATT : Arrêt ou attaque cardiaque
summary(diag_data$CVHATT)
#aucune valeur manquante, mais 17 valeurs à 9
select<-diag_data[CVHATT==9,]
select$cdr
#On a du 0, du 0.5 et du 1
ggplot(na.omit(diag_data[, .(CDR3 , CVHATT)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~CVHATT) + ggtitle("Nombre de diagnostics vs Attaque cardiaque") +
  scale_fill_discrete(name = "CDR") + labs(x = "Attaque cardiaque", y = "nombre de diagnostics")

diag_data$ATT_CARDIAQUE<-as.factor(diag_data$CVHATT)
levels(diag_data$ATT_CARDIAQUE)
summary(diag_data$ATT_CARDIAQUE)
diag_data$CVHATT<-NULL

#CVAFIB : Fribillation auriculaire
summary(diag_data$CVAFIB)
#a1 valeur manquante et 14 valeurs à 9
select<-diag_data[CVAFIB==9,]
select$cdr
#On a du 0, du 0.5 et du 1
ggplot(na.omit(diag_data[, .(CDR3 , CVAFIB)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~CVAFIB) + ggtitle("Nombre de diagnostics vs Fibrillation auriculaire") +
  scale_fill_discrete(name = "CDR") + labs(x = "Fibrillation auriculaire", y = "nombre de diagnostics")

diag_data<-diag_data[is.na(CVAFIB),CVAFIB:=9]
diag_data$FIB_AURICULAIRE<-as.factor(diag_data$CVAFIB)
levels(diag_data$FIB_AURICULAIRE)
summary(diag_data$FIB_AURICULAIRE)
diag_data$CVAFIB<-NULL


#CVANGIO : Angioplastie, Endartériectomie, Stent
summary(diag_data$CVANGIO)
#a1 valeur manquante et 3 valeurs à 9
select<-diag_data[CVANGIO==9,]
select$cdr
#On a du 0 et du 0.5 -> on recode à 0
ggplot(na.omit(diag_data[, .(CDR3 , CVANGIO)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~CVANGIO) + ggtitle("Nombre de diagnostics vs ANGIO_STENT") +
  scale_fill_discrete(name = "CDR") + labs(x = "ANGIO_STENT", y = "nombre de diagnostics")

diag_data<-diag_data[is.na(CVANGIO),CVANGIO:=0]
diag_data<-diag_data[CVANGIO==9,CVANGIO:=0]
diag_data$ANGIO_STENT<-as.factor(diag_data$CVANGIO)
levels(diag_data$ANGIO_STENT)
summary(diag_data$ANGIO_STENT)
diag_data$CVANGIO<-NULL

#CVBYPASS : Pontage cardiaque
summary(diag_data$CVBYPASS)
#1 valeur manquante et 2 valeurs à 9
select<-diag_data[CVBYPASS==9,]
select$cdr
#On a du 0 et du 0.5 -> on recode à 0
ggplot(na.omit(diag_data[, .(CDR3 , CVBYPASS)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~CVBYPASS) + ggtitle("Nombre de diagnostics vs Pontage") +
  scale_fill_discrete(name = "CDR") + labs(x = "Pontage", y = "nombre de diagnostics")

diag_data<-diag_data[is.na(CVBYPASS),CVBYPASS:=0]
diag_data<-diag_data[CVBYPASS==9,CVBYPASS:=0]
diag_data$PONTAGE<-as.factor(diag_data$CVBYPASS)
levels(diag_data$PONTAGE)
summary(diag_data$PONTAGE)
diag_data$CVBYPASS<-NULL


#CVPACE : Pacemaker
summary(diag_data$CVPACE)
#Pas de valeur manquante et 2 valeurs à 9
select<-diag_data[CVPACE==9,]
select$cdr
#On a du 0 et du 0.5 -> on recode à 0
ggplot(na.omit(diag_data[, .(CDR3 , CVPACE)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~CVPACE) + ggtitle("Nombre de diagnostics vs Pacemaker") +
  scale_fill_discrete(name = "CDR") + labs(x = "Pacemaker", y = "nombre de diagnostics")

diag_data<-diag_data[is.na(CVPACE),CVPACE:=0]
diag_data<-diag_data[CVPACE==9,CVPACE:=0]
diag_data$PACEMAKER<-as.factor(diag_data$CVPACE)
levels(diag_data$PONTAGE)
summary(diag_data$PONTAGE)
diag_data$CVPACE<-NULL


#CVCHF : Insuffisance cardiaque congestive
summary(diag_data$CVCHF)
#1 valeur manquante et 6 valeurs à 9
select<-diag_data[CVCHF==9,]
select$cdr
#On a du 0, du 0.5 et du 2 -> on recode à 9
ggplot(na.omit(diag_data[, .(CDR3 , CVCHF)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~CVCHF) + ggtitle("Nombre de diagnostics vs INsuffisance cardiaque") +
  scale_fill_discrete(name = "CDR") + labs(x = "Insuffisance cardiaque", y = "nombre de diagnostics")

diag_data<-diag_data[is.na(CVCHF), CVCHF:=9]
diag_data$INSUFF_CARDIAQUE<-as.factor(diag_data$CVCHF)
levels(diag_data$INSUFF_CARDIAQUE)
summary(diag_data$INSUFF_CARDIAQUE)
diag_data$CVCHF<-NULL


#CVOTHR : Maladie cardiovasculaire
summary(diag_data$CVOTHR)
#2 valeurs manquantes et 13 valeurs à 9
select<-diag_data[CVOTHR==9,]
select$cdr
#On a du 0, du 0.5, du 1 et du 2 -> on recode à 9
ggplot(na.omit(diag_data[, .(CDR3 , CVOTHR)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~CVOTHR) + ggtitle("Nombre de diagnostics vs Maladie cardio-vasculaire") +
  scale_fill_discrete(name = "CDR") + labs(x = "Maladie cardio-vasculaire", y = "nombre de diagnostics")

diag_data<-diag_data[is.na(CVOTHR),CVOTHR:=9]
diag_data$MALAD_CARDIO<-as.factor(diag_data$CVOTHR)
levels(diag_data$MALAD_CARDIO)
summary(diag_data$MALAD_CARDIO)
diag_data$CVOTHR<-NULL


#CBSTROKE : AVC
summary(diag_data$CBSTROKE)
#1 valeurs manquantes et 26 valeurs à 9
select<-diag_data[CBSTROKE==9,]
select$cdr
#On a du 0, du 0.5, du 1 et du 2 -> on recode à 9
ggplot(na.omit(diag_data[, .(CDR3 , CBSTROKE)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~CBSTROKE) + ggtitle("Nombre de diagnostics vs AVC") +
  scale_fill_discrete(name = "CDR") + labs(x = "AVC", y = "nombre de diagnostics")

diag_data<-diag_data[is.na(CBSTROKE),CBSTROKE:=9]
diag_data$AVC<-as.factor(diag_data$CBSTROKE)
levels(diag_data$AVC)
summary(diag_data$AVC)
diag_data$CBSTROKE<-NULL


#CBTIA : Accident transitoire ischémique
summary(diag_data$CBTIA)
#2 valeurs manquantes et de nombreuses valeurs à 9
select<-diag_data[CBTIA==9,]
select$cdr
#On a de tout -> on recode à 9
ggplot(na.omit(diag_data[, .(CDR3 , CBTIA)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~CBTIA) + ggtitle("Nombre de diagnostics vs AIT") +
  scale_fill_discrete(name = "CDR") + labs(x = "AIT", y = "nombre de diagnostics")

diag_data<-diag_data[is.na(CBTIA),CBTIA:=9]
diag_data$AIT<-as.factor(diag_data$CBTIA)
levels(diag_data$AIT)
summary(diag_data$AIT)
diag_data$CBTIA<-NULL


#CBOTHR : Maladie cérébrovasculaire
summary(diag_data$CBOTHR)
#5 valeurs manquantes et 8 valeurs à 9
select<-diag_data[CBOTHR==9,]
select$cdr
#Valeurs de CDR 0, 0.5 et 1 -> on recode à 0
ggplot(na.omit(diag_data[, .(CDR3 , CBOTHR)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~CBOTHR) + ggtitle("Nombre de diagnostics vs Maladie cérébrovasulaire") +
  scale_fill_discrete(name = "CDR") + labs(x = "Maladie cérébrovasulaire", y = "nombre de diagnostics")

diag_data<-diag_data[is.na(CBOTHR),CBOTHR:=0]
diag_data<-diag_data[CBOTHR==9,CBOTHR:=0]
diag_data$MALAD_CEREBROVASC<-as.factor(diag_data$CBOTHR)
levels(diag_data$MALAD_CEREBROVASC)
summary(diag_data$MALAD_CEREBROVASC)
diag_data$CBOTHR<-NULL


#PD : Maladie de Parkinson
summary(diag_data$PD)
#1 valeur manquante et 5 valeurs à 9
select<-diag_data[PD==9,]
select$cdr
#On a du 0.5 et du 0.0 -> on recode à 0
ggplot(na.omit(diag_data[, .(CDR3 , PD)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~PD) + ggtitle("Nombre de diagnostics vs Maladie de Parkinson") +
  scale_fill_discrete(name = "CDR") + labs(x = "Maladie de Parkinson", y = "nombre de diagnostics")

diag_data<-diag_data[is.na(PD),PD:=0]
diag_data<-diag_data[PD==9,PD:=0]
diag_data$PARKINSON<-as.factor(diag_data$PD)
levels(diag_data$PARKINSON)
summary(diag_data$PARKINSON)
diag_data$PD<-NULL


#PDOTHR : Autre trouble de parkinsonisme
summary(diag_data$PDOTHR)
#Aucune valeur manquante et 5 valeurs à 9
select<-diag_data[PDOTHR==9,]
select$cdr
#On a du 0.5, du 0.0 et du 1-> on recode à 0
ggplot(na.omit(diag_data[, .(CDR3 , PDOTHR)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~PDOTHR) + ggtitle("Nombre de diagnostics vs Autre trouble de parkinsonisme") +
  scale_fill_discrete(name = "CDR") + labs(x = "Autre trouble de parkinsonisme", y = "nombre de diagnostics")

diag_data<-diag_data[PDOTHR==9,PD:=0]
diag_data$OTH_PARKINSON<-as.factor(diag_data$PDOTHR)
levels(diag_data$OTH_PARKINSON)
summary(diag_data$OTH_PARKINSON)
levels(diag_data$OTH_PARKINSON)<-c("0","1","0")
diag_data$PDOTHR<-NULL

#SEIZURES : Crise épileptique
summary(diag_data$SEIZURES)
#2 valeurs manquantes et 5 valeurs à 9
select<-diag_data[SEIZURES==9,]
select$cdr
#On a du 0.5, du 0.0 et du 2-> on recode à 9
ggplot(na.omit(diag_data[, .(CDR3 , SEIZURES)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~SEIZURES) + ggtitle("Nombre de diagnostics vs Epilepsie") +
  scale_fill_discrete(name = "CDR") + labs(x = "Epilepsie", y = "nombre de diagnostics")

diag_data<-diag_data[is.na(SEIZURES),SEIZURES:=9]
diag_data$EPILEPSIE<-as.factor(diag_data$SEIZURES)
levels(diag_data$EPILEPSIE)
summary(diag_data$EPILEPSIE)
diag_data$SEIZURES<-NULL


#TRAUMBRF : Traumatisme crânien avec bref perte de conscience (<5 minutes)
summary(diag_data$TRAUMBRF)
#2 valeurs manquantes et de nombreuses valeurs à 9
select<-diag_data[TRAUMBRF==9,]
select$cdr

ggplot(na.omit(diag_data[, .(CDR3 , TRAUMBRF)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~TRAUMBRF) + ggtitle("Nombre de diagnostics vs Traumatisme crânien-brève perte de connaissance") +
  scale_fill_discrete(name = "CDR") + labs(x = "Traumatisme crânien-brève perte de connaissance", y = "nombre de diagnostics")

diag_data<-diag_data[is.na(TRAUMBRF),TRAUMBRF:=9]
diag_data$TRAUMA_BREF<-as.factor(diag_data$TRAUMBRF)
levels(diag_data$TRAUMA_BREF)
summary(diag_data$TRAUMA_BREF)
diag_data$TRAUMBRF<-NULL


#TRAUMEXT : Traumatisme crânien avec longue perte de connaissance (>=5 minutes)
summary(diag_data$TRAUMEXT )
#2 valeurs manquantes et plusieurs valeurs à 9, mais avec des CDR faibles
select<-diag_data[TRAUMEXT ==9,]
select$cdr
#On a du 0.5, du 0.0 -> on recode à 0
ggplot(na.omit(diag_data[, .(CDR3 , TRAUMEXT)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~TRAUMEXT) + ggtitle("Nombre de diagnostics vs Traumatisme crânien avec longue perte de connaissance") +
  scale_fill_discrete(name = "CDR") + labs(x = "Traumatisme crânien avec longue perte de connaissance", y = "nombre de diagnostics")

diag_data<-diag_data[is.na(TRAUMEXT),TRAUMEXT:=0]
diag_data<-diag_data[TRAUMEXT==9,TRAUMEXT:=0]
diag_data$TRAUMA_LONG<-as.factor(diag_data$TRAUMEXT)
levels(diag_data$TRAUMA_LONG)
summary(diag_data$TRAUMA_LONG)
diag_data$TRAUMEXT<-NULL


#TRAUMCHR : Traumatisme crânien avec déficit chronique
summary(diag_data$TRAUMCHR )
#3 valeurs manquantes et 9 valeurs à 9, mais avec des CDR faibles
select<-diag_data[TRAUMCHR ==9,]
select$cdr
#On a du 0.5, du 0.0 et du 1 -> on recode à 0
ggplot(na.omit(diag_data[, .(CDR3 , TRAUMCHR)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~TRAUMCHR) + ggtitle("Nombre de diagnostics vs Traumatisme crânien avec déficit chronique") +
  scale_fill_discrete(name = "CDR") + labs(x = "Traumatisme crânien avec déficit chronique", y = "nombre de diagnostics")

diag_data<-diag_data[is.na(TRAUMCHR),TRAUMCHR:=0]
diag_data<-diag_data[TRAUMCHR==9,TRAUMCHR:=0]
diag_data$TRAUMA_DEFCHRON<-as.factor(diag_data$TRAUMCHR)
levels(diag_data$TRAUMA_DEFCHRON)
summary(diag_data$TRAUMA_DEFCHRON)
diag_data$TRAUMCHR<-NULL



#HYPERTEN : Hypertension
summary(diag_data$HYPERTEN)
#2 valeurs manquantes
select<-diag_data[is.na(diag_data$HYPERTEN)]
select$cdr
#Du 0
#et plusieurs valeurs à 9, mais avec des CDR faibles -> on recode à 0
select<-diag_data[HYPERTEN ==9,]
select$cdr

ggplot(na.omit(diag_data[, .(CDR3 , HYPERTEN)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~HYPERTEN) + ggtitle("Nombre de diagnostics vs Hypertension") +
  scale_fill_discrete(name = "CDR") + labs(x = "Hypertension", y = "nombre de diagnostics")

diag_data<-diag_data[is.na(HYPERTEN),HYPERTEN:=0]
diag_data<-diag_data[HYPERTEN==9,HYPERTEN:=0]
diag_data$HYPERTENSION<-as.factor(diag_data$HYPERTEN)
levels(diag_data$HYPERTENSION)
summary(diag_data$HYPERTENSION)
diag_data$HYPERTEN<-NULL



#NCOTHR : Autres troubles neurologiques
summary(diag_data$NCOTHR)
#6 valeurs manquantes
select<-diag_data[is.na(diag_data$NCOTHR)]
select$cdr
#Du 0, du 0.5 et du 1
#et 11 valeurs à 9, mais avec des CDR faibles -> on recode à 0
select<-diag_data[NCOTHR ==9,]
select$cdr

ggplot(na.omit(diag_data[, .(CDR3 , NCOTHR)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~NCOTHR) + ggtitle("Nombre de diagnostics vs Autre trouble neurologique") +
  scale_fill_discrete(name = "CDR") + labs(x = "Autre trouble neurologique", y = "nombre de diagnostics")

diag_data<-diag_data[is.na(NCOTHR),NCOTHR:=0]
diag_data<-diag_data[NCOTHR==9,NCOTHR:=0]
diag_data$AUTRE_TROUBLE_NEURO<-as.factor(diag_data$NCOTHR)
levels(diag_data$AUTRE_TROUBLE_NEURO)
summary(diag_data$AUTRE_TROUBLE_NEURO)
diag_data$NCOTHR<-NULL




#HYPERCHO : Cholestérol
summary(diag_data$HYPERCHO )
#2 valeurs manquantes
select<-diag_data[is.na(diag_data$HYPERCHO )]
select$cdr
#Du 0
#et plusieurs valeurs à 9, avec des CDR 0, 0.5 et 2 -> on recode à 9
select<-diag_data[HYPERCHO  ==9,]
select$cdr

ggplot(na.omit(diag_data[, .(CDR3 , HYPERCHO )]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~HYPERCHO ) + ggtitle("Nombre de diagnostics vs Cholestérol") +
  scale_fill_discrete(name = "CDR") + labs(x = "Cholestérol", y = "nombre de diagnostics")

diag_data<-diag_data[is.na(HYPERCHO ),HYPERCHO :=9]
diag_data$CHOLESTEROL<-as.factor(diag_data$HYPERCHO )
levels(diag_data$CHOLESTEROL )
summary(diag_data$CHOLESTEROL )
diag_data$HYPERCHO <-NULL


#DIABETES : Diabètes
summary(diag_data$DIABETES )
#2 valeurs manquantes
select<-diag_data[is.na(diag_data$DIABETES)]
select$cdr
#Du 0
#et 10 valeurs à 9, avec des CDR 0, 0.5 -> on recode à 0
select<-diag_data[DIABETES  ==9,]
select$cdr

ggplot(na.omit(diag_data[, .(CDR3 , DIABETES)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~DIABETES) + ggtitle("Nombre de diagnostics vs Diabète") +
  scale_fill_discrete(name = "CDR") + labs(x = "Diabète", y = "nombre de diagnostics")

diag_data<-diag_data[is.na(DIABETES),DIABETES:=0]
diag_data<-diag_data[DIABETES==9,DIABETES:=0]
diag_data$DIABETES<-as.factor(diag_data$DIABETES)
levels(diag_data$DIABETES)
summary(diag_data$DIABETES)



#B12DEF : Carence en vitamine B12
summary(diag_data$B12DEF)
#3 valeurs manquantes
select<-diag_data[is.na(diag_data$B12DEF)]
select$cdr
#Du 0
#et de nombreuses valeurs à 9, avec des CDR variés -> on recode à 9
select<-diag_data[B12DEF==9,]
select$cdr

ggplot(na.omit(diag_data[, .(CDR3 , B12DEF)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~B12DEF) + ggtitle("Nombre de diagnostics vs Carence en B12") +
  scale_fill_discrete(name = "CDR") + labs(x = "Carence en B12", y = "nombre de diagnostics")

diag_data<-diag_data[is.na(B12DEF),B12DEF:=9]
diag_data$CARENCEB12<-as.factor(diag_data$B12DEF)
levels(diag_data$CARENCEB12)
summary(diag_data$CARENCEB12)
diag_data$B12DEF<-NULL



#THYROID : Troubles de la thyroïde
summary(diag_data$THYROID)
#2 valeurs manquantes
select<-diag_data[is.na(diag_data$THYROID )]
select$cdr
#Du 0
#et une dizaine valeurs à 9, avec des CDR faibles -> on recode à 0
select<-diag_data[THYROID ==9,]
select$cdr

ggplot(na.omit(diag_data[, .(CDR3 , THYROID )]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~THYROID) + ggtitle("Nombre de diagnostics vs Troubles de la thyroïde") +
  scale_fill_discrete(name = "CDR") + labs(x = "Troubles de la thyroïde", y = "nombre de diagnostics")

diag_data<-diag_data[is.na(THYROID),THYROID:=0]
diag_data<-diag_data[THYROID==9,THYROID:=0]
diag_data$THYROID<-as.factor(diag_data$THYROID)
levels(diag_data$THYROID)
summary(diag_data$THYROID)


#INCONTU : Incontinence urinaire
summary(diag_data$INCONTU)
#3 valeurs manquantes
select<-diag_data[is.na(diag_data$INCONTU)]
select$cdr
#Du 0 et du 0.5
#et une dizaine valeurs à 9, avec des CDR variés -> on recode à 9
select<-diag_data[INCONTU ==9,]
select$cdr

ggplot(na.omit(diag_data[, .(CDR3 , INCONTU)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~INCONTU) + ggtitle("Nombre de diagnostics vs Incontinence urinaire") +
  scale_fill_discrete(name = "CDR") + labs(x = "Incontinence urinaire", y = "nombre de diagnostics")

diag_data<-diag_data[is.na( INCONTU), INCONTU:=9]
diag_data$INCONTU<-as.factor(diag_data$INCONTU)
levels(diag_data$INCONTU)
summary(diag_data$INCONTU)


#INCONTU : Incontinence urinaire
summary(diag_data$INCONTF)
#4 valeurs manquantes
select<-diag_data[is.na(diag_data$INCONTF)]
select$cdr
#Du 0 et du 0.5
#et une dizaine valeurs à 9, avec des CDR faibles -> on recode à 0
select<-diag_data[INCONTF ==9,]
select$cdr

ggplot(na.omit(diag_data[, .(CDR3 , INCONTF)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~INCONTF) + ggtitle("Nombre de diagnostics vs Incontinence intestinale") +
  scale_fill_discrete(name = "CDR") + labs(x = "Incontinence intestinale", y = "nombre de diagnostics")

diag_data<-diag_data[is.na( INCONTF), INCONTF:=0]
diag_data<-diag_data[INCONTF==9, INCONTF:=0]
diag_data$INCONTI<-as.factor(diag_data$INCONTF)
levels(diag_data$INCONTI)
summary(diag_data$INCONTI)
diag_data$INCONTF<-NULL



#DEP2YRS : Dépression dans les deux années passées
summary(diag_data$DEP2YRS)
#1 valeur manquantes
select<-diag_data[is.na(diag_data$DEP2YRS)]
select$cdr
#Du 0
#et plusieurs valeurs à 9, avec des CDR variés -> on recode à 9
select<-diag_data[DEP2YRS ==9,]
select$cdr

ggplot(na.omit(diag_data[, .(CDR3 , DEP2YRS)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~DEP2YRS) + ggtitle("Nombre de diagnostics vs Dépression dans les 2 années passées") +
  scale_fill_discrete(name = "CDR") + labs(x = "Dépression dans les 2 années passée", y = "nombre de diagnostics")

diag_data<-diag_data[is.na(DEP2YRS), DEP2YRS:=9]
diag_data$DEP2YRS<-as.factor(diag_data$DEP2YRS)
levels(diag_data$DEP2YRS)
summary(diag_data$DEP2YRS)


#DEPOTHR : Autres épisodes dépressifs
summary(diag_data$DEPOTHR)
#5 valeurs manquantes
select<-diag_data[is.na(diag_data$DEPOTHR )]
select$cdr
#Du 0
#et plusieurs valeurs à 9, avec des CDR faibles -> on recode à 0
select<-diag_data[DEPOTHR==9,]
select$cdr

ggplot(na.omit(diag_data[, .(CDR3 , DEPOTHR)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~DEPOTHR) + ggtitle("Nombre de diagnostics vs Autres épisodes dépressifs") +
  scale_fill_discrete(name = "CDR") + labs(x = "Dépression dans les 2 années passée", y = "nombre de diagnostics")

diag_data<-diag_data[is.na(DEPOTHR), DEPOTHR:=0]
diag_data<-diag_data[DEPOTHR==9, DEPOTHR:=0]
diag_data$DEPOTHR <-as.factor(diag_data$DEPOTHR )
levels(diag_data$DEPOTHR)
summary(diag_data$DEPOTHR)


#ALCOHOL : Abus d'alcool dans les 12 derniers mois
summary(diag_data$ALCOHOL)
#6 valeurs manquantes
select<-diag_data[is.na(diag_data$ALCOHOL)]
select$cdr
#Du 0 et du 0.5
#et plusieurs valeurs à 9, avec des CDR faibles -> on recode à 0
select<-diag_data[ALCOHOL==9,]
select$cdr

ggplot(na.omit(diag_data[, .(CDR3 ,ALCOHOL)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~ALCOHOL) + ggtitle("Nombre de diagnostics vs Abus d'alcool") +
  scale_fill_discrete(name = "CDR") + labs(x = "Abus d'alcool", y = "nombre de diagnostics")

diag_data<-diag_data[is.na(ALCOHOL), ALCOHOL:=0]
diag_data<-diag_data[ALCOHOL==9, ALCOHOL:=0]
diag_data$ALCOHOL <-as.factor(diag_data$ALCOHOL)
levels(diag_data$ALCOHOL)
summary(diag_data$ALCOHOL)




#ALCOHOL : Abus d'alcool dans les 12 derniers mois
summary(diag_data$ALCOHOL)
#6 valeurs manquantes
select<-diag_data[is.na(diag_data$ALCOHOL)]
select$cdr
#Du 0 et du 0.5
#et plusieurs valeurs à 9, avec des CDR faibles -> on recode à 0
select<-diag_data[ALCOHOL==9,]
select$cdr

ggplot(na.omit(diag_data[, .(CDR3 ,ALCOHOL)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~ALCOHOL) + ggtitle("Nombre de diagnostics vs Abus d'alcool") +
  scale_fill_discrete(name = "CDR") + labs(x = "Abus d'alcool", y = "nombre de diagnostics")

diag_data<-diag_data[is.na(ALCOHOL), ALCOHOL:=0]
diag_data<-diag_data[ALCOHOL==9, ALCOHOL:=0]
diag_data$ALCOHOL <-as.factor(diag_data$ALCOHOL)
levels(diag_data$ALCOHOL)
summary(diag_data$ALCOHOL)


#TOBAC30 : Cigarette dans les 30 derniers jours
summary(diag_data$TOBAC30)
#1 valeur manquante
select<-diag_data[is.na(diag_data$TOBAC30)]
select$cdr
#Du 0
#et 9 valeurs à 9, avec des CDR faibles -> on recode à 0
select<-diag_data[TOBAC30==9,]
select$cdr

ggplot(na.omit(diag_data[, .(CDR3 ,TOBAC30)]), aes(fill = CDR3, x=CDR3)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~TOBAC30) + ggtitle("Nombre de diagnostics vs Tabac dans les 30 jours") +
  scale_fill_discrete(name = "CDR") + labs(x = "Tabac dans les 30 jours", y = "nombre de diagnostics")

diag_data<-diag_data[is.na(TOBAC30), TOBAC30:=0]
diag_data<-diag_data[TOBAC30==9, TOBAC30:=0]
diag_data$TABAC_30j <-as.factor(diag_data$TOBAC30)
levels(diag_data$TABAC_30j)
summary(diag_data$TABAC_30j)
TOBAC30<-NULL



























