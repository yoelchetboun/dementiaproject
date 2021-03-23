#####################################################################################
# Programme 2 - Statistiques descripives Freesurfer
#####################################################################################

#La table diag_data_finale contient 100 variables issues du dossier du patient établi à chaque diagnostic
#ON a 4011 diagnostics dans ce dossier

#L'objectif de ce programme est de prédire la présence de troubles relatifs à une démence à partir des variables issues
#du dossier du patient

#La table diag_data_finale contient 4011 observations correspondant à des diagnostics, plusieurs variables cibles :
# - dementia
# - CDR
# - CDR3
# - CDR4
# - les différents axes associés au CDR :
#   - CDR_VIECOLL
#   - CDR_OCC_HOB
#   - CDR_JUGEMENT
#   - CDR_MEMOIRE
#   - CDR_ORIENT
#   - CDR_SOINS_PERS
#   - CDR_SOMME

#Elle contient également le MMSE qui est un autre examen que le CDR également très lié au CDR.
# Le MMSE (Folstein, Folstein et McHugh, 1975) a été développé pour dépister les personnes
# souffrant d’une atteinte neurocognitive majeure (démence). Il a ensuite été traduit dans diverses
# langues. Puis, diverses études ont conduit à la création de versions modifiées et à l’élaboration
# de directives pour la passation et la cotation. D’autres études ont permis d’examiner le potentiel
# de l’outil à être utilisé dans d’autres contextes, par exemple la mesure du changement. Maintenant,
# le MMSE sert également à assurer les suivis de l’état cognitif des personnes et à mesurer le déclin
# des fonctions cognitives chez les personnes qui souffrent d’atteintes neurocognitives.

#La table ne contient quasiment que des variables catégorielles
#On va devoir les transformer en facteurs
#Les variables cibles sont des variables catégorielles
#On fera un modèle sur la variable dementia Dément/NOn dément
#On fera également un modèle sur la variable CDR4 : découpage de la variable CDR en classe 4 :
# - 0 : absence de troubles
# - 1 : présence de troubles incertains
# - 2 : Présence de troubles bénins
# - 3 : Présence de troubles modérés et sévères

##################################################################################################################
# Import de la table freesurfer_finale, préparée dans le programme 1
##################################################################################################################

path_root <- "~/GENERIC/dementiaproject/"
load(file=file.path(path_root, "//inst/extdata/diag_data_finale.Rdata"))
load(file=file.path(path_root, "//inst/extdata/subjects_data.Rdata"))

####################################################################################
# Table restreinte aux variables identifiées dans l'étape de modélisation
####################################################################################

diag_data<-subset(diag_data_finale,select = c(Subject, Session, dementia,CDR3,M.F,Hand,age_at_diagnosis,TAILLE,POIDS,
                                                     AUTONOMIE,ENTETEMENT,DEPRESS,ANXIETE,APATHIE,DISINHIB,IRRITAB,
                                                     ARGENT,FACTURES,SHOPPING,JEU,REPAS,SOUV_EVENT,CONCENTRATION,SOUV_DATES,DEPLACEMENT
))

sujet<-unique(subset(diag_data,select=c(Subject,M.F,Hand)))

library(ggplot2)
library(dplyr)
library(RColorBrewer)

diag_data$dementia_label<-factor(diag_data$dementia, labels=c("Absence de trouble","Présence de troubles"))
diag_data$CDR3_label<-factor(diag_data$CDR3, labels=c("Absence de trouble", "Troubles incertains ou bénins", "Troubles modérés ou sévères"))

##############################################################################################
# Pour Shiny
##############################################################################################


############################################################
# Genre du patient
############################################################

eff_genre<-table(sujet$M.F)
#eff

count.data <- data.frame(
  class = c("Masculin","Féminin"),
  n = c(eff_genre[1],eff_genre[2]),
  prop = c(round(eff_genre[1]/(eff_genre[1]+eff_genre[2])*100, 0),round(eff_genre[2]/(eff_genre[1]+eff_genre[2])*100,0))
)
#count.data

# Ajouter la position de l'étiquette
count.data <- count.data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
#count.data


ggplot(count.data, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") + ggtitle("Genre du patient")+
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste(prop,"%")), color = "black")+
  theme_void()+
  scale_fill_brewer(type = "seq",palette="Blues",direction = -1, labels = c("Masculin", "Féminin"))+ theme(legend.position="right") +
    theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title = element_blank())

############################################################
# Droitier ou gaucher
############################################################

eff<-table(sujet$Hand)
#eff

count.data <- data.frame(
  class = c("Ambidextre","Gaucher","Droitier"),
  n = c(eff[1],eff[2],eff[3]),
  prop = c(round(eff[1]/(eff[1]+eff[2]+eff[3])*100, 0),round(eff[2]/(eff[1]+eff[2]+eff[3])*100,0),round(eff[3]/(eff[1]+eff[2]+eff[3])*100,0))
)
#count.data

# Ajouter la position de l'étiquette
count.data <- count.data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
#count.data

ggplot(count.data, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") + ggtitle("Latéralité du patient")+
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste(prop,"%")), color = "black")+
  theme_void()+
  scale_fill_brewer(type = "seq",palette="Blues",direction = -1, labels = c("Ambidextre", "Droitier","Gaucher"))+ theme(legend.position="right") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title = element_blank())

############################################################
#Age au moment du diagnostic
############################################################

hist(diag_data[,"age_at_diagnosis"],main = "Répartition des patients par âge au moment du diagnostic",
     prob=TRUE,
     xlab="Age du patient au moment du diagnostic",
     ylab="Densité",
     col="lightblue")

############################################################
#Taille au moment du diagnostic
############################################################

hist(diag_data[,"TAILLE"],main = "Répartition des patients par taille",
     prob=TRUE,
     xlab="Taille du patient (en cm)",
     ylab="Densité",
     col="lightblue")

############################################################
# Poids au moment du diagnostic
############################################################


hist(diag_data[,"POIDS"],main = "Répartition des patients par poids au moment du diagnostic",
     prob=TRUE,
     xlab="Poids du patient (en kg)",
     ylab="Densité",
     col="lightblue")

############################################################
# AUTONOMIE
############################################################

eff<-table(diag_data$AUTONOMIE)
#eff

count.data <- data.frame(
  class = c("En capacité de vivre seul","Requiert une assistance pour des activités particulièrement complexes",
            "Requiert une assistace pour des activités quotidiennes","Dépendant","Ne sait pas"),
  n = c(eff[1],eff[2],eff[3],eff[4],eff[5]),
  prop = c(round(eff[1]/(eff[1]+eff[2]+eff[3]+eff[4]+eff[5])*100, 0),
           round(eff[2]/(eff[1]+eff[2]+eff[3]+eff[4]+eff[5])*100,0),
           round(eff[3]/(eff[1]+eff[2]+eff[3]+eff[4]+eff[5])*100,0),
           round(eff[4]/(eff[1]+eff[2]+eff[3]+eff[4]+eff[5])*100,0),
           round(eff[5]/(eff[1]+eff[2]+eff[3]+eff[4]+eff[5])*100,0)
))
#count.data

# Ajouter la position de l'étiquette
count.data <- count.data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)


ggplot(count.data, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") + ggtitle("Quel est le niveau d'indépendance du patient ?")+
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste(prop,"%")), color = "black")+
  theme_void()+
  scale_fill_brewer(type = "seq",palette="Blues",direction = -1, labels = c("En capacité de vivre seul","Requiert une assistance pour des activités particulièrement complexes",
                                                                            "Requiert une assistace pour des activités quotidiennes","Dépendant","Ne sait pas"))+ theme(legend.position="right") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title = element_blank())


############################################################
# ENTETEMENT
############################################################

eff<-table(diag_data$ENTETEMENT)
#eff

count.data <- data.frame(
  class = c("Non","Oui"),
  n = c(eff[1],eff[2]),
  prop = c(round(eff[1]/(eff[1]+eff[2])*100, 0),
           round(eff[2]/(eff[1]+eff[2])*100,0)
  ))
#count.data

# Ajouter la position de l'étiquette
count.data <- count.data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
#count.data

ggplot(count.data, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") + ggtitle("Le patient s'entête-t-il et refuse-t-il l'aide des autres ?")+
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste(prop,"%")), color = "black")+
  theme_void()+
  scale_fill_brewer(type = "seq",palette="Blues",direction = -1, labels = c("Non","Oui"))+ theme(legend.position="right") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title = element_blank())


############################################################
# DEPRESS
############################################################

eff<-table(diag_data$DEPRESS)
#eff

count.data <- data.frame(
  class = c("Non","Oui"),
  n = c(eff[1],eff[2]),
  prop = c(round(eff[1]/(eff[1]+eff[2])*100, 0),
           round(eff[2]/(eff[1]+eff[2])*100,0)
  ))
#count.data

# Ajouter la position de l'étiquette
count.data <- count.data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
#count.data

ggplot(count.data, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") + ggtitle("Le patient connait-il des épisodes de dépression ou de dysphorie ?")+
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste(prop,"%")), color = "black")+
  theme_void()+
  scale_fill_brewer(type = "seq",palette="Blues",direction = -1, labels = c("Non","Oui"))+ theme(legend.position="right") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title = element_blank())


############################################################
# ANXIETE
############################################################

eff<-table(diag_data$ANXIETE)
#eff

count.data <- data.frame(
  class = c("Non","Oui"),
  n = c(eff[1],eff[2]),
  prop = c(round(eff[1]/(eff[1]+eff[2])*100, 0),
           round(eff[2]/(eff[1]+eff[2])*100,0)
  ))
#count.data

# Ajouter la position de l'étiquette
count.data <- count.data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
#count.data

ggplot(count.data, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") + ggtitle("Le patient est-il anxieux ?")+
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste(prop,"%")), color = "black")+
  theme_void()+
  scale_fill_brewer(type = "seq",palette="Blues",direction = -1, labels = c("Non","Oui"))+ theme(legend.position="right") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title = element_blank())


############################################################
# APATHIE
############################################################

eff<-table(diag_data$APATHIE)
#eff

count.data <- data.frame(
  class = c("Non","Oui"),
  n = c(eff[1],eff[2]),
  prop = c(round(eff[1]/(eff[1]+eff[2])*100, 0),
           round(eff[2]/(eff[1]+eff[2])*100,0)
  ))
#count.data

# Ajouter la position de l'étiquette
count.data <- count.data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
#count.data

ggplot(count.data, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") + ggtitle("Le patient est-il apathique ?")+
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste(prop,"%")), color = "black")+
  theme_void()+
  scale_fill_brewer(type = "seq",palette="Blues",direction = -1, labels = c("Non","Oui"))+ theme(legend.position="right") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title = element_blank())


############################################################
# DISINHIB
############################################################

eff<-table(diag_data$DISINHIB)
#eff

count.data <- data.frame(
  class = c("Non","Oui"),
  n = c(eff[1],eff[2]),
  prop = c(round(eff[1]/(eff[1]+eff[2])*100,0),
           round(eff[2]/(eff[1]+eff[2])*100,0)
  ))
#count.data

# Ajouter la position de l'étiquette
count.data <- count.data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
#count.data

ggplot(count.data, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") + ggtitle("Le patient est-il désinhibé ?")+
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste(prop,"%")), color = "black")+
  theme_void()+
  scale_fill_brewer(type = "seq",palette="Blues",direction = -1, labels = c("Non","Oui"))+ theme(legend.position="right") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title = element_blank())


############################################################
# IRRITAB
############################################################

eff<-table(diag_data$IRRITAB)
#eff

count.data <- data.frame(
  class = c("Non","Oui"),
  n = c(eff[1],eff[2]),
  prop = c(round(eff[1]/(eff[1]+eff[2])*100,0),
           round(eff[2]/(eff[1]+eff[2])*100,0)
  ))
#count.data

# Ajouter la position de l'étiquette
count.data <- count.data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
#count.data

ggplot(count.data, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") + ggtitle("Le patient est-il irritable ?")+
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste(prop,"%")), color = "black")+
  theme_void()+
  scale_fill_brewer(type = "seq",palette="Blues",direction = -1, labels = c("Non","Oui"))+ theme(legend.position="right") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title = element_blank())


############################################################
# ARGENT
############################################################

eff<-table(diag_data$AUTONOMIE)
#eff

count.data <- data.frame(
  class = c("Non","A rencontré des difficulté, mais a réussi seul",
            "A eu besoin d'une aide","Etait dépendant d'une tierce personne","Ne sait pas"),
  n = c(eff[1],eff[2],eff[3],eff[4],eff[5]),
  prop = c(round(eff[1]/(eff[1]+eff[2]+eff[3]+eff[4])*100, 0),
           round(eff[2]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[3]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[4]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[5]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0)
  ))
#count.data

# Ajouter la position de l'étiquette
count.data <- count.data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
#count.data


ggplot(count.data, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") + ggtitle("Dans les 4 dernières semaines, le patient a-t-il eu des difficultés ou \n besoin d'aide pour écrire un chèque, payer avec des billets... ?")+
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste(prop,"%")), color = "black")+
  theme_void()+
  scale_fill_brewer(type = "seq",palette="Blues",direction = -1, labels = c("Non","Il a rencontré des difficulté, mais a réussi seul",
                                                                            "Il a eu besoin d'une aide","Il a été dépendant d'une tierce personne","Ne sait pas"))+ theme(legend.position="right") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title = element_blank())

############################################################
# FACTURES
############################################################

eff<-table(diag_data$FACTURES)
#eff

count.data <- data.frame(
  class = c("Non","A rencontré des difficulté, mais a réussi seul",
            "A eu besoin d'une aide","Etait dépendant d'une tierce personne","Ne sait pas"),
  n = c(eff[1],eff[2],eff[3],eff[4],eff[5]),
  prop = c(round(eff[1]/(eff[1]+eff[2]+eff[3]+eff[4])*100, 0),
           round(eff[2]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[3]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[4]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[5]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0)
  ))
#count.data

# Ajouter la position de l'étiquette
count.data <- count.data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
#count.data

ggplot(count.data, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") + ggtitle("Dans les 4 dernières semaines, le patient a-t-il eu des difficultés pour \n gérer ses papiers, payer ses factures, etc ?")+
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste(prop,"%")), color = "black")+
  theme_void()+
  scale_fill_brewer(type = "seq",palette="Blues",direction = -1, labels = c("Non","Il a rencontré des difficulté, mais a réussi seul",
                                                                            "Il a eu besoin d'une aide","Il a été dépendant d'une tierce personne","Ne sait pas"))+ theme(legend.position="right") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title = element_blank())


############################################################
# SHOPPING
############################################################

eff<-table(diag_data$SHOPPING)
#eff

count.data <- data.frame(
  class = c("Non","A rencontré des difficulté, mais a réussi seul",
            "A eu besoin d'une aide","Etait dépendant d'une tierce personne","Ne sait pas"),
  n = c(eff[1],eff[2],eff[3],eff[4],eff[5]),
  prop = c(round(eff[1]/(eff[1]+eff[2]+eff[3]+eff[4])*100, 0),
           round(eff[2]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[3]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[4]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[5]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0)
  ))
#count.data

# Ajouter la position de l'étiquette
count.data <- count.data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
#count.data

ggplot(count.data, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") + ggtitle("Dans les 4 dernières semaines, le patient a-t-il eu des difficultés pour faire ses courses ?")+
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste(prop,"%")), color = "black")+
  theme_void()+
  scale_fill_brewer(type = "seq",palette="Blues",direction = -1, labels = c("Non","Il a rencontré des difficulté, mais a réussi seul",
                                                                            "Il a eu besoin d'une aide","Il a été dépendant d'une tierce personne","Ne sait pas"))+ theme(legend.position="right") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title = element_blank())


############################################################
# JEU
############################################################

eff<-table(diag_data$JEU)
#eff

count.data <- data.frame(
  class = c("Non","A rencontré des difficulté, mais a réussi seul",
            "A eu besoin d'une aide","Etait dépendant d'une tierce personne","Ne sait pas"),
  n = c(eff[1],eff[2],eff[3],eff[4],eff[5]),
  prop = c(round(eff[1]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[2]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[3]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[4]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[5]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0)
  ))
#count.data

# Ajouter la position de l'étiquette
count.data <- count.data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
#count.data

ggplot(count.data, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") + ggtitle("Dans les 4 dernières semaines, le patient a-t-il rencontré des difficultés ou \n besoin d'aide pour jouer à un jeu de réflexion tel que le bridge ou les échecs ?")+
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste(prop,"%")), color = "black")+
  theme_void()+
  scale_fill_brewer(type = "seq",palette="Blues",direction = -1, labels = c("Non","Il a rencontré des difficulté, mais a réussi seul",
                                                                            "Il a eu besoin d'une aide","Il a été dépendant d'une tierce personne","Ne sait pas"))+ theme(legend.position="right") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title = element_blank())

############################################################
# REPAS
############################################################

eff<-table(diag_data$REPAS)
#eff

count.data <- data.frame(
  class = c("Non","A rencontré des difficulté, mais a réussi seul",
            "A eu besoin d'une aide","Etait dépendant d'une tierce personne","Ne sait pas"),
  n = c(eff[1],eff[2],eff[3],eff[4],eff[5]),
  prop = c(round(eff[1]/(eff[1]+eff[2]+eff[3]+eff[4])*100, 0),
           round(eff[2]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[3]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[4]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[5]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0)
  ))
#count.data

# Ajouter la position de l'étiquette
count.data <- count.data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
#count.data

ggplot(count.data, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") + ggtitle("Durant les 4 dernières semaines, le patient a-t-il eu des difficultés \n de la préparation d'un repas équilibré ?")+
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste(prop,"%")), color = "black")+
  theme_void()+
  scale_fill_brewer(type = "seq",palette="Blues",direction = -1, labels = c("Non","Il a rencontré des difficulté, mais a réussi seul",
                                                                            "Il a eu besoin d'une aide","Il a été dépendant d'une tierce personne","Ne sait pas"))+ theme(legend.position="right") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title = element_blank())

############################################################
# SOUV_EVENT
############################################################

eff<-table(diag_data$SOUV_EVENT)
#eff

count.data <- data.frame(
  class = c("Non","A rencontré des difficulté, mais a réussi seul",
            "A eu besoin d'une aide","Etait dépendant d'une tierce personne","Ne sait pas"),
  n = c(eff[1],eff[2],eff[3],eff[4],eff[5]),
  prop = c(round(eff[1]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[2]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[3]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[4]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[5]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0)
  ))
#count.data

# Ajouter la position de l'étiquette
count.data <- count.data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
#count.data

ggplot(count.data, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") + ggtitle("Durant les 4 dernières semaines, le patient a-t-il eu des difficultés à se rappeler d'événements courants ?")+
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste(prop,"%")), color = "black")+
  theme_void()+
  scale_fill_brewer(type = "seq",palette="Blues",direction = -1, labels = c("Non","Il a rencontré des difficulté, mais a réussi seul",
                                                                            "Il a eu besoin d'une aide","Il a été dépendant d'une tierce personne","Ne sait pas"))+ theme(legend.position="right") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title = element_blank())


############################################################
# CONCENTRATION
############################################################

eff<-table(diag_data$CONCENTRATION)
#eff

count.data <- data.frame(
  class = c("Non","A rencontré des difficulté, mais a réussi seul",
            "A eu besoin d'une aide","Etait dépendant d'une tierce personne","Ne sait pas"),
  n = c(eff[1],eff[2],eff[3],eff[4],eff[5]),
  prop = c(round(eff[1]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[2]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[3]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[4]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[5]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0)
  ))
#count.data

# Ajouter la position de l'étiquette
count.data <- count.data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
#count.data


ggplot(count.data, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") + ggtitle("Durant les 4 dernières semaines, le patient a-t-il eu des difficultés à \n se concentrer et à comprendre un programme TV, un livre ou un magazine ?")+
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste(prop,"%")), color = "black")+
  theme_void()+
  scale_fill_brewer(type = "seq",palette="Blues",direction = -1, labels = c("Non","Il a rencontré des difficulté, mais a réussi seul",
                                                                            "Il a eu besoin d'une aide","Il a été dépendant d'une tierce personne","Ne sait pas"))+ theme(legend.position="right") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title = element_blank())


############################################################
# SOUV_DATES
############################################################

eff<-table(diag_data$SOUV_DATES)
#eff

count.data <- data.frame(
  class = c("Non","A rencontré des difficulté, mais a réussi seul",
            "A eu besoin d'une aide","Etait dépendant d'une tierce personne","Ne sait pas"),
  n = c(eff[1],eff[2],eff[3],eff[4],eff[5]),
  prop = c(round(eff[1]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[2]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[3]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[4]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[5]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0)
  ))
#count.data

# Ajouter la position de l'étiquette
count.data <- count.data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
#count.data


ggplot(count.data, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") + ggtitle("Durant les 4 dernières semaines, le patient a-t-il eu des difficultés à se souvenir de dates ?")+
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste(prop,"%")), color = "black")+
  theme_void()+
  scale_fill_brewer(type = "seq",palette="Blues",direction = -1, labels = c("Non","Il a rencontré des difficulté, mais a réussi seul",
                                                                            "Il a eu besoin d'une aide","Il a été dépendant d'une tierce personne","Ne sait pas"))+ theme(legend.position="right") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title = element_blank())


############################################################
# DEPLACEMENT
############################################################

eff<-table(diag_data$DEPLACEMENT)
#eff

count.data <- data.frame(
  class = c("Non","A rencontré des difficulté, mais a réussi seul",
            "A eu besoin d'une aide","Etait dépendant d'une tierce personne","Ne sait pas"),
  n = c(eff[1],eff[2],eff[3],eff[4],eff[5]),
  prop = c(round(eff[1]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[2]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[3]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[4]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[5]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0)
  ))
#count.data

# Ajouter la position de l'étiquette
count.data <- count.data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
#count.data

ggplot(count.data, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") + ggtitle("Durant les 4 dernières semaines, le patient a-t-il eu des difficultés à \n se déplacer en dehors de son quartier, à conduire, ou à prendre les transports en commun ?")+
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste(prop,"%")), color = "black")+
  theme_void()+
  scale_fill_brewer(type = "seq",palette="Blues",direction = -1, labels = c("Non","Il a rencontré des difficulté, mais a réussi seul",
                                                                            "Il a eu besoin d'une aide","Il a été dépendant d'une tierce personne","Ne sait pas"))+ theme(legend.position="right") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title = element_blank())


############################################################
# DEPLACEMENT
############################################################

eff<-table(diag_data$DEPLACEMENT)
#eff

count.data <- data.frame(
  class = c("Non","A rencontré des difficulté, mais a réussi seul",
            "A eu besoin d'une aide","Etait dépendant d'une tierce personne","Ne sait pas"),
  n = c(eff[1],eff[2],eff[3],eff[4],eff[5]),
  prop = c(round(eff[1]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[2]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[3]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[4]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0),
           round(eff[5]/(eff[1]+eff[2]+eff[3]+eff[4])*100,0)
  ))
#count.data

# Ajouter la position de l'étiquette
count.data <- count.data %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
#count.data

ggplot(count.data, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 1, stat = "identity", color = "white") + ggtitle("Durant les 4 dernières semaines, le patient a-t-il eu des difficultés à \n se déplacer en dehors de son quartier, à conduire, ou à prendre les transports en commun ?")+
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste(prop,"%")), color = "black")+
  theme_void()+
  scale_fill_brewer(type = "seq",palette="Blues",direction = -1, labels = c("Non","Il a rencontré des difficulté, mais a réussi seul",
                                                                            "Il a eu besoin d'une aide","Il a été dépendant d'une tierce personne","Ne sait pas"))+ theme(legend.position="right") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.title = element_blank())


