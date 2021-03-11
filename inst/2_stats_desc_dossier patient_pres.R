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

path_root <-"C:/Users/Klara/Documents/Datascientist/dementiaproject"
load(file=file.path(path_root, "//inst/extdata/oasis3/bases_R/diag_data_finale.Rdata"))
load(file=file.path(path_root, "//inst/extdata/oasis3/bases_R/subjects_data.Rdata"))

####################################################################################
# Table restreinte aux variables identifiées dans l'étape de modélisation
####################################################################################

diag_data<-subset(diag_data_finale,select = c(Subject, Session, dementia,CDR3,M.F,Hand,age_at_diagnosis,TAILLE,POIDS,
                                                     AUTONOMIE,ENTETEMENT,DEPRESS,ANXIETE,APATHIE,DISINHIB,IRRITAB,
                                                     ARGENT,FACTURES,SHOPPING,JEU,REPAS,SOUV_EVENT,CONCENTRATION,SOUV_DATES,DEPLACEMENT
))

sujet<-unique(subset(diag_data,select=c(Subject,M.F,Hand)))

library(ggplot2)
# library(data.table)
library(dplyr)
library(RColorBrewer)
library(GGally)
library(scales)

diag_data$dementia_label<-factor(diag_data$dementia, labels=c("Absence de trouble","Présence de troubles"))
diag_data$CDR3_label<-factor(diag_data$CDR3, labels=c("Absence de trouble", "Troubles incertains ou bénins", "Troubles modérés ou sévères"))


###########################################################################################
# Statistiques en fonction de la CDR pour la présentation
###########################################################################################


############################################################
#Age au moment du diagnostic - Boxplot
############################################################

ggplot(diag_data[, .(CDR3_label, age_at_diagnosis)], aes(fill = CDR3_label, y= age_at_diagnosis)) +
  geom_boxplot() +
  ggtitle("Distribution de l'âge des patients au moment du diagnostic en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") +
  labs(x = "Démence", y = "Age du patient au moment du diagnostic")+
  theme_minimal() +
  theme(axis.text.x = element_blank())

############################################################
# SOUV_EVENT
############################################################

ggplot(diag_data) +
  aes(
    x = diag_data$CDR3_label, fill = SOUV_EVENT, by = SOUV_EVENT, y = after_stat(prop),
    label = scales::percent(after_stat(prop), accuracy = 1),
  ) +
  geom_bar(stat = "prop", position = position_dodge(.9)) +
  geom_text(
    aes(y = after_stat(prop) - .005), stat = "prop",
    position = position_dodge(.9), vjust = "top"
  ) +
  scale_y_continuous(labels = percent)+
  theme_light() +
  xlab("Démence") +
  ylab("Proportion") +
  labs(fill = "Capacité à se souvenir") +
  ggtitle("Durant les 4 dernières semaines, le patient a-t-il eu \n des difficultés à se rappeler d'événements courants ?") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top"
  ) +
  scale_fill_brewer(labels = c("Non","Il a rencontré des difficulté, mais a réussi seul",
                             "Il a eu besoin d'une aide","Il a été dépendant d'une tierce personne","Ne sait pas"))+
  theme(legend.position="right")

############################################################
# SOUV_DATES
############################################################

ggplot(diag_data) +
  aes(
    x = diag_data$CDR3_label, fill = SOUV_DATES, by = SOUV_DATES, y = after_stat(prop),
    label = scales::percent(after_stat(prop), accuracy = 1),
  ) +
  geom_bar(stat = "prop", position = position_dodge(.9)) +
  geom_text(
    aes(y = after_stat(prop) - .005), stat = "prop",
    position = position_dodge(.9), vjust = "top"
  ) +
  scale_y_continuous(labels = percent)+
  theme_light() +
  xlab("Démence") +
  ylab("Proportion") +
  labs(fill = "Capacité à se souvenir") +
  ggtitle("Durant les 4 dernières semaines, le patient a-t-il eu des difficultés à se souvenir de dates ?") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top"
  ) +
  scale_fill_brewer(labels = c("Non","Il a rencontré des difficulté, mais a réussi seul",
                               "Il a eu besoin d'une aide","Il a été dépendant d'une tierce personne","Ne sait pas"))+
  theme(legend.position="right")


############################################################
# AUTONOMIE
############################################################

ggplot(diag_data) +
  aes(
    x = diag_data$CDR3_label, fill = AUTONOMIE, by = AUTONOMIE, y = after_stat(prop),
    label = scales::percent(after_stat(prop), accuracy = 1),
  ) +
  geom_bar(stat = "prop", position = position_dodge(.9)) +
  geom_text(
    aes(y = after_stat(prop) - .005), stat = "prop",
    position = position_dodge(.9), vjust = "top"
  ) +
  scale_y_continuous(labels = percent)+
  theme_light() +
  xlab("Démence") +
  ylab("Proportion") +
  labs(fill = "Niveau d'indépendance") +
  ggtitle("Quel est le niveau d'indépendance du patient ?") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top"
  ) +
  scale_fill_brewer(labels = c("En capacité de vivre seul","Requiert une assistance pour des activités particulièrement complexes",
                               "Requiert une assistace pour des activités quotidiennes","Dépendant","Ne sait pas"))+
  theme(legend.position="right")

############################################################
# APATHIE
############################################################

ggplot(diag_data) +
  aes(
    x = diag_data$CDR3_label, fill = APATHIE, by = APATHIE, y = after_stat(prop),
    label = scales::percent(after_stat(prop), accuracy = 1),
  ) +
  geom_bar(stat = "prop", position = position_dodge(.9)) +
  geom_text(
    aes(y = after_stat(prop) - .005), stat = "prop",
    position = position_dodge(.9), vjust = "top"
  ) +
  scale_y_continuous(labels = percent)+
  theme_light() +
  xlab("Démence") +
  ylab("Proportion") +
  labs(fill = "Apathie") +
  ggtitle("Le patient est-il apathique ?") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top"
  ) +
  scale_fill_brewer(labels = c("Non","Oui"))+
  theme(legend.position="right")

############################################################
# FACTURES
############################################################

ggplot(diag_data) +
  aes(
    x = diag_data$CDR3_label, fill = FACTURES, by = FACTURES, y = after_stat(prop),
    label = scales::percent(after_stat(prop), accuracy = 1),
  ) +
  geom_bar(stat = "prop", position = position_dodge(.9)) +
  geom_text(
    aes(y = after_stat(prop) - .005), stat = "prop",
    position = position_dodge(.9), vjust = "top"
  ) +
  scale_y_continuous(labels = percent)+
  theme_light() +
  xlab("Démence") +
  ylab("Proportion") +
  labs(fill = "Gestion quotidienne") +
  ggtitle("Dans les 4 dernières semaines, le patient a-t-il eu des difficultés pour \n gérer ses papiers, payer ses factures, etc ?") +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top"
  ) +
  scale_fill_brewer(labels = c("Non","A rencontré des difficulté, mais a réussi seul",
                               "A eu besoin d'une aide","Etait dépendant d'une tierce personne","Ne sait pas"))+
  theme(legend.position="right")


#############################################################
# Statistiques descriptives générales
##############################################################

