######################################################################
# Programme 0 - Import des bases OASIS3
######################################################################

#Tables disponibles :
#oasis3_sujets_complete : table contenant l'ensemble des participants à l'étude
#Table oasis3_freesurfer_complete : table contenant une ligne par diagnostic et l'ensemble des variables issues de l'imagerie médicale
#Table oasis3_diagnostic_complete : table contenant l'ensemble des variables "patient" : une ligne par diagnostic
install.packages("stringr", dependencies=TRUE)

library(data.table)
library(purrr)#permet de faire des boucles plus rapidement
library(ggplot2)
library(dplyr)
library(stringr)#pour gérer les chaînes de caractères


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










