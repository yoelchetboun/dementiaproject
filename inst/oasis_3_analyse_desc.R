library(data.table)
library(purrr)
library(ggplot2)


path_root <- "~/GENERIC/dementiaproject/"
path_data <- "/srv/OASIS_DATA/oasis3/"


##### ---- Subjects data ---- ######

subjects_data <- read.csv2(file.path(path_root, "inst/extdata/oasis3/sujects_data_full.csv"), header = TRUE, sep = ",")
setDT(subjects_data)
str(subjects_data)
subjects_data <- subjects_data[,which(unlist(lapply(subjects_data, function(x)!all(is.na(x))))),with=F] #suppression des colonnes vides
subjects_data$M.F <- as.factor(subjects_data$M.F)

# droitier / gaucher bar plot
ggplot(subjects_data, aes(x = M.F, fill = Hand)) + geom_bar(position = "stack") + theme_minimal() + ggtitle("Nombre de patients dans l'étude OASIS-3") +
  scale_fill_discrete(name = "Préférence manuelle", labels = c("Ambidextre", "Gaucher", "Droitier")) + labs(x = "Genre", y = "Nombre de patients")

#nombre de session d'IRM
ggplot(subjects_data, aes(x = factor(MR.Sessions), fill = factor(1))) + geom_bar(stat = "count") + theme_minimal() + ggtitle("Nombre d'IRM effectués dans l'étude OASIS-3") +
  geom_label(stat='count', aes(label=..count..),  position=position_dodge(.8), show.legend = FALSE, size = 5) +
  labs(x = "Nombre de sessions d'IRM", y = "Nombre de patients") + guides( fill = FALSE) + scale_x_discrete(labels = seq(1,9,1))

##### ---- ADRC data ---- ######

adrc_data <- read.csv2(file.path(path_root, "inst/extdata/oasis3/adrc_clinical_data_full.csv"), header = TRUE, sep = ",")
setDT(adrc_data)
adrc_data <- adrc_data[,which(unlist(lapply(adrc_data, function(x)!all(is.na(x))))),with=F]
str(adrc_data)
adrc_data[, case_percent := nrow(.SD) / nrow(adrc_data), by = "cdr"]
adrc_data$cdr <- as.factor(adrc_data$cdr)
adrc_data[, nb_cdr_per_apoe := nrow(.SD), by = c("apoe", "cdr")]
adrc_data[, nb_per_apoe := nrow(.SD), by = apoe]
adrc_data[, nb_cdr_per_apoe_percent := nb_cdr_per_apoe*100/nb_per_apoe]

# UDS = Uniform Data Set
#dx1 : Diagnostic impression intake and interview culminating with a coded dementia diagnosis that is recorded in the OASIS datatype “ADRC Clinical Data” dx1-dx5.
# Diagnoses for this variable include “cognitively normal”, “AD dementia”, “vascular dementia” and contributing factors such as vitamin deficiency, alcoholism, and mood disorders

#nombre de diagnostics réalisé
ggplot(unique(adrc_data[, .(cdr , case_percent)]), aes(x = cdr, y= case_percent, fill = factor(1))) + geom_bar(stat="identity", position=position_dodge()) + ggtitle(paste0("Nombre de diagnostics réalisés [", nrow(adrc_data), " diagnostics / ", nrow(subjects_data)," patients]")) +
  geom_label(aes(label = round(case_percent, 2)), position=position_dodge(.8), show.legend = FALSE, size = 5) +
  scale_fill_discrete(name = "") + labs(x = "Clinical Dementia Rating [0 à 3]", y = "Nombre de diagnostics [%]") + guides( fill = FALSE) + theme_minimal()

#mmse
ggplot(adrc_data[, .(cdr , mmse )], aes(fill = cdr, y= mmse)) + geom_boxplot() + ggtitle("Distribution du MMSE par groupe diagnostic")
  scale_fill_discrete(name = "CDR") + labs(x = "Clinical Dementia Rating [0 à 3]", y = "Mini-Mental State Examination score [0 à 30]")+ theme_minimal() + theme(axis.text.x = element_blank())

#Genotype
ggplot(na.omit(adrc_data[, .(cdr , apoe )]), aes(fill = cdr, x= cdr)) + geom_bar(stat = "count", position=position_dodge()) + facet_wrap(~apoe) + ggtitle("Nombre de diagnostics vs Genotype") +
  scale_fill_discrete(name = "CDR") + labs(x = "Genotype", y = "Nombre de diagnostics")

ggplot(unique(na.omit(adrc_data[, .(cdr , apoe, nb_cdr_per_apoe_percent)])), aes(fill = cdr, x= cdr, y = nb_cdr_per_apoe_percent)) + geom_bar(stat = "identity", position=position_dodge()) +
  facet_wrap(~apoe) + ggtitle("Nombre de diagnostics vs Genotype") +
  geom_label(aes(label = round(nb_cdr_per_apoe_percent, 1)), position=position_dodge(.8), show.legend = FALSE, size = 5) +
  scale_fill_discrete(name = "CDR") + labs(x = "Genotype", y = "Nombre de diagnostics [%]")

adrc_data[grepl(ADRC_ADRCCLINICALDATA.ID, pattern = "d0000")]
adrc_data[, nb_days_since_entry := sub(pattern = "d", replacement = "", strsplit(ADRC_ADRCCLINICALDATA.ID, split = "_")[[1]][[3]]), by = "ADRC_ADRCCLINICALDATA.ID"]
adrc_data[, nb_days_since_entry := as.numeric(nb_days_since_entry)]
adrc_data[, age_at_diagnosis := as.numeric(ageAtEntry) + nb_days_since_entry / 365]

#age au moment du diag
ggplot(adrc_data[, .(cdr , age_at_diagnosis )], aes(fill = cdr, y= age_at_diagnosis)) + geom_boxplot() + ggtitle("Box plot de l'âge au moment du diagnostic") +
  scale_fill_discrete(name = "CDR") + labs(x = "Clinical Dementia Rating [0 à 3]", y = "Age") + theme_minimal() + theme(axis.text.x = element_blank())

adrc_data <- merge(adrc_data, subjects_data[, .(Subject, Education)], by = "Subject")

#nb années de scolarité
ggplot(adrc_data[, .(cdr , Education )], aes(fill = cdr, y= Education)) + geom_boxplot() + ggtitle("Box plot du nombre d'année en scolarité au moment du diagnostic") +
  scale_fill_discrete(name = "CDR") + labs(x = "Clinical Dementia Rating [0 à 3]", y = "Nombre d'années") + theme_minimal() + theme(axis.text.x = element_blank())

#on filtre par dementia or not
adrc_data[cdr != "0.0", dementia := TRUE]
adrc_data[cdr == "0.0", dementia := FALSE]

#distribution de l'age
ggplot(adrc_data[, .(dementia , age_at_diagnosis )], aes(fill = dementia, x= age_at_diagnosis)) + geom_density(alpha=0.4) + ggtitle("Distribution de l'âge au moment du diagnostic") +
  scale_fill_discrete(name = "", labels = c("Non Dément", "Dément")) + labs(x = "Age au moment du diagnostic", y = "Distribution") + theme_minimal() + theme(axis.text.x = element_blank())

adrc_data$mmse <- as.numeric(adrc_data$mmse)

ggplot(adrc_data[, .(dementia , mmse )], aes(fill = dementia, x= mmse)) + geom_density(alpha=0.4) + ggtitle("Distribution de l'âge au moment du diagnostic") +
  scale_fill_discrete(name = "", labels = c("Non Dément", "Dément")) + labs(x = "Age au moment du diagnostic", y = "Distribution") + theme_minimal() + theme(axis.text.x = element_blank())




##### ---- MRI data ---- ######

mri_data <- read.csv2(file.path(path_root, "inst/extdata/oasis3/mr_sessions_data_full.csv"), header = TRUE, sep = ",")
setDT(mri_data)

#comment matcher la visite adrc et le free surfer?
mri_data[Subject == "OAS30001"]
adrc_data[Subject == "OAS30001"]

mri_data[, nb_days_since_entry := sub(pattern = "d", replacement = "", strsplit(MR.ID, split = "_")[[1]][[3]]), by = "MR.ID"]
mri_data[, nb_days_since_entry := as.numeric(nb_days_since_entry)]

# pour chaque mri session on recup l'ID de la adrc visit qui suit
mri_data[, mri_following_adrc_session := adrc_data[Subject == .SD$Subject & nb_days_since_entry >= .SD$nb_days_since_entry]$ADRC_ADRCCLINICALDATA.ID[1], by = "MR.ID"]
mri_data[, mri_previous_adrc_session := tail(adrc_data[Subject == .SD$Subject & nb_days_since_entry <= .SD$nb_days_since_entry]$ADRC_ADRCCLINICALDATA.ID, 1), by = "MR.ID"]

mri_adrc_join <- copy(mri_data[, .(MR.ID, Subject, nb_days_since_entry, mri_following_adrc_session, mri_previous_adrc_session)])
mri_adrc_join <- merge(mri_adrc_join, adrc_data[, .(ADRC_ADRCCLINICALDATA.ID, cdr)], by.x = "mri_following_adrc_session", by.y = "ADRC_ADRCCLINICALDATA.ID", all.x= TRUE)
setnames(mri_adrc_join, "cdr", "cdr_following_adrc")
mri_adrc_join <- merge(mri_adrc_join, adrc_data[, .(ADRC_ADRCCLINICALDATA.ID, cdr)], by.x = "mri_previous_adrc_session", by.y = "ADRC_ADRCCLINICALDATA.ID", all.x= TRUE)
setnames(mri_adrc_join, "cdr", "cdr_previous_adrc")
mri_adrc_join[is.na(mri_following_adrc_session), ":=" (mri_following_adrc_session = mri_previous_adrc_session,
                                                       cdr_following_adrc = cdr_previous_adrc)]

mri_adrc_join[, ":=" (nb_days_following_adrc = as.numeric(sub(pattern = "d", replacement = "", strsplit(mri_following_adrc_session, split = "_")[[1]][[3]])),
                      nb_days_previous_adrc = as.numeric(sub(pattern = "d", replacement = "", strsplit(mri_previous_adrc_session, split = "_")[[1]][[3]]))), by= MR.ID]
mri_adrc_join[, ":=" (nb_days_btw_mri_fol_adrc = nb_days_following_adrc - nb_days_since_entry,
                      nb_days_btw_mri_prev_adrc = nb_days_since_entry - nb_days_previous_adrc), by= MR.ID]

mri_adrc_join[nb_days_btw_mri_fol_adrc <= nb_days_btw_mri_prev_adrc, adcr_ref := "following"]
mri_adrc_join[nb_days_btw_mri_fol_adrc > nb_days_btw_mri_prev_adrc, adcr_ref := "previous"]
mri_adrc_join[adcr_ref == "following", cdr_ref := cdr_following_adrc]
mri_adrc_join[adcr_ref == "previous", cdr_ref := cdr_previous_adrc]

#save(mri_adrc_join, file =  file.path(path_root, "inst/extdata/oasis3/mri_adrc_join.Rdata"))



##### ---- Free Surfer data ---- ######

fs_data <- read.csv2(file.path(path_root, "inst/extdata/oasis3/freesurfer_data_short.csv"), header = TRUE, sep = ",", dec =".")
setDT(fs_data)
str(fs_data)
fs_data <- fs_data[,which(unlist(lapply(fs_data, function(x)!all(is.na(x))))),with=F] #suppression des colonnes vides

#variables d'interet ?

# IntraCranialVol
# CortexVol
# SubCortGrayVol
# TotalGrayVol
# CorticalWhiteMatterVol
# SupraTentorialVol

# mri_adrc_join <- merge(mri_data, adrc_data, by.x = "mri_following_adrc_session", by.y = "ADRC_ADRCCLINICALDATA.ID")
# mri_adrc_join <- merge(mri_adrc_join, fs_data, by = "Session")
#
# melt_mri_adrc_join <- melt(unique(mri_adrc_join[, .(cdr, IntraCranialVol, CortexVol, SubCortGrayVol,TotalGrayVol, CorticalWhiteMatterVol, SupraTentorialVol)]), id.vars = c("cdr"))
# ggplot(melt_mri_adrc_join, aes(fill = cdr, y= value)) + geom_boxplot() + facet_wrap(~variable, scales = "free") + ggtitle("Box plot des données 'Free Surfer' de la session IRM suite au diagnostic") +
#   scale_fill_discrete(name = "CDR") + labs(x = "Clinical Dementia Rating [0 à 3]", y = "Volume") + theme(axis.text.x = element_blank())
#
#
# melt_mri_adrc_join <- melt(unique(mri_adrc_join[, .(dementia, IntraCranialVol, CortexVol, SubCortGrayVol,TotalGrayVol, CorticalWhiteMatterVol, SupraTentorialVol)]), id.vars = c("dementia"))
# ggplot(melt_mri_adrc_join, aes(fill = dementia, y= value)) + geom_boxplot() + facet_wrap(~variable, scales = "free") + ggtitle("Box plot des données 'Free Surfer' de la session IRM suite au diagnostic") +
#   scale_fill_discrete(name = "", labels = c("Non Dément", "Dément")) + labs(x = "", y = "Volume") + theme(axis.text.x = element_blank())
#
# mri_adrc_join <- mri_adrc_join[, .(MR.ID, mri_following_adrc_session, cdr, nb_days_since_entry.x, nb_days_since_entry.y)]
# setnames(mri_adrc_join, c("nb_days_since_entry.x",  "nb_days_since_entry.y"), c("nb_days_entry_mri",  "nb_days_entry_next_adrc"))
# mri_adrc_join[, nb_days_mri_adrc := nb_days_entry_next_adrc - nb_days_entry_mri]


