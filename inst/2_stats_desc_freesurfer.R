#####################################################################################
# Programme 2 - Statistiques descripives Freesurfer
#####################################################################################

#La table freesurfer cotient 200 variables calculées à partir des images d'IRM, permettant de diagnostiquer une démence

# L'objectif de ce programme est de préparer des statistiques descriptives sur les variables jugées les plus pertinentes
# médicalement pour préjuger de l'apparition d'une démence :

#Volume du cerveau : WholeBrainVolume
#Volume du cortex : CortexVol
#Volume de la matière blanche : CorticalWhiteMatterVol
#Volume de la matière grise : SubCortGrayVol
#Volume total des ventricules : TotalVentricularVolume
#Volume total de l'hyppocampe : TOTAL_HYPPOCAMPUS_VOLUME
#Centiloïde PIB, Centiloïde AV45 (pour mesurer la présence d'amyloïde et donc de plaques séniles) : optionnel non dispo

#Pour chaque variable, on regardera le lien avec :
# - la variable dementia : dement/non dément
# - la variable CDR3 : en 3 classes
# - la variable CDR4 : en 4 classe

##################################################################################################################
# Import de la table freesurfer_finale, préparée dans le programme 1
##################################################################################################################

load(file.path(path_root, "//inst/extdata/oasis3/bases_R/freesurfer_data_finale.Rdata"))


library(ggplot2)
library(data.table)

freesurfer<-freesurfer_data_finale
freesurfer$dementia_label<-factor(freesurfer$dementia_ref, labels=c("Absence de trouble","Présence de troubles"))
freesurfer$CDR3_label<-factor(freesurfer$cdr3_ref, labels=c("Absence de trouble", "Troubles incertains ou bénins", "Troubles modérés ou sévères"))
freesurfer$CDR4_label<-factor(freesurfer$cdr4_ref, labels=c("Absence de trouble", "Troubles incertains", "Troubles bénins" , "Troubles modérés ou sévères"))

###################################
#Le volume du cerveau (normalisé)

ggplot(freesurfer[, .(dementia_label, WholeBrainVolume)], aes(fill = dementia_label, y= WholeBrainVolume)) + geom_boxplot() + ggtitle("Volume cérébral en fonction de la détection d'une démence")+
scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Volume cérébral")+ theme_minimal() + theme(axis.text.x = element_blank())


ggplot(freesurfer[, .(CDR3_label, WholeBrainVolume)], aes(fill = CDR3_label, y= WholeBrainVolume)) + geom_boxplot() + ggtitle("Volume cérébral en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Volume cérébral")+ theme_minimal() + theme(axis.text.x = element_blank())


ggplot(freesurfer[, .(CDR4_label, WholeBrainVolume)], aes(fill = CDR4_label, y= WholeBrainVolume)) + geom_boxplot() + ggtitle("Volume cérébral en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Volume cérébral")+ theme_minimal() + theme(axis.text.x = element_blank())

#Sans valeurs extrêmes

ggplot(freesurfer[WholeBrainVolume<1250000, .(dementia_label, WholeBrainVolume)], aes(fill = dementia_label, y= WholeBrainVolume)) + geom_boxplot() + ggtitle("Distribution du volume cérébral en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Volume cérébral")+ theme_minimal() + theme(axis.text.x = element_blank())


ggplot(freesurfer[WholeBrainVolume<1250000, .(CDR3_label, WholeBrainVolume)], aes(fill = CDR3_label, y= WholeBrainVolume)) + geom_boxplot() + ggtitle("Distribution du volume cérébral en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Volume cérébral")+ theme(axis.text.x = element_blank()) + scale_fill_brewer(palette="Blues")+ theme(legend.position="right")


ggplot(freesurfer[WholeBrainVolume<1250000, .(CDR4_label, WholeBrainVolume)], aes(fill = CDR4_label, y= WholeBrainVolume)) + geom_boxplot() + ggtitle("Distribution du volume cérébral en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Volume cérébral")+ theme_minimal() + theme(axis.text.x = element_blank())


###################################
#Le volume du cortex : CortexVol

ggplot(freesurfer[, .(dementia_label, CortexVol)], aes(fill = dementia_label, y= CortexVol)) + geom_boxplot() + ggtitle("Volume du cortex en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Volume du cortex")+ theme_minimal() + theme(axis.text.x = element_blank())


ggplot(freesurfer[, .(CDR3_label, CortexVol)], aes(fill = CDR3_label, y= CortexVol)) + geom_boxplot() + ggtitle("Volume du cortex en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Volume du cortex")+ scale_fill_brewer(palette="Blues") + theme(axis.text.x = element_blank())


ggplot(freesurfer[, .(CDR4_label, CortexVol)], aes(fill = CDR4_label, y= CortexVol)) + geom_boxplot() + ggtitle("Volume du cortex en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Volume du cortex")+ theme_minimal() + theme(axis.text.x = element_blank())


#########################################################
#Volume de la matière blanche : CorticalWhiteMatterVol

ggplot(freesurfer[, .(dementia_label, CorticalWhiteMatterVol)], aes(fill = dementia_label, y=CorticalWhiteMatterVol)) + geom_boxplot() + ggtitle("Substance blanche en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Quantité de substance blanche")+ theme_minimal() + theme(axis.text.x = element_blank())

ggplot(freesurfer[, .(CDR3_label, CorticalWhiteMatterVol)], aes(fill = CDR3_label, y= CorticalWhiteMatterVol)) + geom_boxplot() + ggtitle("Substance blanche en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Quantité de substance blanche")+ theme_minimal() + theme(axis.text.x = element_blank())


ggplot(freesurfer[, .(CDR4_label, CorticalWhiteMatterVol)], aes(fill = CDR4_label, y= CorticalWhiteMatterVol)) + geom_boxplot() + ggtitle("Substance blanche en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Quantité de substance blanche")+ theme_minimal() + theme(axis.text.x = element_blank())

#On retire la valeur extrême au-delà de 800000

ggplot(freesurfer[CorticalWhiteMatterVol<800000, .(dementia_label, CorticalWhiteMatterVol)], aes(fill = dementia_label, y=CorticalWhiteMatterVol)) + geom_boxplot() + ggtitle("Substance blanche en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Quantité de substance blanche")+ theme_minimal() + theme(axis.text.x = element_blank())


ggplot(freesurfer[CorticalWhiteMatterVol<800000, .(CDR3_label, CorticalWhiteMatterVol)], aes(fill = CDR3_label, y= CorticalWhiteMatterVol)) + geom_boxplot() + ggtitle("Substance blanche en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Quantité de substance blanche")+ scale_fill_brewer(palette="Blues")+ theme(legend.position="right") + theme(axis.text.x = element_blank())


ggplot(freesurfer[CorticalWhiteMatterVol<800000, .(CDR4_label, CorticalWhiteMatterVol)], aes(fill = CDR4_label, y= CorticalWhiteMatterVol)) + geom_boxplot() + ggtitle("Substance blanche en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Quantité de substance blanche")+ theme_minimal() + theme(axis.text.x = element_blank())


#########################################################
#Volume de la matière grise : SubCortGrayVol

ggplot(freesurfer[, .(dementia_label, SubCortGrayVol)], aes(fill = dementia_label, y=SubCortGrayVol)) + geom_boxplot() + ggtitle("Substance grise en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Quantité de substance grise")+ theme_minimal() + theme(axis.text.x = element_blank())

ggplot(freesurfer[, .(CDR3_label, SubCortGrayVol)], aes(fill = CDR3_label, y= SubCortGrayVol)) + geom_boxplot() + ggtitle("Substance grise en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Quantité de substance grise")+ theme_minimal() + theme(axis.text.x = element_blank())


ggplot(freesurfer[, .(CDR4_label, SubCortGrayVol)], aes(fill = CDR4_label, y= SubCortGrayVol)) + geom_boxplot() + ggtitle("Substance grise en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Quantité de substance grise")+ theme_minimal() + theme(axis.text.x = element_blank())

#Sans les valeurs extrêmes

ggplot(freesurfer[SubCortGrayVol<65000, .(dementia_label, SubCortGrayVol)], aes(fill = dementia_label, y=SubCortGrayVol)) + geom_boxplot() + ggtitle("Substance grise en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Quantité de substance grise")+ theme_minimal() + theme(axis.text.x = element_blank())

ggplot(freesurfer[SubCortGrayVol<65000, .(CDR3_label, SubCortGrayVol)], aes(fill = CDR3_label, y= SubCortGrayVol)) + geom_boxplot() + ggtitle("Substance grise en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Quantité de substance grise")+ scale_fill_brewer(palette="Blues")+ theme(axis.text.x = element_blank())


ggplot(freesurfer[SubCortGrayVol<65000, .(CDR4_label, SubCortGrayVol)], aes(fill = CDR4_label, y= SubCortGrayVol)) + geom_boxplot() + ggtitle("Substance grise en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Quantité de substance grise")+ theme_minimal() + theme(axis.text.x = element_blank())


#########################################################
#Volume total des ventricules : TotalVentricularVolume

ggplot(freesurfer[, .(dementia_label, TotalVentricularVolume)], aes(fill = dementia_label, y=TotalVentricularVolume)) + geom_boxplot() + ggtitle("Volume total des ventricules en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Volume total des ventricules")+ theme_minimal() + theme(axis.text.x = element_blank())

ggplot(freesurfer[, .(CDR3_label, TotalVentricularVolume)], aes(fill = CDR3_label, y=TotalVentricularVolume)) + geom_boxplot() + ggtitle("Volume total des ventricules en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Volume total des ventricules")+ theme_minimal() + theme(axis.text.x = element_blank())


ggplot(freesurfer[, .(CDR4_label,  TotalVentricularVolume)], aes(fill = CDR4_label, y=  TotalVentricularVolume)) + geom_boxplot() + ggtitle("Volume total des ventricules en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Volume total des ventricules")+ theme_minimal() + theme(axis.text.x = element_blank())

#Sans les valeurs extrêmes

ggplot(freesurfer[TotalVentricularVolume<80000, .(dementia_label, TotalVentricularVolume)], aes(fill = dementia_label, y=TotalVentricularVolume)) + geom_boxplot() + ggtitle("Volume total des ventricules en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Volume total des ventricules")+ theme_minimal() + theme(axis.text.x = element_blank())

ggplot(freesurfer[TotalVentricularVolume<80000, .(CDR3_label, TotalVentricularVolume)], aes(fill = CDR3_label, y=TotalVentricularVolume)) + geom_boxplot() + ggtitle("Volume total des ventricules en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Volume total des ventricules")+ scale_fill_brewer(palette="Blues")+ theme(axis.text.x = element_blank())

ggplot(freesurfer[TotalVentricularVolume<80000, .(CDR4_label,  TotalVentricularVolume)], aes(fill = CDR4_label, y=TotalVentricularVolume)) + geom_boxplot() + ggtitle("Volume total des ventricules en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Volume total des ventricules")+ theme_minimal() + theme(axis.text.x = element_blank())


#########################################################
#Volume total de l'hyppocampe : TOTAL_HYPPOCAMPUS_VOLUME

ggplot(freesurfer[, .(dementia_label,TOTAL_HIPPOCAMPUS_VOLUME)], aes(fill = dementia_label, y=TOTAL_HIPPOCAMPUS_VOLUME)) + geom_boxplot() + ggtitle("Volume total de l'hyppocampe en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Volume total de l'hyppocampe")+ theme_minimal() + theme(axis.text.x = element_blank())

ggplot(freesurfer[, .(CDR3_label,TOTAL_HIPPOCAMPUS_VOLUME)], aes(fill = CDR3_label, y=TOTAL_HIPPOCAMPUS_VOLUME)) + geom_boxplot() + ggtitle("Volume total de l'hyppocampe en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Volume total de l'hyppocampe")+ scale_fill_brewer(palette="Blues") + theme(axis.text.x = element_blank())

ggplot(freesurfer[, .(CDR4_label,TOTAL_HIPPOCAMPUS_VOLUME)], aes(fill = CDR4_label, y=TOTAL_HIPPOCAMPUS_VOLUME)) + geom_boxplot() + ggtitle("Volume total de l'hyppocampe en fonction de la détection d'une démence")+
  scale_fill_discrete(name = "Démence") + labs(x = "Démence", y = "Volume total de l'hyppocampe")+ theme_minimal() + theme(axis.text.x = element_blank())


#############################################################
# Statistiques descriptives générales
##############################################################

library(DataExplorer)
create_report(freesurfer_data_finale)
