#####################################################################################
# Programme 3 - Modélisation Freesurfer
#####################################################################################

#La table freesurfer cotient 200 variables calculées à partir des images d'IRM, permettant de diagnostiquer une démence

#L'objectif de ce programme est de prédire la présence de troubles relatifs à une démence à partir des variables issues des imageries
#médicales.

#La table freesurfer_data_finale contient 1954 observations correspondant à des diagnostics, plusieurs variables cibles :
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

#La table ne contient que des variables explicatives numériques continues.
#Les variables cibles sont des variables catégorielles
#On fera un modèle sur la variable dementia Dément/NOn dément
#On fera également un modèle sur la variable CDR4 : découpage de la variable CDR en classe 4 :
# - 0 : absence de troubles
# - 1 : présence de troubles incertains
# - 2 : Présence de troubles bénins
# - 3 : Présence de troubles modérés et sévères

################################################################################
# Modélisation dementia
################################################################################

freesurfer_dementia<-freesurfer_data_finale
#On retire les variables qui ne seront pas des variables explicatives
freesurfer_dementia$Subject<-NULL
freesurfer_dementia$adcr_ref<-NULL
freesurfer_dementia$cdr_ref<-NULL
freesurfer_dementia$cdr3_ref<-NULL
freesurfer_dementia$cdr4_ref<-NULL
freesurfer_dementia$dementia<-NULL
freesurfer_dementia$mmse_ref<-NULL
freesurfer_dementia$Session<-NULL

################################################################################
# Création de l'échantillon d'apprentissage et de l'échantillon de validation
################################################################################

n <- nrow(freesurfer_dementia)
#L'échantillon test représentera 20% de l'échantillon total
part_test <- 0.20

set.seed(123)
n_train <- floor(n*(1-part_test))
obs_train <- sample(1:n,n_train)

freesurfer_dementia_train <- freesurfer_dementia[obs_train,]
#On a 1563 observations dans notre échantillon d'aprentissage
freesurfer_dementia_test <- freesurfer_dementia[-obs_train,]
#On a 391 observations dans notre échantillon test

#On vérifie la proportion de déments/non déments dans notre échantillon
#(à comparer à la proportion de déments/non déments dans l'échantillon total)

summary(freesurfer_dementia$dementia)
#effectif
eff_dementia<-table(freesurfer_dementia$dementia)
prop.table(eff_dementia)
#On a 80,8% de personnes sans démence dans l'échantillon total et 19,2% de non déments

eff_dementia_test<-table(freesurfer_dementia_test$dementia)
prop.table(eff_dementia_test)
#82,8% de non déments dans la base de test et 17% de personnes présentant des troubles

eff_dementia_train<-table(freesurfer_dementia_train$dementia)
prop.table(eff_dementia_train)
#80,2% de non déments dans la base de test et 19,8% de personnes présentant des troubles

#####################################################################################
# Installation du package carret pour la modélisation
####################################################################################

library(caret)
#names(getModelInfo())

control <- trainControl(method="cv",number=10) # 10 blocs ici pour accélérer les calculs

#########################################################
# 1) Régression logistique
#########################################################

#On va utiliser la crossvalidation comme méthode de validation

reg_caret <- train(dementia_ref ~ ., data = freesurfer_dementia_train,method="glm",trControl=control)
print(reg_caret)
# Accuracy   Kappa
#0.8310697  0.4362486

summary(reg_caret)
# Coefficients: (5 not defined because of singularities)
# Estimate Std. Error z value Pr(>|z|)
# (Intercept)                            9.640e+00  1.098e+01   0.878 0.380124
# CortexVol                              2.126e-04  2.793e-04   0.761 0.446711
# SubCortGrayVol                        -1.367e-06  3.083e-05  -0.044 0.964644
# TotalGrayVol                           5.079e-05  1.777e-04   0.286 0.775047
# CorticalWhiteMatterVol                -7.766e-05  4.002e-05  -1.941 0.052283 .
# TOTAL_HIPPOCAMPUS_VOLUME               1.037e-04  4.524e-04   0.229 0.818795
# WholeBrainVolume                              NA         NA      NA       NA
# TotalVentricularVolume                -2.061e-05  4.100e-05  -0.503 0.615237
# IntraCranialVol                        2.822e-06  2.152e-06   1.311 0.189710
# lhCortexVol                           -3.229e-04  4.096e-04  -0.788 0.430526
# rhCortexVol                                   NA         NA      NA       NA
# lhCorticalWhiteMatterVol               1.303e-04  6.589e-05   1.977 0.047986 *
#   rhCorticalWhiteMatterVol                      NA         NA      NA       NA
# SupraTentorialVol                      8.186e-06  2.162e-05   0.379 0.705000
# X3rd.Ventricle_volume                  1.734e-04  3.215e-04   0.539 0.589668
# X4th.Ventricle_volume                 -6.216e-05  2.173e-04  -0.286 0.774825
# X5th.Ventricle_volume                 -3.699e-03  8.348e-03  -0.443 0.657721
# Right.Lateral.Ventricle_volume         4.188e-05  6.016e-05   0.696 0.486373
# Left.Lateral.Ventricle_volume                 NA         NA      NA       NA
# Brain.Stem_volume                      7.175e-06  1.007e-04   0.071 0.943210
# CC_Anterior_volume                    -1.631e-04  1.316e-03  -0.124 0.901411
# CC_Central_volume                      3.695e-03  3.209e-03   1.152 0.249455
# CC_Mid_Anterior_volume                -3.492e-03  2.968e-03  -1.177 0.239327
# CC_Mid_Posterior_volume                1.226e-03  2.651e-03   0.462 0.643723
# CC_Posterior_volume                   -6.872e-04  1.101e-03  -0.624 0.532699
# CSF_volume                             2.811e-04  4.964e-04   0.566 0.571158
# Left.Accumbens.area_volume             3.261e-04  1.325e-03   0.246 0.805620
# Left.Amygdala_volume                   2.665e-04  7.689e-04   0.347 0.728878
# Left.Caudate_volume                   -4.823e-04  4.628e-04  -1.042 0.297322
# Left.Cerebellum.White.Matter_volume   -3.106e-04  1.337e-04  -2.323 0.020169 *
#   Left.Cerebellum.Cortex_volume          2.152e-06  1.883e-04   0.011 0.990880
# Left.choroid.plexus_volume             3.650e-04  5.106e-04   0.715 0.474673
# Left.Hippocampus_volume               -1.470e-03  7.510e-04  -1.958 0.050253 .
# Left.Inf.Lat.Vent_volume               1.966e-04  3.498e-04   0.562 0.574017
# Left.Pallidum_volume                   7.931e-07  6.319e-04   0.001 0.998999
# Left.Putamen_volume                   -5.072e-04  3.341e-04  -1.518 0.128967
# Left.Thalamus.Proper_volume           -1.139e-04  2.457e-04  -0.463 0.643063
# Left.VentralDC_volume                 -5.258e-04  5.627e-04  -0.934 0.350129
# Left.vessel_volume                     1.726e-03  2.188e-03   0.789 0.430082
# Optic.Chiasm_volume                    6.251e-03  1.884e-03   3.318 0.000908 ***
#   Right.Accumbens.area_volume            1.727e-03  1.440e-03   1.199 0.230421
# Right.Amygdala_volume                 -2.256e-03  7.224e-04  -3.123 0.001789 **
#   Right.Caudate_volume                   3.142e-04  4.768e-04   0.659 0.509895
# Right.Cerebellum.White.Matter_volume   2.783e-04  1.203e-04   2.313 0.020718 *
#   Right.Cerebellum.Cortex_volume         1.529e-05  1.893e-04   0.081 0.935632
# Right.choroid.plexus_volume           -3.789e-04  4.277e-04  -0.886 0.375686
# Right.Hippocampus_volume                      NA         NA      NA       NA
# Right.Inf.Lat.Vent_volume              1.326e-04  3.558e-04   0.373 0.709276
# Right.Pallidum_volume                  1.755e-04  7.212e-04   0.243 0.807709
# Right.Putamen_volume                   8.978e-05  3.242e-04   0.277 0.781810
# Right.Thalamus.Proper_volume          -2.026e-04  3.411e-04  -0.594 0.552493
# Right.VentralDC_volume                -8.621e-04  6.457e-04  -1.335 0.181823
# Right.vessel_volume                   -1.550e-04  1.873e-03  -0.083 0.934031
# non.WM.hypointensities_volume         -1.239e-03  1.195e-03  -1.036 0.299977
# L.Numvert                              2.010e-04  1.178e-04   1.706 0.088063 .
# R.NumVert                             -1.728e-04  1.154e-04  -1.497 0.134329
# L.SurfArea                            -6.358e-04  2.951e-04  -2.154 0.031213 *
#   R.SurfArea                             5.736e-04  2.885e-04   1.988 0.046779 *
#   lh_bankssts_volume                     7.017e-04  4.719e-04   1.487 0.137018
# lh_bankssts_thickness                  1.437e+00  9.803e-01   1.466 0.142764
# lh_caudalanteriorcingulate_volume      7.688e-04  4.815e-04   1.597 0.110338
# lh_caudalanteriorcingulate_thickness  -4.774e-01  4.964e-01  -0.962 0.336201
# lh_caudalmiddlefrontal_volume          1.608e-04  3.024e-04   0.532 0.594931
# lh_caudalmiddlefrontal_thickness       5.876e-01  1.337e+00   0.439 0.660353
# lh_cuneus_volume                      -3.024e-04  5.385e-04  -0.562 0.574380
# lh_cuneus_thickness                    3.798e+00  1.569e+00   2.420 0.015513 *
#   lh_entorhinal_volume                   3.097e-04  4.751e-04   0.652 0.514449
# lh_entorhinal_thickness               -9.604e-01  4.488e-01  -2.140 0.032353 *
#   lh_frontalpole_volume                 -1.696e-04  1.063e-03  -0.160 0.873247
# lh_frontalpole_thickness              -1.069e+00  5.874e-01  -1.820 0.068832 .
# lh_fusiform_volume                    -2.665e-04  2.834e-04  -0.940 0.347119
# lh_fusiform_thickness                  2.084e+00  1.338e+00   1.558 0.119187
# lh_inferiorparietal_volume             4.542e-04  2.721e-04   1.669 0.095040 .
# lh_inferiorparietal_thickness         -5.190e+00  1.850e+00  -2.805 0.005034 **
#   lh_inferiortemporal_volume             2.057e-04  2.698e-04   0.763 0.445655
# lh_inferiortemporal_thickness         -5.145e-01  1.195e+00  -0.431 0.666700
# lh_insula_volume                       4.734e-04  3.215e-04   1.473 0.140863
# lh_insula_thickness                    1.890e+00  8.806e-01   2.146 0.031839 *
#   lh_isthmuscingulate_volume            -7.415e-04  4.848e-04  -1.530 0.126126
# lh_isthmuscingulate_thickness          5.301e-01  6.699e-01   0.791 0.428730
# lh_lateraloccipital_volume             6.729e-04  2.725e-04   2.470 0.013516 *
#   lh_lateraloccipital_thickness         -1.626e+00  1.682e+00  -0.967 0.333469
# lh_lateralorbitofrontal_volume        -1.761e-04  4.009e-04  -0.439 0.660453
# lh_lateralorbitofrontal_thickness     -1.509e+00  1.254e+00  -1.204 0.228594
# lh_lingual_volume                      3.844e-04  3.313e-04   1.160 0.245943
# lh_lingual_thickness                  -7.403e-01  1.753e+00  -0.422 0.672772
# lh_medialorbitofrontal_volume          3.763e-04  3.012e-04   1.249 0.211515
# lh_medialorbitofrontal_thickness      -1.869e+00  9.324e-01  -2.004 0.045067 *
#   lh_middletemporal_volume              -2.335e-04  2.907e-04  -0.803 0.421757
# lh_middletemporal_thickness           -2.033e+00  1.373e+00  -1.480 0.138771
# lh_paracentral_volume                  2.949e-04  4.340e-04   0.680 0.496787
# lh_paracentral_thickness              -1.333e+00  1.193e+00  -1.118 0.263637
# lh_parahippocampal_volume             -6.593e-04  6.245e-04  -1.056 0.291129
# lh_parahippocampal_thickness           1.101e+00  5.686e-01   1.937 0.052728 .
# lh_parsopercularis_volume              1.140e-04  3.369e-04   0.338 0.735107
# lh_parsopercularis_thickness          -1.734e-01  1.239e+00  -0.140 0.888673
# lh_parsorbitalis_volume               -1.657e-04  6.609e-04  -0.251 0.802006
# lh_parsorbitalis_thickness             6.828e-01  8.302e-01   0.823 0.410791
# lh_parstriangularis_volume             4.887e-04  4.094e-04   1.194 0.232571
# lh_parstriangularis_thickness          8.447e-01  1.168e+00   0.723 0.469700
# lh_pericalcarine_volume                7.577e-04  7.490e-04   1.012 0.311752
# lh_pericalcarine_thickness            -1.864e+00  1.474e+00  -1.265 0.205960
# lh_postcentral_volume                  4.312e-04  3.071e-04   1.404 0.160352
# lh_postcentral_thickness              -4.941e-01  2.043e+00  -0.242 0.808838
# lh_posteriorcingulate_volume          -1.634e-04  4.260e-04  -0.384 0.701282
# lh_posteriorcingulate_thickness       -7.311e-02  8.102e-01  -0.090 0.928100
# lh_precentral_volume                   3.546e-04  2.937e-04   1.207 0.227330
# lh_precentral_thickness               -3.487e+00  1.694e+00  -2.058 0.039596 *
#   lh_precuneus_volume                    1.788e-04  3.566e-04   0.502 0.616005
# lh_precuneus_thickness                -3.339e+00  1.672e+00  -1.996 0.045893 *
#   lh_rostralanteriorcingulate_volume    -3.853e-04  4.505e-04  -0.855 0.392373
# lh_rostralanteriorcingulate_thickness -2.335e-01  5.312e-01  -0.440 0.660201
# lh_rostralmiddlefrontal_volume         2.750e-04  2.713e-04   1.014 0.310737
# lh_rostralmiddlefrontal_thickness     -1.933e+00  1.643e+00  -1.176 0.239473
# lh_superiorfrontal_volume              6.915e-05  2.734e-04   0.253 0.800364
# lh_superiorfrontal_thickness           1.068e+00  1.825e+00   0.585 0.558639
# lh_superiorparietal_volume             5.257e-04  2.859e-04   1.839 0.065904 .
# lh_superiorparietal_thickness          1.258e+00  1.969e+00   0.639 0.522696
# lh_superiortemporal_volume             1.180e-04  3.034e-04   0.389 0.697309
# lh_superiortemporal_thickness         -8.210e-01  1.568e+00  -0.524 0.600597
# lh_supramarginal_volume                1.731e-04  2.731e-04   0.634 0.526184
# lh_supramarginal_thickness            -1.622e+00  1.596e+00  -1.016 0.309547
# lh_temporalpole_volume                 4.501e-04  5.035e-04   0.894 0.371271
# lh_temporalpole_thickness             -4.872e-01  5.134e-01  -0.949 0.342625
# lh_transversetemporal_volume          -7.460e-04  8.889e-04  -0.839 0.401360
# lh_transversetemporal_thickness       -4.583e-02  8.028e-01  -0.057 0.954478
# rh_bankssts_volume                    -9.800e-04  5.419e-04  -1.808 0.070546 .
# rh_bankssts_thickness                  1.017e-01  9.902e-01   0.103 0.918226
# rh_caudalanteriorcingulate_volume     -6.009e-04  4.212e-04  -1.427 0.153653
# rh_caudalanteriorcingulate_thickness   2.203e-01  5.191e-01   0.424 0.671293
# rh_caudalmiddlefrontal_volume         -5.952e-04  2.846e-04  -2.092 0.036475 *
#   rh_caudalmiddlefrontal_thickness       3.701e-02  1.242e+00   0.030 0.976236
# rh_cuneus_volume                      -1.709e-03  4.936e-04  -3.462 0.000536 ***
#   rh_cuneus_thickness                    3.583e-01  1.384e+00   0.259 0.795738
# rh_entorhinal_volume                  -7.488e-04  4.314e-04  -1.735 0.082654 .
# rh_entorhinal_thickness               -7.397e-01  3.839e-01  -1.927 0.054021 .
# rh_frontalpole_volume                 -4.991e-04  8.584e-04  -0.581 0.560980
# rh_frontalpole_thickness               2.188e-01  5.982e-01   0.366 0.714495
# rh_fusiform_volume                    -4.747e-04  2.861e-04  -1.659 0.097131 .
# rh_fusiform_thickness                  2.580e+00  1.341e+00   1.923 0.054438 .
# rh_inferiorparietal_volume            -5.001e-04  2.624e-04  -1.906 0.056690 .
# rh_inferiorparietal_thickness         -1.680e-01  1.768e+00  -0.095 0.924299
# rh_inferiortemporal_volume            -4.027e-04  2.713e-04  -1.484 0.137824
# rh_inferiortemporal_thickness         -2.386e+00  1.176e+00  -2.028 0.042512 *
#   rh_insula_volume                      -3.794e-04  3.100e-04  -1.224 0.220947
# rh_insula_thickness                   -1.360e+00  8.664e-01  -1.570 0.116501
# rh_isthmuscingulate_volume            -3.608e-04  4.968e-04  -0.726 0.467702
# rh_isthmuscingulate_thickness         -3.594e-01  7.412e-01  -0.485 0.627702
# rh_lateraloccipital_volume            -3.764e-04  2.674e-04  -1.408 0.159266
# rh_lateraloccipital_thickness          1.330e+00  1.605e+00   0.828 0.407463
# rh_lateralorbitofrontal_volume         1.576e-04  3.537e-04   0.445 0.656002
# rh_lateralorbitofrontal_thickness      1.464e+00  1.096e+00   1.336 0.181633
# rh_lingual_volume                     -1.559e-04  3.490e-04  -0.447 0.655124
# rh_lingual_thickness                  -3.458e+00  1.684e+00  -2.053 0.040053 *
#   rh_medialorbitofrontal_volume         -6.510e-04  4.129e-04  -1.577 0.114856
# rh_medialorbitofrontal_thickness       1.836e+00  1.048e+00   1.752 0.079847 .
# rh_middletemporal_volume              -2.458e-04  2.753e-04  -0.893 0.371815
# rh_middletemporal_thickness            1.711e+00  1.531e+00   1.118 0.263723
# rh_paracentral_volume                 -8.668e-04  4.099e-04  -2.115 0.034455 *
#   rh_paracentral_thickness               1.883e+00  1.364e+00   1.381 0.167304
# rh_parahippocampal_volume             -5.502e-04  6.850e-04  -0.803 0.421850
# rh_parahippocampal_thickness           4.800e-01  7.196e-01   0.667 0.504780
# rh_parsopercularis_volume             -6.176e-04  3.466e-04  -1.782 0.074738 .
# rh_parsopercularis_thickness           1.938e+00  1.094e+00   1.772 0.076430 .
# rh_parsorbitalis_volume               -9.189e-04  5.559e-04  -1.653 0.098289 .
# rh_parsorbitalis_thickness            -2.485e-01  8.562e-01  -0.290 0.771598
# rh_parstriangularis_volume            -2.770e-04  3.340e-04  -0.830 0.406795
# rh_parstriangularis_thickness          3.016e-01  1.184e+00   0.255 0.798963
# rh_pericalcarine_volume               -1.969e-04  6.638e-04  -0.297 0.766788
# rh_pericalcarine_thickness             1.513e+00  1.502e+00   1.008 0.313586
# rh_postcentral_volume                  6.852e-05  3.018e-04   0.227 0.820419
# rh_postcentral_thickness              -4.785e+00  1.901e+00  -2.517 0.011850 *
#   rh_posteriorcingulate_volume           1.160e-04  4.520e-04   0.257 0.797520
# rh_posteriorcingulate_thickness       -7.154e-01  9.861e-01  -0.726 0.468143
# rh_precentral_volume                  -3.920e-04  2.839e-04  -1.381 0.167340
# rh_precentral_thickness                1.596e+00  1.541e+00   1.036 0.300307
# rh_precuneus_volume                   -1.677e-04  3.181e-04  -0.527 0.597976
# rh_precuneus_thickness                 3.230e+00  1.656e+00   1.951 0.051073 .
# rh_rostralanteriorcingulate_volume     2.297e-04  4.562e-04   0.504 0.614557
# rh_rostralanteriorcingulate_thickness -2.236e-01  5.003e-01  -0.447 0.654861
# rh_rostralmiddlefrontal_volume        -6.730e-04  2.709e-04  -2.484 0.012988 *
#   rh_rostralmiddlefrontal_thickness      6.920e-01  1.700e+00   0.407 0.683874
# rh_superiorfrontal_volume             -6.951e-05  2.545e-04  -0.273 0.784761
# rh_superiorfrontal_thickness          -6.650e-01  1.899e+00  -0.350 0.726152
# rh_superiorparietal_volume            -7.934e-04  2.833e-04  -2.801 0.005097 **
#   rh_superiorparietal_thickness          3.125e+00  2.025e+00   1.543 0.122746
# rh_superiortemporal_volume            -7.482e-05  3.068e-04  -0.244 0.807339
# rh_superiortemporal_thickness         -9.586e-01  1.694e+00  -0.566 0.571414
# rh_supramarginal_volume               -7.111e-04  2.780e-04  -2.557 0.010544 *
#   rh_supramarginal_thickness             1.502e+00  1.612e+00   0.932 0.351549
# rh_temporalpole_volume                -9.039e-04  4.561e-04  -1.982 0.047511 *
#   rh_temporalpole_thickness              1.070e+00  4.279e-01   2.500 0.012424 *
#   rh_transversetemporal_volume          -4.206e-04  1.047e-03  -0.402 0.687936
# rh_transversetemporal_thickness        5.314e-01  7.247e-01   0.733 0.463415
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for binomial family taken to be 1)
#
# Null deviance: 1554.22  on 1562  degrees of freedom
# Residual deviance:  818.19  on 1374  degrees of freedom
# AIC: 1196.2
#
# Number of Fisher Scoring iterations: 7

#Prédiction sur l'échantillon test
freesurfer_dementia_test$dementia_prev_reg <- predict(reg_caret,newdata=freesurfer_dementia_test)
#distribution des classes prédites
print(table(freesurfer_dementia_test$dementia_prev_reg))
#  0   1
#330  61

#matrice de confusion
mat <- confusionMatrix(data=freesurfer_dementia_test$dementia_prev_reg,reference=freesurfer_dementia_test$dementia_ref,positive="1")
print(mat)
# Reference
# Prediction   0   1
# 0 303  27
# 1  21  40
#
# Accuracy : 0.8772
# 95% CI : (0.8405, 0.9081)
# No Information Rate : 0.8286
# P-Value [Acc > NIR] : 0.005068
#
# Kappa : 0.5518
#
# Mcnemar's Test P-Value : 0.470486
#
#             Sensitivity : 0.5970
#             Specificity : 0.9352
#          Pos Pred Value : 0.6557
#          Neg Pred Value : 0.9182
#              Prevalence : 0.1714
#          Detection Rate : 0.1023
#    Detection Prevalence : 0.1560
#       Balanced Accuracy : 0.7661
#
#        'Positive' Class : 1

#accès aux indicateurs globaux
print(mat$overall)
# Accuracy          Kappa  AccuracyLower  AccuracyUpper   AccuracyNull AccuracyPValue  McnemarPValue
#0.877237852    0.551798252    0.840547094    0.908081385    0.828644501    0.005067538    0.470486422

#Par classe
print(mat$byClass)
#            Sensitivity          Specificity       Pos Pred Value       Neg Pred Value            Precision               Recall
# 0.5970149            0.9351852            0.6557377            0.9181818            0.6557377            0.5970149
# F1           Prevalence       Detection Rate Detection Prevalence    Balanced Accuracy
# 0.6250000            0.1713555            0.1023018            0.1560102            0.7661001

#Score des individus positifs
score <- predict(reg_caret,freesurfer_dementia_test,type="prob")[,"1"]
print(quantile(score))
#         0%          25%          50%          75%         100%
#0.0001062036 0.0095403432 0.0512379947 0.2291558175 0.9991936732

#Courbe Lift

#tableau de données pour le scoring
liftdata <- data.frame(classe=freesurfer_dementia_test$dementia_ref)
liftdata$score <- score

#objet lift
lift_obj <- lift(classe ~ score, data=liftdata, class="1")
print(lift_obj)

#affichage de la courbe lift
plot(lift_obj)

#Courbe Roc

library(pROC)

roc_obj <- roc(freesurfer_dementia_test$dementia_ref=="1",score)
#plot de l'objet roc
plot(1-roc_obj$specificities,roc_obj$sensitivities,type="l")
abline(0,1)


#Importance des variables : affichage des 20 variables les plus influentes

print(varImp(reg_caret))
#  Overall
# rh_cuneus_volume                      100.00
# Optic.Chiasm_volume                    95.83
# Right.Amygdala_volume                  90.21
# lh_inferiorparietal_thickness          81.01
# rh_superiorparietal_volume             80.89
# rh_supramarginal_volume                73.86
# rh_postcentral_thickness               72.68
# rh_temporalpole_thickness              72.20
# rh_rostralmiddlefrontal_volume         71.74
# lh_lateraloccipital_volume             71.33
# lh_cuneus_thickness                    69.89
# Left.Cerebellum.White.Matter_volume    67.09
# Right.Cerebellum.White.Matter_volume   66.80
# L.SurfArea                             62.21
# lh_insula_thickness                    61.98
# lh_entorhinal_thickness                61.80
# rh_paracentral_volume                  61.07
# rh_caudalmiddlefrontal_volume          60.40
# lh_precentral_thickness                59.43
# rh_lingual_thickness                   59.29

###############################################################################################
#Randomforest
###############################################################################################

#On veut prédire à l'aide de l'échantillon d'apprentissage
#Puis prédire sur l'échantillon de test
#On charge le package et la library RandomForest


#library(doParallel)



rf_caret <- train(dementia_ref~.,
                  data=freesurfer_dementia_train,
                  method="rf",
                  trControl=control,
                  ntree=400)

varImp(rf_caret)
# Overall
# Right.Inf.Lat.Vent_volume     100.000
# Left.Inf.Lat.Vent_volume       70.294
# Right.Hippocampus_volume       67.515
# Left.Hippocampus_volume        66.972
# TOTAL_HIPPOCAMPUS_VOLUME       63.550
# lh_middletemporal_thickness    18.886
# Left.Amygdala_volume           18.885
# Right.Amygdala_volume          16.535
# rh_inferiortemporal_thickness  12.290
# rh_entorhinal_volume           12.021
# rh_entorhinal_thickness        11.266
# lh_precuneus_thickness         10.400
# lh_entorhinal_volume           10.395
# lh_entorhinal_thickness        10.356
# Optic.Chiasm_volume             9.238
# rh_inferiorparietal_thickness   8.983
# Left.Accumbens.area_volume      8.965
# X3rd.Ventricle_volume           8.671
# lh_parsopercularis_volume       8.472
# CSF_volume                      8.266

freesurfer_dementia_test$dementia_prev_forest <- predict(rf_caret, newdata=freesurfer_dementia_test)

err_foret<-sum(freesurfer_dementia_test$dementia_prev_forest!=freesurfer_dementia_test$dementia_ref)/nrow(freesurfer_dementia_test)
err_foret
# 0.09974425

mat_forest <- confusionMatrix(data=freesurfer_dementia_test$dementia_prev_forest,reference=freesurfer_dementia_test$dementia_ref,positive="1")
print(mat_forest)
# Confusion Matrix and Statistics
#
# Reference
# Prediction   0   1
# 0 314  29
# 1  10  38
#
# Accuracy : 0.9003
# 95% CI : (0.8662, 0.9281)
# No Information Rate : 0.8286
# P-Value [Acc > NIR] : 4.357e-05
#
# Kappa : 0.6043
#
# Mcnemar's Test P-Value : 0.003948
#
#             Sensitivity : 0.56716
#             Specificity : 0.96914
#          Pos Pred Value : 0.79167
#          Neg Pred Value : 0.91545
#              Prevalence : 0.17136
#          Detection Rate : 0.09719
#    Detection Prevalence : 0.12276
#       Balanced Accuracy : 0.76815
#
#        'Positive' Class : 1


###############################################################################
# Boosting
###############################################################################

###########################
#  Adaboost
###########################

gbm_ada_caret <- train(dementia_ref~.,
                      data=freesurfer_dementia_train,
                      method="gbm",
                      distribution="adaboost",
                      trControl=control,
                      verbose=FALSE)

freesurfer_dementia_test$dementia_prev_gbm_ada_caret <- predict(gbm_ada_caret,newdata=freesurfer_dementia_test)

mat_ada_caret <- confusionMatrix(data=freesurfer_dementia_test$dementia_prev_gbm_ada_caret,reference=freesurfer_dementia_test$dementia_ref,positive="1")
print(mat_ada_caret)

#  Reference
# Prediction   0   1
# 0 313  29
# 1  11  38
#
# Accuracy : 0.8977
# 95% CI : (0.8633, 0.9259)
# No Information Rate : 0.8286
# P-Value [Acc > NIR] : 8.172e-05
#
# Kappa : 0.5968
#
# Mcnemar's Test P-Value : 0.00719
#
#             Sensitivity : 0.56716
#             Specificity : 0.96605
#          Pos Pred Value : 0.77551
#          Neg Pred Value : 0.91520
#              Prevalence : 0.17136
#          Detection Rate : 0.09719
#    Detection Prevalence : 0.12532
#       Balanced Accuracy : 0.76661
#
#        'Positive' Class : 1

###########################
# Logitboost
###########################

gbm_logit_caret <- train(dementia_ref~.,
                       data=freesurfer_dementia_train,
                       method="gbm",
                       distribution="bernoulli",
                       trControl=control,
                       verbose=FALSE)

freesurfer_dementia_test$dementia_prev_gbm_logit_caret <- predict(gbm_logit_caret,newdata=freesurfer_dementia_test)

mat_logit_caret <- confusionMatrix(data=freesurfer_dementia_test$dementia_prev_gbm_logit_caret,reference=freesurfer_dementia_test$dementia_ref,positive="1")
print(mat_logit_caret)

#Confusion Matrix and Statistics

# Reference
# Prediction   0   1
# 0 310  25
# 1  14  42
#
# Accuracy : 0.9003
# 95% CI : (0.8662, 0.9281)
# No Information Rate : 0.8286
# P-Value [Acc > NIR] : 4.357e-05
#
# Kappa : 0.6243
#
# Mcnemar's Test P-Value : 0.1093
#
#             Sensitivity : 0.6269
#             Specificity : 0.9568
#          Pos Pred Value : 0.7500
#          Neg Pred Value : 0.9254
#              Prevalence : 0.1714
#          Detection Rate : 0.1074
#    Detection Prevalence : 0.1432
#       Balanced Accuracy : 0.7918
#
#        'Positive' Class : 1


###############################################################################
# SVM avec Caret
###############################################################################

#modélisation avec paramètre de la technique d'apprentissage
#SVM avec noyau linéaire, C = 0.1
SVM_caret <- train(dementia_ref ~ ., data = freesurfer_dementia_train,method="svmLinear",trControl=control,tuneGrid=data.frame(C=0.1))
print(SVM_caret)
# Accuracy   Kappa
#0.8534664  0.4803252

freesurfer_dementia_test$dementia_prev_SVM_caret <- predict(SVM_caret,newdata=freesurfer_dementia_test)

#evaluation en test - svm linéaire
mat_SVM_caret <- confusionMatrix(data=freesurfer_dementia_test$dementia_prev_SVM_caret,reference=freesurfer_dementia_test$dementia_ref,positive="1")
print(mat_SVM_caret )
#Confusion Matrix and Statistics

# Reference
# Prediction   0   1
# 0 309  28
# 1  15  39
#
# Accuracy : 0.89
# 95% CI : (0.8547, 0.9193)
# No Information Rate : 0.8286
# P-Value [Acc > NIR] : 0.000461
#
# Kappa : 0.5805
#
# Mcnemar's Test P-Value : 0.067253
#
#             Sensitivity : 0.58209
#             Specificity : 0.95370
#          Pos Pred Value : 0.72222
#          Neg Pred Value : 0.91691
#              Prevalence : 0.17136
#          Detection Rate : 0.09974
#    Detection Prevalence : 0.13811
#       Balanced Accuracy : 0.76790
#
#        'Positive' Class : 1


# Mais rien ne nous assure que C = 0.1 est la bonne valeur du paramètre de coût. “caret” nous
# donne la possibilité d’évaluer différents scénarios de coût avec l’option tuneGrid.
#svm avec différentes valeurs de C
SVM_caret_eval <- train(dementia_ref ~ ., data =freesurfer_dementia_train,method="svmLinear",trControl=control,tuneGrid=data.frame(C=c(0.05,0.1,0.5,1,10)))
print(SVM_caret_eval)
#Support Vector Machines with Linear Kernel

# 1563 samples
# 193 predictor
# 2 classes: '0', '1'
#
# No pre-processing
# Resampling: Cross-Validated (10 fold)
# Summary of sample sizes: 1406, 1406, 1407, 1408, 1407, 1407, ...
# Resampling results across tuning parameters:
#
#   C      Accuracy   Kappa
# 0.05  0.8560173  0.4775921
# 0.10  0.8432130  0.4399383
# 0.50  0.8374560  0.4401814
# 1.00  0.8316867  0.4269543
# 10.00  0.8271953  0.4130068
#
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was C = 0.05.

# La procédure nous dit que la meilleure valeur pour C est C = 0.05, avec un taux de succès en
# validation croisée de 85%.

freesurfer_dementia_test$dementia_prev_SVM_caret_eval <- predict(SVM_caret_eval,newdata=freesurfer_dementia_test)

#evaluation en test - svm linéaire avec C optimisé
mat_SVM_caret_eval <- confusionMatrix(data=freesurfer_dementia_test$dementia_prev_SVM_caret_eval,reference=freesurfer_dementia_test$dementia_ref,positive="1")
print

#Confusion Matrix and Statistics

# Reference
# Prediction   0   1
# 0 310  29
# 1  14  38
#
# Accuracy : 0.89
# 95% CI : (0.8547, 0.9193)
# No Information Rate : 0.8286
# P-Value [Acc > NIR] : 0.000461
#
# Kappa : 0.575
#
# Mcnemar's Test P-Value : 0.032763
#
#             Sensitivity : 0.56716
#             Specificity : 0.95679
#          Pos Pred Value : 0.73077
#          Neg Pred Value : 0.91445
#              Prevalence : 0.17136
#          Detection Rate : 0.09719
#    Detection Prevalence : 0.13299
#       Balanced Accuracy : 0.76198
#
#        'Positive' Class : 1


###########################################################################################
#On estime les performances des algorithmes
###########################################################################################

#Problème ici !!!

#On souhaite comparer les performances des algorithmes en estimant leur taux de mal classés et leur courbe Roc sur les données
# de validation. On collecte les probabilités estimées d'être dément par les différents algorithmes pour les individus
# de l'échantillon dans un même dataframe

freesurfer_dementia_test$type<-as.numeric(freesurfer_dementia_test$dementia_ref)

prev.forest<-freesurfer_dementia_test$dementia_prev_forest
prev.ada1<-freesurfer_dementia_test$dementia_prev_gbm_ada_caret
prev.ada2<-freesurfer_dementia_test$dementia_prev_gbm_logit_caret
prev.logit1<-freesurfer_dementia_test$dementia_prev_reg

prev.SVM1<-freesurfer_dementia_test$dementia_prev_SVM_caret
prev.SVM2<-freesurfer_dementia_test$dementia_prev_SVM_caret_eval

prev.prob<-data.frame(forest=prev.forest,ada1=prev.ada1,ada2=prev.ada2,
                      logit1=prev.logit1,svm1=prev.SVM1,svm2=prev.SVM2,
                      obs=freesurfer_dementia_test$type)
head(round(prev.prob,3))

#On peut en déduire une estimation de la classe en confrontant ces probabilités au seuil 0.5
prev.class<-round(prev.prob)
head(prev.class)

#On obtient l'erreur de classification estimée pour les algorithmes en confrontant les valeurs prédites aux valeurs observées
library(tidyverse)
prev.class %>%summarise_all(funs(err=mean(obs!=.)))%>%select(-obs_err) %>% round(3)


#La courbe ROC peut s'obtenir avec le package pROC
library(plotROC)
df.roc<-prev.prob%>%gather(key=Methode,value=score,forest,ada1,ada2,logit1,svm1,svm2)
ggplot(df.roc)+aes(d=obs,m=score,color=Methode)+geom_roc()+theme_classic()


#On calcule les AUC

library(pROC)
df.roc%>%group_by(Methode)%>%summarize(AUC=pROC::auc(obs,score))











###################################################################################################################
# Etape 2 :  on prédit la variable CR4
###################################################################################################################

freesurfer_cdr4<-freesurfer_data_finale
#On retire les variables qui ne seront pas des variables explicatives
freesurfer_cdr4$Subject<-NULL
freesurfer_cdr4$adcr_ref<-NULL
freesurfer_cdr4$cdr_ref<-NULL
freesurfer_cdr4$cdr3_ref<-NULL
freesurfer_cdr4$dementia_ref<-NULL
freesurfer_cdr4$mmse_ref<-NULL
freesurfer_cdr4$Session<-NULL

################################################################################
# Création de l'échantillon d'apprentissage et de l'échantillon de validation
################################################################################

n <- nrow(freesurfer_cdr4)
#L'échantillon test représentera 20% de l'échantillon total
part_test <- 0.20

set.seed(123)
n_train <- floor(n*(1-part_test))
obs_train <- sample(1:n,n_train)

freesurfer_cdr4_train <- freesurfer_cdr4[obs_train,]
#On a 1563 observations dans notre échantillon d'aprentissage
freesurfer_cdr4_test <- freesurfer_cdr4[-obs_train,]
#On a 391 observations dans notre échantillon test


###############################################################################################
#Randomforest
###############################################################################################

#On veut prédire à l'aide de l'échantillon d'apprentissage
#Puis prédire sur l'échantillon de test
#On charge le package et la library RandomForest


#library(doParallel)

rf_caret_cdr4 <- train(cdr4_ref~.,
                  data=freesurfer_cdr4_train,
                  method="rf",
                  trControl=control,
                  ntree=400)

varImp(rf_caret_cdr4)
#Overall
# TOTAL_HIPPOCAMPUS_VOLUME      100.000
# Right.Inf.Lat.Vent_volume      95.587
# Right.Hippocampus_volume       44.316
# Left.Hippocampus_volume        42.824
# Left.Amygdala_volume           15.875
# rh_inferiorparietal_thickness  15.721
# Left.Inf.Lat.Vent_volume       15.360
# lh_middletemporal_thickness    14.476
# lh_entorhinal_volume           11.850
# lh_entorhinal_thickness        11.767
# rh_entorhinal_volume           10.441
# lh_frontalpole_thickness       10.033
# Optic.Chiasm_volume             9.392
# lh_isthmuscingulate_thickness   9.338
# Left.Accumbens.area_volume      9.106
# Right.Amygdala_volume           9.071
# lh_inferiortemporal_thickness   8.866
# X3rd.Ventricle_volume           8.603
# lh_fusiform_thickness           8.428
# lh_middletemporal_volume        8.012


freesurfer_cdr4_test$cdr4_prev_forest <- predict(rf_caret_cdr4, newdata=freesurfer_cdr4_test)
print(rf_caret_cdr4)
#Random Forest

# 1563 samples
# 193 predictor
# 4 classes: '0', '1', '2', '3'
#
# No pre-processing
# Resampling: Cross-Validated (10 fold)
# Summary of sample sizes: 1408, 1406, 1407, 1407, 1405, 1406, ...
# Resampling results across tuning parameters:
#
#   mtry  Accuracy   Kappa
# 2   0.8157583  0.1964780
# 97   0.8266524  0.3517938
# 193   0.8292206  0.3668676
#
# Accuracy was used to select the optimal model using the largest value.
# The final value used for the model was mtry = 193.


###############################################################################
# SVM avec Caret
###############################################################################

#modélisation avec paramètre de la technique d'apprentissage
#SVM avec noyau linéaire, C = 0.1
SVM_caret_cdr4 <- train(cdr4_ref ~ ., data = freesurfer_cdr4_train,method="svmLinear",trControl=control,tuneGrid=data.frame(C=0.1))
print(SVM_caret_cdr4)
#Accuracy   Kappa
#0.8196214  0.3753138

freesurfer_cdr4_test$cdr4_prev_SVM_caret <- predict(SVM_caret_cdr4,newdata=freesurfer_cdr4_test)


# Mais rien ne nous assure que C = 0.1 est la bonne valeur du paramètre de coût. “caret” nous
# donne la possibilité d’évaluer différents scénarios de coût avec l’option tuneGrid.
#svm avec différentes valeurs de C
SVM_caret_eval_cdr4 <- train(cdr4_ref ~ ., data =freesurfer_cdr4_train,method="svmLinear",trControl=control,tuneGrid=data.frame(C=c(0.05,0.1,0.5,1,10)))
print(SVM_caret_eval_cdr4)
#On retient c=0.05
#C      Accuracy   Kappa
#0.05  0.8253386  0.3836991

freesurfer_cdr4_test$cdr4_prev_SVM_caret_eval <- predict(SVM_caret_eval_cdr4,newdata=freesurfer_cdr4_test)
