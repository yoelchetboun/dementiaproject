#install.packages("BiocManager")
#install.packages("EBImage") #sudo apt-get install libfftw3-dev pour installer la library fftw necessaire
#BiocManager::install("EBImage")

library(keras)
library(EBImage)
library(stringr)
library(pbapply)

#select 4 png pour chaque MRI sessions / anat
path_root <- "~/GENERIC/dementiaproject/"
path_data <- "/srv/OASIS_DATA/"

#test dashboard
list_dropout_dir <- list.dirs(path = file.path(path_data, "dropout_tuning"), recursive = FALSE)
view_run(list_dropout_dir[6])
compare_runs(runs = c(list_dropout_dir[5], list_dropout_dir[6]))

list_result <- list.files(path = file.path(path_data, "results"), pattern = "summary_result.Rdata",  recursive = TRUE, full.names = TRUE)
rbindlist(map(list_result, ~loadRData(.)))

prev_res_6 <- loadRData(file.path(path_data, "results", "results_6", "resultat_test.Rdata"))
conf_mat_6 <- loadRData(file.path(path_data, "results", "results_6", "conf_matrix.Rdata"))
sum <- loadRData(file.path(path_data, "results", "results_8", "summary_result.Rdata"))
sum <- loadRData(file.path(path_data, "results", "results_9", "summary_result.Rdata"))

sum_7 <- loadRData(file.path(path_data, "results", "results_7", "summary_result.Rdata"))
