library(tfruns)

# runs <- tuning_run("/home/chetbo/GENERIC/dementiaproject/inst/main_cnn_4_class.R", flags = list(
#   preprocess = FALSE,
#   raw_dir = "data_raw_t1",
#   width_target = c(224),
#   height_target = c(224),
#   loss_function = c("sparse_categorical_crossentropy"), #sparse_categorical_crossentropy binary_crossentropy
#   optimizer_var = c("adam"),
#   learning_rate = c(0.00002), # 0.00002, 0.00001
#   metrics_var = c("sparse_categorical_accuracy"), #categorical_accuracy-AUC
#   cut_list = c("136_138_140_142_144"),
#   nb_epoch  =  c(50),
#   batch_size_var =  c(40),
#   val_split =  c(0.2),
#   test_split =  c(0.2),
#   class_number =  4,
#   nb_augment = 1000,
#   redistrib = FALSE),
#   sample = NULL, confirm = FALSE, echo = FALSE, runs_dir = "/srv/OASIS_DATA/dropout_tuning")
#
# path_data <- "/srv/OASIS_DATA/"
# save(runs, file = file.path(path_data, "results", "res_4_classe.Rdata"))


runs <- tuning_run("/home/chetbo/GENERIC/dementiaproject/inst/fine_tuning_2_class.R", flags = list(
  learning_rate = c(2e-5, 1e-6)), # 0.00002, 0.00001),
  sample = NULL, confirm = FALSE, echo = FALSE, runs_dir = "/srv/OASIS_DATA/dropout_tuning")
