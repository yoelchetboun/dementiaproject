library(tfruns)

runs <- tuning_run("/home/chetbo/GENERIC/dementiaproject/inst/main_cnn.R", flags = list(
  preprocess = FALSE,
  raw_dir = "data_raw_t1",
  cut_list = "135_140_145_150",
  width_target = c(224),
  height_target = c(224),
  loss_function = c("sparse_categorical_crossentropy"), #sparse_categorical_crossentropy binary_crossentropy
  optimizer_var = c("adam"),
  learning_rate = c(0.002, 0.001, 0.0005, 0.0002),
  metrics_var = c("accuracy"),
  nb_epoch  =  c(50),
  batch_size_var =  c(40),
  val_split =  c(0.2),
  test_split =  c(0.1)),
  sample = NULL, confirm = FALSE, echo = FALSE, runs_dir = "/srv/OASIS_DATA/dropout_tuning")

path_data <- "/srv/OASIS_DATA/"
save(runs, file = file.path(path_data, "results", "res_tfruns.Rdata"))
