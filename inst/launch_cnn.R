library(tfruns)

runs <- tuning_run("/home/chetbo/GENERIC/dementiaproject/inst/main_cnn.R", flags = list(
  loss_function = c("sparse_categorical_crossentropy"),
  optimizer_var = c("adam", "rmsprop"),
  learning_rate = c(0.1, 0.01, 0.001),
  metrics_var = c("accuracy"),
  nb_epoch  =  c(50),
  batch_size_var =  c(40),
  val_split =  c(0.2),
  test_split =  c(0.1)),
  sample = NULL, confirm = FALSE, echo = FALSE, runs_dir = "/srv/OASIS_DATA/dropout_tuning")
