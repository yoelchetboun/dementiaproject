### preparation data
library(data.table)
library(purrr)
library(tfhub)
library(caret)
library(tensorflow)
library(keras)

nb_class <- 2


path_data <- "/srv/OASIS_DATA/2-class-bal-val"

path_dir_train <- file.path(path_data, "data_augment_train")
path_dir_valid <- file.path(path_data, "data_augment_valid")
path_dir_test <- file.path(path_data, "data_augment_test")

epochs <- 100

FLAGS <- flags(
  flag_numeric("learning_rate", 2e-5)
)




path_data <- "/srv/OASIS_DATA/"
path_processed <-  file.path(path_data, "data_processed_more_cut")

path_dir_train <- file.path(path_data, "data_augment_train_more_cut")
path_dir_valid <- file.path(path_data, "data_augment_valid_more_cut")
path_dir_test <- file.path(path_data, "data_augment_test_more_cut")

dataset <- dementiaproject::loadRData( file.path(path_data, "data_processed", "dataset.Rdata"))
#dataset <- copy(data_distrib)
data_train <- copy(dataset[set == "train"])
data_valid <- copy(dataset[set == "validation"])
data_test <- copy(dataset[set == "test"])

#on vide les repertoires
file.remove(list.files(path_dir_train, pattern = ".png", full.names = TRUE, recursive = TRUE))
file.remove(list.files(path_dir_valid , pattern = ".png", full.names = TRUE, recursive = TRUE))
file.remove(list.files(path_dir_test , pattern = ".png", full.names = TRUE, recursive = TRUE))

#on supprime les dirs
unlink(file.path(path_dir_train, "cdr00"), recursive = TRUE)
unlink(file.path(path_dir_train, "cdr05"), recursive = TRUE)
unlink(file.path(path_dir_train, "cdr10"), recursive = TRUE)
unlink(file.path(path_dir_train, "cdr20"), recursive = TRUE)
unlink(file.path(path_dir_valid, "cdr00"), recursive = TRUE)
unlink(file.path(path_dir_valid, "cdr05"), recursive = TRUE)
unlink(file.path(path_dir_valid, "cdr10"), recursive = TRUE)
unlink(file.path(path_dir_valid, "cdr20"), recursive = TRUE)
unlink(file.path(path_dir_test, "cdr00"), recursive = TRUE)
unlink(file.path(path_dir_test, "cdr05"), recursive = TRUE)
unlink(file.path(path_dir_test, "cdr10"), recursive = TRUE)
unlink(file.path(path_dir_test, "cdr20"), recursive = TRUE)
unlink(file.path(path_dir_train, "dementia"), recursive = TRUE)
unlink(file.path(path_dir_train, "non_dementia"), recursive = TRUE)
unlink(file.path(path_dir_valid, "dementia"), recursive = TRUE)
unlink(file.path(path_dir_valid, "non_dementia"), recursive = TRUE)
unlink(file.path(path_dir_test, "dementia"), recursive = TRUE)
unlink(file.path(path_dir_test, "non_dementia"), recursive = TRUE)

if (nb_class == 2) {
  dir.create(file.path(path_dir_train, "dementia"))
  dir.create(file.path(path_dir_train, "non_dementia"))
  dir.create(file.path(path_dir_valid, "dementia"))
  dir.create(file.path(path_dir_valid, "non_dementia"))
  dir.create(file.path(path_dir_test, "dementia"))
  dir.create(file.path(path_dir_test, "non_dementia"))

  #sous echantillonage
  nb_dementia_train <- nrow(data_train[dementia == "TRUE"])
  data_train_nondement <- copy(data_train[dementia == "FALSE"][sample(nrow(data_train[dementia == "FALSE"]), nb_dementia_train),])
  data_train <- rbind(data_train[dementia == TRUE],  data_train_nondement)


  print("Copie des fichiers de la partie train")
  map(seq(1,nrow(data_train), 1), function(x) {
    #print(paste0("Copie train : ", x, "/", nrow(data_train)))
    if (data_train[x]$dementia == TRUE) {
      file.copy(from = file.path(path_processed, data_train[x]$new_name), to = file.path(path_dir_train, "dementia"))
    } else {
      file.copy(from = file.path(path_processed, data_train[x]$new_name), to = file.path(path_dir_train, "non_dementia"))
    }
  })

  nb_dementia_train <- nrow(data_valid[dementia == "TRUE"])
  data_valid_nondement <- copy(data_valid[dementia == "FALSE"][sample(nrow(data_valid[dementia == "FALSE"]), nb_dementia_train),])
  data_valid <- rbind(data_valid[dementia == TRUE],  data_valid_nondement)

  print("Copie des fichiers de la partie validation")
  map(seq(1,nrow(data_valid), 1), function(x) {
    #print(paste0("Copie train : ", x, "/", nrow(data_train)))
    if (data_valid[x]$dementia == TRUE) {
      file.copy(from = file.path(path_processed, data_valid[x]$new_name), to = file.path(path_dir_valid, "dementia"))
    } else {
      file.copy(from = file.path(path_processed, data_valid[x]$new_name), to = file.path(path_dir_valid, "non_dementia"))
    }
  })

  print("Copie des fichiers de la partie test")
  map(seq(1,nrow(data_test), 1), function(x) {
    #print(paste0("Copie train : ", x, "/", nrow(data_train)))
    if (data_test[x]$dementia == TRUE) {
      file.copy(from = file.path(path_processed, data_test[x]$new_name), to = file.path(path_dir_test, "dementia"))
    } else {
      file.copy(from = file.path(path_processed, data_test[x]$new_name), to = file.path(path_dir_test, "non_dementia"))
    }
  })

}

if (nb_class == 4 ) {
  dir.create(file.path(path_dir_train, "cdr00"))
  dir.create(file.path(path_dir_train, "cdr05"))
  dir.create(file.path(path_dir_train, "cdr10"))
  dir.create(file.path(path_dir_train, "cdr20"))
  dir.create(file.path(path_dir_valid, "cdr00"))
  dir.create(file.path(path_dir_valid, "cdr05"))
  dir.create(file.path(path_dir_valid, "cdr10"))
  dir.create(file.path(path_dir_valid, "cdr20"))
  dir.create(file.path(path_dir_test, "cdr00"))
  dir.create(file.path(path_dir_test, "cdr05"))
  dir.create(file.path(path_dir_test, "cdr10"))
  dir.create(file.path(path_dir_test, "cdr20"))

  print("Copie des fichiers de la partie train")
  map(seq(1,nrow(data_train), 1), function(x) {
    dir <- file.path(path_dir_train, paste0("cdr", sub("[.]", "", as.character(data_train[x]$cdr_ref))))
    file.copy(from = file.path(path_data, "data_processed_train", data_train[x]$new_name), to = dir)
  })


  print("Copie des fichiers de la partie validation")
  map(seq(1,nrow(data_valid), 1), function(x) {
    dir <- file.path(path_dir_valid, paste0("cdr", sub("[.]", "", as.character(data_valid[x]$cdr_ref))))
    file.copy(from = file.path(path_data, "data_processed_valid", data_valid[x]$new_name), to = dir)
  })

  print("Copie des fichiers de la partie test")
  map(seq(1,nrow(data_test), 1), function(x) {
    dir <- file.path(path_dir_test, paste0("cdr", sub("[.]", "", as.character(data_test[x]$cdr_ref))))
    file.copy(from = file.path(path_data, "data_processed_test", data_test[x]$new_name), to = dir)
  })

}


image_generator <- image_data_generator(rescale=1/255)

image_train <- flow_images_from_directory(
  path_dir_train,
  image_generator,
  target_size = c(224, 224),
  classes = c("dementia","non_dementia"),
  color_mode = "rgb",
  class_mode = "binary",
  batch_size = 40
)


image_valid <- flow_images_from_directory(
  path_dir_valid,
  image_generator,
  target_size = c(224, 224),
  classes = c("dementia","non_dementia"),
  color_mode = "rgb",
  class_mode = "binary",
  batch_size = 40
)


image_test <- flow_images_from_directory(
  path_dir_test,
  image_generator,
  #classes=['cdr10'],
  shuffle = FALSE,
  target_size = c(224, 224),
  classes = c("dementia","non_dementia"),
  color_mode = "rgb",
  class_mode = "binary",
  batch_size = 4
)


image_shape <- c(224L, 224L, 3L)

base <- application_resnet50(weights = 'imagenet', include_top = FALSE, input_shape = image_shape)

# mobilnet_url <- "https://tfhub.dev/google/tf2-preview/mobilenet_v2/feature_vector/2"
# inception3_url <- "https://tfhub.dev/google/imagenet/inception_v3/feature_vector/4"
# inception_resnet2_url <- "https://tfhub.dev/google/imagenet/inception_resnet_v2/feature_vector/4"
# voir https://tfhub.dev/google/collections/image/1

#base <- layer_hub(handle = inception3_url,  input_shape = image_shape)
# add our custom layers

predictions <- base$output %>%
  layer_flatten() %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 1, activation = 'sigmoid')

model <- keras_model(inputs = base$input, outputs = predictions)

freeze_weights(base)
unfreeze_weights(base, from = 175)

layers <- base$layers
for (i in 1:length(layers))
  cat(i, layers[[i]]$name, "\n")

summary(model)

model %>% compile(
  optimizer =  optimizer_adam(lr = FLAGS$learning_rate),
  loss = "binary_crossentropy",
  metrics = "accuracy"
)

history <- model %>% fit_generator(
  image_train, epochs=epochs,
  steps_per_epoch = image_train$n / image_train$batch_size,
  validation_data = image_valid,
  validation_steps = image_valid$n / image_valid$batch_size,
  verbose = 1
)

nb_dir <- length(list.dirs(file.path("/srv/OASIS_DATA/", "results"), recursive = FALSE)) + 1
path_dir_result <- file.path("/srv/OASIS_DATA/", "results", paste0("results_", nb_dir))
dir.create(path = path_dir_result)

save_model_hdf5(model, filepath = file.path(path_dir_result, paste0("model_2_class_", as.character(FLAGS$learning_rate), ".Rdata")))

true_labels <- factor(image_test$classes, levels = c(0,1))
file_names <- image_test$filenames

pred <- predict_generator(
  model,
  image_test,
  steps =  671,
  verbose = 1)

class_indices <- image_test$class_indices

save(pred, file = file.path(path_dir_result, "pred.Rdata"))
save(true_labels, file = file.path(path_dir_result, "true_labels.Rdata"))
save(file_names, file = file.path(path_dir_result, "file_names.Rdata"))
save(class_indices, file = file.path(path_dir_result, "class_indices.Rdata"))


