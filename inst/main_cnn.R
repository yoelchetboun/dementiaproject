#script lancement CNN
rm(list=ls(all.names = TRUE))
gc(reset=TRUE, full = TRUE)

library(data.table)
library(keras)
library(EBImage)
library(stringr)
library(pbapply)
library(purrr)
library(dementiaproject)
library(caret)
library(tfruns)

path_data <- "/srv/OASIS_DATA/"
path_root <- "~/GENERIC/dementiaproject/"

FLAGS <- flags(
  flag_boolean("preprocess", FALSE),
  flag_string("raw_dir", "data_raw_t1"),
  flag_string("cut_list", "135_140_145_150"),
  flag_integer("width_target", 224),
  flag_integer("height_target", 224),
  flag_string("loss_function", "binary_crossentropy"),
  flag_string("optimizer_var", "adam"),
  flag_numeric("learning_rate", 0.001),
  flag_string("metrics_var", "accuracy"),
  flag_integer("nb_epoch", 1),
  flag_integer("batch_size_var", 40),
  flag_numeric("val_split", 0.2),
  flag_numeric("test_split", 0.1)
)


## reprocess data extraction function
# path_raw <- file.path(path_data, "data_raw_t1")
# path_selected <- file.path(path_data, "data_selected")
# path_processed <- file.path(path_data, "data_processed_new")
#ds <- cut_selection(path_raw, path_selected, path_processed, cut_list = c(106))

if (FLAGS$preprocess) {
  path_raw <- file.path(path_data, FLAGS$raw_dir)
  #le repertoire de selection et processing est toujours le meme
  path_selected <- file.path(path_data, "data_selected")
  path_processed <- file.path(path_data, "data_processed")
  cut_list <- as.numeric(unlist(str_split(FLAGS$cut_list, pattern = "_")))
  dataset <- cut_selection(path_raw, path_selected, path_processed, cut_list = cut_list)
} else {
  path_processed <- file.path(path_data, "data_processed")
  dataset <- dementiaproject::loadRData(file.path(path_processed, "dataset.Rdata"))
}

##



width_target <- FLAGS$width_target
height_target <- FLAGS$height_target

loss_function <- FLAGS$loss_function
# valeurs possibles
# categorical_crossentropy => si plusieurs cat cible. We expect labels to be provided in a one_hot representation.
# binary_crossentropy => si seulement deux cat cibles
# sparse_categorical_crossentropy => If you want to provide labels as integers, please use SparseCategoricalCrossentropy loss. There should be # classes floating point values per feature
if(FLAGS$optimizer_var == "adam") {
  opt <- optimizer_adam(lr = FLAGS$learning_rate)
}

if(FLAGS$optimizer_var == "rmsprop") {
  opt <- optimizer_rmsprop(lr = FLAGS$learning_rate)
}

if(FLAGS$optimizer_var == "adagrad") {
  opt <- optimizer_adagrad(lr = FLAGS$learning_rate)
}

if(FLAGS$optimizer_var == "sgd") {
  opt <- optimizer_sgd(lr = FLAGS$learning_rate)
}

#optimizer_var <- FLAGS$optimizer_var
# valeurs possibles
# adam  ou optimizer_adam(lr = 0.0001, decay = 1e-6) pour changer le lr
# rmsprop ou optimizer_rmsprop(lr = 0.0001, decay = 1e-6)
# adagrad ou optimizer_adagrad(lr = 0.01)
# sgd : optimizer_sgd(lr = 0.01)

metrics_var <- "accuracy"
#categorical accuracy : "Calculates how often predictions matches one-hot labels."
#SensitivityAtSpecificity : the sensitivity at a given specificity.

nb_epoch <- FLAGS$nb_epoch
batch_size_var <-  FLAGS$batch_size_var
val_split <-  FLAGS$val_split
test_split <- FLAGS$test_split

# pour test
#dataset <- loadRData(file.path(path_root, "inst/extdata/oasis3/dataset.Rdata"))

#on selectionne 10% de l'échantillon pour jeu de test (avant le sous echantillonage pour garder la meme proportion)
dataset$test_set <- sample(1:(test_split*100), nrow(dataset), replace = TRUE)
dataset[test_set == 1, set := "test"]
dataset[test_set != 1, set := "train"]
data_test <- copy(dataset[set == "test"])
print(paste0("Nombre d'images 'dementia' dans le jeu test : ", nrow(data_test[dementia == "TRUE"])))
print(paste0("Nombre d'images 'non dementia' dans le jeu test : ", nrow(data_test[dementia == "FALSE"])))

#on rééquilibre les classes pour le train
data_train <- copy(dataset[set == "train"])
nb_dementia_train <- nrow(data_train[dementia == "TRUE"])
data_train_nondement <- copy(data_train[dementia == "FALSE"][sample(nrow(data_train[dementia == "FALSE"]), nb_dementia_train),])
data_train <- rbind(data_train[dementia == TRUE],  data_train_nondement)
print(paste0("Nombre d'images 'dementia' dans le jeu train : ", nrow(data_train[dementia == "TRUE"])))
print(paste0("Nombre d'images 'non dementia' dans le jeu train : ", nrow(data_train[dementia == "FALSE"])))


#on copie les images dans train/test
path_dir_train <- file.path(path_data, "dataset_processed_train")
path_dir_test <- file.path(path_data, "dataset_processed_test")

#on vide les repertoires
file.remove(list.files(path_dir_train, pattern = ".png", full.names = TRUE))
file.remove(list.files(path_dir_test, pattern = ".png", full.names = TRUE))

print("Copie des fichiers de la partie train")
map(seq(1,nrow(data_train), 1), function(x) {
  #print(paste0("Copie train : ", x, "/", nrow(data_train)))
  file.copy(from = file.path(path_processed, data_train[x]$new_name), to = path_dir_train)
})

print("Copie des fichiers de la partie test")
map(seq(1,nrow(data_test), 1), function(x) {
  #print(paste0("Copie test : ", x, "/", nrow(data_test)))
  file.copy(from = file.path(path_processed, data_test[x]$new_name), to = path_dir_test)
})

rm(data_train_nondement)
data_distrib <- rbind(data_train, data_test)

print("Début extraction des feature")
trainData  <- dementiaproject::extract_feature(path_dir = path_dir_train, data_ref = dataset, width = width_target, height = height_target, labelsExist = TRUE)
print(paste0("Fin extract feature"))

#check trainData
# trainData$X[1]
# trainData$y[1]

#check brain
# testbrain <- t(matrix(as.numeric(trainData$X[2,]),
#                     nrow = width_target, ncol = height_target, T))
# image(t(apply(testbrain, 2, rev)), col = gray.colors(12),
#       axes = F)

print(paste0("Conversion en array"))

train_array <- t(trainData$X)
dim(train_array) <- c(width_target, height_target, nrow(trainData$X), 1)
train_array <- aperm(train_array, c(3,1,2,4)) # Reorder dimensions
train_array_y <- as.numeric(as.logical(trainData$y))
if (FLAGS$loss_function == "binary_crossentropy") {
  train_array_y <- as.logical(train_array_y)
}

rm(trainData)

#afin de gagner en mémoire on redémare la session R
gc(reset=TRUE, full = TRUE)

# Ne marche pas en batch
# e <- as.environment("tools:rstudio")
# e$.rs.restartR()
# #est ce que la session est toujours active ?
# print("Restart ok!")
# print(paste0("premier element de train_array_x : ", train_array[1]))


# Check brain again
# testbrain <- train_array[2,,,]
# image(t(apply(testbrain, 2, rev)), col = gray.colors(12),
#       axes = F)


model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = "relu",
                input_shape = c(width_target,height_target,1)) %>% #format image width_target x height_target
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3,3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_conv_2d(filters = 128, kernel_size = c(3,3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_flatten() %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 2, activation = "softmax") #car 2 catégories

summary(model)

#fonction d'arret si on n'améliore plus la loss au bout de 20 epoch
early_stop <- callback_early_stopping(monitor = "val_loss", patience = 25)

model %>% compile(
  optimizer = opt,
  loss = loss_function, #sparse_categorical_crossentropy, accurary ?
  metrics = metrics_var
)

tik <- Sys.time()
print(paste0("Lancement de l'entrainement : ", tik))

history <- model %>%
  fit(
    x = train_array, y = train_array_y,
    epochs = nb_epoch,
    batch_size = batch_size_var,
    callbacks = list(early_stop),
    validation_split = val_split,
    verbose = 2 #jouer sur le learning rate
  )


tok <- Sys.time()-tik
print(paste0("Fin de l'entrainement : ", tok))


# Compute probabilities and predictions on test set
testData  <- extract_feature(path_dir = path_dir_test, data_ref = dataset, width = width_target, height = height_target, labelsExist = TRUE)
test_array <- t(testData$X)
dim(test_array) <- c(width_target, height_target, nrow(testData$X), 1)
test_array <- aperm(test_array, c(3,1,2,4)) # Reorder dimensions
test_array_y <- as.numeric(as.logical(testData$y))
if (FLAGS$loss_function == "binary_crossentropy") {
  test_array_y <- as.logical(test_array_y)
}

rm(testData)

predictions <-  predict_classes(model, test_array)
probabilities <- predict_proba(model, test_array)

resultat_test <- data.table()
resultat_test$pred <- as.numeric(predictions)
resultat_test$obs <- as.numeric(test_array_y)

#matrice de confusion
conf_matrix <- confusionMatrix(data = as.factor(resultat_test$pred), reference = as.factor(resultat_test$obs))

summary_result <- data.table(width_target_img = width_target,
                             height_target_img = height_target,
                             loss_function = loss_function,
                             optimizer = FLAGS$optimizer_var,
                             learning_rate = FLAGS$learning_rate,
                             metrics = metrics_var,
                             nb_epoch = nb_epoch,
                             batch = batch_size_var,
                             val_split = val_split,
                             nb_test_img = nrow(resultat_test),
                             nb_train_img = length(train_array_y),
                             good_classif_rate = nrow(resultat_test[pred == obs]) / nrow(resultat_test),
                             sensitivity = conf_matrix$byClass["Sensitivity"],
                             specificity = conf_matrix$byClass["Specificity"],
                             time_train = tok,
                             cut_selection = FLAGS$cut_list,
                             preprocess = FLAGS$preprocess,
                             raw_dir = FLAGS$raw_dir)

# # Visual inspection of 32 cases
# set.seed(100)
# random <- sample(1:nrow(testData), 32)
# preds <- predictions[random,]
# probs <- as.vector(round(probabilities[random,], 2))
#
# par(mfrow = c(4, 8), mar = rep(0, 4))
# for(i in 1:length(random)){
#   image(t(apply(test_array[random[i],,,], 2, rev)),
#         col = gray.colors(12), axes = F)
#   legend("topright", legend = ifelse(preds[i] == 0, "Cat", "Dog"),
#          text.col = ifelse(preds[i] == 0, 2, 4), bty = "n", text.font = 2)
#   legend("topleft", legend = probs[i], bty = "n", col = "white")
# }

# Save model
nb_dir <- length(list.dirs(file.path(path_data, "results"), recursive = FALSE)) + 1
path_dir_result <- file.path(path_data, "results", paste0("results_", nb_dir))
dir.create(path = path_dir_result)

save(model, file = file.path(path_dir_result, "model.RData"))
save(history, file = file.path(path_dir_result, "history.Rdata"))
save(predictions, file = file.path(path_dir_result, "predictions.Rdata"))
save(probabilities, file = file.path(path_dir_result, "probabilities.Rdata"))
save(data_distrib, file = file.path(path_dir_result, "data_distrib.Rdata"))
save(resultat_test, file = file.path(path_dir_result, "resultat_test.Rdata"))
save(conf_matrix, file = file.path(path_dir_result, "conf_matrix.Rdata"))
save(summary_result, file = file.path(path_dir_result, "summary_result.Rdata"))
save_model_hdf5(object = model, filepath = file.path(path_dir_result, "model.h5"))

# model %>% compile(
#   loss = 'binary_crossentropy',
#   optimizer = "adam",
#   metrics = c('accuracy')
# )
# history % fit(
#   x = train_array, y = as.numeric(trainData$y),
#   epochs = 30, batch_size = 100,
#   validation_split = 0.2
# )
# plot(history)
