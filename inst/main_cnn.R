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

#image size
width_target <- 254
height_target <- 254
path_data <- "/srv/OASIS_DATA/"
path_root <- "~/GENERIC/dementiaproject/"

load(file.path(path_root, "inst/extdata/oasis3/dataset.Rdata"))

# pour test
dataset <- rbind(dataset[dementia == TRUE],  dataset[dementia == FALSE][1:2400])

#dataset dementia 2400 TRUE / 8700 FALSE (~20%)
dataset$test_set <- sample(1:10, nrow(dataset), replace = TRUE)
dataset[test_set == 1, set := "test"]
dataset[test_set != 1, set := "train"]


#on copie les images dans train/test
path_dir_train <- file.path(path_data, "dataset_processed_train")
path_dir_test <- file.path(path_data, "dataset_processed_test")

#on vide les repertoires
file.remove(list.files(path_dir_train, pattern = ".png", full.names = TRUE))
file.remove(list.files(path_dir_test, pattern = ".png", full.names = TRUE))

map(seq(1,length(dataset$set), 1), function(x) {
  print(paste0("Traitement ", x, "/", length(dataset$set)))
  if(dataset[x]$set == "train") {
    file.copy(from = file.path(path_data, "dataset_processed", dataset[x]$new_name), to = path_dir_train)
  } else {
    file.copy(from = file.path(path_data, "dataset_processed", dataset[x]$new_name), to = path_dir_test)

  }
})


trainData  <- extract_feature(path_dir = path_dir_train, data_ref = dataset, width = width_target, height = height_target, labelsExist = TRUE)
print(paste0("Fin extract feature"))

#check trainData
# trainData$X[1]
# trainData$y[1]

#check brain
# testbrain <- t(matrix(as.numeric(trainData$X[2,]),
#                     nrow = width, ncol = height, T))
# image(t(apply(testbrain, 2, rev)), col = gray.colors(12),
#       axes = F)

print(paste0("Conversion en array"))

train_array <- t(trainData$X)
dim(train_array) <- c(254, 254, nrow(trainData$X), 1)
train_array <- aperm(train_array, c(3,1,2,4)) # Reorder dimensions
train_array_y <- as.numeric(as.logical(trainData$y))
rm(trainData)


# Check brain again
# testbrain <- train_array[2,,,]
# image(t(apply(testbrain, 2, rev)), col = gray.colors(12),
#       axes = F)


model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = "relu",
                input_shape = c(254,254,1)) %>% #format image 256x256
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

model %>% compile(
  optimizer = "adam",
  loss = "categorical_crossentropy", #sparse_categorical_crossentropy, accurary ?
  metrics = "accuracy"
)

tik <- Sys.time()
print(paste0("Lancement de l'entrainement : ", tik))

history <- model %>%
  fit(
    x = train_array, y = train_array_y,
    epochs = 40, batch_size = 50,
    validation_split = 0.2,
    verbose = 2 #jouer sur le learning rate
  )

#save_model_hdf5(model, 'my_model.h5')
print(paste0("Fin de l'entrainement : ", Sys.time()-tik))


# Compute probabilities and predictions on test set
testData  <- extract_feature(path_dir = path_dir_test, data_ref = dataset, width = width_target, height = height_target, labelsExist = TRUE)
test_array <- t(testData$X)
dim(test_array) <- c(254, 254, nrow(testData$X), 1)
test_array <- aperm(test_array, c(3,1,2,4)) # Reorder dimensions
test_array_y <- as.numeric(as.logical(testData$y))
rm(testData)

predictions <-  predict_classes(model, test_array)
probabilities <- predict_proba(model, test_array)

resultat_test <- data.table()
resultat_test$pred <- predictions
resultat_test$obs <- test_array_y

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
save(model, file = "model.RData")
save(history, file = "history.Rdata")
save(predictions, file = "predictions.Rdata")
save(probabilities, file = "probabilities.Rdata")
save(dataset, file = "dataset_distrib.Rdata")
save(resultat_test, file = "resultat_test.Rdata")



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
