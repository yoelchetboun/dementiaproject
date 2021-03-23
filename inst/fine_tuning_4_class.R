### preparation data
library(data.table)
library(purrr)
library(tfhub)
library(caret)
library(tensorflow)
library(keras)

nb_class <- 4

path_data <- "/srv/OASIS_DATA/4-class-imb"

train_data_dir <- file.path(path_data, "data_augment_train")
validation_data_dir <- file.path(path_data, "data_augment_valid")
test_data_dir <- file.path(path_data, "data_augment_test")


FLAGS <- flags(
  flag_numeric("learning_rate", 0.0001)
)


# dimensions of our images.
img_width = 224
img_height = 224

nb_train_samples = 8396
train_nb_cdr00 = 6604
train_nb_cdr05 = 1280
train_nb_cdr10 = 467
train_nb_cdr20 = 45
nb_validation_samples = 2795
nb_test_samples = 2684
epochs = 100
batch_size = 40

weight_for_00 = (1 / train_nb_cdr00) * (nb_train_samples) / 4.0
weight_for_05 = (1 / train_nb_cdr05) * (nb_train_samples) / 4.0
weight_for_10 = (1 / train_nb_cdr10) * (nb_train_samples) / 4.0
weight_for_20 = (1 / train_nb_cdr20) * (nb_train_samples) / 4.0

class_weight = list("0"=weight_for_00,"1"=weight_for_05,"2"=weight_for_10, "2"=weight_for_20)

print(class_weight)

train_datagen = image_data_generator(
  rescale=1/ 255,
  rotation_range=10,
  width_shift_range=0.1,
  height_shift_range=0.1,
  shear_range=0.15,
  zoom_range=0.2,
  channel_shift_range = 150,
  fill_mode='nearest')

test_datagen = image_data_generator(rescale=1. / 255)

train_generator = flow_images_from_directory(
  train_data_dir,
  generator = train_datagen,
  target_size=c(img_height, img_width),
  batch_size=batch_size,
  class_mode='sparse')

validation_generator = flow_images_from_directory(
  validation_data_dir,
  generator = test_datagen,
  target_size=c(img_height, img_width),
  batch_size=batch_size,
  class_mode='sparse')

test_generator = flow_images_from_directory(
  test_data_dir,
  generator = test_datagen,
  target_size=c(img_height, img_width),
  batch_size=batch_size,
  shuffle = FALSE,
  class_mode='sparse')



# architecture

base = application_vgg16(weights='imagenet', include_top=FALSE, input_shape=c(224, 224, 3))
print('Model loaded.')

# initialise top model
predictions <- base$output %>%
  layer_flatten() %>%
  layer_dense(units = 256, activation = 'relu') %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 4, activation = 'softmax')

model <- keras_model(inputs = base$input, outputs = predictions)
summary(model)
freeze_weights(model)
unfreeze_weights(model, from = 19)



model %>% compile(
  optimizer =  optimizer_adam(lr = FLAGS$learning_rate),
  loss = "sparse_categorical_crossentropy",
  metrics = c("sparse_categorical_accuracy", "auc")
)

early_stop <- callback_early_stopping(monitor = "val_loss", patience = 20, restore_best_weights = TRUE)

history = model %>% fit_generator(
  generator = train_generator,
  steps_per_epoch=round(nb_train_samples / batch_size)-1,
  epochs=1,
  validation_data=validation_generator,
  validation_steps=round(nb_validation_samples / batch_size)-1,
  callbacks = early_stop,
  class_weight = class_weight,
  verbose=1)


pred <- model %>% predict_generator(
  test_generator,
  steps =  (nb_test_samples / 4),
  verbose = 1)


save(history, file = "history_fine_tune_lr_", FLAGS$lr, ".Rdata")
save(pred, file = "pred", FLAGS$lr, ".Rdata")


#retest shiny

path_data <- "/srv/OASIS_DATA/data_base_shiny/"
path_root <- "~/GENERIC/dementiaproject/"
test_data_dir = '/srv/OASIS_DATA/test_shiny'
model <- load_model_hdf5(list.files(file.path(path_data, "models/3_class_irm"), pattern = ".h5", full.names = TRUE))
summary(model)

# image_generator <- image_data_generator(rescale=1/255)
# image_test <- flow_images_from_directory(
#   path_dir_pred,
#   image_generator,
#   shuffle = FALSE,
#   target_size = c(160, 224),
#   color_mode = "rgb",
#   class_mode = "categorical",
#   batch_size = 1)


batch_size = 40
test_datagen = image_data_generator(rescale=1. / 255)
test_generator = flow_images_from_directory(
  directory = test_data_dir,
  generator = test_datagen,
  target_size= c(160, 224),
  batch_size=batch_size,
  shuffle = FALSE,
  class_mode='categorical')

class(test_generator)

file_names <- test_generator$filenames

pred <- predict(
  model,
  test_generator,
  steps =  2,
  verbose = 1)

library(data.table)
pred_dt <- as.data.table(pred)
pred_dt[V1 > V2 & V1 > V3, cdr := 0]
pred_dt[V2 > V1 & V2 > V3, cdr := 0.5]
pred_dt[V3 > V1 & V3 > V2, cdr := 1]
pred_dt[cdr == 0.5]
pred_dt[cdr == 1]

