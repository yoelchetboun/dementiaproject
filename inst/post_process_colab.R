#post_process colab vgg

library(data.table)
library(purrr)
library(tfhub)
library(caret)
library(tensorflow)
library(keras)

nb_class <- 2

path_data <- "/srv/OASIS_DATA/"
path_processed <-  file.path(path_data, "data_processed")
num_model <- "1"
path_model <- file.path(path_data, "modeles_colab", paste0("modele_", num_model))

path_dir_train <- file.path(path_data, "data_augment_train")
path_dir_valid <- file.path(path_data, "data_augment_valid")
path_dir_test <- file.path(path_data, "data_augment_test")

dataset <- dementiaproject::loadRData( file.path(path_data, "data_processed", "dataset.Rdata"))
data_train <- copy(dataset[set == "train"])
data_valid <- copy(dataset[set == "validation"])
data_test <- copy(dataset[set == "test"])


image_generator <- image_data_generator(
  rescale=1 / 255,
  rotation_range=10,
  width_shift_range=0.1,
  height_shift_range=0.1,
  shear_range=0.15,
  zoom_range=0.2,
  channel_shift_range = 150,
  fill_mode='nearest')

image_generator_valid <- image_data_generator(rescale=1/255)

image_train <- flow_images_from_directory(
  path_dir_train,
  image_generator,
  target_size= c(224, 224),
  classes = c("dementia","non_dementia"),
  batch_size=40,
  class_mode='binary'
)


image_valid <- flow_images_from_directory(
  path_dir_valid,
  image_generator_valid,
  target_size = c(224, 224),
  classes = c("dementia","non_dementia"),
  class_mode = "binary",
  batch_size = 40,
  shuffle = FALSE
)


image_test <- flow_images_from_directory(
  path_dir_test,
  image_generator_valid,
  target_size = c(224, 224),
  classes = c("dementia","non_dementia"),
  class_mode = "binary",
  batch_size = 40,
  shuffle = FALSE
)

model <- load_model_hdf5(file.path(path_model, paste0("modele_", num_model, ".h5")))

#true_labels <- factor(image_test$classes, levels = c(0,1))
#file_names <- image_valid$filenames

evaluate_generator(model, image_valid, image_valid$n / image_valid$batch_size)

pred <- predict_generator(
  model,
  image_test,
  steps =  image_test$n / image_test$batch_size, #c'est à chauqe eopch qu'on reprend le meme nombre d'image de notre trainingen les modifiant légèrement
  verbose = 1)

res <- data.table()
res$pred_prob <- pred
res[pred_prob >=0.5, pred := 0]
res[pred_prob <0.5, pred := 1]

res$obs <- true_labels[1:length(pred)]
conf_matrix <- confusionMatrix(data = factor(res$pred, levels = c(0,1)), reference = factor(res$obs, levels = c(0,1)))
eval <- evaluate_generator(
  model,
  image_test,
  steps =  image_test$n / image_test$batch_size)




## ggplot des resultats

library(data.table)
library(purrr)
library(tfhub)
library(caret)
library(tensorflow)
library(keras)
library(ggplot2)

nb_class <- 2

path_data <- "/srv/OASIS_DATA/"
list_model <- list.dirs(file.path(path_data, "modeles_colab"),recursive = FALSE)
list_history <- list.files(file.path(path_data, "modeles_colab"), pattern = "history.csv", recursive = TRUE, full.names = TRUE)
list_history <- list_history[!grepl(pattern = "3_class",list_history)]

#comparaison des learning rate pour vgg16
hist <- rbindlist(map(list_history, function(x) {
  hist <- read.csv2(x, header = TRUE, sep = ",", dec = ".")
  setDT(hist)
  setnames(hist, "X", "epoch")
  hist[, model := str_split(dirname(x), "/")[[1]][[6]]]
  }))

hist <- hist[model %in% c("vgg_1e5", "vgg_2e5", "vgg_5e6")]
ggplot(hist) + geom_point(aes(x = epoch, y = accuracy, col =model)) + geom_line(aes(x = epoch, y = val_accuracy   , col =model))  +
  labs(x = "Epoch", y = "Accuracy", col = "Learning rate") + ggtitle("Modèle VGG16 - Points : training - Lignes : validation") + scale_color_discrete(name = "Learning Rate", labels = c("1e-5", "2e-5", "1e-6"))

ggplot(hist) + geom_point(aes(x = epoch, y = loss, col =model)) + geom_line(aes(x = epoch, y = val_loss   , col =model)) +
  labs(x = "Epoch", y = "Loss", col = "Learning rate") + ggtitle("Modèle VGG16 - Points : training - Lignes : validation") + scale_color_discrete(name = "Learning Rate", labels = c("1e-5", "2e-5", "1e-6"))


#comparaison des differnets architectures
hist <- hist[model %in% c("inception_resnetV2_1e5", "inceptionV3_1e5", "resnet50_1e5", "vgg_1e5", "xception_1e5")]
ggplot(hist) + geom_point(aes(x = epoch, y = accuracy, col =model)) +
  labs(x = "Epoch", y = "Accuracy", col = "") + ggtitle("Comparaison d'architectures  - Accuracy [train]") + scale_color_discrete(name = "", labels = c("Inception-Resnet", "Inception", "Resnet50","VGG16", "Xception")) + theme(legend.position = "bottom")

ggplot(hist) + geom_line(aes(x = epoch, y = val_accuracy   , col =model)) +
  labs(x = "Epoch", y = "Accuracy", col = "") + ggtitle("Comparaison d'architectures  - Accuracy [validation]") + scale_color_discrete(name = "", labels = c("Inception-Resnet", "Inception", "Resnet50","VGG16", "Xception")) + theme(legend.position = "bottom")

ggplot(hist) + geom_point(aes(x = epoch, y = loss, col =model)) +
  labs(x = "Epoch", y = "Loss", col = "") + ggtitle("Comparaison d'architectures  - Loss [train]") + scale_color_discrete(name = "", labels = c("Inception-Resnet", "Inception", "Resnet50","VGG16", "Xception")) + theme(legend.position = "bottom")

ggplot(hist) + geom_line(aes(x = epoch, y = val_loss   , col =model)) +
  labs(x = "Epoch", y = "Loss", col = "") + ggtitle("Comparaison d'architectures  - Loss [validation]") + scale_color_discrete(name = "", labels = c("Inception-Resnet", "Inception", "Resnet50","VGG16", "Xception")) + theme(legend.position = "bottom")

hist <- hist[model %in% c("vgg_1e5")]
ggplot(hist) + geom_point(aes(x = epoch, y = accuracy, col =model)) + geom_line(aes(x = epoch, y = val_accuracy   , col =model)) +
  labs(x = "Epoch", y = "Accuracy", col = "Learning rate") + ggtitle("Architecture retenue - Points : training - Lignes : validation") + scale_color_manual(name = "", values = "#0099ff", "VGG 16")

ggplot(hist) + geom_point(aes(x = epoch, y = loss, col =model)) + geom_line(aes(x = epoch, y = val_loss   , col =model)) +
  labs(x = "Epoch", y = "Loss", col = "Learning rate") + ggtitle("Architecture retenue - Points : training - Lignes : validation") + scale_color_manual(name = "", values = "#0099ff", "VGG 16")




#comparaison des differnets fine tuning
hist <- hist[model %in% c("vgg16_1block", "vgg16_2block", "vgg16_3block", "vgg_2e5")]
hist[model %in% c("vgg16_1block", "vgg16_2block", "vgg16_3block"), epoch := epoch + 100]
ggplot(hist) + geom_point(aes(x = epoch, y = accuracy, col =model)) +
  labs(x = "Epoch", y = "Accuracy", col = "Architecture") + ggtitle("Amélioration par 'fine-tuning' du modèle VGG16 - Accuracy [train]") +
  scale_color_discrete(name = "Méthode : ", labels = c("Feature-extraction", "Fine-tuning (1 couche)", "Fine-tuning (2 couches)", "Fine-tuning (3 couches)")) +
  geom_vline(xintercept = 100, linetype="dotted",
             color = "red", size=0.5) + theme(legend.position = "bottom") +
  geom_text(aes(x=105, label="Fine-tuning", y=0.6), colour="red", angle=90, vjust = 0.5, family ="sans", fontface = 'italic', size = 3)+
  geom_text(aes(x=95, label="Feature-extraction", y=0.6), colour="red", angle=90, vjust = 0.5, family ="sans", fontface = 'italic', size = 3)

ggplot(hist) + geom_line(aes(x = epoch, y = val_accuracy, col =model)) +
  labs(x = "Epoch", y = "Accuracy", col = "Architecture") + ggtitle("Amélioration par 'fine-tuning' du modèle VGG16 - Accuracy [validation]") +
  scale_color_discrete(name = "Méthode : ", labels = c("Feature-extraction", "Fine-tuning (1 couche)", "Fine-tuning (2 couches)", "Fine-tuning (3 couches)")) +
  geom_vline(xintercept = 100, linetype="dotted",
             color = "red", size=0.5) +   theme(legend.position = "bottom") +
  geom_text(aes(x=105, label="Fine-tuning", y=0.68), colour="red", angle=90, vjust = 0.5, family ="sans", fontface = 'italic', size = 3) +
  geom_text(aes(x=95, label="Feature-extraction", y=0.68), colour="red", angle=90, vjust = 0.5, family ="sans", fontface = 'italic', size = 3)

ggplot(hist) + geom_point(aes(x = epoch, y = loss, col =model)) +
  labs(x = "Epoch", y = "Loss", col = "Architecture") + ggtitle("Amélioration par 'fine-tuning' du modèle VGG16 - Loss [train]") +
  scale_color_discrete(name = "Méthode : ", labels = c("Feature-extraction", "Fine-tuning (1 couche)", "Fine-tuning (2 couches)", "Fine-tuning (3 couches)")) +
  geom_vline(xintercept = 100, linetype="dotted",
             color = "red", size=0.5) +  theme(legend.position = "bottom") +
  geom_text(aes(x=105, label="Fine-tuning", y=0.4), colour="red", angle=90, vjust = 0.5, family ="sans", fontface = 'italic', size = 3)+
  geom_text(aes(x=95, label="Feature-extraction", y=0.4), colour="red", angle=90, vjust = 0.5, family ="sans", fontface = 'italic', size = 3)

ggplot(hist) + geom_line(aes(x = epoch, y = val_loss, col =model)) +
  labs(x = "Epoch", y = "Loss", col = "Architecture") + ggtitle("Amélioration par 'fine-tuning' du modèle VGG16 - Loss [validation]") +
  scale_color_discrete(name = "Méthode : ", labels = c("Feature-extraction", "Fine-tuning (1 couche)", "Fine-tuning (2 couches)", "Fine-tuning (3 couches)")) +
  geom_vline(xintercept = 100, linetype="dotted",
             color = "red", size=0.5) +theme(legend.position = "bottom") +
  geom_text(aes(x=105, label="Fine-tuning", y=0.6), colour="red", angle=90, vjust = 0.5, family ="sans", fontface = 'italic', size = 3) +
  geom_text(aes(x=95, label="Feature-extraction", y=0.6), colour="red", angle=90, vjust = 0.5, family ="sans", fontface = 'italic', size = 3)

#comparaison des differents fine tuning more cut
hist <- rbindlist(map(list_history, function(x) {
  hist <- read.csv2(x, header = TRUE, sep = ",", dec = ".")
  setDT(hist)
  setnames(hist, "X", "epoch")
  hist[, model := str_split(dirname(x), "/")[[1]][[6]]]
}))
hist <- hist[model %in% c("vgg_2e5_more_cut_3block", "vgg_2e5_more_cut_3block_run2", "vgg_2e5_more_cut", "vgg_2e5", "vgg16_3block")]
hist[model %in% c("vgg_2e5_more_cut_3block"), epoch := epoch + 100]
hist[model %in% c("vgg_2e5_more_cut_3block_run2"), epoch := epoch + 250]
hist[model %in% c("vgg16_3block"), epoch := epoch + 100]
hist[model %in% c("vgg16_3block", "vgg_2e5"), model := "Base classique"]
hist[model %in% c("vgg_2e5_more_cut", "vgg_2e5_more_cut_3block_run2", "vgg_2e5_more_cut_3block"), model := "Base étendue"]

ggplot(hist) + geom_point(aes(x = epoch, y = accuracy, col =model)) +
  labs(x = "Epoch", y = "Accuracy", col = "Architecture") + ggtitle("Changement de la base d'apprentissage - Accuracy [train]") +
  scale_color_discrete(name = "Base de données : ", labels = c("Base training de 3584 images", "Base training de 6997 images")) +
  geom_vline(xintercept = 100, linetype="dotted",
             color = "red", size=0.5) +
  geom_text(aes(x=105, label="Fine-tuning", y=0.6), colour="red", angle=90, vjust = 0.5, family ="sans", fontface = 'italic', size = 3)+
  geom_text(aes(x=95, label="Feature-extraction", y=0.6), colour="red", angle=90, vjust = 0.5, family ="sans", fontface = 'italic', size = 3)+
  theme(legend.position = "bottom")


ggplot(hist) + geom_point(aes(x = epoch, y = loss, col =model)) +
  labs(x = "Epoch", y = "Loss", col = "Architecture") + ggtitle("Changement de la base d'apprentissage - Loss [train]") +
  scale_color_discrete(name = "Base de données : ", labels = c("Base training de 3584 images", "Base training de 6997 images")) +
  geom_vline(xintercept = 100, linetype="dotted",
             color = "red", size=0.5) +
  geom_text(aes(x=105, label="Fine-tuning", y=0.32), colour="red", angle=90, vjust = 0.5, family ="sans", fontface = 'italic', size = 3)+
  geom_text(aes(x=95, label="Feature-extraction", y=0.32), colour="red", angle=90, vjust = 0.5, family ="sans", fontface = 'italic', size = 3)+
  theme(legend.position = "bottom")


ggplot(hist) + geom_line(aes(x = epoch, y = val_accuracy, col =model)) +
  labs(x = "Epoch", y = "Accuracy", col = "Architecture") + ggtitle("Changement de la base d'apprentissage - Accuracy [validation]") +
  scale_color_discrete(name = "Base de données : ", labels = c("Base training de 3584 images", "Base training de 6997 images")) +
  geom_vline(xintercept = 100, linetype="dotted",
             color = "red", size=0.5) +
  geom_text(aes(x=105, label="Fine-tuning", y=0.86), colour="red", angle=90, vjust = 0.5, family ="sans", fontface = 'italic', size = 3)+
  geom_text(aes(x=95, label="Feature-extraction", y=0.86), colour="red", angle=90, vjust = 0.5, family ="sans", fontface = 'italic', size = 3)+
  theme(legend.position = "bottom")


ggplot(hist) + geom_line(aes(x = epoch, y = val_loss, col =model)) +
  labs(x = "Epoch", y = "Loss", col = "Architecture") + ggtitle("Changement de la base d'apprentissage - Loss [validation]") +
  scale_color_discrete(name = "Base de données : ", labels = c("Base training de 3584 images", "Base training de 6997 images")) +
  geom_vline(xintercept = 100, linetype="dotted",
             color = "red", size=0.5) +
  geom_text(aes(x=105, label="Fine-tuning", y=0.32), colour="red", angle=90, vjust = 0.5, family ="sans", fontface = 'italic', size = 3)+
  geom_text(aes(x=95, label="Feature-extraction", y=0.32), colour="red", angle=90, vjust = 0.5, family ="sans", fontface = 'italic', size = 3)+
  theme(legend.position = "bottom")



#pre-process 3 class
#
# dataset <- dementiaproject::loadRData("/srv/OASIS_DATA/data_processed_more_cut/dataset.Rdata")
# split <- function(path_dir) {
#   dir.create(file.path(dirname(path_dir),  paste0(basename(path_dir), "_3_class")))
#   dir.create(file.path(dirname(path_dir),  paste0(basename(path_dir), "_3_class"), "cdr00"))
#   dir.create(file.path(dirname(path_dir),  paste0(basename(path_dir), "_3_class"), "cdr05"))
#   dir.create(file.path(dirname(path_dir),  paste0(basename(path_dir), "_3_class"), "cdr10"))
#   list_files <- list.files(path_dir, pattern = ".png", recursive = TRUE, full.names = TRUE)
#   map(list_files, function(x) {
#     if (dataset[basename(x) == new_name]$cdr_ref == "0.0") {
#       file.copy(from = x, to =file.path(dirname(path_dir),  paste0(basename(path_dir), "_3_class"), "cdr00"))
#     }
#     if (dataset[basename(x) == new_name]$cdr_ref == "0.5") {
#       file.copy(from = x, to =file.path(dirname(path_dir),  paste0(basename(path_dir), "_3_class"), "cdr05"))
#     }
#     if (dataset[basename(x) == new_name]$cdr_ref == "1.0") {
#       file.copy(from = x, to =file.path(dirname(path_dir),  paste0(basename(path_dir), "_3_class"), "cdr10"))
#     }
#     if (dataset[basename(x) == new_name]$cdr_ref == "2.0") {
#       file.copy(from = x, to =file.path(dirname(path_dir),  paste0(basename(path_dir), "_3_class"), "cdr10"))
#     }
#   })
# }
#
# split("/srv/OASIS_DATA/data_augment_test_more_cut")
# split("/srv/OASIS_DATA/data_augment_train_more_cut")
# split("/srv/OASIS_DATA/data_augment_valid_more_cut")


path_data <- "/srv/OASIS_DATA/"
list_model <- list.dirs(file.path(path_data, "modeles_colab"),recursive = FALSE)
list_history <- list.files(file.path(path_data, "modeles_colab"), pattern = "history.csv", recursive = TRUE, full.names = TRUE)
list_history <- list_history[grepl(list_history, pattern = "3_class")]

hist <- rbindlist(map(list_history, function(x) {
  hist <- read.csv2(x, header = TRUE, sep = ",", dec = ".")
  setDT(hist)
  setnames(hist, "X", "epoch")
  hist[, model := str_split(dirname(x), "/")[[1]][[6]]]
}))
hist <- hist[model %in% c("vgg_2e5_3_class_run2"), epoch := epoch + 150]
hist <- hist[model %in% c("vgg_2e5_3_class_run3"), epoch := epoch + 250]
hist <- hist[model %in% c("vgg_2e5_3_class_run4"), epoch := epoch + 350]

ggplot(hist) + geom_point(aes(x = epoch, y = categorical_accuracy , col =model)) + geom_line(aes(x = epoch, y = val_categorical_accuracy, col =model)) +
  labs(x = "Epoch", y = "Categorical Accuracy", col = "Modèle") + ggtitle("VGG16 - 3 classes - Points : training - Lignes : validation")

ggplot(hist) + geom_point(aes(x = epoch, y = loss , col =model)) + geom_line(aes(x = epoch, y = val_loss, col =model)) +
  labs(x = "Epoch", y = "Loss", col = "Modèle") +  ggtitle("VGG16 - 3 classes - Points : training - Lignes : validation")


ggplot(hist) + geom_point(aes(x = epoch, y = auc , col =model)) + geom_line(aes(x = epoch, y = val_auc, col =model)) +
  labs(x = "Epoch", y = "AUC", col = "Modèle") +  ggtitle("VGG16 - 3 classes - Points : training - Lignes : validation")

ggplot(hist) + geom_point(aes(x = epoch, y = precision , col =model)) + geom_line(aes(x = epoch, y = val_precision, col =model)) +
  labs(x = "Epoch", y = "Precision", col = "Modèle") +  ggtitle("VGG16 - 3 classes - Points : training - Lignes : validation")

ggplot(hist) + geom_point(aes(x = epoch, y = recall , col =model)) + geom_line(aes(x = epoch, y = val_recall, col =model)) +
  labs(x = "Epoch", y = "Recall", col = "Modèle") +  ggtitle("VGG16 - 3 classes - Points : training - Lignes : validation")





path_data <- "/srv/OASIS_DATA/"
list_model <- list.dirs(file.path(path_data, "modeles_colab"),recursive = FALSE)
list_history <- list.files(file.path(path_data, "modeles_colab"), pattern = "history.csv", recursive = TRUE, full.names = TRUE)
list_history <- list_history[grepl(list_history, pattern = "3_class")]

hist <- rbindlist(map(list_history, function(x) {
  hist <- read.csv2(x, header = TRUE, sep = ",", dec = ".")
  setDT(hist)
  setnames(hist, "X", "epoch")
  hist[, model := str_split(dirname(x), "/")[[1]][[6]]]
}))
hist <- hist[model %in% c("vgg_2e5_3_class_run2"), epoch := epoch + 150]
hist <- hist[model %in% c("vgg_2e5_3_class_run3"), epoch := epoch + 250]
hist <- hist[model %in% c("vgg_2e5_3_class_run4"), epoch := epoch + 350]
hist <- hist[model %in% c("vgg_3block_3_class_ft"), epoch := epoch + 450]
hist <- hist[model %in% c("vgg_3block_3_class_ft_run2"), epoch := epoch + 600]
hist <- hist[model %in% c("vgg_3block_3_class_ft_run3"), epoch := epoch + 670]

hist[model %in% c("vgg_2e5_3_class", "vgg_2e5_3_class_run2", "vgg_2e5_3_class_run3", "vgg_2e5_3_class_run4"),set := "Extract-feature"]
hist[model %in% c("vgg_3block_3_class_ft", "vgg_3block_3_class_ft_run2", "vgg_3block_3_class_ft_run3"),set := "Fine-tuning"]

ggplot(hist) + geom_point(aes(x = epoch, y = categorical_accuracy , col =set)) + geom_line(aes(x = epoch, y = val_categorical_accuracy, col =set)) +
  labs(x = "Epoch", y = "Categorical Accuracy", col = "Stratégie") + ggtitle("VGG16 - 3 classes - Points : training - Lignes : validation")

ggplot(hist) + geom_point(aes(x = epoch, y = loss , col =set)) + geom_line(aes(x = epoch, y = val_loss, col =set)) +
  labs(x = "Epoch", y = "Loss", col = "Stratégie") +  ggtitle("VGG16 - 3 classes - Points : training - Lignes : validation")



#plot matrice de confusion
library(cvms)
library(broom)    # tidy()
library(tibble)   # tibble()
library(ggimage)
library(rsvg)
library(ggnewscale)
library(data.table)

set.seed(1)

basic_table <- table(target = c(0, 1), prediction = c(1, 0))
#irm 2 classes
basic_table[1,1] <- 3746
basic_table[1,2] <- 105
basic_table[2,2] <- 1077
basic_table[2,1] <- 297

#patient 2 classes
basic_table[1,1] <- 548
basic_table[1,2] <- 16
basic_table[2,2] <- 179
basic_table[2,1] <- 54

#free surfer 2 classes
basic_table[1,1] <- 280
basic_table[1,2] <- 31
basic_table[2,2] <- 47
basic_table[2,1] <- 31

cfm <- tidy(basic_table)

plot_confusion_matrix(cfm,
                      target_col = "target",
                      prediction_col = "prediction",
                      counts_col = "n",
                      add_sums = TRUE,
                      sums_settings = sum_tile_settings(
                        palette = "Greens",
                        label = "Total",
                        tc_tile_border_color = "black"
                      ))

cm <- copy(basic_table)
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
precision = diag / colsums
recall = diag / rowsums
f1 = 2 * precision * recall / (precision + recall)
data.frame(precision, recall, f1)




basic_table <- table(target = c(0, 1, 2), prediction = c(2, 1, 0))
#irm 2 classes
basic_table[1,1] <- 3687
basic_table[1,2] <- 65
basic_table[1,3] <- 10
basic_table[2,1] <- 328
basic_table[2,2] <- 764
basic_table[2,3] <- 10
basic_table[3,1] <- 28
basic_table[3,2] <- 5
basic_table[3,3] <- 328

cfm <- tidy(basic_table)

plot_confusion_matrix(cfm,
                      target_col = "target",
                      prediction_col = "prediction",
                      counts_col = "n",
                      add_sums = TRUE,
                      sums_settings = sum_tile_settings(
                        palette = "Greens",
                        label = "Total",
                        tc_tile_border_color = "black"
                      ))

cm <- copy(basic_table)
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes
accuracy = sum(diag) / n
precision = diag / colsums
recall = diag / rowsums
f1 = 2 * precision * recall / (precision + recall)
data.frame(precision, recall, f1)


res <- data.table(model= c('Patient', "IRM", "FS"),
                  Accuracy = c(0.9097, 0.9230, 0.8406),
                  Precision = c(0.9117, 0.91116, 0.6025),
                  Rappel = c(0.759, 0.7838, 0.6025))
melt(res, id.vars=c("model"))

ggplot(melt(res, id.vars=c("model")), aes(x = variable, y = value, fill = factor(model) ))+ geom_bar(colour = "black", stat="identity", position=position_dodge()) +
  geom_label(aes(label = 100*round(value, 3)), position=position_dodge(.9), show.legend = FALSE, size = 5) +  scale_fill_discrete(labels = c("FreeSurfer", "IRM", "Patient")) + labs(x = "Métrique", y = "Score [%]", fill= "Modèle") + scale_fill_brewer(palette="Blues")

fs <- dementiaproject::loadRData("/home/chetbo/GENERIC/dementiaproject/inst/extdata/oasis3/freesurfer_data_finale.Rdata")
str(fs)
fs_melt <- melt(fs[, .(Session,cdr3_ref , WholeBrainVolume, CortexVol, TotalVentricularVolume, TOTAL_HIPPOCAMPUS_VOLUME, CorticalWhiteMatterVol, SubCortGrayVol)]
, id.vars=c("Session", "cdr3_ref"))
fs_melt[variable == "WholeBrainVolume",variable := "Cérébral"]
fs_melt[variable == "CortexVol",variable := "Cortex"]
fs_melt[variable == "TotalVentricularVolume",variable := "Ventricules"]
fs_melt[variable == "TOTAL_HIPPOCAMPUS_VOLUME",variable := "Hyppocampe"]
fs_melt[variable == "CorticalWhiteMatterVol",variable := "Substance blanche"]
fs_melt[variable == "SubCortGrayVol",variable := "Substance grise"]
fs_melt[variable == "Substance grise" & value > 70000, value := NA]
fs_melt[variable == "Ventricules" & value > 130000, value := NA]
fs_melt[variable == "Cérébral" & value > 1300000, value := NA]

ggplot(na.omit(fs_melt), aes(x = cdr3_ref, y = value, fill = factor(cdr3_ref) ))+ geom_boxplot(outlier.shape = NA) + facet_wrap(~variable, scales = "free") +
  scale_fill_brewer(palette="Blues", labels = c("Absence de démence", "Troubles incertains", "Troubles bénins, modérés ou sévères")) + theme(axis.text.x = element_blank()) + theme(legend.position = "bottom") +
  labs(x = "Démence", y = "Volume", fill= "CDR : ")
# IntraCranialVol
# CortexVol
# SubCortGrayVol
# TotalGrayVol
# CorticalWhiteMatterVol
# SupraTentorialVol


