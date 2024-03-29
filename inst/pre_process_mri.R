#install.packages("BiocManager")
#install.packages("EBImage") #sudo apt-get install libfftw3-dev pour installer la library fftw necessaire
#BiocManager::install("EBImage")

library(keras)
library(EBImage)
library(stringr)
library(pbapply)
library(OpenImageR)

#select 4 png pour chaque MRI sessions / anat
path_root <- "~/GENERIC/dementiaproject/"
path_data <- "/srv/OASIS_DATA/oasis3/"

# To do only one time
list_png <- unlist(map(seq(135, 150, 5), ~list.files(path_data, pattern = paste0("z", ., ".png") , full.names = TRUE, recursive = TRUE)))
length(list_png) #11 100
map(list_png, ~file.copy(from = ., to = "/srv/OASIS_DATA/dataset"))

#pre-process
path_dataset <- "/srv/OASIS_DATA/dataset"
list_png <- list.files(path_dataset, pattern = ".png", full.names = TRUE, recursive = TRUE)
dataset <- data.table(name_old = basename(list_png))
dataset[, subject := sub(pattern = "sub-", replacement = "", strsplit(name_old, split = "_")[[1]][[1]]), by = name_old]
dataset[, nb_days_entry_mri := as.integer(sub(pattern = "ses-d", replacement = "", strsplit(name_old, split = "_")[[1]][[2]])), by = name_old]
dataset[, MR.ID := paste0(subject, "_MR_", sub(pattern = "ses-", replacement = "", strsplit(name_old, split = "_")[[1]][[2]])), by = name_old]

mri_adrc_join <- loadRData(file.path(path_root, "inst/extdata/oasis3/mri_adrc_join.Rdata"))
dataset <- merge(dataset, mri_adrc_join, by = "MR.ID", all.x = TRUE)

#dataset est une table de ref des fichiers images extraits
dataset[, new_name := paste0("mri_picture_", formatC(.I,flag=0,width=5), ".png")]
#save(dataset, file =  file.path(path_root, "inst/extdata/oasis3/dataset.Rdata"))

file.rename(from = file.path(path_dataset, dataset$name_old), to = file.path(path_dataset, dataset$new_name))

dataset$width <- unlist(map(dataset$new_name, function(x) {
  dim(readImage(file.path(path_dataset, x)))[1]
}))

dataset$height <- unlist(map(dataset$new_name, function(x) {
  dim(readImage(file.path(path_dataset, x)))[2]
}))

#dim min = 160 x 224
#dim max = 176 x 256

#resize each picture in 256x256
width_max <- 256
height_max <- 256

map(seq(1,length(dataset$new_name), 1), function(x) {
  print(paste0(x, "/", length(dataset$new_name)))
  file_name <- dataset$new_name[x]

  img <- readImage(file.path(path_dataset, file_name))
  width <- dim(img)[1]
  heigth <- dim(img)[2]
  pixel2add_width <- width_max - width
  pixel2add_height <- height_max - heigth
  px2add_w_l <- round(pixel2add_width/2)
  px2add_w_r <- round(pixel2add_width/2) + pixel2add_width%%2
  px2add_h_u <- pixel2add_height/2
  px2add_h_b <- round(pixel2add_height/2) + pixel2add_height%%2

  #width extension
  data2add <- rep(imageData(img)[1,], times = px2add_w_l) + runif(length(imageData(img)[1,])*px2add_w_l, 0, 0.01)
  data2add[data2add > 0.01] <-   data2add[data2add > 0.01]/100 #si jamais on a des pixel blanc qui trainent
  imageData(img) <- rbind(imageData(img), matrix(data = data2add, nrow = px2add_w_l, ncol = heigth))
  data2add <- rep(imageData(img)[-1,], times = px2add_w_r) + runif(length(imageData(img)[-1,])*px2add_w_r, 0, 0.01)
  data2add[data2add > 0.01] <-   data2add[data2add > 0.01]/100 #si jamais on a des pixel blanc qui trainent
  imageData(img) <-rbind(matrix(data = data2add, nrow = px2add_w_r, ncol = heigth), imageData(img))
  #display(img)

  #heigth extension
  width <- dim(img)[1]
  data2add <- rep(imageData(img)[,1], times = px2add_h_u) + runif(length(imageData(img)[,1])*px2add_h_u, 0, 0.01)
  data2add[data2add > 0.01] <-   data2add[data2add > 0.01]/10 #si jamais on a des pixel blanc qui trainent
  imageData(img) <- cbind(imageData(img), matrix(data = data2add, nrow = width, ncol = px2add_h_u))
  data2add <- rep(imageData(img)[-1,], times = px2add_h_b) + runif(length(imageData(img)[-1,])*px2add_h_b, 0, 0.01)
  data2add[data2add > 0.01] <-   data2add[data2add > 0.01]/10 #si jamais on a des pixel blanc qui trainent
  imageData(img) <- cbind(matrix(data = data2add, nrow = width, ncol = px2add_h_b), imageData(img))
  #display(img)

  writeImage(img, file.path(paste0(path_dataset, "_processed"), file_name))
})


dataset$width_new <- unlist(map(dataset$new_name, function(x) {
  dim(readImage(file.path(paste0(path_dataset, "_processed"), x)))[1]
}))

dataset$height_new <- unlist(map(dataset$new_name, function(x) {
  dim(readImage(file.path(paste0(path_dataset, "_processed"), x)))[2]
}))

dataset[width_new != 256 | height_new != 256]
dataset[cdr_ref != "0.0", dementia := TRUE]
dataset[cdr_ref == "0.0", dementia := FALSE]

#save(dataset, file =  file.path(path_root, "inst/extdata/oasis3/dataset.Rdata"))

#test data augmentation

path_root <- "~/GENERIC/dementiaproject/"
path_data <- "/srv/OASIS_DATA/"
ls_png <- list.files(path = file.path(path_data, "data_processed"), pattern = ".png", full.names = TRUE)
img_test <- ls_png[3]
img_test <- EBImage::readImage(img_test)
dim(img_test)
EBImage::display(img_test)

out <- Augmentation(imageData(img_test), rotate_angle = 10, verbose = TRUE, rotate_method = "bilinear") #pas terrrible
out <- rotateFixed(imageData(img_test), angle = 90)
out <- Augmentation(imageData(img_test), shift_rows = 10, verbose = TRUE) #shift vers la gauche ok
out <- Augmentation(imageData(img_test), shift_rows = -10, verbose = TRUE) #shift vers la droite ok
out <- Augmentation(imageData(img_test), resiz_width = 250, verbose = TRUE) #resize en largeur ok
out <- Augmentation(imageData(img_test), resiz_height = 300, verbose = TRUE) #resize en hauteur ok pour zoomer puis apres cropper
out <- Augmentation(imageData(out), crop_height = seq(1:200), verbose = TRUE) #crop en hauteur (on coupe le bas)
out <- Augmentation(imageData(out), crop_width = seq(1:150), verbose = TRUE) #crop en largeur (on coupe à gauche)
out <- down_sample_image(image = imageData(img_test), factor = 1.1,gaussian_blur =  T) # dezoom  de 10% > on doit faire un resize apres...
out <- extend_image(out, width_max = 176, height_max = 256)

out <- imageData(img_test) + 0.2 #ajoute de la luminosité
out <- imageData(img_test) * 1.2 #ajoute du contraste
out <- imageData(img_test) ^ 1.2 #ajoute du contraste


EBImage::display(out)

#Whitening (or sphering) is the preprocessing needed for some algorithms.
# If we are training on images, the raw input is redundant, since adjacent pixel values are highly correlated.
# The purpose of whitening is to reduce the correlation between features and to return features with the same variance,


#recut_selection

path_data <- "/srv/OASIS_DATA/"
path_root <- "~/GENERIC/dementiaproject/"

path_raw <- file.path(path_data, "data_raw_t1")
path_selected <- file.path(path_data, "data_selected_more_cut")
path_processed <- file.path(path_data, "data_processed_more_cut")
dataset <- dementiaproject::cut_selection(path_raw, path_selected, path_processed, cut_list = c(136, 137, 138, 139, 140, 141, 142,143, 144, 145))



# lancer un script en arrière plan sous linux :
.rs.restartR()
rm(list=ls(all.names = TRUE))
gc(reset=TRUE, full = TRUE)
library(cronR)
myscript  = "/home/chetbo/GENERIC/dementiaproject/inst/launch_cnn.R"
file.remove("/home/chetbo/GENERIC/dementiaproject/inst/launch_cnn.log")
cmd <- cron_rscript(myscript)
cron_rm(id = "cnn_launch")
cron_add(cmd, frequency = '16 18 05 03 *', id = 'cnn_launch', description = 'cnn_launch')



