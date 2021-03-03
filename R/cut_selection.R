#' A partir d'un repertoire contenant l'ensemble des png, selectionne les coupes et les copies dans path_output
#'
#' @param path_raw Repertoire où sont stockées l'ensemble des coupes
#' @param path_selected Repertoire où vont etre stocké les coupes selectionnées
#' @param path_processed Repertoire où vont etre stocké les coupes selectionnées et pré-proccessées
#' @param cut_list Liste des slices à selectionner
#'
#' @import data.table
#' @importFrom purrr map
#' @importFrom stats runif
#' @importFrom EBImage readImage writeImage imageData
#'
#' @return Retourne un datatable contenant les infos sur les coupes selectionnees
#' @export
#'
cut_selection <- function(path_raw, path_selected, path_processed, cut_list = c(106)) {

  subject <- NULL
  name_old <- NULL
  nb_days_entry_mri <- NULL
  MR.ID <- NULL
  new_name <- NULL
  cdr_ref <- NULL
  dementia <- NULL

  # To do only one time
  print(paste0("Lecture du dossier : ", path_raw))
  list_png <- unlist(map(cut_list, ~list.files(path_raw, pattern = paste0("z", ., ".png") , full.names = TRUE, recursive = TRUE)))
  #length(list_png) #11 100

  #on vide les repertoires
  print(paste0("Copie des fichiers selectionnes dans : ", path_selected))
  file.remove(list.files(path_selected, pattern = ".png", full.names = TRUE))
  map(list_png, ~file.copy(from = ., to = path_selected))

  #pre-process
  print(paste0("Pre-process des coupes selectionnees dans : ", path_processed))
  list_png <- list.files(path_selected, pattern = ".png", full.names = TRUE, recursive = TRUE)
  dataset <- data.table(name_old = basename(list_png))
  dataset[, subject := sub(pattern = "sub-", replacement = "", strsplit(name_old, split = "_")[[1]][[1]]), by = name_old]
  dataset[, nb_days_entry_mri := as.integer(sub(pattern = "ses-d", replacement = "", strsplit(name_old, split = "_")[[1]][[2]])), by = name_old]
  dataset[, MR.ID := paste0(subject, "_MR_", sub(pattern = "ses-", replacement = "", strsplit(name_old, split = "_")[[1]][[2]])), by = name_old]

  #on charge la table mri_adrc_join du package
  mri_adrc_join <- dementiaproject::loadRData(system.file("extdata/oasis3/mri_adrc_join.Rdata", package = "dementiaproject"))
  dataset <- merge(dataset, mri_adrc_join, by = "MR.ID", all.x = TRUE)

  #dataset est une table de ref des fichiers images extraits
  dataset[, new_name := paste0("mri_picture_", formatC(.I,flag=0,width=5), ".png")]

  file.rename(from = file.path(path_selected, dataset$name_old), to = file.path(path_selected, dataset$new_name))

  dataset$width <- unlist(map(dataset$new_name, function(x) {
    dim(EBImage::readImage(file.path(path_selected, x)))[1]
  }))

  dataset$height <- unlist(map(dataset$new_name, function(x) {
    dim(EBImage::readImage(file.path(path_selected, x)))[2]
  }))

  #resize each picture in 256x256
  width_max <- 256
  height_max <- 256

  #à mettre dans une nouvelle fonction
  file.remove(list.files(path_processed, pattern = ".png", full.names = TRUE))
  file.remove(list.files(path_processed, pattern = ".Rdata", full.names = TRUE))

  #à mettre dans une fonction
  map(seq(1,length(dataset$new_name), 1), function(x) {

    #print(paste0(x, "/", length(dataset$new_name)))
    file_name <- dataset$new_name[x]

    img <- readImage(file.path(path_selected, file_name))
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

    #heigth extension
    width <- dim(img)[1]
    data2add <- rep(imageData(img)[,1], times = px2add_h_u) + runif(length(imageData(img)[,1])*px2add_h_u, 0, 0.01)
    data2add[data2add > 0.01] <-   data2add[data2add > 0.01]/10 #si jamais on a des pixel blanc qui trainent
    imageData(img) <- cbind(imageData(img), matrix(data = data2add, nrow = width, ncol = px2add_h_u))
    data2add <- rep(imageData(img)[-1,], times = px2add_h_b) + runif(length(imageData(img)[-1,])*px2add_h_b, 0, 0.01)
    data2add[data2add > 0.01] <-   data2add[data2add > 0.01]/10 #si jamais on a des pixel blanc qui trainent
    imageData(img) <- cbind(matrix(data = data2add, nrow = width, ncol = px2add_h_b), imageData(img))

    writeImage(img, file.path(path_processed, file_name))
  })

  dataset$width_new <- unlist(map(dataset$new_name, function(x) {
    dim(readImage(file.path(path_processed, x)))[1]
  }))

  dataset$height_new <- unlist(map(dataset$new_name, function(x) {
    dim(readImage(file.path(path_processed, x)))[2]
  }))

  dataset[cdr_ref != "0.0", dementia := TRUE]
  dataset[cdr_ref == "0.0", dementia := FALSE]
  print(paste0("Sauvegarde du dataset de ref dans : ", file.path(path_processed, "dataset.Rdata")))
  save(dataset, file =  file.path(path_processed, "dataset.Rdata"))

  return(dataset)
}
