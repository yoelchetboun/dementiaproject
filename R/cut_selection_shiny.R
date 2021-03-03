#' A partir d'un repertoire contenant l'ensemble des png, selectionne les coupes et les copies dans path_output
#'
#' @param path_raw Repertoire où sont stockées l'ensemble des coupes
#' @param path_selected Repertoire où vont etre stocké les coupes selectionnées
#' @param path_processed Repertoire où vont etre stocké les coupes selectionnées et pré-proccessées
#' @param cut_list Liste des slices à selectionner
#' @param process Bool si nous devons preprocesser ou pas les coupes selectionnées
#'
#' @import data.table
#' @importFrom purrr map
#' @importFrom stats runif
#' @importFrom EBImage readImage writeImage imageData
#'
#' @return Retourne un datatable contenant les infos sur les coupes selectionnees
#' @export
#'
cut_selection_shiny <- function(path_raw, path_selected, path_processed, cut_list = c(106), process = FALSE) {

  subject <- NULL
  name_old <- NULL
  nb_days_entry_mri <- NULL
  MR.ID <- NULL
  new_name <- NULL
  cdr_ref <- NULL
  dementia <- NULL

  print(paste0("Lecture du dossier : ", path_raw))
  list_png <- unlist(map(formatC(cut_list, digits = 2, flag = "0", mode = "integer"), ~list.files(path_raw, pattern = paste0("z", ., ".png") , full.names = TRUE, recursive = TRUE)))

  print(paste0("Copie des fichiers selectionnes dans : ", path_selected))
  file.remove(list.files(path_selected, pattern = ".png", full.names = TRUE))
  map(list_png, ~file.copy(from = ., to = path_selected))

  #pre-process

  if (process == TRUE) {
    print(paste0("Pre-process des coupes selectionnees dans : ", path_processed))
    list_png <- list.files(path_selected, pattern = ".png", full.names = TRUE, recursive = TRUE)

    #resize each picture in 256x256
    width_max <- 256
    height_max <- 256

    #à mettre dans une nouvelle fonction
    file.remove(list.files(path_processed, pattern = ".png", full.names = TRUE))

    #à mettre dans une fonction
    map(list_png, function(x) {

      #print(paste0(x, "/", length(dataset$new_name)))
      img <- EBImage::readImage(x)
      width <- dim(img)[1]
      heigth <- dim(img)[2]
      pixel2add_width <- width_max - width
      pixel2add_height <- height_max - heigth
      px2add_w_l <- round(pixel2add_width/2)
      px2add_w_r <- round(pixel2add_width/2) + pixel2add_width%%2
      px2add_h_u <- pixel2add_height/2
      px2add_h_b <- round(pixel2add_height/2) + pixel2add_height%%2

      #width extension
      data2add <- rep(EBImage::imageData(img)[1,], times = px2add_w_l) + runif(length(EBImage::imageData(img)[1,])*px2add_w_l, 0, 0.01)
      data2add[data2add > 0.01] <-   data2add[data2add > 0.01]/100 #si jamais on a des pixel blanc qui trainent
      EBImage::imageData(img) <- rbind(EBImage::imageData(img), matrix(data = data2add, nrow = px2add_w_l, ncol = heigth))
      data2add <- rep(EBImage::imageData(img)[-1,], times = px2add_w_r) + runif(length(EBImage::imageData(img)[-1,])*px2add_w_r, 0, 0.01)
      data2add[data2add > 0.01] <-   data2add[data2add > 0.01]/100 #si jamais on a des pixel blanc qui trainent
      EBImage::imageData(img) <-rbind(matrix(data = data2add, nrow = px2add_w_r, ncol = heigth), EBImage::imageData(img))

      #heigth extension
      width <- dim(img)[1]
      data2add <- rep(EBImage::imageData(img)[,1], times = px2add_h_u) + runif(length(EBImage::imageData(img)[,1])*px2add_h_u, 0, 0.01)
      data2add[data2add > 0.01] <-   data2add[data2add > 0.01]/10 #si jamais on a des pixel blanc qui trainent
      EBImage::imageData(img) <- cbind(EBImage::imageData(img), matrix(data = data2add, nrow = width, ncol = px2add_h_u))
      data2add <- rep(EBImage::imageData(img)[-1,], times = px2add_h_b) + runif(length(EBImage::imageData(img)[-1,])*px2add_h_b, 0, 0.01)
      data2add[data2add > 0.01] <-   data2add[data2add > 0.01]/10 #si jamais on a des pixel blanc qui trainent
      EBImage::imageData(img) <- cbind(matrix(data = data2add, nrow = width, ncol = px2add_h_b), EBImage::imageData(img))

      EBImage::writeImage(img, file.path(path_processed, basename(x)))
    })

  }

}
