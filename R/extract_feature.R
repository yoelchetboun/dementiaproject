#' Extrait les features d'une repertoire contenant des images
#'
#' @param path_dir Repertoire contenant les images
#' @param width Largeur de l'image cible
#' @param height Hauteur de l'image cible
#' @param labelsExist Booléen si le label existe ou non
#' @param data_ref Table de reference du dataset
#' @param nb_class Nombre de classe
#' @param colormode Greyscale ou RGB (change le nombre de channel)
#'
#' @import pbapply
#' @import data.table
#'
#' @return Retourne une liste (X = feature_matrix, y = y)) si lalelsExist = T ou uniquement feature_matrix si lalelsExist = F
#' @export
#'
extract_feature <- function(path_dir, data_ref, width = 256, height = 256, labelsExist = T, nb_class = 2, colormode = "greyscale") {
  new_name <- NULL

  ## List images in path
  images_names <- list.files(path_dir, pattern = ".png", full.names = TRUE)
  img_size <- width * height

  if (labelsExist){
    if (nb_class == 2) {
      y <- data_ref[new_name %in% basename(images_names)]$dementia
    } else {
      y <- data_ref[new_name %in% basename(images_names)]$cdr_ref

    }
  }

  print(paste("Start processing", length(images_names), "images"))
  ## This function will resize an image, turn it into greyscale
  feature_list <- pblapply(images_names, feature_list, width = width, height = height, colormode = colormode)

  ## bind the list of vector into matrix
  feature_matrix <- do.call(rbind, feature_list)
  feature_matrix <- as.data.frame(feature_matrix)
  ## Set names
  names(feature_matrix) <- paste0("pixel", c(1:img_size))
  if(labelsExist){
    return(list(X = feature_matrix, y = y))
  }else{
    return(feature_matrix)
  }
}
