#' Extrait les features d'une image unique
#'
#' @param path_img Chemin absolu de l'image
#' @param width Largeur cible
#' @param height Hauteur cible
#'
#' @import pbapply
#' @import EBImage
#'
#' @return retourne un vecteur
#' @export
#'
feature_list <- function(path_img, width = 254, height = 254) {
  ## Read image
  img <- readImage(path_img)
  ## Resize image
  img_resized <- resize(img, w = width, h = height)
  ## Get the image as a matrix
  img_matrix <- img_resized@.Data
  ## Coerce to a vector (row-wise)
  img_vector <- as.vector(t(img_matrix))
  return(img_vector)
}
