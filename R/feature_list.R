#' Extrait les features d'une image unique
#'
#' @param path_img Chemin absolu de l'image
#' @param width Largeur cible
#' @param height Hauteur cible
#' @param colormode Greyscale ou RGB (change le nombre de channel)
#'
#' @import pbapply
#' @importFrom EBImage readImage resize toRGB channel
#'
#' @return retourne un vecteur
#' @export
#'
feature_list <- function(path_img, width = 254, height = 254, colormode = "greyscale") {
  ## Read image
  img <- readImage(path_img)
  ## Resize image
  img_resized <- resize(img, w = width, h = height)

  if (colormode == "greyscale") {
    col_img <- channel(img_resized, "gray")

  } else {
    col_img <- toRGB(img_resized)
  }

  ## Get the image as a matrix
  img_matrix <- col_img@.Data
  ## Coerce to a vector (row-wise)
  img_vector <- as.vector(t(img_matrix))
  return(img_vector)
}
