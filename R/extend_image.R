#' Extrait les features d'une image unique
#'
#' @param img Chemin absolu de l'image
#' @param width_max Largeur cible
#' @param height_max Hauteur cible
#'
#' @import pbapply
#' @importFrom EBImage readImage resize
#'
#' @return retourne un vecteur
#' @export
#'
extend_image <- function(img, width_max = 254, height_max = 254) {
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

  return(img)
}
