#' Data augmentation d'un repertoire donnee
#'
#' @param nb_target nombre d'image cible
#' @param data_ref table de reference des images.
#' @param path_source Chemin source des images
#' @param path_dest Chemin destination images augmentees
#' @param cat2augment Categorie a augmenter
#' @param translat Coef de translation max
#' @param max_zoom Coef de zoom max
#' @param max_angle Coef de rotation max
#'
#' @import pbapply
#' @import data.table
#' @import EBImage
#' @import OpenImageR
#'
#' @return
#' @export
#'
#'
data_augment <- function(nb_target, path_source, path_dest, data_ref, cat2augment, translat = 1.1, max_zoom = 1.1, max_angle = 5) {
  #translat = 1.1, max_zoom = 1.1, max_angle = 5
  cdr_ref <- NULL
  new_name <- NULL

  data2augment <- copy(data_ref[cdr_ref == cat2augment])

  if (nb_target <= nrow(data2augment)) {
    #on selectionne nb_target ligne aleatoirement et on les copies dans path_dest
    new_data_ref <- copy(data2augment[sample(nrow(data2augment), nb_target),])
    list_files_2_mv <- file.path(path_source,  data2augment[new_name %in% new_data_ref$new_name,]$new_name)
    map(seq(1,length(list_files_2_mv), 1), function(x) {
      file.copy(from = list_files_2_mv[x], to = path_dest)
    })
    #on return le data_ref filtree sur la cat et les images selectionnees
    return(new_data_ref)
  } else {

    #on copie d'abord toutes celles qu'on a
    list_files_2_mv <- file.path(path_source,  data2augment$new_name)
    map(seq(1,length(list_files_2_mv), 1), function(x) {
      file.copy(from = list_files_2_mv[x], to = path_dest)
    })

    nb_2_generate <- nb_target - nrow(data2augment)
    #generer une nouvelle config a partir de data_ref
    nb_files_dest <- length(list.files(path_dest, pattern = ".png"))

    data_generated <- rbindlist(map(seq(1, nb_2_generate, 1), function(x) {
      source <- data2augment[sample(nrow(data2augment), 1)]$new_name
      method <- sample(1:4, 1)
      img <- EBImage::readImage(file.path(path_source, source))
      new_img_name <- paste0("mri_picture_augmented_", nb_files_dest+x, ".png")
      dim_img <- dim(img)[1]

      if (method == 1) {
        #c'est un shift
        shift_rows <- sample(-translat:translat, 1)
        shift_cols <- sample(-translat:translat, 1)
        out <- Augmentation(imageData(img), shift_rows = shift_rows, shift_cols = shift_cols)
        writeImage(out, file.path(path_dest, new_img_name))
      }
      if (method == 2) {
        #c'est un zoom
        resiz_width <- round(dim_img*runif(1, min = 1, max = max_zoom))
        resiz_height <- round(dim_img*runif(1, min = 1, max = max_zoom))
        out <- Augmentation(imageData(img), resiz_width = resiz_width, resiz_height= resiz_height)
        width_decal <-  round((dim(out)[1] - dim_img)/2)
        height_decal <-  round((dim(out)[2] -dim_img)/2)
        if(width_decal !=0 & height_decal != 0){
          out <- Augmentation(imageData(out), crop_width = seq((1+width_decal), (dim_img+width_decal), 1), crop_height = seq((1+height_decal), dim_img+height_decal)) #crop centre
        }
        if(width_decal ==0 & height_decal != 0){
          out <- Augmentation(imageData(out), crop_height = seq((1+height_decal), dim_img+height_decal)) #crop centre
        }
        if(width_decal !=0 & height_decal == 0){
          out <- Augmentation(imageData(out), crop_width = seq((1+width_decal), (dim_img+width_decal), 1)) #crop centre
        }
        writeImage(out, file.path(path_dest, new_img_name))
      }
      if (method == 3) {
        #c'est un dezoom
        dezoom_factor <- runif(1, min = 1, max = max_zoom)
        out <- down_sample_image(image = imageData(img), factor = dezoom_factor, gaussian_blur =  F)
        out <- extend_image(out, width_max = dim_img, height_max = dim_img)
        writeImage(out, file.path(path_dest, new_img_name))
      }
      if (method == 4) {
        rotate_angle <- sample(-max_angle:max_angle, 1) %% 360
        out <- Augmentation(imageData(img), rotate_angle = rotate_angle, verbose = FALSE, rotate_method = "bilinear") #pas terrrible
        writeImage(out, file.path(path_dest, new_img_name))
      }

      print(paste0("Nouvelle image creee : ", new_img_name, " a partir de : ", source, ". Methode : ", method))
      #add brightness, contrast and gamma ?
      return(data.table(source_img = source, new_img = new_img_name, method = method))


      }))
    data_generated <- merge(data_generated, data2augment , by.x = "source_img", by.y = "new_name", all.x = TRUE)
    data_generated$name_old <- NULL
    data_generated$method <- NULL
    setnames(data_generated, c("source_img", "new_img"), c("name_old", "new_name"))
    data_generated <- rbind(data2augment, data_generated)

    return(data_generated)

  }


}
