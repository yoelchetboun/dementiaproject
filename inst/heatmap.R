library(keras)
library(tensorflow)
library(grid)         # for producing section 1 & 2 images
library(gridExtra)    # for producing section 1 & 2 images
library(magick)       # for producing section 3 images
library(viridis)      # for producing section 3 images
library(lime)         # for producing section 3 images

tf$compat$v1$disable_eager_execution()

path_data <- "/srv/OASIS_DATA/"

file_local <- file.path(path_data, "data_base_shiny/models/2_class_irm", "vgg_2e5_more_cut_3block_run2.h5")
model <- load_model_hdf5(file_local)
model

# Preprocesses the image into a 4D tensor
img_path <- file.path("/srv/OASIS_DATA/", "data_augment_valid_more_cut/dementia", "mri_picture_01004.png")
img_path <- file.path("/srv/OASIS_DATA/", "data_augment_valid_more_cut/non_dementia", "mri_picture_01298.png")

img <- image_load(img_path, target_size = c(224, 160))

img_tensor <- image_to_array(img)
img_tensor <- array_reshape(img_tensor, c(1, 224, 160, 3))
img_tensor <- aperm(img_tensor, c(1,3,2,4)) # Reorder dimensions

img_tensor <- img_tensor / 255

dim(img_tensor)
plot(as.raster(img_tensor[1,,,]))
EBImage::display(img_tensor[1,,,])


layer_outputs <- lapply(model$layers[2:19], function(layer) layer$output) #19 layers de conv
activation_model <- keras_model(inputs = model$input, outputs = layer_outputs)
activations <- activation_model %>% predict(img_tensor)
first_layer_activation <- activations[[1]]
dim(first_layer_activation)
plot_channel <- function(channel) {
  rotate <- function(x) t(apply(x, 2, rev))
  image(rotate(channel), axes = FALSE, asp = 1,
        col = terrain.colors(12))
}
plot_channel(first_layer_activation[1,,,20])
plot_channel(first_layer_activation[1,,,17])
plot_channel(first_layer_activation[1,,,64])

# dir.create("brain_activations")
# image_size <- 58
# images_per_row <- 16
# for (i in 1:8) {
#
#   layer_activation <- activations[[i]]
#   layer_name <- model$layers[[i]]$name
#
#   n_features <- dim(layer_activation)[[4]]
#   n_cols <- n_features %/% images_per_row
#
#   png(paste0("brain_activations/", i, "_", layer_name, ".png"),
#       width = image_size * images_per_row,
#       height = image_size * n_cols)
#   op <- par(mfrow = c(n_cols, images_per_row), mai = rep_len(0.02, 4))
#
#   for (col in 0:(n_cols - 1)) {
#     for (row in 0:(images_per_row - 1)) {
#       channel_image <- layer_activation[1,,,(col*images_per_row) + row + 1]
#       plot_channel(channel_image)
#     }
#   }
#
#   par(op)
#   dev.off()
# }

test <- function(layer_name) {

# This is the prediction vector for dementia
last_conv_layer <- model %>% get_layer(layer_name)
brain_output <- model$output[, 1]
grads <- k_gradients(brain_output, last_conv_layer$output)[[1]]
pooled_grads <- k_mean(grads, axis = c(1, 2, 3))

# This function allows us to access the values of the quantities we just defined:
# `pooled_grads` and the output feature map of `conv2d_3`,
# given a sample image
iterate <- k_function(list(model$input),
                      list(pooled_grads, last_conv_layer$output[1,,,]))

n_conv <- length(pooled_grads)

# These are the values of these two quantities, as arrays,
# given our sample image
c(pooled_grads_value, conv_layer_output_value) %<-% iterate(list(img_tensor))

# We multiply each channel in the feature map array by
# "how important this channel is" with regard to the dog class
for (i in 1:n_conv) {
  conv_layer_output_value[,,i] <-
    conv_layer_output_value[,,i] * pooled_grads_value[[i]]
}

# The channel-wise mean of the resulting feature map
# is our heatmap of class activation
heatmap <- apply(conv_layer_output_value, c(1,2), mean)
heatmap <- pmax(heatmap, 0)
heatmap <- heatmap / max(heatmap)
write_heatmap <- function(heatmap, filename, width = 150, height = 150,
                          bg = "white", col = terrain.colors(12)) {
  png(filename, width = width, height = height, bg = bg)
  op = par(mar = c(0,0,0,0))
  on.exit({par(op); dev.off()}, add = TRUE)
  rotate <- function(x) t(apply(x, 2, rev))
  image(rotate(heatmap), axes = FALSE, asp = 1, col = col)
}
write_heatmap(heatmap, "non_dementia_heatmap.png")

library(magick)
library(viridis)
# Read the original elephant image and it's geometry
image <- image_read(img_path)
info <- image_info(image)
geometry <- sprintf("%dx%d!", info$width, info$height)
# Create a blended / transparent version of the heatmap image
pal <- col2rgb(viridis(20), alpha = TRUE)
alpha <- floor(seq(0, 255, length = ncol(pal)))
pal_col <- rgb(t(pal), alpha = alpha, maxColorValue = 255)
write_heatmap(heatmap, "non_dementia_overlay.png",
              width = 14, height = 14, bg = NA, col = pal_col)
# Overlay the heatmap
image_read("non_dementia_overlay.png") %>%
  image_resize(geometry, filter = "quadratic") %>%
  image_composite(image, operator = "blend", compose_args = "20") %>%
  plot()

}

purrr::map(c("block1_conv1", "block1_conv2",
             "block2_conv1", "block2_conv2", "block3_conv1", "block3_conv2" , "block3_conv3",
             "block4_conv1" , "block4_conv2" , "block4_conv3", "block5_conv1", "block5_conv2"  , "block5_conv3"), ~test(.) )

