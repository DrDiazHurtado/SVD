# Hey there, let's play around with some image processing using R!
# First, we'll load the necessary libraries.
library(raster)
library(jpeg)

# Now, let's create a simple matrix just for fun.
A <- matrix(1:12, nrow = 4, ncol = 3, byrow = TRUE)

# Time for some SVD magic!
matriz.svd <- svd(A)
D <- diag(matriz.svd$d)
U <- matriz.svd$u
V <- matriz.svd$v

# We've decomposed our matrix. Now, let's put it back together.
T <- U %*% D %*% t(V)

# Check out the residuals. How well did we do?
residuales <- T - A
residuales

# Now, let's apply SVD to an image of a cute dog.
imagen <- readJPEG("dog.jpg")

# Check out the image dimensions.
dim(imagen)

# Separate the image into its Red, Green, and Blue channels.
imagenR <- imagen[, , 1]
imagenG <- imagen[, , 2]
imagenB <- imagen[, , 3]

# Create a function to compress and save the image
compress_and_save <- function(channel, num_svd, filename) {
  svd_channel <- svd(channel)
  D <- diag(svd_channel$d[1:num_svd])
  U <- svd_channel$u[, 1:num_svd]
  V <- svd_channel$v[, 1:num_svd]
  compressed_channel <- U %*% D %*% t(V)
  nueva_imagen <- EBImage::rgbImage(red = compressed_channel, green = compressed_channel, blue = compressed_channel)
  jpeg::writeJPEG(nueva_imagen, filename)
}

# Let's compress and save the image with different numbers of singular values.
compress_and_save(imagenR, 10, "compressed_10.jpg")
compress_and_save(imagenR, 50, "compressed_50.jpg")
compress_and_save(imagenR, 100,"compressed_100.jpg")

# Voila! You've compressed and saved the image with different numbers of singular values.
