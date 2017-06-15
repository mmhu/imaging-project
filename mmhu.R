library(tidyverse)
library(jpeg)
library(png)
library(radiomics)

setwd("C:/Users/Mimi/Documents/Research/BDSI/training_set")
jpegs <- list.files(pattern = ".jpg")
my.jpeg <- jpegs[1:30]
pngs <- list.files(pattern = ".png")
my.png <- pngs[1:30]
jpeg.list <- lapply(my.jpeg, readJPEG, native = FALSE)
png.list <- lapply(my.png, readPNG, native = FALSE)

grayscale <- function(input.img) {
  gray.img <- input.img[,,1]*0.21 + input.img[,,2]*0.72 + input.img[,,3]*0.07
}

gray.list <- lapply(jpeg.list, grayscale)

remove.bg <- function(input.img, input.seg) {
  is.na(input.seg) <- input.seg == 0
  lesion.image <- input.img * input.seg
}

lesion.list <- lapply(1:30, function(i) remove.bg(gray.list[[i]], png.list[[i]]))

glcm.list <- lapply(lesion.list, glcm, angle = 0, d = 1, n_grey = 4, normalize = FALSE)
glrlm.list <- lapply(lesion.list, glrlm, angle = 0, n_grey = 4)
glszm.list <- lapply(lesion.list, glszm, n_grey = 5)
# mglszm.list <- lapply(lesion.list, mglszm)

glcm.feat.list <- lapply(glcm.list, calc_features)
glrlm.feat.list <- lapply(glrlm.list, calc_features)
glszm.feat.list <- lapply(glszm.list, calc_features)
# mglszm.feat.list <- lapply(mglszm.list, calc_features)

glcm.matrix <- matrix(unlist(glcm.feat.list),nrow=10,byrow=T)
glrlm.matrix <- matrix(unlist(glrlm.feat.list),nrow=10,byrow=T)
glszm.matrix <- matrix(unlist(glszm.feat.list),nrow=10,byrow=T)

features <- cbind(glcm.matrix, glrlm.matrix)
features <- cbind(features, glszm.matrix)
features <- cbind(my.jpeg, features)

write.table(features, file="feature_list.csv", sep=",", col.names=FALSE)