library(tidyverse)
library(jpeg)
library(png)
library(radiomics)

setwd("C:/Users/Mimi/Documents/Research/BDSI/training_set")
jpegs <- list.files(pattern = ".jpg")
my.jpeg <- jpegs[1:40]
pngs <- list.files(pattern = ".png")
my.png <- pngs[1:40]
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

lesion.list <- lapply(1:40, function(i) remove.bg(gray.list[[i]], png.list[[i]]))

glcm_list <- lapply(X=lesion.list, FUN=glcm, angle=0, d=1, n_grey=4, normalize=FALSE)
glrlm_list <- lapply(X=lesion.list, FUN=glrlm, angle=0, n_grey=4)
glszm_list <- lappy(X=lesion.list, FUN=glszm,n_grey=5)
mglszm_list <- lapply(X=lesion.list, FUN=mglszm)

glcm_feat_list <- lapply(glcm_list, calc_features)
glrlm_feat_list <- lapply(glrlm_list, calc_features)
glszm_feat_list <- lapply(glszm_list, calc_features)
mglszm_feat_list <- lapply(mglszm_list, calc_features)