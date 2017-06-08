library(tidyverse)
library(jpeg)
library(png)
library(radiomics)

setwd("C:/Users/Mimi/Documents/Research/BDSI/training_set")
jpegs <- list.files(pattern = ".jpg")
my.jpeg <- jpegs[1:40]
pngs <- list.files(pattern = ".png")
my.png <- pngs[1:40]
jpeg_list <- lapply(my.jpeg, readJPEG, native = FALSE)
png_list <- lapply(my.png, readPNG, native = FALSE)

grayscale <- function(input.img){
  gray.img <- input.img[,,1]*0.21 + input.img[,,2]*0.72 + input.img[,,3]*0.07
}

gray.list <- lapply(jpeg_list, grayscale)

remove_bg <- function(input.img, input.seg){
  is.na(input.seg) <- input.seg == 0
  lesion.image <- input.img * input.seg
}

lesion.list <- lapply(1:40, function(i) remove_bg(gray.list[[i]], png_list[[i]]))
