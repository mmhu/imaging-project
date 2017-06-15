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

<<<<<<< HEAD
glcm.matrix <- matrix(unlist(glcm.feat.list),nrow=10,byrow=T)
glrlm.matrix <- matrix(unlist(glrlm.feat.list),nrow=10,byrow=T)
glszm.matrix <- matrix(unlist(glszm.feat.list),nrow=10,byrow=T)

features <- cbind(glcm.matrix, glrlm.matrix)
features <- cbind(features, glszm.matrix)
features <- cbind(my.jpeg, features)

write.table(features, file="feature_list.csv", sep=",", col.names=FALSE)
=======
glcm.feat <- data_frame(glcm.feat.list) %>% unnest
glrlm.feat <- data_frame(glrlm.feat.list) %>% unnest
glszm.feat <- data_frame(glszm.feat.list) %>% unnest
# mglszm.feat <- data_frame(mglszm.feat.list) %>% unnest

features <- bind_cols(data_frame(my.jpeg), glcm.feat, glrlm.feat, glszm.feat)

write.csv(features, file = "features.csv", row.names = FALSE)

# First Order Features
feat <- lapply(1:30, function(i) calc_features(lesion.list[[i]])) %>% unlist
feat <- matrix(feat, 30, 13, byrow = TRUE)
colnames(feat) <- c("calc_energy", "calc_entropy", "calc_kurtosis", "calc_meanDeviation",
                    "calc_skewness", "calc_uniformity", "calc_mean", "calc_median",
                    "calc_max", "calc_min", "calc_variance", "calc_RMS", "calc_sd")
feat <- as.data.frame(feat)
write.csv(feat, file = "firstorder.csv", row.names = FALSE)
>>>>>>> cb1a677802f3868364a9b4196b6b4ce00b5eeca2
