library(dplyr)
library(jpeg)
library(png)
library(dl)
library(radiomics)
setwd("~/Documents/Mine New/bdsi/imaging/training_set")

jpegs <- list.files(pattern = ".jpg")
my.jpeg <- jpegs[1:30]
pngs <- list.files(pattern = ".png")
my.png <- pngs[1:30]

jpeg.list <- lapply(my.jpeg, readJPEG, native = FALSE)
png.list <- lapply(my.png, readPNG, native = FALSE)

##copy of segmentation image for lesion only
lesions <- png.list
dim(lesions[[1]])
lesions[[1]][1:3,1:3]

##remove background skin from image
remove.bg <- function(lesion, jpeg){
  is.na(lesion) <- lesion == 0
  masked <- array(c(lesion*jpeg[,,1],lesion*jpeg[,,2],lesion*jpeg[,,3]),c(767,1022,3))
}
masked_list <- lapply(1:30, function(i) remove.bg(lesions[[i]], jpeg.list[[i]]))

##sanity check of isolated tumor list
dim(masked.list[[1]])
masked.list[[1]][1:3,1:3,1]
sum(is.na(masked.list[[1]]))

##isolate background skin
skins <- lapply(png.list, function(x) !x)

#sanity check for skin
skins[[1]][1:3,1:3]

# skin_rbg <- array(c(skin*image_jpeg[,,1],skin*image_jpeg[,,2],skin*image_jpeg[,,3]),c(767,1022,3)) 
skin[1:3,1:3]
image_jpeg[1:3,1:3,1]
(skin*image_jpeg[,,1])[1:3,1:3]

## find mean color of skin
mean.skin.color <- function(skin, jpeg){
  mean_skin <- c(mean(skin*jpeg[,,1]),mean(skin*jpeg[,,2]),mean(skin*jpeg[,,3]))
}
mean_skins <- lapply(1:30, function(i) mean.skin.color(skin[[i]], jpeg.list[[i]]))

## normalize tumor color
lesion.normalize <- function(masked, mean_skin){
  lesion_normal <- array(c(masked[,,1] - mean_skin[1],masked[,,2] - mean_skin[2],masked[,,3] - mean_skin[3]),c(767,1022,3))
}
lesions_normal <- lapply(1:30, function(i) lesion.normalize(masked_list[[i]], mean_skins[[i]]))

## mean of normalized tumor color
color.operation <- function(image, fun){
  c(fun(image[,,1],na.rm=T),fun(image[,,2],na.rm=T),fun(image[,,3],na.rm=T))
}
mean_lesions <- lapply(lesions_normal, function(x) color.operation(x, mean))

## sd of normalized tumor color
sd_lesions <- lapply(lesions_normal, function(x) color.operation(x, sd))
sd_lesion <-c(sd(lesion_normal[,,1],na.rm=T),sd(lesion_normal[,,2],na.rm=T),sd(lesion_normal[,,3],na.rm=T))
sd_lesions


