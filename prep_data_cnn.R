library(EBImage)
# get features (from resize_images.R)
setwd("/Users/bensadis/Desktop/BDSI/training_set")
a = 1 # index of first image to analyze
n = 1 # number of images to analyze
jpegs <- list.files(pattern = ".jpg")
my.jpeg <- jpegs[a:(a+n-1)]
pngs <- list.files(pattern = ".png")
my.png <- pngs[a:(a+n-1)]
jpeg.list <- lapply(my.jpeg, readImage, native = FALSE)
png.list <- lapply(my.png, readImage, native = FALSE)

jpeg.list.r <- lapply(jpeg.list, resize, w=128, h=128)
png.list.r <- lapply(png.list, resize, w=128, h=128)

grayscale <- function(img) {
  gray.img <- Image(img, dim=c(128, 128), colormode = "Grayscale")
}
gray.list.r <- lapply(jpeg.list.r, grayscale)

remove_bg <- function(input.img, input.seg) {
  lesion.image <- input.img * input.seg
}
lesion.list <- lapply(1:n, function(i) remove_bg(gray.list.r[[i]], png.list.r[[i]]))

rotated.list <- lapply(lesion.list, rotate, 90)
flipped.list <- lapply(lesion.list, flip)
flopped.list <- lapply(lesion.list, flop)

vector.les.list <- lapply(lesion.list, as.vector)
vector.rot.list <- lapply(rotated.list, as.vector)
vector.flip.list <- lapply(flipped.list, as.vector)
vector.flop.list <- lapply(flopped.list, as.vector)

features <- vector.les.list[[1]]
features <- rbind(features, vector.rot.list[[1]])
features <- rbind(features, vector.flip.list[[1]])
features <- rbind(features, vector.flop.list[[1]])

for (i in 2:(1+n-1)) {
  features <- rbind(features, vector.les.list[[i]])
  features <- rbind(features, vector.rot.list[[i]])
  features <- rbind(features, vector.flip.list[[i]])
  features <- rbind(features, vector.flop.list[[i]])
}


# write features to csv file
setwd("/Users/bensadis/Desktop/BDSI")
write.table(features, file="pixels.csv", sep=",", append=TRUE, row.names = FALSE, col.names=FALSE)
