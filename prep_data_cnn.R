library(EBImage)
# get features (from resize_images.R)
setwd("/Users/bensadis/Desktop/BDSI/training_set")
a = 321 # index of first image to analyze
n = 50 # number of images to analyze
jpegs <- list.files(pattern = ".jpg")
my.jpeg <- jpegs[a:(a+n-1)]
pngs <- list.files(pattern = ".png")
my.png <- pngs[a:(a+n-1)]
jpeg.list <- lapply(my.jpeg, readImage, native = FALSE)
png.list <- lapply(my.png, readImage, native = FALSE)

jpeg.list.r <- lapply(jpeg.list, resize, w=64, h=64)
png.list.r <- lapply(png.list, resize, w=64, h=64)

grayscale <- function(img) {
  gray.img <- Image(img, dim=c(64, 64), colormode = "Grayscale")
}
gray.list.r <- lapply(jpeg.list.r, grayscale)

remove_bg <- function(input.img, input.seg) {
  lesion.image <- input.img * input.seg
}
lesion.list.r <- lapply(1:n, function(i) remove_bg(gray.list.r[[i]], png.list.r[[i]]))

vector.list <- lapply(lesion.list.r, as.vector)

features <- vector.list[[1]]
for (i in 2:(1+n-1)) {
  features <- rbind(features, vector.list[[i]])
}

# write features to csv file
setwd("/Users/bensadis/Desktop/BDSI")
write.table(features, file="pixels.csv", sep=",", append=TRUE, row.names = FALSE, col.names=FALSE)
