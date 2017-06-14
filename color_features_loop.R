library(dplyr)
library(jpeg)
library(png)
library(plotly)
library(devtools)
library(ggbiplot)
setwd("~/Documents/Mine New/bdsi/imaging/training_set")

jpegs <- list.files(pattern = ".jpg")
my.jpeg <- jpegs[467:700]
pngs <- list.files(pattern = ".png")
my.png <- pngs[467:700]

jpeg.list <- lapply(my.jpeg, readJPEG, native = FALSE)
png.list <- lapply(my.png, readPNG, native = FALSE)

##copy of segmentation image for lesion only
lesions <- png.list

##remove background skin from image
remove.bg <- function(lesion, jpeg){
  is.na(lesion) <- lesion == 0
  masked <- array(c(lesion*jpeg[,,1],lesion*jpeg[,,2],lesion*jpeg[,,3]),c(767,1022,3))
}
masked_list <- lapply(1:234, function(i) remove.bg(lesions[[i]], jpeg.list[[i]]))

##sanity check of isolated tumor list
dim(masked_list[[1]])
masked_list[[1]][1:3,1:3,1]
sum(is.na(masked_list[[1]]))

##isolate background skin
skins <- lapply(png.list, function(x) !x)

#sanity check for skin
skins[[1]][1:3,1:3]


## find mean color of skin
mean.skin.color <- function(skin, jpeg){
  mean_skin <- c(mean(skin*jpeg[,,1]),mean(skin*jpeg[,,2]),mean(skin*jpeg[,,3]))
}
mean_skins <- lapply(1:234, function(i) mean.skin.color(skins[[i]], jpeg.list[[i]]))

## normalize tumor color
lesion.normalize <- function(masked, mean_skin){
  lesion_normal <- array(c(masked[,,1] - mean_skin[1],masked[,,2] - mean_skin[2],masked[,,3] - mean_skin[3]),c(767,1022,3))
}
lesions_normal <- lapply(1:234, function(i) lesion.normalize(masked_list[[i]], mean_skins[[i]]))

## mean of normalized tumor color
color.operation <- function(image, fun){
  c(fun(image[,,1],na.rm=T),fun(image[,,2],na.rm=T),fun(image[,,3],na.rm=T))
}
mean_lesions <- lapply(lesions_normal, function(x) color.operation(x, mean))

mean_lesions_df <- data.frame(matrix(unlist(mean_lesions), byrow=TRUE, nrow=length(mean_lesions)))

## sd of normalized tumor color
sd_lesions <- lapply(lesions_normal, function(x) color.operation(x, sd))

sd_lesions_df <- data.frame(matrix(unlist(sd_lesions), byrow=TRUE, nrow=length(sd_lesions)))

features <- bind_cols(data_frame(my.jpeg), mean_lesions_df, sd_lesions_df)
colnames(features) <- c("image", "mean_r", "mean_b", "mean_g", "sd_r", "sd_b", "sd_g")

##append truth
training_set_truth = read.csv("~/Documents/Mine New/bdsi/imaging/training_set/training_set_truth.csv", head=F)
features <- mutate(features, training_set_truth[467:700,3])
colnames(features) <- c("image", "mean_r", "mean_b", "mean_g", "sd_r", "sd_b", "sd_g", "truth")

write.table(features, file = "colors.csv", sep = ",",row.names=FALSE, append = FALSE)
View(features)

##data visualization
qqnorm(features$mean_r); qqline(features$mean_r)
qqnorm(features$mean_b); qqline(features$mean_b)
qqnorm(features$mean_g); qqline(features$mean_g)

qqnorm(features$sd_r); qqline(features$sd_r)
qqnorm(features$sd_b); qqline(features$sd_b)
qqnorm(features$sd_g); qqline(features$sd_g)

plot_ly(features, x = ~mean_r, y= ~mean_b, z=~mean_g, color = ~truth, colors = c('#0C4B8E', '#BF382A'))
plot_ly(features, x = ~sd_r, y= ~sd_b, z=~sd_g, color = ~truth, colors = c('#0C4B8E', '#BF382A'))

features %>% group_by(truth) %>% count()

color.pca=prcomp(~ mean_r + mean_b + mean_g+sd_r+sd_b+sd_g, data = features, scale = TRUE)
color.pca=prcomp(~ sd_r + sd_b + sd_g, data = features, scale = TRUE)
ggbiplot(color.pca,group=features$truth, obs.scale = 1, var.scale = 1)	#Biplot

