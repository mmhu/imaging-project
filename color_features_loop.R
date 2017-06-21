library(dplyr)
library(jpeg)
library(png)
library(plotly)
library(devtools)
library(ggbiplot)
setwd("~/Documents/Mine New/bdsi/imaging/training_set")

jpegs <- list.files(pattern = ".jpg")
my.jpeg <- jpegs[234:466]
pngs <- list.files(pattern = ".png")
my.png <- pngs[234:466]

jpeg.list <- lapply(my.jpeg, readJPEG, native = FALSE)
png.list <- lapply(my.png, readPNG, native = FALSE)

##copy of segmentation image for lesion only
lesions <- png.list

##remove background skin from image
remove.bg <- function(lesion, jpeg){
  masked <- array(c(lesion*jpeg[,,1],lesion*jpeg[,,2],lesion*jpeg[,,3]),c(768,1024,3))
}
masked_list <- lapply(1:233, function(i) remove.bg(lesions[[i]], jpeg.list[[i]]))

## mean of tumor color
color.operation <- function(image, fun){
  c(fun(image[,,1]),fun(image[,,2]),fun(image[,,3]))
}
mean_lesions <- lapply(masked_list, function(x) color.operation(x, mean))
mean_lesions_df <- data.frame(matrix(unlist(mean_lesions), byrow=TRUE, nrow=length(mean_lesions)))

## sd of tumor color
sd_lesions <- lapply(masked_list, function(x) color.operation(x, sd))

sd_lesions_df <- data.frame(matrix(unlist(sd_lesions), byrow=TRUE, nrow=length(sd_lesions)))

##intensity
intensity <- function(image, na.rm){
  sqrt(image[,,1]^2+image[,,2]^2+image[,,3]^2)
}

i_lesions <- lapply(masked_list, function(image) sqrt(image[,,1]**2+image[,,2]**2+image[,,3]**2))
mean_i <- lapply(i_lesions, function(x) mean(x))
sd_i <- lapply(i_lesions, function(x) sd(x))

mean_i_df <- data.frame(unlist(mean_i))
sd_i_df <- data.frame(unlist(sd_i))

##color variegation
red_cv <- log(sd_lesions_df[1]/mean_lesions_df[1])
blue_cv <- log(sd_lesions_df[2]/mean_lesions_df[2])
green_cv <- log(sd_lesions_df[3]/mean_lesions_df[3])
i_cv <- log(sd_i_df/mean_i_df)

##combine features
features466 <- bind_cols(data_frame(my.jpeg), mean_lesions_df, sd_lesions_df, 
                      mean_i_df, sd_i_df, red_cv, blue_cv, green_cv, i_cv
                      )
colnames(features466) <- c("image", "mean_r", "mean_b", "mean_g", "sd_r", 
                        "sd_b", "sd_g", "mean_i", "sd_i", "red_cv", "blue_cv",
                        "green_cv","i_cv")
color <- bind_rows(features233,features466,features700)

##add to grayscale
grayscale = read.csv("~/Documents/Mine New/bdsi/imaging/training_set/grayscale.csv", head=T)
features <- bind_cols(color, grayscale)
write.table(features, file = "color_grayscale.csv", sep = ",",row.names=FALSE, append = FALSE)

##append truth
training_set_truth = read.csv("~/Documents/Mine New/bdsi/imaging/training_set/training_set_truth.csv", head=F)
write.table(features, file = "colors.csv", sep = ",",row.names=FALSE, append = FALSE)
View(features)

color = read.csv("~/Documents/Mine New/bdsi/imaging/training_set/colors.csv", head=F)
View(color)
color <- color[-1,-1]
colnames(color) <- c("image", "mean_r", "mean_b", "mean_g", "sd_r", "sd_b", "sd_g")
rownames(color) <- NULL
write.table(color, file = "colors.csv", sep = ",",row.names=FALSE, append = FALSE)

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
colnames(features)
color.pca=prcomp(~ mean_r + mean_b + mean_g+sd_r+sd_b+sd_g, data = features, scale = TRUE)
color.pca=prcomp(~ sd_r + sd_b + sd_g, data = features, scale = TRUE)
ggbiplot(color.pca,group=truth, obs.scale = 1, var.scale = 1)	#Biplot

color_features <- color[-1,2:7]

truth <- training_set_truth[467:700,3]
truth <- recode(truth, benign = "0")
truth <- recode(truth, malignant="1")
head(truth)
head(color_features)
View(color_features)
rownames(color_features) <- NULL
length(truth)
glm.tmp = glm(truth~.,data=color_features,family=binomial("logit"), max=100)
glm.fit(color_features, truth)
summary(glm.tmp)
glm.tmp$fitted.values
