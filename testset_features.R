library(dplyr)
library(jpeg)
library(png)
library(plotly)
library(devtools)
library(ggbiplot)

library(sp)
library(rgeos)
library(spTest)
setwd("~/Documents/Mine New/bdsi/imaging/testing_set")

jpegs <- list.files(pattern = ".jpg")

pngs <- list.files(pattern = ".png")

jpeg.list <- lapply(jpegs, readJPEG, native = FALSE)
png.list <- lapply(pngs, readPNG, native = FALSE)

##copy of segmentation image for lesion only
lesions <- png.list

##remove background skin from image
remove.bg <- function(lesion, jpeg){
  masked <- array(c(lesion*jpeg[,,1],lesion*jpeg[,,2],lesion*jpeg[,,3]),dim(jpeg))
}
masked_list <- lapply(1:200, function(i) remove.bg(lesions[[i]], jpeg.list[[i]]))
sum(masked_list[[2]][,,1])
## mean of tumor color
color.operation <- function(image, fun){
  c(fun(image[,,1]),fun(image[,,2]),fun(image[,,3]))
}
mean_r_lesions <- lapply(masked_list, function(x) mean(x[,,1]))
mean_b_lesions <- lapply(masked_list, function(x) mean(x[,,2]))
mean_g_lesions <- lapply(masked_list, function(x) mean(x[,,3]))

mean_r_lesions_df <- data.frame(matrix(unlist(mean_r_lesions), byrow=TRUE, nrow=length(mean_r_lesions)))
mean_b_lesions_df <- data.frame(matrix(unlist(mean_b_lesions), byrow=TRUE, nrow=length(mean_b_lesions)))
mean_g_lesions_df <- data.frame(matrix(unlist(mean_g_lesions), byrow=TRUE, nrow=length(mean_g_lesions)))

## sd of tumor color
sd_lesions <- lapply(masked_list, function(x) color.operation(x, sd))

sd_r_lesions <- lapply(masked_list, function(x) sd(x[,,1]))
sd_b_lesions <- lapply(masked_list, function(x) sd(x[,,2]))
sd_g_lesions <- lapply(masked_list, function(x) sd(x[,,3]))

sd_r_lesions_df <- data.frame(matrix(unlist(sd_r_lesions), byrow=TRUE, nrow=length(sd_r_lesions)))
sd_b_lesions_df <- data.frame(matrix(unlist(sd_b_lesions), byrow=TRUE, nrow=length(sd_b_lesions)))
sd_g_lesions_df <- data.frame(matrix(unlist(sd_g_lesions), byrow=TRUE, nrow=length(sd_g_lesions)))

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
red_cv <- log(sd_r_lesions_df/mean_r_lesions_df)
blue_cv <- log(sd_b_lesions_df/mean_b_lesions_df)
green_cv <- log(sd_g_lesions_df/mean_g_lesions_df)
i_cv <- log(sd_i_df/mean_i_df)
sum(is.na(i_cv))

##combine features
colors <- bind_cols(mean_r_lesions_df, mean_b_lesions_df, 
                         mean_g_lesions_df, sd_r_lesions_df, sd_b_lesions_df,
                         sd_g_lesions_df,mean_i_df, sd_i_df, red_cv, blue_cv, green_cv, i_cv
)
colnames(colors) <- c("mean_r", "mean_b", "mean_g", "sd_r", 
                           "sd_b", "sd_g", "mean_i", "sd_i", "red_cv", "blue_cv",
                           "green_cv","i_cv")
write.table(colors, file = "testing_color.csv", sep = ",",row.names=FALSE, append = FALSE)

##asymmetry

m <- 90
s1 <- function(z) cbind(rep(0,nrow(z)), z[,-ncol(z)] )
s2 <- function(z) cbind(z[,-1], rep(0,nrow(z)) )
s3 <- function(z) rbind(rep(0,ncol(z)), z[-nrow(z),] )
s4 <- function(z) rbind(z[-1,], rep(0,ncol(z)) )
edge <- function(z) z & !(s1(z)&s2(z)&s3(z)&s4(z))
perimeter <- function(z) {
  e <- edge(z)
  ( 
    # horizontal and vertical segments
    sum( e & s1(e) ) + sum( e & s2(e) ) + sum( e & s3(e) ) + sum( e & s4(e) ) + 
      # diagonal segments
      sqrt(2)*( sum(e & s1(s3(e))) + sum(e & s1(s4(e))) + sum(e & s2(s3(e))) + sum(e & s2(s4(e))) )
  ) / 2  # Each segment was counted twice, once for each end
}

sd_list <- rep(NA,200)

for (i in seq(1,200)){
  png1 <- png.list[[i]]
  ones <- which(png1==1,arr.ind=TRUE)
  centroid <- c(mean(ones[,1]),mean(ones[,2]))
  
  ###################################################
  perim1 <- perimeter(png1)
  
  #ASD
  radius <- perim1/(2*pi)
  
  edges <- edge(png1)
  edges_index <- which(edges==TRUE,arr.ind=TRUE)
  
  #find radial lines
  angles <- lapply(seq(1,m-1),function(x) (2*x*pi/m))
  angles <- angles[c(seq(1,m/2-1),seq(m/2+1,m-1))]
  m_pts <- matrix(NA, ncol=2,nrow=m-2)
  
  #tumor line - WARNING: rotated 90 CCW
  l1 <- Line(edges_index)
  ll1 <- Lines(list(l1), ID = "1")
  sl1 <- SpatialLines(list(ll1))
  
  ##find m-2 points on perimeter
  for (j in seq(1,m-2)){
    l2 <- Line(rbind(centroid, c(centroid[1]+radius*cos(angles[[j]]),centroid[2]+radius*sin(angles[[j]]))))
    ll2 <- Lines(list(l2), ID = "1")
    sl2 <- SpatialLines(list(ll2))
    
    int.pts <- gIntersection(sl1, sl2, byid = TRUE)
    
    int.coords <- int.pts@coords
    
    m_pts[j,1] <- int.coords[which.max(spDistsN1(int.coords,centroid)),1]
    m_pts[j,2] <- int.coords[which.max(spDistsN1(int.coords,centroid)),2]
  }
  
  ##normalize so that max distance from centroid = 1
  scalefactor <- sqrt(max((m_pts[,1]-centroid[1])^2+(m_pts[,2]-centroid[2])^2))
  m_pts_norm <- cbind((m_pts[,1]-centroid[1])/scalefactor+centroid[1],(m_pts[,2]-centroid[2])/scalefactor+centroid[2])
  
  #########rotate##################
  
  rotated_pts <- matrix(NA, ncol=2,nrow=m-2)
  for (j in seq(1,m-2)){
    theta <- angles[[j]]
    rotate <- matrix(data=c(cos(theta),-sin(theta),sin(theta),cos(theta)), nrow=2,
                     ncol=2,byrow=TRUE)
    x<- m_pts_norm[j,1]
    y<- m_pts_norm[j,2]
    rotated <- rotate%*%matrix(data=c(x-centroid[1],y-centroid[2]),nrow=2,byrow=TRUE)
    rotated_pts[j,1] <- rotated[1]+centroid[1]
    rotated_pts[j,2] <- rotated[2]+centroid[2]
  }
  
  p0 <- c(mean(rotated_pts[,1]),mean(rotated_pts[,2]))
  rotated_pts2 <- matrix(NA, ncol=2,nrow=m-2)
  p0_centroid <- matrix(data=c(p0[1]-centroid[1],p0[2]-centroid[2]),nrow=2,byrow=TRUE)
  
  for (j in seq(1,m-2)){
    theta <- angles[[j]]
    rotate <- matrix(data=c(cos(-theta),-sin(-theta),sin(-theta),cos(-theta)), nrow=2,
                     ncol=2,byrow=TRUE)
    
    rotated <- rotate%*%p0_centroid
    rotated_pts2[j,1] <- rotated[1]+centroid[1]
    rotated_pts2[j,2] <- rotated[2]+centroid[2]
  }
  
  #find SD
  sd_list[i] <- sum((m_pts[,1]-rotated_pts2[,1])^2+(m_pts[,2]-rotated_pts[,2])^2)/m
}

asym <- bind_cols(data_frame(my.png), data.frame(unlist(sd_list), stringsAsFactors = F))
colnames(asym) <- c("image","sym_d")
write.table(sd_list, file = "testing_asym.csv", sep = ",",row.names=FALSE, append = FALSE)


##circularity
circ_list <- rep(NA,200)

for (i in seq(1,200)){
  png1 <- png.list[[i]]
  area <- sum(png1)
  perim <- perimeter(png1)
  circ_list[i] <- 4*area*pi/(perim^2)
}

write.table(circ_list, file="testing_circ.csv",sep=",",row.names=FALSE, append=FALSE)
