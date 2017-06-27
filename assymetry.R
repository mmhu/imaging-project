library(jpeg)
library(png)
library(dplyr)
library(sp)
library(rgeos)
library(spTest)

setwd("~/Documents/Mine New/bdsi/imaging/training_set")
jpeg1 <- readJPEG("ISIC_0000001.jpg", native=FALSE)
png1 <- readPNG("ISIC_0000004_Segmentation.png", native=FALSE)
View(png1)
ones <- which(png1==1,arr.ind=TRUE)
centroid <- c(mean(ones[,1]),mean(ones[,2]))

###################################################
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

#circularity
perim1 <- perimeter(png1)
area1 <- sum(png1)
circularity <- 4*pi*area1/(perim1)^2

#ASD
radius <- perim1/(2*pi)

edges <- edge(png1)
edges_index <- which(edges==TRUE,arr.ind=TRUE)
dim(edges)

#find radial lines
angles <- lapply(seq(1,m-1),function(x) (2*x*pi/m))
angles <- angles[c(seq(1,m/2-1),seq(m/2+1,m-1))]
m_pts <- matrix(NA, ncol=2,nrow=m-2)

#tumor line - WARNING: rotated 90 CCW
l1 <- Line(edges_index)
ll1 <- Lines(list(l1), ID = "1")
sl1 <- SpatialLines(list(ll1))
plot(sl1)

for (i in seq(1,m-2)){
  l2 <- Line(rbind(centroid, c(centroid[1]+radius*cos(angles[[i]]),centroid[2]+radius*sin(angles[[i]]))))
  ll2 <- Lines(list(l2), ID = "1")
  sl2 <- SpatialLines(list(ll2))
  
  int.pts <- gIntersection(sl1, sl2, byid = TRUE)

  int.coords <- int.pts@coords

  m_pts[i,1] <- int.coords[which.max(spDistsN1(int.coords,centroid)),1]
  m_pts[i,2] <- int.coords[which.max(spDistsN1(int.coords,centroid)),2]
}

View(m_pts)

############sp package##################

plot(sl1)
points(centroid[1],centroid[2],col="red")

rbPal <- colorRampPalette(c('red','blue'))
colorpal <- rbPal(9)[as.numeric(cut(m_pts[,2],breaks=10))]
  
for (i in seq(1,(m-2))){
  points(m_pts[i,1],m_pts[i,2],col="green")
}
points(m_pts[45,1],m_pts[45,2],col="yellow")
points(edges_index[],edges_index,col="blue")

#########rotate##################

rotated_pts <- matrix(NA, ncol=2,nrow=m-2)
for (i in seq(1,m-2)){
  theta <- angles[[i]]
  rotate <- matrix(data=c(cos(theta),-sin(theta),sin(theta),cos(theta)), nrow=2,
                   ncol=2,byrow=TRUE)
  x<- m_pts[i,1]
  y<- m_pts[i,2]
  rotated <- rotate%*%matrix(data=c(x-centroid[1],y-centroid[2]),nrow=2,byrow=TRUE)
  rotated_pts[i,1] <- rotated[1]+centroid[1]
  rotated_pts[i,2] <- rotated[2]+centroid[2]
}

p0 <- c(mean(rotated_pts[,1]),mean(rotated_pts[,2]))
rotated_pts2 <- matrix(NA, ncol=2,nrow=m-2)

for (i in seq(1,m-2)){
  theta <- angles[[i]]
  rotate <- matrix(data=c(cos(-theta),-sin(-theta),sin(-theta),cos(-theta)), nrow=2,
                   ncol=2,byrow=TRUE)

  rotated <- rotate%*%matrix(data=c(p0[1]-centroid[1],p0[2]-centroid[2]),nrow=2,byrow=TRUE)
  rotated_pts2[i,1] <- rotated[1]+centroid[1]
  rotated_pts2[i,2] <- rotated[2]+centroid[2]
}

#plot transformations
for (i in seq(1,(m-2))){
  points(rotated_pts[i,1],rotated_pts[i,2],col="gray")
}
points(p0[1],p0[2],col="yellow")
for (i in seq(1,(m-2))){
  points(rotated_pts2[i,1],rotated_pts2[i,2],col="salmon")
}

#find SD
sd <- sum((m_pts[,1]-rotated_pts2[,1])^2+(m_pts[,2]-rotated_pts[,2])^2)/m
