setwd("C:/Users/Ruby/Desktop/jaffejpg")
library(EBImage)
library(jpeg)
library(raster)

an <- list.files("C:/Users/Ruby/Desktop/jaffejpg/angry")
ha <- list.files("C:/Users/Ruby/Desktop/jaffejpg/happy")
sa <- list.files("C:/Users/Ruby/Desktop/jaffejpg/sad")
di <- list.files("C:/Users/Ruby/Desktop/jaffejpg/disgust")
su <- list.files("C:/Users/Ruby/Desktop/jaffejpg/surprise")



image.64 <- function(x, y){
  dir <- paste0("C:/Users/Ruby/Desktop/jaffejpg/", y)
  setwd(dir)
  mat <- matrix(, nrow=length(x), ncol=4096)
  for (i in 1:length(x)) {
    a <- rbind(as.vector(resize(
      readImage(x[i]),
      64,
      64,
      filter = "bilinear",
      output.dim = c(64, 64)
    ))[1:4096])
    
    mat[i,] <- a
  }
  mat
}

angry <- image.64(an, "angry")
happy <- image.64(ha, "happy")
sad <- image.64(sa, "sad")
disgust <- image.64(di, "disgust")
surprise <- image.64(su, "surprise")



show <- function(x, i) {
  x11()
  plot(c(0, 64), c(0, 64))
  dims <- dim(matrix(x[i,], nrow = 64, byrow = F))
  rasterImage(t(matrix(
    x[i,], nrow = 64, byrow = F
  )), 0, 0, dims[1], dims[2])
}

show(happy, 1)