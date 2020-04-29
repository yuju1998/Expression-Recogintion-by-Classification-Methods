library(jpeg)
library(OpenImageR)
library(flip)
library(EBImage)

an.t <- list.files("C:/Users/Ruby/Desktop/taffejpg/angry")
ha.t <- list.files("C:/Users/Ruby/Desktop/taffejpg/happy")
sa.t <- list.files("C:/Users/Ruby/Desktop/taffejpg/sad")
di.t <- list.files("C:/Users/Ruby/Desktop/taffejpg/disgust")
su.t <- list.files("C:/Users/Ruby/Desktop/taffejpg/surprise")


cutself <- function(x, y){
  
  library(jpeg)
  library(OpenImageR)
  dir <- paste0("C:/Users/Ruby/Desktop/taffejpg/", y)
  setwd(dir)
  mat <- matrix(, nrow=length(x), ncol=105*130)
  
  for (i in 1:length(x)) {
    #讀圖
    nml <- resize(readJPEG(x[i]), 256, 256)
    flp <- flipImage(flipImage(nml, mode = 'vertical'), 'horizontal')
    #灰階化
    rgb.weight <- c(0.2989, 0.587, 0.114)
    gray <- rgb.weight[1] * imageData(flp)[,,1]+
      rgb.weight[2] * imageData(flp)[,,2]+
      rgb.weight[3] * imageData(flp)[,,3]
    #擷取
    cut <- gray[76:180, 31:160]
    mat[i, ] <- as.vector(t(cut))
  }
  
  mat
  
}

setwd("C:/Users/Ruby/Desktop/taffejpg/surprise")
a <- resize(readJPEG(su.t[12]), 256, 256)
a[, , 3] <- matrix(as.vector(a[, , 3]), byrow = T, nrow=256)
a[, , 2] <- matrix(as.vector(a[, , 2]), byrow = T, nrow=256)
a[, , 1] <- matrix(as.vector(a[, , 1]), byrow = T, nrow=256)
n <- flipImage(a, mode='horizontal')
g <- rgb.weight[1] * n[,,1]+
  rgb.weight[2] * n[,,2]+
  rgb.weight[3] * n[,,3]
b <- g[96:225, 76:180]
x11()
plot(c(0, 105), c(0, 130), xlab="", ylab="", type='n', xaxt='n', yaxt='n')
rasterImage(b, 0, 0, 105, 130)

angry.t <- cutself(an.t, "angry")
happy.t <- cutself(ha.t, "happy")
sad.t <- cutself(sa.t, "sad")
disgust.t <- cutself(di.t, "disgust")
surprise.t <- cutself(su.t, "surprise")

write.table(angry.t,file="C:/Users/Ruby/Desktop/專題/cuts.csv",sep=",",row.names=rep(1, 20), na = "NA", append=TRUE,col.names=FALSE)
write.table(happy.t,file="C:/Users/Ruby/Desktop/專題/cuts.csv",sep=",",row.names=rep(2, 20), na = "NA", append=TRUE,col.names=FALSE)
write.table(sad.t,file="C:/Users/Ruby/Desktop/專題/cuts.csv",sep=",",row.names=rep(3, 20), na = "NA", append=TRUE,col.names=FALSE)
write.table(disgust.t,file="C:/Users/Ruby/Desktop/專題/cuts.csv",sep=",",row.names=rep(4, 20), na = "NA", append=TRUE,col.names=FALSE)
write.table(surprise.t,file="C:/Users/Ruby/Desktop/專題/cuts.csv",sep=",",row.names=rep(5, 20), na = "NA", append=TRUE,col.names=FALSE)



show.cut.img <- function(x, i){
  image.mat <- matrix(x[i, ], nrow = 105, byrow = T)
  image(image.mat, col = grey(seq(0, 1, length = 255)), x=1:105, y=1:130)
}

x11()
show.cut.img(angry.t, 4)

show.cut.img(happy.t, 8)
