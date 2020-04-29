library(jpeg)
library(OpenImageR)
library(flip)
library(EBImage)

an <- list.files("C:/Users/Ruby/Desktop/jaffejpg/angry")
ha <- list.files("C:/Users/Ruby/Desktop/jaffejpg/happy")
sa <- list.files("C:/Users/Ruby/Desktop/jaffejpg/sad")
di <- list.files("C:/Users/Ruby/Desktop/jaffejpg/disgust")
su <- list.files("C:/Users/Ruby/Desktop/jaffejpg/surprise")


cutgray <- function(x, y){
  
  library(jpeg)
  library(OpenImageR)
  dir <- paste0("C:/Users/Ruby/Desktop/jaffejpg/", y)
  setwd(dir)
  mat <- matrix(, nrow=length(x), ncol=105*130)
  
  for (i in 1:length(x)) {
    #讀圖
    flp <- flipImage(readJPEG(x[i]), mode = 'vertical')
    #灰階化
    rgb.weight <- c(0.2989, 0.587, 0.114)
    gray <- rgb.weight[1] * imageData(flp)[,,1]+
      rgb.weight[2] * imageData(flp)[,,2]+
      rgb.weight[3] * imageData(flp)[,,3]
    #擷取
    cut <- matrix(as.vector(gray), nrow = 256, byrow = T)[76:180, 31:160]
    mat[i, ] <- as.vector(t(cut))
  }
  
  mat
  
}

angry <- cutgray(an, "angry")
happy <- cutgray(ha, "happy")
sad <- cutgray(sa, "sad")
disgust <- cutgray(di, "disgust")
surprise <- cutgray(su, "surprise")



show.cut.img <- function(x, i){
  image.mat <- matrix(x[i, ], nrow = 105, byrow = T)
  image(image.mat, col = grey(seq(0, 1, length = 255)))
}

x11()
par(mfrow=c(3, 2))
show.cut.img(angry, 15)
show.cut.img(happy, 15)
show.cut.img(sad, 15)
show.cut.img(disgust, 15)
show.cut.img(surprise, 15)



write.table(angry,file="C:/Users/Ruby/Desktop/專題/cut256.csv",sep=",",row.names=rep(1, 30), na = "NA", append=TRUE,col.names=FALSE)
write.table(happy,file="C:/Users/Ruby/Desktop/專題/cut256.csv",sep=",",row.names=rep(2, 31), na = "NA", append=TRUE,col.names=FALSE)
write.table(sad,file="C:/Users/Ruby/Desktop/專題/cut256.csv",sep=",",row.names=rep(3, 31), na = "NA", append=TRUE,col.names=FALSE)
write.table(disgust,file="C:/Users/Ruby/Desktop/專題/cut256.csv",sep=",",row.names=rep(4, 29), na = "NA", append=TRUE,col.names=FALSE)
write.table(surprise,file="C:/Users/Ruby/Desktop/專題/cut256.csv",sep=",",row.names=rep(5, 30), na = "NA", append=TRUE,col.names=FALSE)

