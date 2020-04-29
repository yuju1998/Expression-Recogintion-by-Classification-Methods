setwd("C:/Users/Ruby/Desktop/專題")
library(data.table)
data <- fread("combined.csv", sep=',', header = F)

library(caret)

# corr plot gif

library(png)
library(caTools)
library(abind)
library(animation)

saveGIF(for (i in 1:1000) {
  a = i * 10 + 1
  b = (i + 1) * 10
  data.cor <- cor(cbind(data[, 1], data[, a:b]), use = 'pairwise.complete.obs')
  #corrplot::corrplot(data.cor)
})


# corr plot
corrplot(get_pca_var(data.pc)$cos2, is.corr=FALSE)

# combined pca
pc <- preProcess(data[, -1], 
                  method = c('pca', 'center', 'scale'), 
                  thresh = .7, 
                  verbose = T)
pcdata <- predict(pc, data)

# seperate pca
pc1 <- preProcess(data[1:151, -1], 
                 method = c('pca', 'center', 'scale'), 
                 thresh = .7, 
                 verbose = T)
data.pca1 <- predict(pc1, data[1:151, ])
#write.table(data.pca1, file = "pcdata_jaffe.csv", sep=',')

pc2 <- preProcess(data[152:251, -1], 
                 method = c('pca', 'center', 'scale'), 
                 thresh = .7, 
                 verbose = T)
data.pca2 <- predict(pc2, data[152:251, ])
#write.table(data.pca2, file = "pcdata_student.csv", sep=',')


# PCA 解說

data <- as.data.frame(data)
library(FactoMineR)
library(factoextra)
library(corrplot)
data.pc <- PCA(data[, -1], ncp = 12, graph = F)
jaffe.pc <- PCA(data[1:151, -1], ncp = 10, graph = F)
student.pc <- PCA(data[152:251, -1], ncp = 15, graph = F)

get_eigenvalue(data.pc)  # Extract the eigenvalues/variances of principal components


eigvar <- cbind(scale(data.pca1$eig[, "eigenvalue"]), data.pca1$eig[, -1])
write.table(eigvar, file = "eigen_and_variance_jaffe.csv", sep=',', row.names = T, col.names = T)


x11()
fviz_screeplot(data.pc$eig, xlab="Principle Components") # Visualize the eigenvalues
plot(data.pc$eig[1:15, "cumulative percentage of variance"], pch=20, 
     xlab = "Principle Components", ylab = "cumulative percentage of variance", 
     col=mycol[10])

x11()
barplot(data.pc$eig[1:15, "cumulative percentage of variance"], col=mycol[7], xlab = "Principle Components", ylab = "cumulative percentage of variance")
lines(data.pc$eig[1:15, "cumulative percentage of variance"], lwd=2, col=mycol[10])
points(data.pc$eig[1:15, "cumulative percentage of variance"], pch=20, col=mycol[11])
abline()

get_pca_ind(data.pc); get_pca_var(data.pc)  # Extract the results for individuals and variables, respectively.
apply(get_pca_var(data.pc)$contrib, 2, which.max)
apply(get_pca_ind(data.pc)$contrib, 2, which.max)


fviz_pca_ind(data.pc)
fviz_pca_var(data.pc) # Visualize the results individuals and variables, respectively.
fviz_pca_biplot(data.pc)

predict(data.pc, data)

scale(data.pc$eig[, "eigenvalue"])