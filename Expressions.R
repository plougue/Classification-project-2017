############# INITIALISATION DES DONNEES


library(klaR)
library(randomForest)
library(MASS)
library(mclust)

data <- read.table('expressions_train.txt')
p <- length(data)
N <- dim(data)[1]
data.x <- data[,-p]
data.y <- data[,p]


## On supprime les pixels noirs
pixelsNoirs <- which(colMeans(data.x) == rep(0,p-1))
data <- data[,-pixelsNoirs]
data.x <- data.x[,-pixelsNoirs]



## On transforme l'image pour qu'elle puisse être affichée proprement et on l'affiche
printImg <- function(i)
{
  I<- matrix(as.numeric(data.x[i,]),60,70)
  I1 <- apply(I, 1, rev)
  image(t(I1),col=gray(0:255 / 255))
}





############## LDA vs FDA


K <- 5

CMax <- 5

lda.cv <- NULL
proj.lda.cv <- NULL
for(c in 1:CMax)
{
  
  # Re-ordonnée les lignes de data de manière aléatoire
  data <- data[order(runif(nrow(data))),]
  
  data.x <- floor(data[,-length(data)] / c) * c
  data.y <- data[,length(data)]
  
  # Séparer les nombres de 1 à n en K parties égales
  testKeys <- split(1:N, factor(sort(rank(1:N)%%K)))
  
  lda.cv.err <- 0
  proj.lda.cv.err <- 0
  
  for(k in 1:K)
  {
    testKey <- testKeys[[k]]
    app <- data[-testKey,]
    test <- data[testKey,]
    app.x <- app[,-length(app)]
    lda <- lda(y ~ ., data=app)
    
    proj <- data.frame( t(t(lda$scaling) %*% t(data.x)), y=data.y)
    proj.app <- proj[-testKey,]
    proj.test <- proj[testKey,]
    
    proj.lda <- lda(y~., data=proj.app)
    pred.proj.lda <- predict(proj.lda, newdata = proj.test)
    proj.lda.err <- ( 1 - sum(diag(table(pred.proj.lda$class, test$y)))/length(testKey) )
    proj.lda.cv.err <- proj.lda.cv.err + (proj.lda.err / K)
    
    lda <- lda(y~., data=app)
    pred.lda <- predict(lda, newdata = test) 
    lda.err <- ( 1 - sum(diag(table(pred.lda$class, test$y)))/length(testKey) ) 
    lda.cv.err <- lda.cv.err + (lda.err / K)
    
    cat("|")
    # 
    
    
  }
  
  lda.cv <- c(lda.cv,lda.cv.err)
  proj.lda.cv <- c(proj.lda.cv,proj.lda.cv.err)
  
  cat(" [" , c , "/" , CMax , "]\n")
  
}

boxplot(proj.lda.cv, lda.cv, col=c("blue", "red"))


############## LDA vs mcLust vs FDA vs FDA+mcLust


K <- 5

CMax <- 5

lda.cv <- NULL
proj.lda.cv <- NULL
mclustDA.cv <- NULL
for(c in 1:CMax)
{
  
  # Re-ordonnée les lignes de data de manière aléatoire
  data <- data[order(runif(nrow(data))),]
  
  data.x <- floor(data[,-length(data)] / c) * c
  data.y <- data[,length(data)]
  
  # Séparer les nombres de 1 à n en K parties égales
  testKeys <- split(1:N, factor(sort(rank(1:N)%%K)))
  
  lda.cv.err <- 0
  proj.lda.cv.err <- 0
  mclustDA.cv.err <- 0
  
  for(k in 1:K)
  {
    testKey <- testKeys[[k]]
    app <- data[-testKey,]
    test <- data[testKey,]
    app.x <- app[,-length(app)]
    lda <- lda(y ~ ., data=app)
    
    proj <- data.frame( t(t(lda$scaling) %*% t(data.x)), y=data.y)
    proj.app <- proj[-testKey,]
    proj.app.x <- proj.app[,-length(proj.app)]
    proj.app.y <- proj.app[,length(proj.app)]
    proj.test <- proj[testKey,]
    proj.test.x <- proj.test[,-length(proj.test)]
    proj.test.y <- proj.test[,length(proj.test)]
    
    proj.lda <- lda(y~., data=proj.app)
    pred.proj.lda <- predict(proj.lda, newdata = proj.test)
    proj.lda.err <- ( 1 - sum(diag(table(pred.proj.lda$class, test$y)))/length(testKey) )
    proj.lda.cv.err <- proj.lda.cv.err + (proj.lda.err / K)
    
    lda <- lda(y~., data=app)
    pred.lda <- predict(lda, newdata = test) 
    lda.err <- ( 1 - sum(diag(table(pred.lda$class, test$y)))/length(testKey) )
    lda.cv.err <- lda.cv.err + (lda.err / K)
    
    proj.mclustDA <- MclustDA(proj.app.x, proj.app.y)
    a <- summary(proj.mclustDA, newdata = proj.test.x, newclass = proj.test.y)
    print(a$err.newdata)
    # pred.proj.mclustDA <- predict(proj.mclustDA, newdata=proj.test.x)
    # print(mclustDA.err)
    mclustDA.cv.err <- mclustDA.cv.err + (a$err.newdata/ K)
    
    cat("|")
    # 
    
    
  }
  
  lda.cv <- c(lda.cv,lda.cv.err)
  proj.lda.cv <- c(proj.lda.cv,proj.lda.cv.err)
  mclustDA.cv <- c(mclustDA.cv, mclustDA.cv.err)
  
  cat(" [" , c , "/" , CMax , "]\n")
  
}

boxplot(proj.lda.cv, lda.cv, mclustDA.cv, col=c("blue", "red", "green"))