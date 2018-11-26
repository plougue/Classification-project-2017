############# INITIALISATION DES DONNEES


library(klaR)
library(randomForest)
library(MASS)
library(mclust)
library(e1071)

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





############## FDA vs FDA + SVM


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

    
    pred.svm <- oneoneSVMpred(proj.app, proj.test)
    
    cat("|")
    # 
    
    
  }
  
  lda.cv <- c(lda.cv,lda.cv.err)
  proj.lda.cv <- c(proj.lda.cv,proj.lda.cv.err)
  
  cat(" [" , c , "/" , CMax , "]\n")
  
}

boxplot(proj.lda.cv, lda.cv, col=c("blue", "red"))


oneoneSVMpred <- function (app, test)
{
  levels <- levels(app$y)
  K <- length(levels)
  N <- (dim(proj.app)[1])
  votes <- rep(0, N)
  
  for(i in 1:K)
  {
    for(j in i+1:K)
    {
      app.classes <- app[app$y==levels(proj.app$y)[i] | app$y==levels(proj.app$y)[j]]
      svmfit <- svm(y~., data=app.classes, kernel="radial", gamma=0.1, cost=100)
    }
    
  }
}