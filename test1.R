app <- read.table('expressions_train.txt')
app.x <- app[,-N]
app.y <- app[,N]
N <- length(app)


printImg <- function(i)
{
  I<- matrix(as.numeric(app.x[i,]),60,70)
  print(t(I))
  I1 <- apply(I, 1, rev)
  image(t(I1),col=gray(0:255 / 255))
}
printImg(1)
printImg(2)
printImg(3)
printImg(4)