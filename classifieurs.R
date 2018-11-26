classifieur_expressions <- function(dataset) {
  library('MASS')
load("env.Rdata") ## load expressions.lda
predictions <- predict(expressions.lda, newdata = dataset)

return(predictions$class)
}
classifieur_characters <- function(dataset) {
  library('e1071')
load("env.Rdata") # load characters.loadedData, characters.cost, characters.gamma, and oneoneSVMpred
dataset <- cbind(dataset[,-1], y=dataset$Y)
pred <- oneoneSVMpred(app=characters.loadedData,test=dataset, kernel="radial",
                            cost=characters.cost, gamma=characters.gamma)
return(pred$majorite)
}
classifieur_parole <- function(dataset) {
  library('e1071')
load("env.Rdata") #load parole.loadedData; parole.gamma;  and oneoneSVMpred
pred <- oneoneSVMpred(app=parole.loadedData, test=dataset, kernel="polynomial",
                            cost=1, degree=1, gamma=parole.gamma)
return(pred$majorite)
}


parole <- read.table("../SY19/parole/parole_train.txt")
1-sum(parole$y==classifieur_parole(parole))/dim(parole)[1]

expressions <- read.table("../SY19/expressions/expressions_train.txt")
1-sum(expressions$y==classifieur_expressions(expressions))/dim(expressions)[1]

characters <- read.table("../SY19/characters/characters_train.txt")
pred <- classifieur_characters(characters)
1-sum(characters$Y==pred)/dim(characters)[1]
