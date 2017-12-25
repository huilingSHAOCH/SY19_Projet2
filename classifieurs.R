classifieur_expressions <- function(dataset) {
  # Chargement de l’environnement
  load("env.Rdata")
  
  #LDA renvoie le meilleur résultat
  
  #Remove column full of 0 of training data
  cleanDatatraining<-data_expressions[,colSums(abs(data_expressions[,1:ncol(data_expressions)-1])) !=0]
 
   #FDA A CALCULER QUE POUR DONNEES TRAIN 
  library("MASS")
  lda_data<- lda(y~.,data=cleanDatatraining)
  U<-lda_data$scaling
  X<-as.matrix(cleanDatatraining[,1:ncol(cleanDatatraining)-1])
  Z<-X%*%U
  Z<-as.data.frame(Z)
  y<-cleanDatatraining$y
  trainFDA<-cbind(Z,y)

  #Remove column full of 0 of test data
  cleanDatatest<-dataset[,colSums(abs(dataset[,1:ncol(dataset)-1])) !=0]
  
  #Apply FDA on test data
  X<-as.matrix(cleanDatatest[,1:ncol(cleanDatatest)-1])
  Z<-X%*%U
  Z<-as.data.frame(Z)
  y<-cleanDatatest$y
  testFDA<-cbind(Z,y)
  
  
  #LDA
  lda_data<- lda(y~.,data=trainFDA)
  predictions<-predict(lda_data,newdata=testFDA)
  predictions <- predictions$class

  return(predictions)
}



classifieur_characters <- function(dataset) {
  # Chargement de l’environnement
  load("env.Rdata")
  # Random Forest renvoie le meilleur résultat
  library(randomForest)
  #m = sqrt(p)
  rdForest_data = randomForest(Y~., data=data_characters,mtry=4)
  predictions = predict(rdForest_data, newdata=dataset, type = 'response')
  return(predictions)
}


classifieur_parole <- function(dataset) {
  # Chargement de l’environnement
  load("env.Rdata")
  # Bayésien naïf est donc le meilleur pour parole

  #FDA A CALCULER QUE POUR DONNEES TRAIN 
  library("MASS")
  lda_data<- lda(y~.,data=data_parole)
  U<-lda_data$scaling
  X<-as.matrix(data_parole[,1:ncol(data_parole)-1])
  Z<-X%*%U
  Z<-as.data.frame(Z)
  y<-data_parole$y
  trainFDA<-cbind(Z,y)
  
  
  #Apply FDA on test data
  X<-as.matrix(dataset[,1:ncol(dataset)-1])
  Z<-X%*%U
  Z<-as.data.frame(Z)
  y<-dataset$y
  testFDA<-cbind(Z,y)
  
  library(klaR)
  
  naivB_data<-NaiveBayes(y~.,data=trainFDA)
  predictions<-predict(naivB_data,newdata=testFDA)
  predictions <- predictions$class
  return(predictions)
}
