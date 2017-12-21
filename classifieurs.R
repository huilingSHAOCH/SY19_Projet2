classifieur_expressions <- function(dataset) {
  # Chargement de l’environnement
  # load("env.Rdata")
  
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
  y<-data$y
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