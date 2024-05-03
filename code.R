#install.packages("package_name")
library(tree)
library(party)
library(rpart.plot)
library(rpart)
library(C50)
library(ROCR)
library(ggplot2)
library(plotly)
library(regclass)
library(imbalance)
library(pROC)
library(caret)
library(cluster)
library(dbscan)
library(fpc)
library(tree)


#---Exploration des donnés


data <- read.csv("C:/Users/ulric/OneDrive/Bureau/data/Data_Projet_1.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)

qplot(age, data=data, color=fraudulent , fill=fraudulent,bins=5)
qplot(gender, data=data, color=fraudulent , fill=fraudulent)
qplot(incident_cause, data=data, color=fraudulent , fill=fraudulent)
qplot(days_to_incident, data=data, color=fraudulent , fill=fraudulent,bins=5)
qplot(claim_area, data=data, color=fraudulent , fill=fraudulent)
qplot(police_report, data=data, color=fraudulent , fill=fraudulent)
cqplot(claim_type, data=data, color=fraudulent , fill=fraudulent)
qplot(claim_amount, data=data, color=fraudulent , fill=fraudulent,bins=5)
qplot(total_policy_claims, data=data, color=fraudulent , fill=fraudulent)

qplot(fraudulent, data=data, color=fraudulent , fill=fraudulent)

data <- read.csv("C:/Users/ulric/OneDrive/Bureau/data/Data_Projet_1.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
data <- subset(data, select = -claim_id)
data <- subset(data, select = -customer_id)
data <- subset(data, select = -fraudulent)

dmatrix <- daisy(data)
summary(dmatrix)

km <- kmeans(dmatrix,10)
data <- read.csv("C:/Users/ulric/OneDrive/Bureau/data/Data_Projet_1.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
data <- data.frame(data, km$cluster)
qplot(km.cluster, data=data, color=fraudulent , fill=fraudulent)

km <- kmeans(dmatrix,4)
data <- read.csv("C:/Users/ulric/OneDrive/Bureau/data/Data_Projet_1.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
data <- data.frame(data, km$cluster)
qplot(km.cluster, data=data, color=fraudulent , fill=fraudulent)

km <- kmeans(dmatrix,50)
data <- read.csv("C:/Users/ulric/OneDrive/Bureau/data/Data_Projet_1.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
data <- data.frame(data, km$cluster)
qplot(km.cluster, data=data, color=fraudulent , fill=fraudulent)

#---Cluster-dbscan

data <- read.csv("C:/Users/ulric/OneDrive/Bureau/data/Data_Projet_1.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
data <- subset(data, select = -claim_id)
data <- subset(data, select = -customer_id)
data <- subset(data, select = -fraudulent)

dmatrix <- daisy(data)
summary(dmatrix)

km <- dbscan(dmatrix,eps=0.15,MinPts = 5)
data <- read.csv("C:/Users/ulric/OneDrive/Bureau/data/Data_Projet_1.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
data <- data.frame(data, km)
qplot(km.cluster, data=data, color=fraudulent , fill=fraudulent)


clustering <- function(data,n){
  dmatrix <- daisy(data)
  km <- kmeans(dmatrix,n)
  return(km$cluster)
}

data <- read.csv("C:/Users/ulric/OneDrive/Bureau/data/Data_Projet_1.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
data <- subset(data, select = -claim_id)
data <- subset(data, select = -customer_id)
data <- subset(data, select = -fraudulent)
dmatrix <- daisy(data)
agn <- agnes(dmatrix)
agn4 <- cutree(agn, k=15)
data <- read.csv("C:/Users/ulric/OneDrive/Bureau/data/Data_Projet_1.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
data <- data.frame(data, agn4)
qplot(agn4, data=data, color=fraudulent , fill=fraudulent)


clustering2 <- function(data,n){
  dmatrix <- daisy(data)
  agn <- agnes(dmatrix)
  agn4 <- cutree(agn, k=n)
  return(agn4)
}

n = 100
data <- read.csv("C:/Users/ulric/OneDrive/Bureau/data/Data_Projet_1.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
data <- subset(data, select = -claim_id)
data <- subset(data, select = -customer_id)
data <- subset(data, select = -fraudulent)
km <- clustering(data,n)
data <- read.csv("C:/Users/ulric/OneDrive/Bureau/data/Data_Projet_1.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
data <- data.frame(data, km$cluster)
qplot(km.cluster, data=data, color=fraudulent , fill=fraudulent)

data <- read.csv("C:/Users/ulric/OneDrive/Bureau/data/Data_Projet_1.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
data <- subset(data, select = -claim_id)
data <- subset(data, select = -customer_id)
data_EA <- data[1:800,]
data_ET <- data[801:1100,]
tree1 <- rpart(fraudulent ~. , data=data_EA)
test <- predict(tree1, data_ET, type="class")

prob <- predict(tree1, data_ET, type="prob")
roc_pred <- prediction(prob[,2], data_ET$fraudulent)
roc_perf <- performance(roc_pred,"tpr","fpr")
plot(roc_perf,col='green')















#---Prédiction

reussite_save=0
save_roc=0
n = 25
for( i in 1:2){
  data <- read.csv("C:/Users/ulric/OneDrive/Bureau/data/Data_Projet_1.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
  data <- subset(data, select = -claim_id)
  data <- subset(data, select = -customer_id)
  
  data_EA <- data[1:800,]
  data_ET <- data[801:1100,]
  data_ET_test <- data[801:1100,]
  
  nb_aleatoire1=50
  nb_aleatoire2=1
  nb_aleatoire3=1
  
  yes = sum(data_EA$fraudulent=='Yes')
  no = sum(data_EA$fraudulent=='No')
  print(abs(yes-no))
  
  build=data_EA[data_EA$fraudulent=="Yes", ]
  new_data = data.frame()
  for(i in sample(1:yes, nb_aleatoire1 ,replace=T)){
    new_data = rbind(new_data, build[i,])
  }
  
  data_EA=rbind(data, new_data)
  
  positiveWeight = 3.5
  negativeWeight = 1
  
  
  modelWeights <- ifelse(data_EA$fraudulent== 'Yes', positiveWeight, negativeWeight)
  
  tree3 <- tree(fraudulent ~. , data=data_EA)
  tree2 <- rpart(fraudulent ~. , data=data_EA)
  tree1 <- C5.0(fraudulent ~. , data=data_EA)
  
  test3 <- predict(tree3, data_ET, type="class")
  test2 <- predict(tree2, data_ET, type="class")
  test <- predict(tree1, data_ET) #, type="prob"
  
  
  reussite=sum(diag(table(test, data_ET$fraudulent)))/sum(table(test, data_ET$fraudulent))
  echec=1-reussite
  
  reussite2=sum(diag(table(test2, data_ET$fraudulent)))/sum(table(test2, data_ET$fraudulent))
  echec2=1-reussite2
  
  reussite3=sum(diag(table(test3, data_ET$fraudulent)))/sum(table(test3, data_ET$fraudulent))
  echec3=1-reussite3
  
  prob <- predict(tree1, data_ET, type="prob")
  roc_pred <- prediction(prob[,2], data_ET$fraudulent)
  roc_perf <- performance(roc_pred,"tpr","fpr")
  
  auc <- performance(roc_pred, "auc")
  roc = attr(auc, "y.values")[[1]][1]
  
  
  prob2 <- predict(tree2, data_ET, type="prob")
  roc_pred2 <- prediction(prob2[,2], data_ET$fraudulent)
  roc_perf2 <- performance(roc_pred2,"tpr","fpr")
  
  auc2 <- performance(roc_pred2, "auc")
  roc2 = attr(auc2, "y.values")[[1]][1]
  
  
  prob3 <- predict(tree3, data_ET)
  roc_pred3 <- prediction(prob3[,2], data_ET$fraudulent)
  roc_perf3 <- performance(roc_pred3,"tpr","fpr")
  
  auc3 <- performance(roc_pred3, "auc")
  roc3 = attr(auc3, "y.values")[[1]][1]
  
}

plot.new() 
plot(roc_perf, col = "green",)
plot(roc_perf2, col = 'red', add = TRUE)
plot(roc_perf3, col = 'blue', add = TRUE)

legend( 0, 1, legend=c("C5.0", "rpart",'tree'),
        col=c('green',"red", "blue"), lty=1:2, cex=0.8)


print("C5.0: ")
print(table(test, data_ET$fraudulent))
print(reussite)
print(1-reussite)
print(roc)


print("rpart: ")
print(table(test2, data_ET$fraudulent))
print(reussite2)
print(1-reussite2)
print(roc2)


print("tree: ")
print(table(test3, data_ET$fraudulent))
print(reussite3)
print(1-reussite3)
print(roc3)
















#--Boucle finale


reussite_save=0
save_roc=0
n = 25
for( i in 1:100){
  data <- read.csv("C:/Users/ulric/OneDrive/Bureau/data/Data_Projet_1.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
  data <- subset(data, select = -claim_id)
  data <- subset(data, select = -customer_id)
  
  
  # fraudulent <- data$fraudulent
  # 
  # data <- subset(data, select = -fraudulent)
  # km <- clustering2(data,n)
  # 
  # data <- data.frame(data, km)
  # data <- data.frame(data, fraudulent)
  
  
  data_EA <- data[1:800,]
  data_ET <- data[801:1100,]
  data_ET_test <- data[801:1100,]
  
  #summary(km4)
  #qplot(km4$cluster, data=data, fill=data$fraudulent)
  #data <- subset(data, select = -claim_id)
  #data <- subset(data, select = -customer_id)
  
  
  #dmatrix_ET <- daisy(data_ET)
  #km4_ET <- kmeans(dmatrix_ET, 2)
  #produit_km4 <- data.frame(data_ET, km4_ET$cluster)
  #data_ET <- produit_km4
  
  #colnames(data_ET)[colnames(data_ET) == "km4_ET.cluster"] <- "cluster"
  
  
  #nb_aleatoire1=100
  nb_aleatoire1=50
  nb_aleatoire2=1
  nb_aleatoire3=1
  
  #nb_aleatoire1=sample(1:250,1)
  #nb_aleatoire2=runif(1,0,10)
  
  yes = sum(data_EA$fraudulent=='Yes')
  no = sum(data_EA$fraudulent=='No')
  print(abs(yes-no))
  
  build=data_EA[data_EA$fraudulent=="Yes", ]
  new_data = data.frame()
  for(i in sample(1:yes, nb_aleatoire1 ,replace=T)){
    new_data = rbind(new_data, build[i,])
  }
  qplot(fraudulent, data=new_data, color=fraudulent , fill=fraudulent)
  
  data_EA=rbind(data, new_data)
  
  #data_EA <- subset(dat)
  
  #positiveWeight = nb_aleatoire3 / (nrow(subset(data_EA, fraudulent == 'Yes')) / nrow(data_EA))
  #negativeWeight = nb_aleatoire2 / (nrow(subset(data_EA, fraudulent != 'Yes')) / nrow(data_EA))
  
  positiveWeight = 3.5
  negativeWeight = 1
  
  
  modelWeights <- ifelse(data_EA$fraudulent== 'Yes', positiveWeight, negativeWeight)
  
  #dmatrix_EA <- daisy(data_EA)
  #km4_EA <- kmeans(dmatrix_EA, 2)
  #produit_km4 <- data.frame(data_EA, km4_EA$cluster)
  #data_EA <- produit_km4
  #colnames(data_EA)[colnames(data_EA) == "km4_EA.cluster"] <- "cluster"
  
  #tree1 <- rpart(fraudulent ~. , data=data_EA , weights=modelWeights)
  tree1 <- C5.0(fraudulent ~. , data=data_EA , weights=modelWeights)
  
  #test <- predict(tree1, data_ET, type="class")
  test <- predict(tree1, data_ET) #, type="prob"
  
  
  reussite=sum(diag(table(test, data_ET$fraudulent)))/sum(table(test, data_ET$fraudulent))
  echec=1-reussite
  
  prob <- predict(tree1, data_ET, type="prob")
  roc_pred <- prediction(prob[,2], data_ET$fraudulent)
  roc_perf <- performance(roc_pred,"tpr","fpr")
  
  auc <- performance(roc_pred, "auc")
  attr(auc, "y.values")
  
  if(attr(auc, "y.values")[[1]][1]>save_roc && reussite>reussite_save){
    test_save=test
    tree_save=tree1
    save_roc=attr(auc, "y.values")[[1]][1]
    save_roc_plot=roc_perf
    reussite_save=reussite
    
    #km_save_ET<-km4_ET
    #km_save_EA<-km4_EA
    
    test_factor <- as.factor(test)
    test_verif <- as.factor(data_ET$fraudulent)
    confusion <- confusionMatrix(test_factor,test_verif)
  }
  
}

#prp(tree_save, type=1, extra=1, box.palette = "auto")
plot(save_roc_plot, col = "green")


print(table(test_save, data_ET_test$fraudulent))
print(reussite_save)
print(1-reussite_save)
print(save_roc)



data <- read.csv("C:/Users/ulric/OneDrive/Bureau/data/Data_Projet_1_New.csv", header = TRUE, sep = ",", dec = ".", stringsAsFactors = TRUE)
data_prediction <- predict(tree_save, data)
data_prediction_proba <- predict(tree_save, data, type='prob')[1:200] #colomne des NO
data <- data.frame(data, data_prediction)

Proba <- c()
for(i in 1:200){
  if(data_prediction_proba[i]>=0.5){
    p = data_prediction_proba[i]
  }
  else{
    p = 1-data_prediction_proba[i]
  }
  Proba = c(Proba,p)
}

data <- data.frame(data, Proba)

data <- subset(data, select = -claim_id)
data <- subset(data, select = -age)
data <- subset(data, select = -gender)
data <- subset(data, select = -incident_cause)
data <- subset(data, select = -days_to_incident)
data <- subset(data, select = -claim_area)
data <- subset(data, select = -police_report)
data <- subset(data, select = -claim_type)
data <- subset(data, select = -claim_amount)
data <- subset(data, select = -total_policy_claims)

write.table(x = data, file = "monFichier2.csv")

