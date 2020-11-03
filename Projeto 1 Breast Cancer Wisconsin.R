library(class)
library(rpart)
library(rpart.plot)
library(plot)
library(ggplot2)

#1
setwd('E:\\faculdade\\Inteligencia artificial');
data <-read.csv("wdbc.data", header= FALSE, sep=",");

View(data)

columnName<-c("ID", "Diagnosis", 
            "Mean_Radius", "Mean_Texture", 
            "Mean_Perimeter", "Mean_Area", 
            "Mean_Smoothness", "Mean_Compactness", 
            "Mean_Concavity", "Mean_Concave_Points", 
            "Mean_Symmetry", "Mean_Fractal_Dimension", 
            "SE_Radius", "SE_Texture", "SE_Perimeter", 
            "SE_Area", "SE_Smoothness", "SE_Compactness", 
            "SE_Concavity", "SE_Concave_Points", "SE_Symmetry", 
            "SE_Fractal_Dimension", "Worst_Radius", "Worst_Texture", 
            "Worst_Perimeter", "Worst_Area", "Worst_Smoothness", 
            "Worst_Compactness", "Worst_Concavity", "Worst_Concave_Points", 
            "Worst_Symmetry", "Worst_Fractal_Dimension")

for(i in 1:length(columnName))
{
  names(data)[names(data)==paste0("V",i)]<- columnName[i]
}

View(data)

empty <-is.na(data)

which(empty == TRUE)
  newData <-data[-1]
  
View(newData)

#2
set.seed(123)
testData <-sample(1:nrow(newData),round(0.75 * nrow(newData)))
setData<- newData[testData,]
setTest <- newData[-testData,]

#3
ggplot(setData, aes(Worst_Perimeter, Worst_Area, color = Worst_Smoothness)) +
  geom_point()

ggplot(setData, aes(Worst_Compactness, Worst_Concavity, color = Worst_Concave_Points)) +
  geom_point()

#4
classesSet <- setData[,1]
testSet <- setData[,-1]

classTest <-setTest[,1]
testTest <-setTest[,-1]

Ks<-c(1,3,5,11)

verifyKnn(testSet,testTest,Ks)

verifyKnn <- function(Trainer, Test, arrayK)
{
 for(i in arrayK){
   result<-knn(Trainer, Test, classesSet, arrayK)
   accuracy<-(sum(classTest==result)/length(classTest))
   print(paste0("Taxa de acerto:",accuracy))
 }
}

#5
model <- rpart(Diagnosis~.,setData, method="class", control = rpart.control(minsplit = 1))
plot <- rpart.plot(model, type=3, box.palette = "auto", shadow.col = 0,extra = "auto")

#6
accuracyTree <- setTest[,1]
testTree <- setTest[,-1]
prediction <- predict(model, testTree, type="class")
table(accuracyTree,prediction)

hitRate<-(sum(accuracyTree==pred)/length(accuracyTree))
print(paste0("Taxa de acerto:",hitRate))

#7
#Knn
accuracy<-(sum(classTest==result)/length(classTest))
print(paste0("Taxa de acerto:",accuracy))
#Arvore
hitRate<-(sum(accuracyTree==pred)/length(accuracyTree))
print(paste0("Taxa de acerto:",hitRate))
