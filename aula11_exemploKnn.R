#dividindo o dataset em treino e teste (80% treino e 20% teste)
library(class)

plot(iris, col= iris$Species)

set.seed(200)
index<-sample(1:nrow(iris),round(0.80*nrow(iris))) 


train<-iris[index, ]

test<-iris[-index, ]


classesTrain<-train[,5]
train<-train[,-5]

classesTest<-test[,5]
test<-test[,-5]

verificaKnn()


result<-knn(train, test, classesTrain, 1)  #k ={5,7,11}
table(result)
table(classesTest)
table(classesTest, result)

#Automatizar a verificação de k´s
#Apresentar a matriz de confusão de confusão de cada k
# Calcular índice de acertos de k

verificaKnn <- function(datasetTreino, datasetTest, vetorK)
{
  
}


