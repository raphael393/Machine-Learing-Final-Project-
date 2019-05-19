library(caret)
library(readr)
library(rpart)
library(randomForest)
library(ggplot2)
library(rpart.plot)
library(lattice)

set.seed(4321)

## Loading Packages
pml.training <- read_csv("pml-training.csv")
pml.testing <- read_csv("pml-testing.csv")

pml.training$classe <- as.factor(pml.training$classe)
pml.testing$classe <- as.factor(pml.testing$classe)

## Deleting columns with no variables
pml.training<-pml.training[,colSums(is.na(pml.training)) == 0]
pml.testing <-pml.testing[,colSums(is.na(pml.testing)) == 0]

## Deleting columns that are not needed
pml.training <-pml.training[,-c(1:7)]
pml.testing <-pml.testing[,-c(1:7)]

## So what do we have
dim(pml.testing)
dim(pml.training)
names(pml.training)

## Performing partitions using caret package 
intrain <- createDataPartition(y=pml.training$classe, p=3/4, list=FALSE)
training <- pml.training[intrain,]
testing <- pml.training[-intrain,]
dim(training)
dim(testing)

##What data do we have
qplot(x=training$classe,main = "Variable Classe in the 
      Training set", data=training,geom="bar",xlab="Levels",ylab="Frequency")

## Let's model 
modfit1 <- rpart(classe~.,data=training,method="class")
prediction1<- predict(modfit1,testing,type="class")
confusionMatrix(prediction1,testing$classe)


##Model 2
modfit2 <- train(classe~.,data=training, method="rf",ntree=20)
prediction2 <- predict(modfit2,testing)
confusionMatrix(prediction2,testing$classe)

#Predict final 
finalpredict <- predict(modfit2,pml.testing)
finalpredict