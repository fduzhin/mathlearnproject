library(ggplot2)
library(rpart)
library(caret)
library(rpart.plot)
library(rattle)
library(randomForest)

# Windows
setwd("C:\\Users\\fduzhin\\Desktop\\Dropbox\\Data Analysis with R\\Mach learning project")

# Mac
setwd("/Users/orlenkoirina/Dropbox/Data Analysis with R/Mach learning project")

D <- read.csv("pml-training.csv")
T <- read.csv("pml-testing.csv")
dim(D)
summary(D)
names(D)
unique(D$classe)

training <- D[,8:160]
testing <- T[,8:160]
names(testing)
types <- data.frame(var=names(testing),type=as.character(lapply(testing,class)))
head(types)
head(testing[,1:8])

relevant_var <- (types$type=='numeric')|(types$type=='integer')
training <- training[,relevant_var]
training$classe <- D$classe
testing <- testing[,relevant_var]
dim(training)
dim(testing)



#### Let's try to create folds:

folds <- createFolds(y=training$classe,k=5,
                     list=TRUE,returnTrain=TRUE)

names(folds)

Mod <- as.list(1:5)

for (i in 1:5) {
        cur.train <- training[folds[[i]],]
        cur.test <- training[-folds[[i]],]
        cur.model <- train(classe ~ ., method="rpart", data=cur.train)
        fancyRpartPlot(cur.model$finalModel)
        Mod[[i]] <- cur.model
}

imp <- sapply(Mod,varImp)
imp
ind <- imp[[1]]>0
ind

imp[[1]]$Overall
Hru <- rownames(imp[[1]][imp[[1]]$Overall>0,])
Hru

#### End of folds strategy


### Create just one tree and figure out 14 important variables

modTree <- train(classe ~ ., method="rpart", data=training)
fancyRpartPlot(modTree$finalModel)
varImp(modTree)

important.var <- varImp(modTree)
class(important.var)
important.var
good.var <- row.names(important.var$importance)[1:14]
good.var

### Create training and validation sets for another model

inTrain <- createDataPartition(y=training$classe,
                               p=0.7, list=FALSE)
training.set <- training[inTrain,]
validation.set <- training[-inTrain,]
dim(training.set)
dim(validation.set)
good.var
training.set <- training.set[,c(good.var,"classe")]
validation.set <- validation.set[,c(good.var,"classe")]
testing <- testing[,c(good.var,"problem_id")]
qplot(data=training, x=magnet_dumbbell_y, y=pitch_forearm, colour=classe)

# GBM model

modGBM <- train(classe ~ ., method="gbm",data=training.set)
print(modGBM)

predictGBM <- predict(modGBM, newdata=validation.set)
C <- confusionMatrix(predictGBM,validation.set$classe)
names(C)
C$overall[1]
C$table

### Use the GBM model to solve the testing set

predict.test <- predict(modGBM, newdata=testing)

answers = predict.test 

pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

pml_write_files(answers)

# Model based prediction - bad accuracy

modlda = train(classe ~ .,data=training.set,method="lda")
predictLDA <- predict(modlda, newdata=validation.set)
confusionMatrix(predictLDA,validation.set$classe)


# Random forest

modForest <- train(classe ~ ., method="rf", data=training.set)



# Submit answers to the actual test set

answers = predictGBM 

pml_write_files = function(x){
        n = length(x)
        for(i in 1:n){
                filename = paste0("problem_id_",i,".txt")
                write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
        }
}

pml_write_files(answers)