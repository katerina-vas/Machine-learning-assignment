install.packages("caret")
library(caret)
library(randomForest)
setwd("C:\\Users\\kater_000\\Downloads")

training<-read.csv("pml-training.csv",header = TRUE,na.strings=c("NA","#DIV/0!",""))#, stringsAsFactors = F )
testing <-read.csv("pml-testing.csv",header = TRUE ,na.strings=c("NA","#DIV/0!",""))#, stringsAsFactors = F)



#### cross validation
inTrain <- createDataPartition(y=training$classe, p=0.60, list=FALSE)
train <- training[inTrain, ] 
test<- training[-inTrain, ]

removeNAcols   <- function(x) { x[ , colSums( is.na(x) ) < nrow(x) ] }
train <- removeNAcols(train)
test  <- removeNAcols(test)
complete       <- function(x) {x[,sapply(x, function(y) !any(is.na(y)))] }
incompl        <- function(x) {names( x[,sapply(x, function(y) any(is.na(y)))] ) }
trtr.na.var    <- incompl(train)
trts.na.var    <- incompl(test)
train <- complete(train)
test  <- complete(test)

#prepare testing
testing.clean  <- removeNAcols(testing)
testing.clean <- testing.clean[,-c(60)]
testing.clean$magnet_dumbbell_z <- as.numeric(testing.clean$magnet_dumbbell_z)
testing.clean$magnet_forearm_y <- as.numeric(testing.clean$magnet_forearm_y)
testing.clean$magnet_forearm_z <- as.numeric(testing.clean$magnet_forearm_z)
mydummy <- rbind(testing.clean, train[,-60])
mydummy <- mydummy[c(1:20),]
random.forest <- train(train[,-60],
                       train$classe,
                       tuneGrid=data.frame(mtry=3),
                       trControl=trainControl(method="none")
)

confusionMatrix(predict(random.forest,
                        newdata=test[,-60]),
                test$classe
)
