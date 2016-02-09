setwd('~/Desktop/personal/kaggle/Africa Soil Property Prediction Challenge/code/')

trainRaw <- read.csv('../data/training.csv')
testRaw <- read.csv('../data/sorted_test.csv')
names(trainRaw)[1:3595] <- paste0("V",1:3595)
names(testRaw)[1:3595] <- paste0("V",1:3595)

## Pre Processing
preprocessModel <- preProcess(trainRaw[,2:3594], method = "BoxCox")
train <- predict(preprocessModel, trainRaw[, 2:3594])
train <- cbind(train, trainRaw[, 3595:3600])
colnames(train)[3594] <- "type"

test <- predict(preprocessModel, testRaw[, 2:3594])
test <- cbind(test, testRaw[, 3595])
colnames(test)[3594] <- "type"

## initalize H2O
library(h2o)
localH2O <- h2o.init(max_mem_size = '3g')

## Push Dataset in H2O
train.h2o <- as.h2o(localH2O, train)
test.h2o <- as.h2o(localH2O, test)


## Create Model
nameLabel <- c("Ca", "P", "pH", "SOC", "Sand")
outputPredict <<- list()
for(i in 4:length(nameLabel))
{
  predictLabel(3594 +i)
}
predictLabel <- function(outputIndex){
                          cat("Building model for ", nameLabel[i], "\n")
                          model <- h2o.deeplearning(x = 1:3594,
                                                    y = outputIndex,
                                                    data = train.h2o,
                                                    nfolds = 10,
                                                    activation = "Rectifier",
                                                    hidden = c(50, 50, 50),
                                                    epochs = 100,
                                                    classification = FALSE,
                                                    balance_classes = FALSE)
                              
                            print(model)
                            cat("Predicting value for", nameLabel[i], "\n")
                            predict <- as.matrix(h2o.predict(model, test.h2o))
                            outputPredict <<- cbind(outputPredict, predict)
}


soc_predict <- read.csv('soc.csv', quote = '')
sand_predict <- read.csv('sand.csv', quote = '')

predictOutput <- read.csv('predict_ca_p_ph.csv', quote = '')
predictOutput$SOC <- soc_predict$SOC
predictOutput$Sand <- sand_predict$Sand

predictOutput <- cbind(testRaw$V1, predictOutput )
