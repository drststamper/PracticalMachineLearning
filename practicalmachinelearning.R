practicalmachinelearning <- function{
  
#load data and perform exploratory analysis
require(ggplot2)
require(caret)
require(randomForest)
data <- read.csv("pml-training.csv", stringsAsFactors=FALSE, na.strings=c("NA", "NULL"))
testdata <- read.csv("pml-testing.csv", stringsAsFactors=FALSE, na.strings=c("NA", "NULL"))
data$classe <- as.factor(data$classe)

#quick look at class distribution
class.histogram <- ggplot(data, aes(x=classe, fill = classe)) +
  geom_histogram()+
  labs(title = "Figure A: histogram of classe variable", x = "the class", y = "total nr of cases for that class")
class.histogram  

#select only non-NA value columns for training + testdata files
indices <- grepl("_x$|_y$|_z$|^roll_|^pitch_|^yaw_|^total_accel_|classe", names(data))
data <- data[,indices]
indices_test <- grepl("_x$|_y$|_z$|^roll_|^pitch_|^yaw_|^total_accel_|problem_id", names(testdata))
testdata <- testdata[,indices_test]


#divide train data into train (60%) & validation (40%)
inTrain <- createDataPartition(y=data$classe, p=0.6, list=FALSE)
training <- data[inTrain,]
validation <- data[-inTrain,]

#make results reproducible by setting seed
set.seed(123)

#train model (randomForest, using 600 trees instead of 500)
model <- randomForest(x=training[,-53], y=training$classe, ntree=600, keep.forest=TRUE)
model 

#validate model with validation set
confusionMatrix(validation$classe, predict(model, validation))

#apply to testdata
answers <- as.character(predict(model, testdata))

#taken from provided code on submission page of assignment
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

#write the answers for the prediction to files
pml_write_files(answers)








}