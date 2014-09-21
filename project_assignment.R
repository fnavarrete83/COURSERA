#Loading the necessary libraries

library(rpart)
library(randomForest)
library(caret)
library(e1071)

# Setting the working directory. IT HAS TO BE CHANGED TO THE FOLDER
# WHERE THE DATA IS STORED (otherwise the script cannot run)

setwd("C:/COURSERA/2014/PracticalMachineLearning/ProjectWriteup")

# The training and testing data sets have to be read

training <- read.csv("pml-training.csv",header=TRUE,stringsAsFactors=FALSE)
testing <- read.csv("pml-testing.csv",header=TRUE,stringsAsFactors=FALSE)

inTrain <- createDataPartition(y=training$classe,p=0.6,list=FALSE)

training <- training[inTrain,]
validation <- training[-inTrain,]

# The data has to be pre-processed before having fun
# The first obvious thing that pops-out from the data is that there
# are a lot of features, and that many of these features have values NA

# The data classe classifier is plotted against index
# It is clear the step-like pattern

length(training$classe)
index <- seq(1,length(training$classe))
qplot(index,classe,data=training,na.rm=TRUE)
ratio <- double()
keeps <- character()
count=1

# I iterate over all the variables to find out which are mainly composed of NA
# values and remove them.

for (i in names(training)) {
    #print(length(training[[i]]))
    new <- is.na(training[[i]])
    #print (length(training$X[new]))
    #if (count == 20){
    #    print ("aqui!!!")
    #    print (training[[i]])[!new]
    #}
    newratio = (length(training$X[new]))/(length(training[[i]]))
    #print ((length(training$X[new])))
    #cat ((length(training$X[new])),newratio)
    ratio <- c(ratio,newratio)
    if (newratio <= 0.5 && typeof(training[[i]]) != 'character'){
        newkeep = i
        cat('count= ',count)
        print('')
        keeps <- c(keeps,newkeep)
    }
    if (typeof(training[[i]]) == 'character'){
        num_empty = sum(training[[i]] == '')
        newratio = num_empty/(length(training[[i]]))
        if (newratio <= 0.5){
            newkeep = i
            cat('count= ',count)
            print('')
            print('funciono')
            keeps <- c(keeps,newkeep)
        }        
     }
    count=count+1
}

training2 <- training[keeps]
validation2 <- validation[keeps] 

var <- sapply(training2,class)


qplot(X,classe,data=training2,na.rm=TRUE,color=user_name)
var2 = which(var == 'character')

training3 <- training2[,-var2]

M <- abs(cor(training3))
diag(M) <- 0
which(M > 0.8,arr.ind=T)
plot(training3$yaw_dumbbell,training3$accel_dumbbell_z)
# Now in this new set I'll fill the missed values 

nsv <- nearZeroVar(training3,saveMetrics=TRUE)

dummies <- dummyVars(classe ~ new_window,data=training2) 
head(predict(dummies,newdata=training2))

var3 = which(var != 'character')
subset <-  c(names(training2[var3]),names(training2)[60])

training4 <- training2[subset]
training4 <- training4[,5:57]

# Once tha data is filled out we proceed to adjust the values, so the 
# distributions are not so skewed



# All that is left now is to apply a method in order to find the best answer
# We will apply different methods and see when we get the best prediction on 
# the test data set

#subset <-  c(names(training2)[8:10],names(training2)[93])
subset <-  c(names(training2)[37:39],names(training2)[93])

training3 <-  training2[subset]
validation3 <-  validation2[subset]

ind0 <- which(training4$classe == "A") 
ind1 <- which(training4$classe != "A")

training4$classe[ind0] = 'yes'
training4$classe[ind1] = 'no'


classe=as.character(unique(training3$classe))
for (i in classe) {
    ind <- which(training$classe == i)
    print(i)
    print(length(ind))
}

training4$classe <- as.factor(training4$classe)

modFitAll <- train(classe ~ ., data=training4,method='glm')
modpred <- predict(modFitAll, validation3)
ind <- pred == validation3$classe
length(which(ind))/length(pred)

prediction <- predict(modFitAll,testing,type="response")



# Once this is finished we will apply all the method once again, but this time
# the test sets will be a fraction of the test data set. This allows us to compute
# errors.




# Now that we have compared methods onlhy using the training set to compute errors
# the different methods are applied to the real test data set and check the outcome


