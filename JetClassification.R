# Setting the working directory
setwd("~/Clones/jet-classification")

# Importing the dataset
signal_data <- read.table('signal_PU0_13TeV_MJ-65-95_PTJ-250-300_ext.txt')
print(dim(signal_data))
print(as.matrix(signal_data[1:10,625]))

# Printing the Heatmap
mean_signal <- colMeans(signal_data)
dim(mean_signal) <- c(25,25)
colfunc <- colorRampPalette(c("#FCEFD0", "#751503"))
image(mean_signal, col=colfunc(10))

# Adding "1" after every result, classifing them as a signal
signal_data[,626] <- 1
print(as.matrix(signal_data[1:10,626]))

# Repeat preprocessing steps with the background data
background_data = read.table('background_PU0_13TeV_MJ-65-95_PTJ-250-300_ext.txt')
print(dim(background_data))

mean_background <- colMeans(background_data)
dim(mean_background) <-c(25, 25)
image(mean_background, col=colfunc(10))

background_data[,626] <- 0
print(as.matrix(background_data[1:10,626]))

# Concatenating frames and shuffling
full_data <- rbind(signal_data,background_data)
set.seed(123) # reproduce results
full_data <- full_data[sample(nrow(full_data)),] 
print(full_data[1:10,626])

# Defining and Training the model
## Separing Training and Test set
library(caTools)
set.seed(123)
split = sample.split(full_data[[626]], SplitRatio = 0.7)
training_set = subset(full_data, split == TRUE)
test_set = subset(full_data, split == FALSE)
print(dim(training_set))
print(dim(test_set))

## Removing zero variance columns
test_set <- test_set[,apply(training_set, 2, var, na.rm=TRUE) != 0]
training_set <-training_set[,apply(training_set, 2, var, na.rm=TRUE) != 0]

# Applying PCA
# install.packages('caret')
library(caret)
# install.packages('e1071')
library(e1071)
pcaElem = 60
pca = preProcess(x = training_set[-540], method = 'pca', pcaComp = pcaElem) # 100 elem. to use the 10x10 square seen around the jets  
training_set = predict(pca, training_set)
training_set = training_set[c(2:(pcaElem+1),1)]
test_set = predict(pca, test_set)
test_set = test_set[c(2:(pcaElem+1),1)]

## Fitting Logistic Regression to the Training set
classifier <- glm(formula = V626 ~ .,
                family = binomial,
                data = training_set)

## Predicting the Test set results
prob_pred <- predict(classifier, type = 'response', newdata = test_set[-(pcaElem+1)])
y_pred <- ifelse(prob_pred > 0.5, 1, 0)

## Making the Confusion Matrix
cm <- table(test_set[, (pcaElem+1)], y_pred > 0.5)

## Computing accuracy
misClasificError <- mean((y_pred > 0.5) != test_set[(pcaElem+1)])
print(paste('Accuracy',1-misClasificError))