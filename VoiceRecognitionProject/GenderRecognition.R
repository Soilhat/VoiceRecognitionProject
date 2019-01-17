dataset <- read.csv("voice.csv")
library(caTools);
set.seed('12')
split = sample.split(dataset$label, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Logistic Regression
summary(glm(formula = label ~ meandom, family = "binomial", data = training_set))
model <- glm(formula = label ~ ., family = "binomial", data = training_set) # 0.9760101 accuracy
#model <- glm(label ~ minfun + meanfun + mode + sfm + sp.ent + Q25 + sd, family = binomial, data = training_set)#0.9734848 accuracy

#Prediction
test <- test_set[1,1:20]
predictor = predict(model, newdata = test, type="response")
avgpredictor = ifelse(predictor > 0.5, 1,0)

#ConfusionMatrix
cm = table(test[,21], avgpredictor)
cm
mosaicplot(cm,col=sample(1:8,2))
accuracy = (cm[1,1] + cm[2,2]) / sum(cm)
accuracy

#Random Forest

# install.packages("randomForest")
library(randomForest)
modelForest <- randomForest(label ~., data= training_set, ntree = 500, na.action = na.omit)
modelForest
cmF <- table(predict(modelForest, test_set), test_set$label)
(cmF[1,1] + cmF[2,2]) / sum(cmF) # accuracy

# Decision Tree

library(rpart)
tree <- rpart (label ~ ., training_set)
plot(tree)
text(tree)

findgender <- function(voicedata) # Using Logistic Regression
{
  dataset <- read.csv("voice.csv")
  library(caTools);
  model <- glm(formula = label ~ ., family = "binomial", data = dataset)
  predictor = predict(model, newdata = voicedata, type="response")
  return(ifelse(predictor > 0.5, "Male", "Female"))
}

test = findgender(dataset[1,1:20])
test