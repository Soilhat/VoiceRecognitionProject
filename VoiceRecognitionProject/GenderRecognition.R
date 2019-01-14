dataset <- read.csv("voice.csv")
library(caTools);
set.seed('79')
split = sample.split(dataset$label, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
# Logistic Regression
summary(glm(formula = label ~ meandom, family = "binomial", data = training_set))
model <- glm(formula = label ~ ., family = "binomial", data = training_set) # 0.9760101 accuracy
#model <- glm(label ~ minfun + meanfun + mode + sfm + sp.ent + Q25 + sd, family = binomial, data = training_set)#0.9734848 accuracy
#Prediction
predictor = predict(model, newdata = test_set, type="response")
avgpredictor = ifelse(predictor > 0.5, 1,0)
#ConfusionMatrix
cm = table(test_set[,21], avgpredictor)
cm
accuracy = (cm[1,1] + cm[2,2]) / sum(cm)
