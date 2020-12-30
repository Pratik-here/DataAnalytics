census = read.csv('census.csv',stringsAsFactors = TRUE)

#Log Reg Model
library(caTools)
library(ROCR)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)

set.seed(2000)

spl = sample.split(census$over50k, SplitRatio = 0.6)
train = census[spl == T,]
test = census[spl == F,]

regMod = glm(over50k ~ ., data = train, family = "binomial")
summary(regMod)

pred <- predict(regMod, newdata = test, type = 'response') 
#used to return values to natural scale  ( using type = "response")
table(test$over50k, pred > 0.5)
(9051+1888)/nrow(test)

table(test$over50k)
9713/nrow(test)

#auc for the model

pred.forauc = prediction(pred, test$over50k)
auc = as.numeric(performance(pred.forauc,"auc")@y.values)


#Building a Tree
treeMod = rpart(over50k ~. , data = train , method = 'class')
prp(treeMod)
pred.treeMod = predict(treeMod, newdata = test, type = 'class')
table(test$over50k,pred.treeMod)
(9243+1596)/nrow(test)

#CART model performs worse on out of sample dataset when compared with logistic Reg Model.

#checking ROC and AUC
pred.treeMod  = predict(treeMod, newdata = test)
pred.treeMod = pred.treeMod[,2]
roc.pred <- prediction(pred.treeMod,test$over50k)
as.numeric(performance(roc.pred,"auc")@y.values)

#Random Forest Method
set.seed(1)
trainSmall = train[sample(nrow(train), 2000), ] #Random Sampling

RF.Mod = randomForest(over50k ~ . , data = trainSmall)
pred.RF.Mod <- predict(RF.Mod, newdata = test )
prop.table(table(test$over50k,pred.RF.Mod))

#checking the number of variables used
vu = varUsed(RF.Mod, count=TRUE)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(RF.Mod$forest$xlevels[vusorted$ix]))

#Impurity 
varImpPlot(RF.Mod)

#k-fold cross validation
tr.control = trainControl(method ='cv', number = 10)
cp.grid =expand.grid(.cp = seq(.002,.1,0.002))
tr = train(over50k ~ . , 
           data = train, 
           method = "rpart",
           trControl = tr.control,
           tuneGrid = cp.grid)

#best Tree Fit
best.tree = rpart(over50k ~ . ,data = train ,method = 'class',cp = 0.002 )
pred.best.tree  = predict(best.tree, test, type = 'class')
table(test$over50k,pred.best.tree)
SSE = sum((best.tree.pred - test$MEDV)**2)