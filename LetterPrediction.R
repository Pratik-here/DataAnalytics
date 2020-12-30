letters = read.csv('letters_ABPR.csv')
letters$isB = as.factor(letters$letter == "B")

library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
spl = sample.split(letters$letter, SplitRatio = 0.5)
train = letters[spl == TRUE,]
test = letters[spl == FALSE,]

table(letters$isB)
2350/(2350+766)

#CART Implement
treeMod = rpart(isB ~ . - letter, data = train, method = 'class')
pred <- predict(treeMod, newdata = test ,type = 'class')
table(test$isB, pred)
(1133+294)/(1133+294+89+42)

#Random Forest Implement
set.seed(1000)
RandForest.Model = randomForest(isB ~ . - letter, data = train , method = "class")
pred.RF = predict(RandForest.Model, newdata = test, type = 'class')
table(test$isB, pred.RF)
(1164+357)/(26+11 +1164+357)

#Building a Multiclass Classification Random Forest
letters$letter = as.factor(letters$letter)
set.seed(2000)
train = letters[spl == TRUE,]
test = letters[spl == FALSE,]

#baseline model is to predict the most frequent class among all.
table(letters$letter)
803/(803+789+766+758)

#CART Model
MultiMod.CART = rpart(letter ~ . - isB, data = train , method = 'class')
pred.MM = predict(MultiMod.CART , newdata = test , type = 'class')
table(test$letter, pred.MM)

#accuracy
(346+295+369+347)/nrow(test)


#Model New Random Forest
MultiMod = randomForest(letter ~ . - isB, data = train , method = 'class')
pred.MM = predict(MultiMod, newdata = test , type = 'class')
table(test$letter, pred.MM)

(391+377+393+367)/(391+377+393+367+4+6+20)

