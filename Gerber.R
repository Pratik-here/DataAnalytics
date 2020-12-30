gerber = read.csv('gerber.csv')
str(gerber)
summary(gerber)

#prop of people who voted
sum(gerber$voting[gerber$voting == 1])/nrow(gerber) 

#group by control groups -> percent of people who voted

hawthrone<-table(gerber$voting, gerber$hawthorne)
prop.table(hawthrone) #.036

civicduty<-table(gerber$voting, gerber$civicduty)
prop.table(civicduty) #.034

neighbors<-table(gerber$voting, gerber$neighbors)
prop.table(neighbors) #.041

control<-table(gerber$voting, gerber$control)
prop.table(control) #.164 

#Logistic regression model
logMod = glm(voting ~ hawthorne + civicduty + neighbors + self + control + sex + yob, data = gerber)
summary(logMod)

#confusion matrix
pred <- predict(logMod, type = 'response')
table(gerber$voting, pred>0.5)
(554+234350)/(554 + 234350 + 108142 + 1038)

#Computing auc
library(ROCR)
ROCRpred = prediction(pred,gerber$voting)
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf)
auc <- as.numeric(performance(ROCRpred,"auc")@y.values)

#Using TREE model for comparision
library(rpart)
library(rpart.plot)

treeMod = rpart(voting ~ hawthorne + civicduty + neighbors + self + sex ,data = gerber, cp =0.0 )
prp(treeMod)

test.treeMod.1 = rpart(voting ~ control, data = gerber, cp=0)
prp(test.treeMod.1, digits = 6)

test.treeMod.2 = rpart(voting ~ control + sex, data = gerber, cp=0)
prp(test.treeMod.2, digits = 6)

test.logMod.1 = glm(voting ~ control + sex ,data = gerber, family = 'binomial')
summary(test.logMod.1)

#NEW way of evaluating logistic models
poss = data.frame(sex= c(0,0,1,1), control = c(0,1,0,1))
pred.test.logMod.1 = predict(test.logMod.1, newdata = poss, type = 'response')
pred.test.logMod.1

#order of pred test is : Male, NC : Male, C : Female, NC : Female, C 

#Final Log Model
final.LogMod = glm(voting ~ sex + control +sex:control, data = gerber, family = 'binomial')
summary(final.LogMod)
