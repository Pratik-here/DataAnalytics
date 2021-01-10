FluTrain <- read.csv('FluTrain.csv')
which.max(FluTrain$ILI)
FluTrain$Week[303]

hist(FluTrain$ILI)
plot(log(FluTrain$ILI),FluTrain$Queries)
hist(log(FluTrain$ILI))

LogMod = lm(log(ILI) ~ Queries, data = FluTrain)
summary(LogMod)
cor(FluTrain$ILI, FluTrain$Queries)
# In case of a single dep / independent var, cor* cor = R sq.

test <- read.csv('FluTest.csv')
pred <- predict(LogMod, newdata = test)
which(test == '2012-03-11 - 2012-03-17')
pred <- exp(pred) # change the predictions to base values.

library(zoo)
ILIlag = lag(zoo(FluTrain$ILI),-2,na.pad = TRUE)
summary(ILIlag)

plot(log(ILIlag), log(FluTrain$ILI)) #Strong Linear Relation

Mod2 = lm(ILI ~ Queries + log(ILIlag), data = FluTrain)
summary(Mod2)

# The second model is a better model since it has a higher R squared.
# It also uses a time series lag term 
# We use a log instead of a normal pred because the hist is skewed.



