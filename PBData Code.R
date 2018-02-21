###########################INITIALISATIONS
install.packages("tree")
library(tree)

install.packages("e1071")
library(e1071)

install.packages("adabag")
library(adabag)

install.packages("randomForest")
library(randomForest)

install.packages("ROCR")
library(ROCR)


#Import dataset as data frame
PBRead = read.csv("PBData.csv")

#create random seed
set.seed(26875730) 
PBD = PBRead[sample(nrow(PBRead), 1000), ] # sample 1000 rows
rm(PBRead) 
newPBD = na.omit(PBD) #remove na values
a = table(newPBD$subscribed) 

#Percentage of SUCCESSFUL subscribed clients
ratio = a[[1]]/(a[[2]] + a[[1]]) * 100
summary(newPBD) #summary of all atributes

attach(newPBD)



#split data up into training and test data set
set.seed(26875730)
train.row = sample(1:nrow(newPBD), 0.7*nrow(newPBD))
PBD.train = newPBD[train.row, ]
PBD.test = newPBD[-train.row, ]
rm(PBD)



#############Decision Tree
tree.fit = tree(subscribed ~. -day -month -duration, data = PBD.train)
plot(tree.fit)
text(tree.fit, pretty = 0)

###type = vector gives probability
tree.predict.raw = predict(tree.fit, PBD.test, type = "vector")
###type = class predicts outcome
tree.predict = predict(tree.fit, PBD.test, type = "class")
table(actual = PBD.test$subscribed, predicted = tree.predict)

#############Naive Bayes 
###use setdiff to exclude columns day, month and duration
naive.model = naiveBayes(subscribed ~., data = PBD.train[, setdiff(colnames(PBD.train), c("day", "month", "duration"))])
naive.predict.raw = predict(naive.model, PBD.test, type = "raw")
naive.predict = predict(naive.model, PBD.test)
table(actual = PBD.test$subscribed, predicted = naive.predict)

#############Bagging
bag.model = bagging(subscribed ~., data = PBD.train[, setdiff(colnames(PBD.train), c("day", "month", "duration"))], mfinal = 5)
bag.predict = predict.bagging(bag.model, newdata = PBD.test)

table(actual = PBD.test$subscribed, predicted = bag.predict$class)

#############Boosting
boost.model = boosting(subscribed ~., data = PBD.train[, setdiff(colnames(PBD.train), c("day", "month", "duration"))], mfinal = 10)
boost.predict = predict.boosting(boost.model, newdata = PBD.test)
table(actual = PBD.test$subscribed, predicted = boost.predict$class)

#############Random Forest
random.model = randomForest(subscribed ~., data = PBD.train[, setdiff(colnames(PBD.train), c("day", "month", "duration"))])
random.predict = predict(random.model, PBD.test)
random.predict.raw = predict(random.model, PBD.test, type = "prob")
table(actual = PBD.test$subscribed, predicted = random.predict)


#############ROC
######Test performance of each method and measure accuracy
###decision tree
tree.pred.roc = prediction(tree.predict.raw[,2], PBD.test$subscribed)
tree.perf = performance(tree.pred.roc,"tpr","fpr")
plot(tree.perf, col = "red")
abline(0,1)
cauc = performance(tpred, "auc")
print(as.numeric(cauc@y.values))

###naive bayes
naive.pred.roc = prediction(naive.predict.raw[,2], PBD.test$subscribed)
naive.perf = performance(naive.pred.roc,"tpr","fpr")
plot(naive.perf, add = TRUE, col = "blue")
abline(0,1)
cauc = performance(tpred, "auc")
print(as.numeric(cauc@y.values))

###bagging
bag.pred.roc = prediction(bag.predict$prob[,2], PBD.test$subscribed)
bag.perf = performance(bag.pred.roc,"tpr","fpr")
plot(bag.perf, add = TRUE, col = "green")
abline(0,1)
cauc =	performance(tpred,	"auc")
print(as.numeric(cauc@y.values))

###boosting
bost.pred.roc = prediction(boost.predict$prob[,2], PBD.test$subscribed)
boost.perf = performance(bost.pred.roc,"tpr","fpr")
plot(boperf, add = TRUE, col = "brown")
abline(0,1)
cauc =	performance(tpred,	"auc")
print(as.numeric(cauc@y.values))

#random forest
forest.predict.roc = prediction(random.predict.raw[,2], PBD.test$subscribed)
forest.perf = performance(forest.predict.roc,"tpr","fpr")
plot(forest.perf, add = TRUE, col = "lightblue")
abline(0,1)
cauc =	performance(tpred,	"auc")
print(as.numeric(cauc@y.values))

legend('bottomright', legend = c("Decision Tree", "Naive Bayes", "Bagging", "Boosting", "Random Forest"), 
       col = c("red", 'blue', 'green', 'brown', 'lightblue'), bty='n', cex=0.75, lty=c(1,1), lwd = 2)

title(main="ROC curve")
