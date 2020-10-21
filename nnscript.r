library(neuralnet)
library(Metrics)
library(caret)
set.seed(123)
train.indices <- createDataPartition(numeric.data$Total.Smoking.Rate, p = .7, list = F)
train.data <- numeric.data[train.indices,]
test.data <- numeric.data[-train.indices,]


head(test.data)

nnet <- neuralnet(Total.Smoking.Rate ~ ., data = train.data, hidden = c(12,6), linear.output = T)




plot(nnet)


nnpred <- predict(nnet, test.data[,1:22])
nnpred

test.data <- cbind(test.data, nnpred)
test.data$Total.Smoking.Rate
nnpred <- nnpred * (max(data$Total.Smoking.Rate) - min(data$Total.Smoking.Rate)) + min(data$Total.Smoking.Rate)
test.r <- (test.data$Total.Smoking.Rate) * (max(data$Total.Smoking.Rate) - min(data$Total.Smoking.Rate)) + min(data$Total.Smoking.Rate)

head(nnpred)
head(test.r)


mse.nn <- sum((test.data$Total.Smoking.Rate - nnpred)^2)  / nrow(test.data)
mse.nn

mae.nn <- mae(test.data$Total.Smoking.Rate, nnpred)
mae.nn

mape.nn <- mape(test.data$Total.Smoking.Rate, nnpred)
mape.nn

main.comp <- prcomp(numeric.data)
summary(main.comp)

new.data <- main.comp$x[,1:11]
summary(new.data)

numeric.data[,c(24:42)] <- NULL



numeric.data <- cbind(numeric.data, new.data)

train.data2 <- numeric.data[train.indices,]
test.data2 <- numeric.data[-train.indices,]



nnet2 <- neuralnet(Total.Smoking.Rate ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 + PC11, data = train.data2, hidden = c(12,6), linear.output = T)



plot(nnet2)
nnpred2 <- predict(nnet2, test.data2)

head(test.data2$Total.Smoking.Rate)
head(nnpred2)

mse.nn2 <- sum((test.data2$Total.Smoking.Rate - nnpred2)^2)  / nrow(test.data2)
mse.nn2

mae.nn2 <- mae(test.data2$Total.Smoking.Rate, nnpred2)
mae.nn2

mape.nn2 <- mape(test.data2$Total.Smoking.Rate, nnpred2)
mape.nn2

summary(numeric.data)
str(numeric.data)



