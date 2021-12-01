library(caret)
library(e1071)
library(tree)
library(ROCR)

df <- read.csv("~/Downloads/Titanic_updated.csv")
df <- data.frame(lapply(df[,c(1, 2, 7)], as.factor), df[,c(3, 4, 5, 6, 8)])
str(df)

sapply(df, function(x) sum(length(which(is.na(x))))) 
df$Age[is.na(df$Age)] <- median(df$Age, na.rm=TRUE)

train_index <- createDataPartition(df$Pclass, p = 0.75, list = FALSE)
df_train <- df[ train_index,]
df_test  <- df[-train_index,]

survival_nb=naiveBayes(Survived~., data=df_train)
print(survival_nb)

survival_tree <- tree(Survived~., df_train)
print(survival_tree)

survived_act <- as.factor(df_test$Survived)
df_test$Survived <- NULL
ptree <- predict(survival_tree, df_test)
ptree <- as.factor(round(ptree,0))
pnb <- predict(survival_nb, df_test)
confusionMatrix(survived_act, ptree)
confusionMatrix(survived_act, pnb)

predtree <- prediction(as.numeric(ptree), as.numeric(survived_act))
perftree <- performance(predtree, "tpr", "fpr")
prednb <- prediction(as.numeric(pnb), as.numeric(survived_act))
perfnb <- performance(prednb, "tpr", "fpr")
plot(perftree, avg="threshold", col = 'red', spread.estimate="boxplot", main = 'ROC Curve for Naive Bayes and Tree Models')
par(new = TRUE)
plot(perfnb,avg="threshold", col = 'blue', spread.estimate="boxplot")
legend(0.65, 0.1, legend=c("Tree", "Naive-Bayes"),col=c("red", "blue"), lty=1, cex=0.8)

train_index <- createDataPartition(df$Pclass, p = 0.25, list = FALSE)
df_train25 <- df[ train_index,]
df_test25  <- df[-train_index,]
train_index <- createDataPartition(df$Pclass, p = 0.50, list = FALSE)
df_train50 <- df[ train_index,]
df_test50  <- df[-train_index,]

survival_tree25 <- tree(Survived~., df_train25)
survival_tree50 <- tree(Survived~., df_train50)

survived_act25 <- as.factor(df_test25$Survived)
df_test25$Survived <- NULL
survived_act50 <- as.factor(df_test50$Survived)
df_test50$Survived <- NULL
ptree25 <- predict(survival_tree25, df_test25)
ptree25 <- as.factor(round(ptree25,0))
ptree50 <- predict(survival_tree50, df_test50)
ptree50 <- as.factor(round(ptree50,0))
confusionMatrix(survived_act25, ptree25)
confusionMatrix(survived_act50, ptree50)
