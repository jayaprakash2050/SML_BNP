library(party);
library(sandwich);
library(sgd);
library(e1071);


Train <- data.frame(read.csv("../../Data/train.csv"));
Test <- data.frame(read.csv("../../Data/test.csv"));

# variables with a high number of categorical variables, which we can incoporate later if needed
# v112,v113,v125

trainset <- data.frame(Train[,-c(1)]);
testset <- data.frame(Test[,-c(1)]);

svm.model <- svm(target~., data = trainset, cost=100, gamma=1);

naive_bayes.model <- naiveBayes(target~.,data=trainset);

naive_bayes.pred <- predict(naive_bayes.model,testset);


NbInstances <- nrow(Data);
NbVariables <- ncol(Data)-2;



Data_summary <- summary(Data);


Decision_Tree <- ctree(target ~. , data=Data);
plot (Decision_Tree, type="simple");

Logit <- glm(formula= target~., family=binomial(link="logit"),data=Data);

K_Means <- kmeans(Data,5);



