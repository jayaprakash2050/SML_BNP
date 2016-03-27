library(sandwich);
library(party);
library(sgd);
library(e1071);
library(ggplot2);
#options(java.home="C:\\Program Files\\R\\R-2.12.1\\bin\\x64\");
library(cluster);

library(devtools);
library(scales);
#install_git("https://github.com/hadley/devtools.git");
#install_github("Rtools");
#install_github("ggbiplot", "vqv");
library(ggbiplot);

source("SML_BNP\\Example.R");



#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7') # for 64-bit version
#Sys.setenv(JAVA_HOME="C:\\java\\jdk1.7.0_55\Java\\jre") # for 32-bit version
library(rJava);
library(FSelector);

#install.packages('rJava', .libPaths()[1], repos='http://www.rforge.net/')

Train <- read.csv("Data/train.csv",head=TRUE);
#Test <- data.frame(read.csv("../Data/test.csv"));

####INFORMATION GAIN by Variable
weights <- data.frame(information.gain(target~.,Train[,-1]));
weights.unordered <- as.data.frame(cbind(Variable=rownames(weights),weights),row.names=NULL);
colnames(weights.unordered) <- c("Variable","Info_Gain");
weights.ordered <- weights.unordered[order(-weights.unordered$Info_Gain),];

x <- subset(Train,select=-c(ID,target,v3,v22,v24,v30,v31,v47,v52,v56,v66,v71,v74,v75,v79,v91,v107,v110,v112,v113,v125));
y <- subset(Train,select=c(target));

x_woutNA <- x;
x_woutNA[is.na(x_woutNA)] <- 1000;

#x_woutNA.pca <- prcomp(x_woutNA[sample(1:nrow(x_woutNA),10000,replace=FALSE),]);#,center=TRUE,scale. = TRUE);
#biplot(x_woutNA.pca);
	

	
	
data <- cbind(y,x_woutNA);
#x <- as.numeric(x);

sample.x <- x_woutNA[sample(1:nrow(x_woutNA),100,replace=FALSE),];

unscaled.theta <- gradient.descent(x=x_woutNA, y=y, num.iterations=10000, output.path=TRUE);
write.csv(unscaled.theta,file="output.csv");


sample  <- data[sample(1:nrow(data), 1000, replace = TRUE),];

hc <- hclust(dist(sample[,-1]));
plot(hc);

unscaled.theta <- gradient.descent(x=x, y=y, num.iterations=num.iterations, output.path=TRUE)
scaled.theta <- gradient.descent(x=x.scaled, y=y, num.iterations=num.iterations, output.path=TRUE)

qplot(1:(nrow(scaled.theta)), scaled.theta[,1], geom=c("line"), xlab="iteration", ylab="theta_1")
qplot(1:(nrow(scaled.theta)), scaled.theta[,2], geom=c("line"), xlab="iteration", ylab="theta_2")


NData <- nrow(Data);

PER_Train <- 0.66;
PER_Test <- 1-PER_Train;

trainset <- data.frame(Train[1:(NData*PER_Train),-c(1)]);
testset <- data.frame(Train[1:(NData*PER_Test),-c(1)]);

red_trainset <- subset(trainset, select= -c(v22,v56,v112,v113,v125));
red_testset <-  subset(testset, select= -c(v22,v56,v112,v113,v125));

#sample_red_trainset <- sample(red_trainset, 10000, replace = TRUE, prob=NULL);

Data <- sample_red_trainset;


svm.model <- svm(target~., data = Data);

Logit <- glm(formula= target~., family=binomial(link="logit"),data=Data);







naive_bayes.model <- naiveBayes(target~.,data=trainset);
naive_bayes.pred <- predict(naive_bayes.model,testset);

NbInstances <- nrow(Data);
NbVariables <- ncol(Data)-2;

Data_summary <- summary(Data);

Decision_Tree <- ctree(target ~. , data=Data);
plot (Decision_Tree, type="simple");

Logit <- glm(formula= target~., family=binomial(link="logit"),data=Data);

K_Means <- kmeans(Data,5);





#testset <- data.frame(Test[,-c(1)]);
#
## variables with a high number of categorical variables, which we can incoporate later if needed
## str(trainset)
## str(testset)
## str(trainset[,1:98])
## str(trainset[,99:132])
## v22, v112,v113,v125
#
## Try to obtain the information gain from variables in order to understand how theyr are interacting
#red_trainset <- subset(trainset, select= -c(v22,v56,v112,v113,v125));
#red_testset <- subset(testset, select= -c(v22,v56,v112,v113,v125));
#
#logReg.model <- glm(formula= target~., family=binomial(link="logit"),data=red_trainset);
#
#info_gain <- information.gain(target~.,data=red_trainset);
#
#sample_red_trainset <- sample(red_trainset, 10000, replace = TRUE, prob=NULL);
#
#
#svm.model <- svm(target~., data = sample_red_trainset);
#
#
#logReg.pred <- predict(logReg.model,red_testset);
#
#svm.pred <- predict(svm.model,testset);
