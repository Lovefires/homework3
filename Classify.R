setwd('C:\\Users\\Administrator\\Desktop\\homework3')

# 导入训练数据 和 测试数据
train_data <- read.table("train.csv", header = TRUE, sep = ",")
test_data <- read.table("test.csv", header = TRUE, sep = ",")

library("dplyr")
train_data <- select(train_data,Survived, Pclass, Sex, Age)
test_data <- select(test_data,Pclass, Sex, Age)
train_data <- na.omit(train_data)
test_data <- na.omit(test_data)

# 将数据转换成因子类型
train_data$Pclass <- as.factor(train_data$Pclass)
train_data$Sex <- as.factor(train_data$Sex)
train_data$Age <- as.factor(train_data$Age)
train_data$Survived <- as.factor(train_data$Survived)
# 将年龄这一属性划分成四个阶段（儿童，青年，成年，老年）
num = as.numeric(as.character(train_data$Age))
train_data[num <10, 'Age'] = 1
train_data[num >= 10 & num <20, 'Age'] = 2
train_data[num >= 20 & num <50, 'Age'] = 3
train_data[num >= 50, 'Age'] = 4

test_data$Pclass <- as.factor(test_data$Pclass)
test_data$Sex <- as.factor(test_data$Sex)
test_data$Age <- as.factor(test_data$Age)
# 将年龄这一属性划分成四个阶段（儿童，青年，成年，老年）
num = as.numeric(as.character(test_data$Age))
test_data[num <10, 'Age'] = 1
test_data[num >= 10 & num <20, 'Age'] = 2
test_data[num >= 20 & num <50, 'Age'] = 3
test_data[num >= 50, 'Age'] = 4

#开始构建分类模型
#install.packages("e1071")
library("e1071")
mod.formula <- as.formula("Survived~Pclass+Sex+Age") 

#朴素贝叶斯分类器
nb.sol <- naiveBayes(mod.formula, train_data)
nb.predict <- predict(nb.sol, newdata = test_data)
Survived = as.factor(nb.predict)
Bayes_Predict <- test_data;
Bayes_Predict <- data.frame(Survived, Bayes_Predict);
#View(Bayes_Predict)
write.csv(Bayes_Predict, file = "Bayes_Predict.csv", row.names = F)

# SVM分类器 (测试数据与模型不匹配)
svm.sol <- svm(mod.formula, train_data)
svm.predict <- predict(svm.sol, train_data)
Survived = as.factor(svm.predict)
Svm_Predict <- select(train_data,Pclass, Sex, Age);
Svm_Predict <- data.frame(Survived, Svm_Predict);
write.csv(Svm_Predict, file = "Svm_Predict.csv", row.names = F)

# 可视化朴素贝叶斯分类器的效果
plot(Bayes_Predict$Pclass, Bayes_Predict$Survived)
plot(Bayes_Predict$Sex, Bayes_Predict$Survived)
plot(Bayes_Predict$Age, Bayes_Predict$Survived)

#可视化SVM分类器的效果
plot(Svm_Predict$Pclass,Svm_Predict$Survived)
plot(Svm_Predict$Sex,Svm_Predict$Survived)
plot(Svm_Predict$Age,Svm_Predict$Survived)
