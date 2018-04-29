setwd('C:\\Users\\Administrator\\Desktop\\homework3')

# 导入训练数据 和 测试数据
train_data <- read.table("train.csv", header = TRUE, sep = ",")
test_data <- read.table("test.csv", header = TRUE, sep = ",")

library("dplyr")
train_data <- select(train_data,Survived, Pclass, Sex, Age)
test_data <- select(test_data,Pclass, Sex, Age)
train_data <- na.omit(train_data)
#View(train_data)
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
#View(train_data)

#kmeans的方法
library("kernlab")
library("magrittr")
# 生存与年龄
cluster_data <- select(train_data, Survived, Age)
res <- kmeans(cluster_data,4)
plot(cluster_data, col = res$cluster)
#jpeg(file = paste('survived_age','.jpg'))
#生存与仓次
cluster_data <- select(train_data, Survived, Pclass)
res <- kmeans(cluster_data,3)
plot(cluster_data, col = res$cluster)
#jpeg(file = paste('survived_pclass','.jpg'))


# 层次聚类
cluster_data <- select(train_data, Survived, Age, Pclass)
distance <- dist(cluster_data)
cluster_data.hc <- hclust(distance)
plot(cluster_data.hc, hang = -1)
re <- rect.hclust(cluster_data.hc, k = 2)
