###############################################################################################################################
# social_netwok for churn analysis
# zhifeng_guo
# Version: 1.1
###############################################################################################################################

#设置工作路径
setwd("D:/data/")
#清除工作空间
rm(list = ls())
#导入社交网络的包
library(igraph)

#导入数据
load('Edgelist.RData')
load('Customers.RData')

#查看数据
head(edgeList)
head(customers)

#生成网络
network <- graph_from_data_frame(edgeList, directed = FALSE)



#查看流失客户与非流失客户的数量
table(customers$churn)

# 给网络节点增加是否流失的属性
V(network)$churn <- customers$churn

# 画图
plot(network, vertex.label = NA, edge.label = NA,  edge.color = 'black', vertex.size = 2)
#################################################################################################################################
# 给节点增加个颜色属性，颜色属性与是否流失一一对应
V(network)$color <- V(network)$churn

# 设置流失与非流失客户的节点颜色
V(network)$color <- gsub("1","red",V(network)$color)
V(network)$color <- gsub("0","white",V(network)$color)

# 画图
plot(network, vertex.label = NA, edge.label = NA,edge.color = "black", vertex.size = 2)
#################################################################################################################################

# 只画流失客户的图像
churnerNetwork <- induced_subgraph(network, v = V(network)[which(V(network)$churn == 1)])

# 画图                 
plot(churnerNetwork, vertex.label = NA, vertex.size = 2)

# 非流失客户
churnerNetwork0 <- induced_subgraph(network, v = V(network)[which(V(network)$churn == 0)])

# 画图                     
plot(churnerNetwork0, vertex.label = NA, vertex.size = 2,col = 'red')



#########################################################################################################################
#一些基本分析
#分析节点

# Add the column edgeList$FromLabel
edgeList$FromLabel <- customers[match(edgeList$from, customers$id), 2]
head(edgeList)
# Add the column edgeList$ToLabel
edgeList$ToLabel <- customers[match(edgeList$to, customers$id), 2]
head(edgeList)
# Add the column edgeList$edgeType
edgeList$edgeType <- edgeList$FromLabel + edgeList$ToLabel
head(edgeList)
# Count the number of each type of edge
table(edgeList$edgeType)

#########################################################################################################################

#分析边
# Count churn edges
ChurnEdges <- sum(edgeList$edgeType == 2)
ChurnEdges
# Count non-churn edges
NonChurnEdges <- sum(edgeList$edgeType == 0)
NonChurnEdges

# Count mixed edges
MixedEdges <- sum(edgeList$edgeType == 1)
MixedEdges
# Count all edges
edges <- ChurnEdges + NonChurnEdges + MixedEdges

#Print the number of edges
edges

##########################################################################################################################
# 生成网络节点的度数特征
V(network)$degree <- degree(network, normalized=TRUE)
# 生成 betweenness网络特征
V(network)$betweenness <- betweenness(network, normalized=TRUE)


# 生成 transitivity特征
V(network)$transitivity <- transitivity(network, type='local', isolates='zero')

# 计算网络的 transitivity
transitivity(network)

#########################################################################################################################
# 计算零阶矩阵
AdjacencyMatrix <- as_adjacency_matrix(network)

# 计算二阶邻接矩阵
SecondOrderMatrix_adj <- AdjacencyMatrix %*% AdjacencyMatrix

# 调整二阶邻接矩阵
#先转换成true，false，再通过加0转换成0，1的值
SecondOrderMatrix <- ((SecondOrderMatrix_adj) > 0) + 0
diag(SecondOrderMatrix) <- 0



# 计算邻居节点中流失客户的个数
V(network)$ChurnNeighbors <- as.vector(AdjacencyMatrix %*% V(network)$churn)

# 计算邻居节点中非流失客户的个数
V(network)$NonChurnNeighbors <- as.vector(AdjacencyMatrix %*% (1 - V(network)$churn))

# 计算相对概率
V(network)$RelationalNeighbor <- as.vector(V(network)$ChurnNeighbors / (V(network)$ChurnNeighbors + V(network)$NonChurnNeighbors))



##############################################################################################################################

# 计算二度关系中，流失客户的个数
V(network)$ChurnNeighbors2 <- as.vector(SecondOrderMatrix %*% V(network)$churn)

# 计算二度关系中，非流失客户的个数
V(network)$NonChurnNeighbors2 <- as.vector(SecondOrderMatrix %*% (1 - V(network)$churn))

# 计算相对比例
V(network)$RelationalNeighbor2 <- as.vector(V(network)$ChurnNeighbors2 /  (V(network)$ChurnNeighbors2 + V(network)$NonChurnNeighbors2))


################################################################################################################################
# 计算每个节点的pagerank值
V(network)$pr_0.85 <- page.rank(network)$vector


################################################################################################################################
# 将网络数据变成数据框
dataset_full <- as_data_frame(network, what = "vertices")

# 查看数据
head(dataset_full)


# 移除没有用的列

dataset <- dataset_full[, -1]
head(dataset)

# 查看特征
summary(dataset$RelationalNeighbor2)

# 发现缺失值
toReplace <- which(is.na(dataset$RelationalNeighbor2))

# 用0替换缺失值
dataset$RelationalNeighbor2[toReplace] <- 0

# 再次检查特征
summary(dataset$RelationalNeighbor2)
################################################################################################################################
#特征相关性分析
# 导入相关性包
library(corrplot)

# 生成相关系数矩阵
M <- cor(dataset[, -1])

# 画图
corrplot(M, method = "circle")


################################################################################################################################
#生成训练样本与验证样本
# 设置随机数种子
set.seed(12345)

# 生成指标向量
index_train <- sample(1:nrow(dataset), 2 / 3 * nrow(dataset))

# 训练集
training_set <- dataset[index_train, ]

# 测试集
test_set <- dataset[-index_train, ]



# 导入包
library(randomForest)

# 构造模型
rfModel <- randomForest(as.factor(churn)~. ,data=training_set)

# 特征重要性
varImpPlot(rfModel)

# 导入包
library(pROC)

# 进行验证
Predictions <- predict(rfModel, newdata = test_set, type = "response")

table(firstPredictions,test_set$churn)
