# 加载第三方包
library(pROC)
library(ggplot2)

# 读取数据
purchase <- read.csv(file = file.choose())
purchase$Purchased = factor(purchase$Purchased)

# 数据类型
str(purchase)
# 各变量缺失情况
sapply(purchase, function(x) sum(is.na(x)))

# 数据集拆分为训练集和测试集
set.seed(0)
idx = sample(1:nrow(purchase), size = 0.75*nrow(purchase))
train = purchase[idx,]
test = purchase[-idx,]

# 构建Logistic模型
logit = glm(Purchased ~ ., data = train[,-1], family = binomial(link = "logit"))
summary(logit)

# 删除性别这个变量
train2 = purchase[idx,-c(1,2)]
test2 = purchase[-idx,-c(1,2)]

# 重新构建Logistic模型
logit2 = glm(Purchased ~ ., data = train2, family = binomial(link = "logit"))
summary(logit2)

# 预测
prob = predict(logit2, newdata = test2)
pred = factor(ifelse(prob >= 0.5, 1, 0), levels = c(0, 1), ordered = TRUE)

# 混淆矩阵
Freq = table(test$Purchased, pred)
# 准确率
sum(diag(Freq))/sum(Freq)

# 绘制ROC曲线
ROC = roc(factor(test2$Purchased, levels=c(0,1), ordered = TRUE), pred)
df = data.frame(x = 1-ROC$specificities, y = ROC$sensitivities)

ggplot(df, aes(x = x, y = y)) +
  geom_area(alpha=0.5, fill = 'steelblue') +
  geom_line() +
  geom_abline(linetype='dashed',color = 'red') +
  labs(x = '1-specificity', y = 'Sensitivity',
       title = paste0('ROC Curve AUC=', round(ROC$auc,3))) + 
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))