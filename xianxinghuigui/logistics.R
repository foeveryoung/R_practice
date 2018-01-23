# ���ص�������
library(pROC)
library(ggplot2)

# ��ȡ����
purchase <- read.csv(file = file.choose())
purchase$Purchased = factor(purchase$Purchased)

# ��������
str(purchase)
# ������ȱʧ���
sapply(purchase, function(x) sum(is.na(x)))

# ���ݼ����Ϊѵ�����Ͳ��Լ�
set.seed(0)
idx = sample(1:nrow(purchase), size = 0.75*nrow(purchase))
train = purchase[idx,]
test = purchase[-idx,]

# ����Logisticģ��
logit = glm(Purchased ~ ., data = train[,-1], family = binomial(link = "logit"))
summary(logit)

# ɾ���Ա��������
train2 = purchase[idx,-c(1,2)]
test2 = purchase[-idx,-c(1,2)]

# ���¹���Logisticģ��
logit2 = glm(Purchased ~ ., data = train2, family = binomial(link = "logit"))
summary(logit2)

# Ԥ��
prob = predict(logit2, newdata = test2)
pred = factor(ifelse(prob >= 0.5, 1, 0), levels = c(0, 1), ordered = TRUE)

# ��������
Freq = table(test$Purchased, pred)
# ׼ȷ��
sum(diag(Freq))/sum(Freq)

# ����ROC����
ROC = roc(factor(test2$Purchased, levels=c(0,1), ordered = TRUE), pred)
df = data.frame(x = 1-ROC$specificities, y = ROC$sensitivities)

ggplot(df, aes(x = x, y = y)) +
  geom_area(alpha=0.5, fill = 'steelblue') +
  geom_line() +
  geom_abline(linetype='dashed',color = 'red') +
  labs(x = '1-specificity', y = 'Sensitivity',
       title = paste0('ROC Curve AUC=', round(ROC$auc,3))) + 
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'))