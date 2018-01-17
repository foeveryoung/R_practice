#http://mp.weixin.qq.com/s/zCUlQDMYMVkysKAI9Dr-vQ
library(readxl)
library(GGally)
# 读取数据
ccpp <- read_excel(path = file.choose())
summary(ccpp)
# 绘制各变量之间的散点图与相关系数
ggpairs(ccpp)
# 建模
fit <- lm(PE ~ AT + V + AP, data = ccpp)
summary(fit)
# 计算模型的RMSE值
RMSE = sqrt(mean(fit$residuals**2))
RMSE
# 多重共线性检验
vif(fit)
# 异常点检验# 高杠杆值点（帽子矩阵）
leverage <- hatvalues(fit)
head(leverage)
# dffits值
Dffits <- dffits(fit)
head(Dffits)
# 学生化残差
resid_stu <- Dffits/sqrt(leverage/(1-leverage))
head(resid_stu)
# cook距离
cook <- cooks.distance(fit)
# 计算异常值数量的比例
outliers_ratio = sum(abs(ccpp_outliers$resid_stu)>2)/nrow(ccpp_outliers)
outliers_ratio

# 删除异常值
ccpp_outliers = ccpp_outliers[abs(ccpp_outliers$resid_stu)<=2,]
head(cook)
# covratio值
Covratio <- covratio(fit)
head(Covratio)
# 将上面的几种异常值检验统计量与原始数据集合并
ccpp_outliers <- cbind(ccpp, data.frame(leverage, Dffits, resid_stu, cook, Covratio))
head(ccpp_outliers)

# 重新建模
fit2 = lm(PE~AT+V+AP,data = ccpp_outliers)
summary(fit2)

# 计算模型的RMSE值
RMSE2 = sqrt(mean(fit2$residuals**2))
RMSE2

# 正态性检验#绘制直方图
hist(x = fit2$residuals, freq = FALSE,
     breaks = 100, main = 'x的直方图',
     ylab = '核密度值',xlab = NULL, col = 'steelblue')

#添加核密度图
lines(density(fit2$residuals), col = 'red', lty = 1, lwd = 2)

#添加正态分布图
x <- fit2$residuals[order(fit2$residuals)]
lines(x, dnorm(x, mean(x), sd(x)),
      col = 'blue', lty = 2, lwd = 2.5)

#添加图例
legend('topright',legend = c('核密度曲线','正态分布曲线'),
       col = c('red','blue'), lty = c(1,2),
       lwd = c(2,2.5), bty = 'n')

# PP图
real_dist <- ppoints(fit2$residuals)
theory_dist <- pnorm(fit2$residuals, mean = mean(fit2$residuals), 
                     sd = sd(fit2$residuals))

# 绘图
plot(sort(theory_dist), real_dist, col = 'steelblue', 
     pch = 20, main = 'PP图', xlab = '理论正态分布累计概率', 
     ylab = '实际累计概率')

# 添加对角线作为参考线
abline(a = 0,b = 1, col = 'red', lwd = 2)

# QQ图
qqnorm(fit2$residuals, col = 'steelblue', pch = 20,
       main = 'QQ图', xlab = '理论分位数', 
       ylab = '实际分位数')
# 绘制参考线
qqline(fit2$residuals, col = 'red', lwd = 2)
# shapiro正态性检验

# shapiro <- shapiro.test(fit2$residuals)
# shapiro

# K-S正态性检验
ks <- ks.test(fit2$residuals, 'pnorm', 
              mean = mean(fit2$residuals), 
              sd = sd(fit2$residuals))
ks