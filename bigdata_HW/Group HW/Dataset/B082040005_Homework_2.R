# 請加載給定的數據集“redwinequality.csv”並回答以下數據管理問題。

library(data.table)
redwinequality <- fread("C:/Users/user/Desktop/bigdata_HW/Group HW/Dataset/redwinequality.csv",stringsAsFactors = T)

# 1.1 [10 pts] 請將數據分成訓練集（70%）和測試集（30%），設置了“set.seed(2022)”
#     擬合預測“質量”和計算訓練和測試數據的平均絕對誤差 (MAE)。

set.seed(2022)
train_idx <- sample(1:nrow(redwinequality), nrow(redwinequality) * 0.7)
train <- redwinequality[train_idx, ]
test <- redwinequality[setdiff(1:nrow(redwinequality), train_idx),] 

library(dplyr)
mae <- function(y, yhat){
  return(mean(abs(y - yhat)))
}

#train/test

train_mae = list()
test_mae = list()

model1 = glm(quality ~ . ,data = train)
summary(model1)
y_hat_training1 <- predict(model1)
train_mae[[1]] = mae(train$quality ,y_hat_training1) # 0.4914242
y_hat_test1 <- predict(model1, newdata = test)
test_mae[[1]] = mae(test$quality, y_hat_test1)       # 0.5215846

# 1.2 [10 pts] 請使用向前和向後選擇來找到你最好的子集變量（最多雙向交互）。
#     這些選擇是否選擇了相同的變量？此外，報告每個選擇的變量的訓練/測試 MAE方法

# backward
model2 = glm(quality ~ (.)^2 ,data = train)
backmodel = step(model2,direction = "backward" )
summary(backmodel)

y_hat_training2 <- predict(backmodel)
train_mae[[2]] = mae(train$quality ,y_hat_training2) # 0.475894
y_hat_test2 <- predict(backmodel, newdata = test)
test_mae[[2]] = mae(test$quality, y_hat_test2)       # 0.5100406


# forbward
model3 = glm(quality ~ alcohol ,data = train)
biggest <- formula(lm(quality ~ .^2, train))
formodel = step(model3,direction = "forward", scope=biggest )
summary(formodel)

y_hat_training3 <- predict(formodel)
train_mae[[3]] = mae(train$quality ,y_hat_training3) # 0.4825258
y_hat_test3 <- predict(formodel, newdata = test)
test_mae[[3]] = mae(test$quality, y_hat_test3)       # 0.5063145

# ANS : 不一樣，因為加了交互項變化太大，不然單純 11 個變數，高機率一樣



# 1.3 [10分] 參考第6章（線性模型選擇與正則化）教科書（ISLR）。
#     請用所有變量和 R 包“glmnet”安裝 LASSO，使用cv.glmnet() 找到最好的 lambda。
#     還報告培訓/測試 MAE。

# https://rpubs.com/skydome20/R-Note18-Subsets_Shrinkage_Methods

# install.packages("glmnet")
library(glmnet)

#調參 lamda
lasso_best <- cv.glmnet(x = as.matrix(train[,c(1:11)]),y = train$quality,alpha = 1)
# 交叉驗證 預設k=10，alpha = 0為ridge, =1為lasso
lasso_best$lambda.min  
# min 0.0006283377    70  0.4066 0.01989      11

lasso = glmnet(x = as.matrix(train[,c(1:11)]),y = train$quality,alpha = 1)
summary(lasso)

lasso_train = predict(lasso,
                      s = lasso_best$lambda.min,
                      newx = as.matrix(train[,c(1:11)]))
train_mae[[4]] = mae(train$quality, lasso_train)
# 0.4915512

lasso_test = predict(lasso,
                     s = lasso_best$lambda.min,
                     newx = as.matrix(test[,c(1:11)]))
test_mae[[4]] = mae(test$quality, lasso_test)
# 0.5215206


# 1.4 [10 分] 用所有變量擬合隨機森林並報告訓練/測試 MAE。打印得出隨機森林方差的
#     重要性並按重要性排序。是隨機森林排名與線性模型生成的排名不同？

# variable importance from randomForest 
library(randomForest)

colnames(redwinequality) = c("fixed_acidity","volatile_acidity","citric_acid",
                             "residual_sugar","chlorides","free_sulfur_dioxide",
                             "total_sulfur_dioxide","density","pH","sulphates","alcohol","quality")  

colnames(train) = colnames(redwinequality)
colnames(test) = colnames(redwinequality)

rf = randomForest(quality ~ .,
                  data = redwinequality[train_idx,],
                  na.action=na.roughfix)
summary(rf)

rf_train <- predict(rf)
train_mae[[5]] = mae(train$quality ,rf_train)  # 0.4339049
rf_test <- predict(rf, newdata = test)
test_mae[[5]] = mae(test$quality, rf_test)     # 0.4470132   

sort(abs(randomForest::importance(rf))[-1], decreasing = T)
vip::vip(rf)

# alcohol
# sulphates
# volatile acidity
# density
# total sulfur dioxide
# chlorides
# citric acid
# fixed acidity
# pH
# residual sugar

# Importance

vip::vip(model1)
sort(abs(caret::varImp(model1))[,'Overall'], decreasing = T)

# alcohol
# sulphates
# volatile acidity
# total sulfur dioxide
# chlorides
# fixed acidity
# density
# citric acid
# free sulfur dioxide
# residual sugar

# 不完全相同，因為各模型所注重的東西不同，線性重線性關係，樹重分類的純度

# 2. 請加載給定的數據集“diabetes.csv”。回答下列問題。

diabetes <- fread("C:/Users/user/Desktop/bigdata_HW/Group HW/Dataset/diabetes.csv",stringsAsFactors = T)

# 2.1 [10 分] 將數據分成訓練 (70%) 和測試 (30%) 數據集設置種子（2022）。
#     擬合邏輯回歸模型以預測“結果”並報告訓練和測試 AUC。

library("pROC")

set.seed(2022)
train_idx <- sample(1:nrow(diabetes), nrow(diabetes) * 0.7)
train <- diabetes[train_idx, ]
test <- diabetes[setdiff(1:nrow(diabetes), train_idx),] 

#train/test

train_mae = list()
test_mae = list()

model1 = glm(Outcome ~ . ,data = train, family = "binomial")
summary(model1)
y_hat_training1 <- predict(model1)
train_mae[[1]] = roc(train$Outcome ,y_hat_training1) # 0.8458
y_hat_test1 <- predict(model1, newdata = test)
test_mae[[1]] = roc(test$Outcome, y_hat_test1)       # 0.8127

# 2.2 [10 分] 基於樹的算法，例如 CART，能夠識別非線性來自數據的關係並表示
#     可解釋規則中的關係。請使用 R 包“rpart”和“rpart.plot”來擬合預測的分類樹
#     “結果”。選擇（基於交叉驗證錯誤）並繪製您最好的樹。然後報告訓練和測試 AUC。

library(rpart); library(rpart.plot)

# Regression tree with 10-fold CV
set.seed(2022)
RT = rpart(Outcome ~ .,
           data = train, 
           control = rpart.control(xval = 10))

vip::vip(RT)

# Plot tree
rpart.plot(RT, digits = 3)

y_hat_training2 <- predict(RT)
train_mae[[2]] = roc(train$Outcome ,y_hat_training2) # 0.8758
y_hat_test2 <- predict(RT, newdata = test)
test_mae[[2]] = roc(test$Outcome, y_hat_test2)       # 0.776

# Print normalized errors and cost-complexity parameters
printcp(RT)

# Plot cost-complexity parameters (CP) to help select the best tree 
plotcp(RT, upper = "splits")

# Seems that a tree with 3 or 6 splits perform
# relatively ok. Let's say we'd like the tree with 6 splits. We can 
# prune the tree as:
# Always prune tree with cp threshold higher then cp of the tree we need 
hitSalary_RT_6split = prune(RT,cp = 0.017)    
printcp(hitSalary_RT_6split)
# Plot the tree
rpart.plot(hitSalary_RT_6split)

# k-fold CV + 1 SE
realCPTable$xerr_1std = realCPTable$xerror + realCPTable$xstd

# Let rpart() select its best tree
hitSalary_RT = rpart(Outcome ~ Glucose + BMI + Age + DiabetesPedigreeFunction + Pregnancies + Insulin + BloodPressure,
                     data = train,
                     control = rpart.control(xval = 10))
rpart.plot(hitSalary_RT)

y_hat_training3 <- predict(hitSalary_RT)
train_mae[[3]] = roc(train$Outcome ,y_hat_training3) # 0.8758
y_hat_test3 <- predict(hitSalary_RT, newdata = test)
test_mae[[3]] = roc(test$Outcome, y_hat_test3)       # 0.776

# 最後還是最一開始的結果test會表現最好

# 2.3 [10 pts] 像隨機森林這樣的集成學習方法通常優於其他方法就預測的準確性而言，
#     簡單的模型單獨存在，因為它們減少了通過聚合多個模型預測的方差。
#     請擬合一個隨機森林模型和報告培訓/測試 AUC。

diabetes$Outcome = as.factor(diabetes$Outcome)
train$Outcome = as.factor(train$Outcome)
test$Outcome = as.factor(test$Outcome)

rf = randomForest(Outcome ~ .,
                  data = diabetes[train_idx,],
                  na.action=na.roughfix)
summary(rf)

rf_train <- predict(rf, type='prob')
train_mae[[4]] = roc(train$Outcome ,rf_train[,"1"])  # 0.8325
rf_test <- predict(rf, newdata = test, type='prob')
test_mae[[4]] = roc(test$Outcome, rf_test[,"1"])     # 0.8084

# 2.4 [10 分] 你認為哪個變量對預測“結果”最重要？用你的分析結果證明你的發現。

sort(abs(randomForest::importance(rf))[-1], decreasing = T)
randomForest::importance(rf)
vip::vip(rf)

# IncNodePurity
# Pregnancies                   9.746806
# Glucose                      28.838922
# BloodPressure                 8.697256
# SkinThickness                 7.708846
# Insulin                       7.994443
# BMI                          16.996199
# DiabetesPedigreeFunction     13.591504
# Age                          15.087635

# MeanDecreaseGini
# Pregnancies                      21.00626
# Glucose                          62.50071
# BloodPressure                    21.27464
# SkinThickness                    17.53524
# Insulin                          17.67951
# BMI                              36.45311
# DiabetesPedigreeFunction         30.43480
# Age                              33.27620

# Glucose 最重要，從隨機森林變數重要性(IncNodePurity)可以看出他佔了0.28838922，將近3成(沒把目標換成因子前)
# Glucose 還是最重要，從隨機森林變數重要性(MeanDecreaseGini)可以看出他能降62.50071的不純度(把目標換成因子後)

# 3. 請加載給定數據“吸煙的身體信號”數據集（smoking.csv）。參考在這裡了解更多信息

smoking <- fread("C:/Users/user/Desktop/bigdata_HW/Group HW/Dataset/smoking.csv", data.table = F,stringsAsFactors = T)

# 3.1 [10 pts] 使用 R 包 caret 將數據拆分為訓練 (70%) 和測試 (30%)
#     帶有 set.seed(2022) 的數據集。擬合預測“吸煙”的邏輯回歸模型。
#     使用或創建可以更好地預測目標的任何變量。
#     什麼是給定訓練和測試數據集的預測準確性默認截止值 0.5？

library(caret)

set.seed(2022)
train_idx <- createDataPartition(smoking$smoking,
                                 p = 0.7,
                                 list = FALSE)
train <- smoking[train_idx, ]
test <- smoking[-train_idx, ] 

train$gender = as.numeric(train$gender)
train$oral = as.numeric(train$oral)
train$tartar = as.numeric(train$tartar)

test$gender = as.numeric(test$gender)
test$oral = as.numeric(test$oral)
test$tartar = as.numeric(test$tartar)

#train/test

model1 = glm(smoking ~ . ,data = train, family = "binomial")   # 機率負的???
summary(model1)

model2 = step(model1)
summary(model2)

y_hat_training1 <- predict(model2)
pred_class = Map(function(x) factor(ifelse( y_hat_training1 > x, "Yes", "No")), c(0.5) )
table(train$smoking, unlist(pred_class))

# No   Yes
# 0 22721  1953
# 1  9043  5268

acc = (22721+5268)/length(train$smoking)  # 0.7179941(model1) --> 0.7179428

y_hat_test1 <- predict(model2, newdata = test)
pred_class = Map(function(x) factor(ifelse( y_hat_test1 > x, "Yes", "No")), c(0.5) )
table(test$smoking, unlist(pred_class))

# No  Yes
# 0 9730  833
# 1 3935 2209

acc = (9730+2209)/length(test$smoking)  # 0.7149698(model1) --> 0.7146106

# 3.2 [10 pts] 可以看出target存在類不平衡問題（抽煙）。
#     我們知道調整預測的類別概率截止值可能會有所幫助預測罕見的情況。
#     基於 Youden's J 指數的最佳截止值是多少？
#     還請報告您的模型真實陽性率（敏感性）與不同截止值（0.5 和“最佳”值）。

table(train$smoking)
table(test$smoking)

library("pROC")
default_glm_roc = roc(train$smoking, y_hat_training1)

# ROC for 3 cutoffs
plot.roc(default_glm_roc, print.thres = c(0.8, 0.5, 0.1, -0.464), print.auc = T, xlim = c(1, 0))

# Optimal threshold/cutoff based on Youden's Index 
# Notice that, again, in real-world applications, you should use an independent # sample (instead the testing data in our sample code) to identify this cutoff.
a = plot.roc(default_glm_roc, print.thres = "best", print.thres.best.method = "youden", print.auc = T, xlim = c(1, 0)) # -0.464

pred_class = Map(function(x) factor(ifelse( y_hat_training1 > x, 1, 0)), c(-0.464) )
con = table( unlist(pred_class),train$smoking )

# 0     1
# No  15336  1334
# Yes  9338 12977

sen = 12977/(12977+1334)  # 0.907 新

pred_class = Map(function(x) factor(ifelse( y_hat_test1 > x, 1, 0)), c(-0.464) )
con = table(unlist(pred_class),test$smoking)

# 0    1
# No  6530  589
# Yes 4033 5555

sen = 5555/(5555+589)  # 0.904 新

######################

pred_class = Map(function(x) factor(ifelse( y_hat_training1 > x, "yes", "no")), c(0.5) )
table( unlist(pred_class),train$smoking )

sen = 5268/(5268+9043)  # 0.368 舊

pred_class = Map(function(x) factor(ifelse( y_hat_test1 > x, "yes", "no")), c(0.5) )
table(unlist(pred_class),test$smoking)

sen = 2209/(2209+3935)  # 0.360 舊

# ROC for 3 cutoffs
plot.roc(default_glm_roc, print.thres = c(0.8, 0.5, 0.1, -0.464), print.auc = T, xlim = c(1, 0))

# 3.3 [10 分] 繪製模型的 ROC 曲線用於訓練和測試數據集。根據 AUC 比較和報告您的模型性能

# train

default_glm_roc = roc(train$smoking, y_hat_training1) # 0.8319
plot.roc(default_glm_roc, print.thres = c(0.8, 0.5, 0.1, -0.464), print.auc = T, xlim = c(1, 0))


# test

default_glm_roc = roc(test$smoking, y_hat_test1)  # 0.828
plot.roc(default_glm_roc, print.thres = "best", print.thres.best.method = "youden", print.auc = T, xlim = c(1, 0)) # -0.372
plot.roc(default_glm_roc, print.thres = c(0.8, 0.5, 0.1, -0.372), print.auc = T, xlim = c(1, 0))

# 訓練集AUC大於測試集AUC，差距不大，推測沒有過擬合
# 整體表現0.8以上，對邏輯斯回歸來說，其實已經是個不錯的模型了
# 如果樹模型沒有顯著AUC上升，那可以考慮使用邏輯斯就好，因為數據集其實頗大，應該會train很久

# 最神奇的地方就是為啥邏輯斯會train出負的???

