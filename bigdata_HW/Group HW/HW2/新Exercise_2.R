#load the built-in Credit data in the ISLR package and remove the variable "ID". Enter “?Credit” to check out the data description
library(ISLR)
?Credit
credit <- as.data.frame(Credit)
credit$ID <- NULL

#1.1  Split the dataset into training (70%) and testing (30%) sets with random seed set.seed(1). After that, rescale the training and testing sets if necessary
set.seed(1)
train_idx <- sample(1:nrow(credit), nrow(credit) * 0.7)
train <- credit[train_idx, ]
test <- credit[setdiff(1:nrow(credit), train_idx),] 

# rescale
library("caret")
rescale <- preProcess(train, method = c("center", "scale"))
#double check with method
rescale$method
scaled_train <- predict(rescale, train)
scaled_test <- predict(rescale, test)

#1.2 Please rank the importance of variables by absolute standardized regression coefficients of general linear models that predict “Balance”.
#model
mod = lm(Balance ~ .,scaled_train)
#rank
mod_abs_rank <- data.frame(sort(abs(mod$coefficients[-1]), decreasing = T))
mod_abs_rank
#Limit                                                 1.1827979926
#StudentYes                                            0.9130658793
#Income                                                0.5967432498
#Rating                                                0.1832191335
#Cards                                                 0.0650093365
#EthnicityAsian                                        0.0298429112
#GenderFemale                                          0.0292098017
#Age                                                   0.0244660454
#EthnicityCaucasian                                    0.0010226793
#MarriedYes                                            0.0007940206
#Education                                             0.0004343542

#1.3 Please create a leave-one-out cross-validation (LOOCV) MAE to evaluate general linear models.
LOOCV_lm_mae = function(f, d){
  errs = sapply(1:nrow(d), FUN = function(k){
    reponse_var = all.vars(f)[1]; # Name of the response variable
    m = lm(f, d[- k,], na.action = na.omit)
    return(abs((d[[reponse_var]][k]) - predict(m, newdata = d[k,])))
  })
  return(round(mean(errs),4))
}

#1.4 Please build general linear models and perform the forward selection discussed in the class.
#We here consider the most important variable identified by the ranking we created in 1.3 as the “force-in” variable
#Then, add the remaining variables to the baseline model with the force-in variable, respectively
Map(function(f) LOOCV_lm_mae(f,scaled_train),
    #forward
    list(Balance ~ Limit,
      Balance ~ Limit + Student,
      Balance ~ Limit + Income,
      Balance ~ Limit + Rating,
      Balance ~ Limit + Cards,
      Balance ~ Limit + Ethnicity,
      Balance ~ Limit + Gender,
      Balance ~ Limit + Age,
      Balance ~ Limit + Married,
      Balance ~ Limit + Education
      ))
#After the first round of the forward selection, what is the best linear model in terms of the lowest validation error?
#Balance ~ Limit + Income,0.2726

#1.5 What is your best linear model in terms of the testing MAE? Please report and plot training, validation, and testing MAEs of your model selection process(at least 5 model errors)
library(dplyr)
mae <- function(y, yhat){
  return(mean(abs(y - yhat)))
}
#train/test

train_mae = list()
test_mae = list()

model1 = glm(Balance ~ Limit + Income ,data = scaled_train)
y_hat_training1 <- predict(model1)
train_mae[[1]] = mae(scaled_train$Balance,y_hat_training1) # 0.2698192
y_hat_test1 <- predict(model1, newdata = scaled_test)
test_mae[[1]] = mae(scaled_test$Balance, y_hat_test1) # 0.2654454

model2 = glm(Balance ~ Limit + Income + Student ,data = scaled_train)
y_hat_training2 <- predict(model2)
train_mae[[2]] = mae(scaled_train$Balance,y_hat_training2) # 0.1722851
y_hat_test2 <- predict(model2, newdata = scaled_test)
test_mae[[2]] = mae(scaled_test$Balance, y_hat_test2) # 0.1882051


model3 = glm(Balance ~ . ^ 2 ,data = scaled_train)
y_hat_training3 <- predict(model3)
train_mae[[3]] = mae(scaled_train$Balance,y_hat_training3) # 0.09308703
y_hat_test3 <- predict(model3, newdata = scaled_test)
test_mae[[3]] = mae(scaled_test$Balance, y_hat_test3) # 0.124302

model4 = glm(Balance ~ (Limit + Income) ^ 2 ,data = scaled_train)
y_hat_training4 <- predict(model4)
train_mae[[4]] = mae(scaled_train$Balance,y_hat_training4) # 0.2661482
y_hat_test4 <- predict(model4, newdata = scaled_test)
test_mae[[4]] = mae(scaled_test$Balance, y_hat_test4) # 0.2546808

model5 = glm(Balance ~ (Limit + Income + Student) ^ 2 ,data = scaled_train)
y_hat_training5 <- predict(model5)
train_mae[[5]] = mae(scaled_train$Balance,y_hat_training5) # 0.1629608
y_hat_test5 <- predict(model5, newdata = scaled_test)
test_mae[[5]] = mae(scaled_test$Balance, y_hat_test5) # 0.1665697

#vaLlidation
validation <- Map(function(f) LOOCV_lm_mae(f, scaled_train),
                  c(Balance ~ Limit + Income,
                    Balance ~ Limit + Income + Student,
                    Balance ~ . ^ 2,
                    Balance ~ (Limit + Income) ^ 2,
                    Balance ~ (Limit + Income + Student) ^ 2))

train_mae
# 0.2698192, 0.1722851, 0.09308703, 0.2661482, 0.1629608
validation
# 0.2726,    0.1749,    0.1286,     0.2696,    0.1667
test_mae
# 0.2654454, 0.1882051, 0.124302,   0.2546808, 0.1665697

#化簡
funmae = function(f){
  tt_errs = lapply(list("train_mae" = scaled_train, "test_mae" = scaled_test), FUN = function(k){
    tt_reponse_var = all.vars(f)[1];
    tt_mae <- mae(k[, tt_reponse_var], predict(lm(f, data = scaled_train), k))
  })
}
traintest <- Map(function(f) funmae(f),
                 c(Balance ~ Limit + Income,
                   Balance ~ Limit + Income + Student,
                   Balance ~ . ^ 2,
                   Balance ~ (Limit + Income) ^ 2,
                   Balance ~ (Limit + Income + Student) ^ 2))
library(data.table)
traintest <- rbindlist(traintest, fill=TRUE)
#train_mae  test_mae
#1: 0.26981923 0.2654454
#2: 0.17228509 0.1882051
#3: 0.09308703 0.1243020
#4: 0.26614816 0.2546808
#5: 0.16296077 0.1665697


#plot

tibble(
  models = rep(1:5, times = 3),
  MAE = c(unlist(train_mae),unlist(validation), unlist(test_mae)),
  Type = rep(c("Training", "validation", "Test"), each = 5)
) %>%
  ggplot(aes(x = models, y = MAE, colour = Type)) +
  geom_line(aes(group = Type)) +
  scale_x_continuous(breaks = 1:10) +
  theme_bw()

# https://heds.nz/posts/training-test-mse-r/

# Does your model overfit or underfit the training data? And why? 

# 從測試集的角度，第3個模型 testing MAE 最小，以 MAE 的角度，可說是這些模型中最好的模型
# 第3個模型，測試集MAE明顯大於訓練集(0.09308703 < 0.124302)，推測有過擬和的現象，
# 仔細觀察會發現 model3 用了所有變數加交互向，變數太多可能會訓練到 noise，
# 但從驗證集的 MAE 看，可以發現其結果跟測試集差不多，就看不出有 overfitting 了，
# 這裡正好驗證了 LOOCV 減少 Variance (穩定)的性質
# model、4 MAE 相對大，可能還有些重要的變數需要再考慮，偏向 underfitting




#load the given dataset "mushrooms.csv"
library(data.table)
mushrooms <- fread("mushrooms.csv", data.table = F ,stringsAsFactors = T)
#2.1  Convert the variable type from character to factor and replace the variable names with the following "col_name"
col_name <- c("class", "cap_shape", "cap_surface", "cap_color", "bruises", "odor", "gill_attachment", "gill_spacing", "gill_size", "gill_color", "stalk_shape", "stalk_root", "stalk_surface_above_ring","stalk_surface_below_ring", "stalk_color_above_ring", "stalk_color_below_ring", "veil_type", "veil_color", "ring_number", "ring_type", "spore_print_color", "population", "habitat")
colnames(mushrooms) <- col_name
View(mushrooms)
str(mushrooms)


#2.2 Refer to the given data description file “Mushrooms_Readme.pdf” for more information about the data. Please remove those records with missing “stalk_root”
mushrooms <- mushrooms[-which(mushrooms$stalk_root == "?"), ]

#2.4 Please fit a simple logistic regression model and computer the predicted probability of class = poisonous when mushrooms have bruises (bruises = t) or not (bruises = f)
glm_default <- glm(class ~ bruises, data = mushrooms, family = "binomial")
summary(glm_default)
#predict
predict(glm_default, data.frame(bruises = c("t","f")), type = "response") #t:0.185659 /f:0.692578


#2.4 Create a crosstab "bruises by class" and calculate the percentages. Then compute odds ratio (OR) using this crosstab and briefly describe your finding
p = prop.table(xtabs( ~ bruises + class, mushrooms), margin = 1); p
#       class
#bruises   edible poisonous
#f 0.307422  0.692578
#t 0.814341  0.185659

#odds ratio
odds = p[,2] / p[,1]
odds[2] / odds[1] #0.101199 

#describe your finding
#p = prop.table(xtabs( ~ bruises + class, mushrooms), margin = 1); p

#bruises是t的poisonous機率是bruises是f的poisonous的0.101199倍
