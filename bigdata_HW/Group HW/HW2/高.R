# Please load the built-in Credit data in the ISLR package 
# and remove the variable "ID". Enter “?Credit” to check out the data description

library(ISLR)

?Credit

Credit = Credit[,-1]
str(Credit)

# Split the dataset into training (70%) and testing (30%) sets 
# with random seed set.seed(1). 
# After that, rescale the training and testing sets if necessary


set.seed(1)

train_idx <- sample(1:nrow(Credit), 0.7*nrow(Credit)) 
train_d <- Credit[train_idx,] 
test_d <- Credit[setdiff(1:nrow(Credit), train_idx),] 

library(caret)

norm_data <- train_d
norm_test_d2 = test_d

process <- preProcess(norm_data[,c(1:6,11)], method = c("center", "scale"))
norm_data[,c(1:6,11)] <- predict(process, norm_data[,c(1:6,11)])

norm_test_d2[,c(1:6,11)] = predict(process, norm_test_d2[,c(1:6,11)])

# https://rpubs.com/SiQingYe/752656

# Please rank the importance of variables by absolute standardized regression
# coefficients of general linear models that predict “Balance”.

model = glm( Balance ~ . ,data = norm_data)
summary(model)
names(model)

sort(abs(model$coefficients)[-1],decreasing = T)

# Please create a leave-one-out cross-validation (LOOCV) MAE 
# to evaluate general linear models


library(caret)

trControl = trainControl(method = "LOOCV")
train(Balance ~ . ,data = norm_data , method = "glm",trControl = trControl)
# MAE : 0.1756705

# Or we can use stats::step() 
nullModel = glm( Balance ~ Student , data = train_d) # Base/null model. We may add "forced-in" variables here
fullModel = glm( Balance ~ (.) , data = train_d) # Full model.
# Forward Selection
stepAutoXY = step(nullModel, scope = list(lower = nullModel, upper = fullModel ), direction = "forward", test = "F" )
stepAutoXY

# 手刻
# Income    Limit    Rating   Cards    Age
# Education Gender   Student  Married  Ethnicity    

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



