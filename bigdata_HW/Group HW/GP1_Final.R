library(ISLR)

#1.1
library(car)
library(ggplot2)
scatterplotMatrix(Credit[,c(2:7,12)])
barchart(table(Credit$Gender),Credit)
barchart(table(Credit$Student),Credit)
barchart(table(Credit$Married),Credit)
barchart(table(Credit$Ethnicity),Credit)


#1.2
#Consider doing a series of bivariate analyses on Balance vs. the rest of variables.Specifically, plot your data and perform bivariate statistical tests to understand the relationships among the variables.
#bivariate analyses on Balance vs. the rest of variables
bivarite <- lm(Balance~.,Credit)
summary(bivarite)

#1.3
#Please perform normality tests on Balance. Does it seem "normal”? If not, do you think fitting general linear models to predict or explain the outcome is appropriate? 
shapiro.test(dat$Balance)  # p-value = 5.344e-14
plot(density(dat$Balance)) 
# Not normal，明顯右偏，線性模型有常態假設，故結果可能不如預期

anova(bivarite)

#1.4
#Consider fitting linear models with manually selected variables (i.e., multivariate analysis). What is your best model? You may consider those variables with "p < 0.05"
modelBest <- lm (Balance~Income + Limit + Rating + Cards + Age + Student, Credit) #This is the Best model
model1 <- lm (Balance~Income + Limit + Cards + Age + Student, Credit)
model2 <- lm(Balance~Income+Limit+Rating, Credit)

# model = lm(Balance~Income+Limit+Rating,dat)
# summary(model)
# model2 = lm(Balance~.,dat)
# summary(model2)
# best = step(model2)    # AIC
# summary(best)

#1.5
library(caret)
set.seed(1) 
train_idx <- sample(1:nrow(Credit), 0.7*nrow(Credit)) 
train_d <- Credit[train_idx,] 
test_d <- Credit[setdiff(1:nrow(Credit), train_idx),] 
modelBest <- lm(Balance~ Income + Limit + Rating + Cards + Age + Student, train_d)
model1 <- lm(Balance~ Income + Limit + Cards + Age + Student, train_d)
model2 <- lm(Balance~Income+Limit+Rating,train_d)
p <- predict(modelBest, test_d)


#1.6
RMSE <- function(x, y){
  sqrt(mean((x-y)^2))
}
RMSE(modelBest$fitted.values, train_d$Balance) # lowest RMSE
RMSE(model1$fitted.values, train_d$Balance)
RMSE(model2$fitted.values, train_d$Balance)

#1.7
#Run summary() to get more information about your linear models, and report the variables with p-value < 0.05
summary(modelBest) #Income, Limit, Cards, Age, Student p-value < 0.05
summary(model1) #Income, Limit, Cards, Age, Student p-value < 0.05
summary(model2) #Income, Limit, Rating p-value < 0.05

#1.7
library(corrplot)
cormatrix = cor(Credit[,c(2:7,12)])
testRes = cor.mtest(Credit[,c(2:7,12)], conf.level = 0.95)
## specialized the insignificant value according to the significant level
corrplot(cormatrix, p.mat = testRes$p,
         insig='blank',addCoef.col ='red',
         number.cex = 1,order = 'hclust', addrect = 2)
#最好的模型有考慮到Card 但 Card 跟 Balance相關性不高，所以相關係數不適合拿來挑選變數

#2.1
library(data.table)
library(dplyr)
LoanStats <- read.csv("C:/Users/user/Desktop/bigdata_HW/LoanStats.csv/LoanStats.csv", skip=1)
LoanStats <- LoanStats[-c(39789:39791),]
LoanStats <- LoanStats %>% 
  select_if(~ !any(is.na(.))) %>% 
  filter(loan_status == "Fully Paid" | loan_status == "Charged Off")

table(LoanStats$loan_status) %>% prop.table

#2.2
library(sqldf)
result <- sqldf('select emp_length "Employment_Length", AVG(loan_amnt) "Loan_amount_average" 
      from LoanStats 
      group by emp_length')
result

#2.3
LoanStats %>% 
  group_by(purpose) %>% 
  filter(n() >= 5000) %>% 
  group_by(grade) %>% 
  summarize(Grade_Count = n())

table(LoanStats$purpose)