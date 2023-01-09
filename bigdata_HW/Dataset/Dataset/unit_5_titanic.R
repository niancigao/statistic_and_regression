
# Unit 5. Try it. 
# Titanic data
library(data.table); library(epiDisplay)

library(titanic)
data("titanic_train")
titanic_train

titanic_train$Sex = factor(titanic_train$Sex, levels = c("male", "female"))
titanic_train$Pclass = factor(titanic_train$Pclass, labels = c("3rd", "2nd", "1st"), levels = c("3", "2", "1"))
titanic_train$age_cat = factor(ifelse(titanic_train$Age <= 18, "Child", "Adult"))

# Make "survived" more readable
titanic_train$Survived = factor(titanic_train$Survived, levels = c(0,1), labels = c("No", "Yes"))

# Logistic Regression
titanic_logit = glm(Survived ~ Pclass + age_cat + Sex, family = "binomial", data = titanic_train)
epiDisplay::logistic.display( titanic_logit)

# Probabilities
# Jack
predict(titanic_logit, newdata = data.frame(Pclass = "3rd", age_cat = "Adult", Sex = "male"), type = "response")

# Rose 
predict(titanic_logit, newdata = data.frame(Pclass = "1st", age_cat = "Adult", Sex = "female"), type = "response")


