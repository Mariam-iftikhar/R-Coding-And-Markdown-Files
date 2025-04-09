install.packages("readr")
library(readr)
library(data.table)
install.packages(c("rpart", "rpart.plot"))
library(rpart)
library(rpart.plot)

data_train <- read_csv("C:/Users/maria/OneDrive/Desktop/adult_ch6_training.csv")
data_test <- read_csv("C:/Users/maria/OneDrive/Desktop/adult_ch6_test.csv")
head(data_train)
head(data_test)


# 14. Create a CART model using the training data set that predicts income using marital status
# and capital gains and losses. Visualize the decision tree (that is, provide the decision tree output). Describe the first few splits in the decision tree.

colnames(data_train)[1] <- "maritalStatus"
data_train$Income <- factor(data_train$Income)
data_train$maritalStatus <- factor(data_train$maritalStatus)
cart01 <- rpart(formula = Income ~ maritalStatus + Cap_Gains_Losses,
                data = data_train, method = "class")
rpart.plot(cart01)

X_train = data.frame(maritalStatus = data_train$maritalStatus, Cap_Gains_Losses =
                 data_train$Cap_Gains_Losses)
predIncomeCART_train = predict(object = cart01, newdata = X_train, type = "class")


# 15. Develop a CART model using the test data set that utilizes the same target and predictor 
# variables. Visualize the decision tree. Compare the decision trees. Does the test data result match the training data result?

colnames(data_test)[1] <- "maritalStatus"
data_test$Income <- factor(data_test$Income)
data_test$maritalStatus <- factor(data_test$maritalStatus)
cart011 <- rpart(formula = Income ~ maritalStatus + Cap_Gains_Losses,
                data = data_test, method = "class")
rpart.plot(cart011)

X_test = data.frame(maritalStatus = data_test$maritalStatus, Cap_Gains_Losses =
                       data_test$Cap_Gains_Losses)
predIncomeCART_test = predict(object = cart01, newdata = X_test, type = "class")


