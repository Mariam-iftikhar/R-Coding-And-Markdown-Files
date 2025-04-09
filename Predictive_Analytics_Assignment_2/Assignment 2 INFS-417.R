install.packages("readr")
library(readr)
library(data.table)
library(ggplot2)

data <- read_csv("C:/Users/maria/OneDrive/Desktop/adult.csv")
head(data)
spec(data)

#21. Partition the data set, so that 50% of the records are included in the training data set and 50% are included in the test data set. Use a bar graph to confirm your proportions.

n <- dim(data)[1]
n

data_distribution <- sample(1:n, size = n * 0.5)

data_train <- data[data_distribution, ]
data_test <- data[-data_distribution, ]

cat("Training Set Rows:", nrow(data_train), "\n")
cat("Testing Set Rows:", nrow(data_test), "\n")


partitions_ <- data.frame(
  Set = c("Training", "Testing"),
  Count = c(nrow(data_train), nrow(data_test))
)
partitions_

ggplot(partitions_, aes(x = Set, y = Count, fill = Set)) +
  geom_bar(stat = "identity") +
  labs(title = "Training vs Testing Distribution") +
  theme_minimal()

#22. Identify the total number of records in the training data set, and how many records in the training data set have an income value of >50 K.

total_records_train <- nrow(data_train)
total_records_train

income_above_50k_train <- sum(data_train$income == ">50K.")
income_above_50k_train

View(data_train)
cat("Total records in training set:", total_records_train)

cat("Records with income >50K in training set:", income_above_50k_train)

#23. Use your answers from the previous exercise to calculate how many records with income >50 K you need to resample in order to have 35% of the rebalanced data set have incomes of >50 K.


desired_ratio <- 0.35

required_50k_records<- round(desired_ratio * total_records_train)
required_50k_records

calculation_50k_records <- (required_50k_records - income_above_50k_train) /(1-0.35)
calculation_50k_records
cat("Records with income >50K needed:", calculation_50k_records, "\n")


income_above_50k_records <- which(data_train$income == ">50K.")
length(income_above_50k_records)

resample_records <- sample(x=income_above_50k_records, calculation_50k_records, replace = TRUE)
resample_records
resample_records <- data_train [resample_records, ]
resample_records


rebalanced_train_data <- rbind(data_train, resample_records)
rebalanced_train_data
View(rebalanced_train_data)

#24. Perform the rebalancing described in the previous exercise and confirm that 35% of the records in the rebalanced data set have incomes >50 K.

t.v1 <- table(rebalanced_train_data$income)
t.v2 <- rbind(t.v1, round(prop.table(t.v1), 4))

t.v2
# OR
rebalance_proportions <- mean(rebalanced_train_data$income == ">50K.")
cat("Proportion of income >50K in rebalanced set:", round(rebalance_proportions * 100, 2), "%\n")


#25. Which baseline model do we use to compare our classification model performance against? To which value does this baseline model assign all predictions? What is the accuracy of this baseline model?


majority_class_ <- ifelse(sum(data$income == ">50K.") > sum(data$income == "<=50K."), ">50K", "<=50K.")
baseline_accuracy_ <- mean(data$income == majority_class_)

cat("Baseline model predicts all records as:", majority_class_)

cat("Accuracy of baseline model:", round(baseline_accuracy_ * 100, 2))
