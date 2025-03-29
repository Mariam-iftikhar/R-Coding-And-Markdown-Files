--------------- # Project 1: Sampling and Group Analysis:------------------------------

## Objective: 

# Apply sampling theory and group analysis techniques to a dataset containing demographic and financial attributes. The goal is to perform statistical analysis and hypothesis testing to uncover spending patterns.

## Dataset Information:

# Use the Shop Customer Dataset from Kaggle. (https://www.kaggle.com/datasets/datascientistanna/customers-dataset ) This dataset contains demographic and financial attributes, including Gender, Age, Annual Income, and Spending Score, making it suitable for stratified sampling and group analysis.

# Code:

install.packages("tidyverse")
install.packages("caret")
library(tidyverse)
library(caret)

# Import dataset
customer_data <- read.csv("C:/Users/maria/Downloads/Customers.csv", header = TRUE)

# View structure
str(customer_data)

# Check column names to confirm the correct variable name
colnames(customer_data)

# Rename column for consistency
customer_data <- customer_data %>% rename(Annual_Income = `Annual.Income....`, Spending_Score = `Spending.Score..1.100.`)

customer_data <- customer_data %>%
  mutate(IncomeGroup = case_when(
    Annual_Income < 40000 ~ "Low",
    Annual_Income >= 40000 & Annual_Income < 70000 ~ "Medium",
    Annual_Income >= 70000 ~ "High"
  ))

# Random sampling
set.seed(42)
random_sample <- customer_data %>% sample_frac(0.2)

# Stratified sampling by Gender
stratified_sample <- customer_data %>%
  group_by(Gender) %>%
  sample_frac(0.2)

# Group analysis
group_summary <- customer_data %>%
  group_by(IncomeGroup) %>%
  summarize(
    AvgSpending = mean(Spending_Score, na.rm = TRUE),
    MedianSpending = median(Spending_Score, na.rm = TRUE),
    SDSpending = sd(Spending_Score, na.rm = TRUE)
  )

# Filter groups
low_income <- customer_data %>% filter(IncomeGroup == "Low") %>% pull(Spending_Score)
high_income <- customer_data %>% filter(IncomeGroup == "High") %>% pull(Spending_Score)

# Perform t-test
t_test_result <- t.test(low_income, high_income)

# Display result
print(t_test_result)

# Visualization
ggplot(group_summary, aes(x = IncomeGroup, y = AvgSpending, fill = IncomeGroup)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Average Spending Score by Income Group",
    x = "Income Group",
    y = "Average Spending Score"
  ) +
  theme_minimal()

