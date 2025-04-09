install.packages("readr")
library(readr)
library(data.table)

data <- read_csv("C:/Users/maria/OneDrive/Desktop/adult_ch3_training.csv")
head(data)
spec(data)


#26. Add a record index field to the data set.
n <- dim(data)[1]
n
data$Index <- c(1:n)
data$Index
spec(data)
head(data)
View(data)


#27. Determine whether any outliers exist for the education field.
par(mfrow = c(1, 1))
boxplot(data$education,ylab = "education")

# OR

# Calculate the IQR for education
Q1 <- quantile(data$education, 0.25, na.rm = TRUE)
Q3 <- quantile(data$education, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

education_outliers <- data$education[data$education < lower_bound | data$education > upper_bound]

length(education_outliers)
# Comments: Yes, the outlier exists in the education column. 
#There are 552 values in the data which needs to be considered
# while proceeding.


#28. Do the following for the age field.
par(mfrow = c(1, 1))
boxplot(data$age,ylab = "age")

#OR

Q1_ <- quantile(data$age, 0.25, na.rm = TRUE)
Q3_ <- quantile(data$age, 0.75, na.rm = TRUE)
IQR_value_1 <- Q3_ - Q1_

lower_bound1 <- Q1_ - 1.5 * IQR_value_1
upper_bound1 <- Q3_ + 1.5 * IQR_value_1

age_outliers1_ <- data$age[data$age < lower_bound1 | data$age > upper_bound1]

#Comments: There are 70 values which acts as an outlier before scaling.

#a. Standardize the variable.
z_scores <- scale(data$age)
z_scores
#b. Identify how many outliers there are and identify the most extreme outlier.
outliers <- which( z_scores < -3  |z_scores > 3 ,)
outliers
length(outliers)

outlier_values <- data$age[outliers]
print(outlier_values)

par(mfrow = c(1, 2))
boxplot(data$age, main = "Age Distribution (Before Scaling)")
boxplot(z_scores, main = "Age Distribution (After Scaling)")

extreme_outlier <- data[which.max(abs(z_scores)), ]
extreme_outlier

# Comments: There are 60 values that are outliers and 1 row with extreme value i.e age 90.


# 29. Derive a flag for capital‐gain, called capital‐gain‐flag, which equals 0 for capital gain equals zero, and 1 otherwise.
data$capital_gain_flag <- ifelse(data$`capital-gain` == 0, 0, 1)
data$capital_gain_flag

# 30. Age anomaly? Select only records with age at least 80. Construct a histogram of age.
#Explain what you see in one sentence and why it is like that in another sentence.
par(mfrow = c(1, 1))
age_80_plus <- subset(data, age >= 80)
hist(age_80_plus$age, main="Histogram of Age (80+)", xlab="Age", col="blue")


# Comments: This histogram illustrates the age distribution for 
#individuals aged 80 and older.

#The Reason Behind It: The distribution likely looks this way 
#because there are fewer people in the older age brackets,
#as those over 80 represent a smaller segment of the overall population.