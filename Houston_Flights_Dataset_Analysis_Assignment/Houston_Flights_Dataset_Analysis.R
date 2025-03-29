--------------- # Project 2: Efficient Analysis of Large Datasets Using the Houston Flights Dataset----------------------------

# Objective:

# Learn how to connect R to a relational database (SQLite), extract data efficiently, and use `dplyr` to filter, group, and summarize large datasets.

# Dataset Information:

# The `hflights` dataset contains information about 227,496 flights departing from Houston in 2011. It includes variables such as flight origin, destination, departure delay, arrival delay, carrier, and more.

# Code:

install.packages("tidyverse")
install.packages("DBI")
install.packages("RSQLite")
install.packages("hflights")
# Load libraries
library(tidyverse)
library(DBI)
library(RSQLite)
library(hflights)

# Create an in-memory SQLite database
con <- dbConnect(RSQLite::SQLite(), ":memory:")

# Load the hflights dataset into the database
dbWriteTable(con, "hflights", hflights)

# Verify that the table was created
dbListTables(con)
# Load the flights table into a tibble for inspection
hflights_data <- tbl(con, "hflights")

# Check the structure of the dataset
glimpse(hflights_data)

# Check the dimensions of the dataset
dim(hflights_data)

# Summarize missing values
hflights_data %>%
  summarise(across(everything(), ~ sum(is.na(.))))


# Filter flights departing from IAH in January
iah_january_flights <- hflights_data %>%
  filter(Origin == "IAH", Month == 1)

# Group by carrier and calculate average departure delay
avg_delay_by_uniquecarrier <- iah_january_flights %>%
  group_by(UniqueCarrier ) %>%
  summarise(avg_dep_delay = mean(DepDelay, na.rm = TRUE)) 

# Sort by average departure delay

sorted_avg_delay <- avg_delay_by_uniquecarrier %>%
  arrange(desc(avg_dep_delay))


# View the sorted results
print(sorted_avg_delay)

# Create a bar plot of average departure delays by carrier
ggplot(sorted_avg_delay, aes(x = reorder(UniqueCarrier, -avg_dep_delay), y = avg_dep_delay)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Average Departure Delay by Carrier (IAH, January)",
    x = "UniqueCarrier",
    y = "Average Departure Delay (minutes)"
  ) +
  theme_minimal()