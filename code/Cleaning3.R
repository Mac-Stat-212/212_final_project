# Load necessary library
library(dplyr)
library(tidyverse)

# Read the dataset
modified_data <- read.csv('../data/finished_data.csv')

# Create the median_volume variable based on the Volume per stock_name
modified_data <- modified_data %>%
  group_by(stock_name) %>%
  mutate(median_volume = median(Volume, na.rm = TRUE)) %>%  # Calculate the median volume per stock_name
  mutate(volume_range = case_when(
    median_volume <= 10000 ~ "0-10 thousand",
    median_volume <= 100000 ~ "10 thousand-100 thousand",
    median_volume <= 1000000 ~ "100 thousand-1 million",
    median_volume <= 9999000 ~ "1 million-10 million",
    median_volume > 10000000 ~ "Above 10 million"
  )) %>%
  ungroup()  # Ungroup to remove stock_name grouping

# Save the modified dataset to a new CSV file
write.csv(modified_data, '../data/modified_data.csv', row.names = FALSE)




