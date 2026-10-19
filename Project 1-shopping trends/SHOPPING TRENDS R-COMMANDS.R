# Base R method
my_data <- read.csv("CLEANED.csv")

# Tidyverse method 
library(readr)
my_data <- read_csv("CLEANED.csv")
#Get the empty sets
sum(is.na(my_data))
#summary statistics
summary(my_data)
# Install and load 'janitor' to fix column names automatically
install.packages("janitor")
library(janitor)

# Changing "Purchase Amount (USD)" to "purchase_amount_usd"
my_data <- clean_names(my_data)

# Check the new names
names(my_data)
# Convert Subscription Status to 1 (Yes) and 0 (No)
my_data$sub_dummy <- ifelse(my_data$subscription_status == "Yes", 1, 0)
# Define the model
# Formula: Y (Dependent) ~ X1 + X2 + X3 (Independent variables)
business_model <- lm(purchase_amount_usd ~ age + review_rating + sub_dummy, data = my_data)

# Generate the results table
summary(business_model)
library(ggplot2)

# Plotting Age vs. Purchase Amount with a trend line
ggplot(my_data, aes(x = age, y = purchase_amount_usd)) +
  geom_jitter(alpha = 0.2, color = "darkblue") +  # Jitter helps see density
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Effect of Customer Age on Total Spending",
       subtitle = "Linear Regression Analysis",
       x = "Customer Age",
       y = "Purchase Amount (USD)") +
  theme_minimal()
library(dplyr)
# Ensure dplyr is active
library(dplyr)

# Calculate summary stats without the internal assignment error
category_summary <- my_data %>%
  filter(category %in% c("Clothing", "Accessories")) %>%
  group_by(category) %>%
  dplyr::summarize(
    average_spend = mean(purchase_amount_usd, na.rm = TRUE),
    total_customers = n()
  )

# View your results table
print(category_summary)
# Compare the two groups statistically
t_test_results <- t.test(purchase_amount_usd ~ category, 
                         data = subset(my_data, category %in% c("Clothing", "Accessories")))

# View the p-value
t_test_results
