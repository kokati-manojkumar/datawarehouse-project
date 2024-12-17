# Create a sample dataset
customer_data <- data.frame(
  CustomerID = 1:5,
  Gender = c("Male", "Female", "Female", "Male", "Female"),
  Country = c("USA", "Canada", "USA", "Canada", "USA"),
  PurchaseAmount = c(250, NA, 150, NA, 300),
  Age = c(25, 30, NA, 22, 29)
)

#  To Display the original dataset
print("Original Dataset:")
print(customer_data)

#  TO Check for missing values
missing_values <- sum(is.na(customer_data))
print(paste("Total missing values:", missing_values))

#  To Drop rows with any missing values
customer_data_dropped <- customer_data %>% drop_na()
print("Dataset after dropping rows with missing values:")
print(customer_data_dropped)

#  To Fill missing values with the mean
customer_data_filled <- customer_data %>%
  mutate(PurchaseAmount = ifelse(is.na(PurchaseAmount), mean(PurchaseAmount, na.rm = TRUE), PurchaseAmount),
         Age = ifelse(is.na(Age), mean(Age, na.rm = TRUE), Age))

print("Dataset after filling missing values with the mean:")
print(customer_data_filled)

#  To Normalize the data using Min-Max scaling
customer_data_normalized <- customer_data_filled %>%
  mutate(Normalized_PurchaseAmount = rescale(PurchaseAmount),
         Normalized_Age = rescale(Age))

print("Normalized Dataset:")
print(customer_data_normalized)

# Check for correlation
correlation_matrix <- cor(customer_data_normalized[, c("PurchaseAmount", "Age", "Normalized_PurchaseAmount", "Normalized_Age")])
print("Correlation Matrix:")
print(correlation_matrix)

# To Visualize correlation matrix
corrplot(correlation_matrix, method = "circle")

#  To Original Purchase Amount Distribution
ggplot(customer_data, aes(x = PurchaseAmount)) +
  geom_histogram(binwidth = 50, fill = "blue", alpha = 0.5) +
  labs(title = "Distribution of Purchase Amount (Original)",
       x = "Purchase Amount", y = "Frequency")

# To Normalized Purchase Amount Distribution
ggplot(customer_data_normalized, aes(x = Normalized_PurchaseAmount)) +
  geom_histogram(binwidth = 0.1, fill = "green", alpha = 0.5) +
  labs(title = "Distribution of Normalized Purchase Amount",
       x = "Normalized Purchase Amount", y = "Frequency")

ggplot(customer_data, aes(y = PurchaseAmount)) +
  geom_boxplot(fill = "orange", outlier.colour = "red") +
  labs(title = "Boxplot of Purchase Amount",
       y = "Purchase Amount")

set.seed(123)  # For reproducibility
kmeans_result <- kmeans(customer_data_normalized[, c("Normalized_PurchaseAmount", "Normalized_Age")], centers = 2)
customer_data_normalized$Cluster <- as.factor(kmeans_result$cluster)

ggplot(customer_data_normalized, aes(x = Normalized_PurchaseAmount, y = Normalized_Age, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "K-means Clustering of Customers",
       x = "Normalized Purchase Amount", y = "Normalized Age")

write.csv(customer_data_normalized, "customer_data_normalized.csv", row.names = FALSE)
