# LSE Data Analytics Online Career Accelerator 
# DA301: Advanced Analytics for Organisational Impact

# The most important observations can be found in the report or presentation.


# Assignment Activity Four and Five.
# 1. Prepare the workstation, check for missing values, duplicates and,
# column data types in the dataset.
# Import the necessary packages.
library(tidyverse)
library(psych)
library(DataExplorer)
library(plotly)
library(caret)

# Set the working directory.
setwd("C:/Users/matti/OneDrive/1- LSE/5- Course Three (Python and R)/Assignment")

# Import the turtle_sales.csv file into a variable named turtle_sales.
turtle_sales <- read.csv("turtle_sales.csv")

# Create a subset of the data frame without the redundant columns.
turtle_sales_subset <- turtle_sales %>%
  select(-Ranking, -Year, -Genre, -Publisher)

# Check for missing values and duplicate rows.
missing_values <- sum(is.na(turtle_sales_subset))
duplicate_rows <- sum(duplicated(turtle_sales_subset))

# View the number of missing values and duplicate rows.
cat("Missing Values:", missing_values, "\n")
cat("Duplicate Rows:", duplicate_rows, "\n")

# Check the data types of each column.
col_dt <- sapply(turtle_sales_subset, class)

# View the data types. 
col_dt


# 2. Create a new data frame, explore its descriptive statistics,
# and check for outliers.
# Group the data by the product column.
turtle_sales_subset2 <- turtle_sales_subset %>%
  group_by(Product) %>%
  summarise(NA_Sales = sum(NA_Sales),
            EU_Sales = sum(EU_Sales),
            Global_Sales = sum(Global_Sales))
            
# View the descriptive statistics for the sales columns using summary(),
# and describe().
summary(turtle_sales_subset2[, c("NA_Sales", "EU_Sales", "Global_Sales")])
describe(turtle_sales_subset2[, c("NA_Sales", "EU_Sales", "Global_Sales")])

# View the descriptive statistics of the whole data frame using DataExplorer.
DataExplorer::create_report(turtle_sales_subset2)

# Calculate the quartiles and IQR for the NA_Sales column.
NA_Q1 <- quantile(turtle_sales_subset2$NA_Sales, 0.25)
NA_Q3 <- quantile(turtle_sales_subset2$NA_Sales, 0.75)
NA_IQR <- NA_Q3 - NA_Q1

# Define the lower and upper bounds for the outliers.
NA_lower_bound <- NA_Q1 - 1.5 * NA_IQR
NA_upper_bound <- NA_Q3 + 1.5 * NA_IQR

# Identify the outliers in the NA_Sales column.
NA_outliers <- turtle_sales_subset2$NA_Sales[turtle_sales_subset2$NA_Sales 
                                            < NA_lower_bound | 
                                              turtle_sales_subset2$NA_Sales 
                                            > NA_upper_bound]

# Count the number of outliers.
NA_num_outliers <- length(NA_outliers)

# View the number of outliers.
cat("Number of outliers in the NA_Sales column:", NA_num_outliers, "\n")

# View the values of the outliers.
cat("Outlier values for the NA_Sales column:", NA_outliers, "\n")

# Calculate the quartiles and IQR for the EU_Sales column.
EU_Q1 <- quantile(turtle_sales_subset2$EU_Sales, 0.25)
EU_Q3 <- quantile(turtle_sales_subset2$EU_Sales, 0.75)
EU_IQR <- EU_Q3 - EU_Q1

# Define the lower and upper bounds for the outliers.
EU_lower_bound <- EU_Q1 - 1.5 * EU_IQR
EU_upper_bound <- EU_Q3 + 1.5 * EU_IQR

# Identify the outliers in the EU_Sales column.
EU_outliers <- turtle_sales_subset2$EU_Sales[turtle_sales_subset2$EU_Sales 
                                            < EU_lower_bound | 
                                              turtle_sales_subset2$EU_Sales 
                                            > EU_upper_bound]

# Count the number of outliers.
EU_num_outliers <- length(EU_outliers)

# View the number of outliers.
cat("Number of outliers in the EU_Sales column:", EU_num_outliers, "\n")

# View the values of the outliers.
cat("Outlier values for the EU_Sales column:", EU_outliers, "\n")

# Calculate the quartiles and IQR for the Global_Sales column.
Global_Q1 <- quantile(turtle_sales_subset2$Global_Sales, 0.25)
Global_Q3 <- quantile(turtle_sales_subset2$Global_Sales, 0.75)
Global_IQR <- Global_Q3 - Global_Q1

# Define the lower and upper bounds for the outliers.
Global_lower_bound <- Global_Q1 - 1.5 * Global_IQR
Global_upper_bound <- Global_Q3 + 1.5 * Global_IQR

# Identify the outliers in the Global_Sales column.
Global_outliers <- turtle_sales_subset2$Global_Sales[turtle_sales_subset2
                                                    $Global_Sales 
                                                    < Global_lower_bound | 
                                                      turtle_sales_subset2
                                                    $Global_Sales 
                                                    > Global_upper_bound]

# Count the number of outliers.
Global_num_outliers <- length(Global_outliers)

# View the number of outliers.
cat("Number of outliers in the Global_Sales column:", Global_num_outliers, "\n")

# View the values of the outliers.
cat("Outlier values for the Global_Sales column:", Global_outliers, "\n")


# 3. Explore the turtle_sales_subset2 data frame by creating various plots.
# Create a histogram for NA_Sales.
# Calculate the bin width using the Freedman-Diaconis rule.
NA_bw <- 2 * IQR(turtle_sales_subset2$NA_Sales) / 
  length(turtle_sales_subset2$NA_Sales)^(1/3)

# Plot the NA_Sales histogram.
NA_Sales_histogram <- ggplot(turtle_sales_subset2, aes(x = NA_Sales)) +
  geom_histogram(binwidth = NA_bw, fill = "red", color = "black") +
  labs(title = "Distribution of North American Sales",
       x = "NA Sales (in millions of £)",
       y = "Frequency")

# Make the NA_Sales histogram interactive.
NA_Sales_histogram <- ggplotly(NA_Sales_histogram)

# View the interactive NA_Sales histogram.
NA_Sales_histogram

# Create a histogram for EU_Sales.
# Calculate the bin width using the Freedman-Diaconis rule.
EU_bw <- 2 * IQR(turtle_sales_subset2$EU_Sales) / 
  length(turtle_sales_subset2$EU_Sales)^(1/3)

# Plot the EU_Sales histogram.
EU_Sales_histogram <- ggplot(turtle_sales_subset2, aes(x = EU_Sales)) +
  geom_histogram(binwidth = EU_bw, fill = "blue", color = "black") +
  labs(title = "Distribution of European Sales",
       x = "EU Sales (in millions of £)",
       y = "Frequency")

# Make the EU_Sales histogram interactive.
EU_Sales_histogram <- ggplotly(EU_Sales_histogram)

# View the interactive EU_Sales histogram.
EU_Sales_histogram

# Create a histogram for Global_Sales.
# Calculate the bin width using the Freedman-Diaconis rule.
Global_bw <- 2 * IQR(turtle_sales_subset2$Global_Sales) / 
  length(turtle_sales_subset2$Global_Sales)^(1/3)

# Plot the Global_Sales histogram.
Global_Sales_histogram <- ggplot(turtle_sales_subset2, aes(x = Global_Sales)) +
  geom_histogram(binwidth = Global_bw, fill = "green", color = "black") +
  labs(title = "Distribution of Global Sales",
       x = "Global Sales (in millions of £)",
       y = "Frequency")

# Make the Global_Sales histogram interactive.
Global_Sales_histogram <- ggplotly(Global_Sales_histogram)

# View the interactive Global_Sales histogram.
Global_Sales_histogram

# Create a boxplot for NA_Sales.
NA_Sales_boxplot <- ggplot(turtle_sales_subset2, aes(y = NA_Sales)) +
  geom_boxplot(fill = "red", color = "black") +
  labs(title = "Boxplot of North American Sales",
       y = "NA Sales (in millions of £)")

# Make the NA_Sales boxplot interactive.
NA_Sales_boxplot <- ggplotly(NA_Sales_boxplot)

# View the interactive NA_Sales boxplot.
NA_Sales_boxplot

# Create a boxplot for EU_Sales.
EU_Sales_boxplot <- ggplot(turtle_sales_subset2, aes(y = EU_Sales)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Boxplot of European Sales",
       y = "EU Sales (in millions of £)")

# Make the EU_Sales boxplot interactive.
EU_Sales_boxplot <- ggplotly(EU_Sales_boxplot)

# View the interactive EU_Sales boxplot.
EU_Sales_boxplot

# Create a boxplot for Global_Sales
Global_Sales_boxplot <- ggplot(turtle_sales_subset2, aes(y = Global_Sales)) +
  geom_boxplot(fill = "green", color = "black") +
  labs(title = "Boxplot of Global Sales",
       y = "Global Sales (in millions of £)")

# Make the Global_Sales boxplot interactive.
Global_Sales_boxplot <- ggplotly(Global_Sales_boxplot)

# Display the interactive Global_Sales boxplot.
Global_Sales_boxplot

# Create a scatterplot of NA_Sales vs EU_Sales.
NA_EU_scatterplot <- ggplot(turtle_sales_subset2, aes(x = NA_Sales, 
                                                       y = EU_Sales,
                                                       label = Product)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +
  labs(title = "Scatterplot of North American Sales vs European Sales",
       x = "NA Sales (in millions of £)",
       y = "EU Sales (in millions of £)")

# Make the NA_EU_scatterplot interactive.
NA_EU_scatterplot <- ggplotly(NA_EU_scatterplot, 
                               tooltip = c("NA_Sales", "EU_Sales", "label"))

# View the NA_EU_scatterplot.
NA_EU_scatterplot

# Create a scatterplot of NA_Sales vs Global_Sales.
NA_Global_scatterplot <- ggplot(turtle_sales_subset2, aes(x = NA_Sales, 
                                                           y = Global_Sales,
                                                           label = Product)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +
  labs(title = "Scatterplot of North American Sales vs Global Sales",
       x = "NA Sales (in millions of £)",
       y = "Global Sales (in millions of £)")

# Make the NA_Global_scatterplot interactive.
NA_Global_scatterplot <- ggplotly(NA_Global_scatterplot, 
                                   tooltip = c("NA_Sales", "Global_Sales", 
                                               "label"))

# View the NA_Global_scatterplot.
NA_Global_scatterplot

# Create a scatterplot of EU_Sales vs Global_Sales.
EU_Global_scatterplot <- ggplot(turtle_sales_subset2, aes(x = EU_Sales, 
                                                           y = Global_Sales,
                                                           label = Product)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "purple") +
  labs(title = "Scatterplot of European Sales vs Global Sales",
       x = "EU Sales (in millions of £)",
       y = "Global Sales (in millions of £)")

# Make the EU_Global_scatterplot interactive.
EU_Global_scatterplot <- ggplotly(EU_Global_scatterplot, 
                                   tooltip = c("EU_Sales", "Global_Sales",
                                               "label")) 

# View the EU_Global_scatterplot.
EU_Global_scatterplot


# 4. Determine the normality of the dataset.
# Create a Q-Q plot for NA_Sales.
NA_QQ_plot <- ggplot(turtle_sales_subset2, aes(sample = NA_Sales)) +
  geom_qq(color = "red") +
  geom_qq_line() +
  labs(title = "Q-Q Plot for North American Sales",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")

# Make the Q-Q plot for NA_Sales interactive.
NA_QQ_plot <- ggplotly(NA_QQ_plot, tooltip = c("theoretical", "NA_Sales"))

# View the interactive Q-Q plot for NA_Sales.
NA_QQ_plot

# Create a Q-Q plot for EU_Sales.
EU_QQ_plot <- ggplot(turtle_sales_subset2, aes(sample = EU_Sales)) +
  geom_qq(color = "blue") +
  geom_qq_line() +
  labs(title = "Q-Q Plot for European Sales",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")

# Make the Q-Q plot for EU_Sales interactive.
EU_QQ_plot <- ggplotly(EU_QQ_plot, tooltip = c("theoretical", "EU_Sales"))

# Display the interactive Q-Q plot for EU_Sales.
EU_QQ_plot

# Create a Q-Q plot for Global_Sales.
Global_QQ_plot <- ggplot(turtle_sales_subset2, aes(sample = Global_Sales)) +
  geom_qq(color = "green") +
  geom_qq_line() +
  labs(title = "Q-Q Plot for Global Sales",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles")

# Make the Q-Q plot for Global_Sales interactive.
Global_QQ_plot <- ggplotly(Global_QQ_plot, tooltip = c("theoretical", 
                                                       "Global_Sales"))

# Display the interactive Q-Q plot for Global_Sales.
Global_QQ_plot

# Perform the Shapiro-Wilk test for NA_Sales.
NA_SW_test <- shapiro.test(turtle_sales_subset2$NA_Sales)

# View the Shapiro-Wilk test results for NA_Sales.
cat("Shapiro-Wilk Test Results for NA_Sales:\n")
cat("W statistic:", NA_SW_test$statistic, "\n")
cat("p-value:", NA_SW_test$p.value, "\n")
cat("Normality assumption for NA_Sales:", ifelse(NA_SW_test$p.value < 0.05, 
                                                 "Rejected", "Not Rejected"), 
    "\n")

# Perform the Shapiro-Wilk test for EU_Sales.
EU_SW_test <- shapiro.test(turtle_sales_subset2$EU_Sales)

# View the Shapiro-Wilk test results for EU_Sales.
cat("Shapiro-Wilk Test Results for EU_Sales:\n")
cat("W statistic:", EU_SW_test$statistic, "\n")
cat("p-value:", EU_SW_test$p.value, "\n")
cat("Normality assumption for EU_Sales:", ifelse(EU_SW_test$p.value < 0.05, 
                                                 "Rejected", 
                                                 "Not Rejected"), "\n")

# Perform the Shapiro-Wilk test for Global_Sales.
Global_SW_test <- shapiro.test(turtle_sales_subset2$Global_Sales)

# View the Shapiro-Wilk test results for Global_Sales.
cat("Shapiro-Wilk Test Results for Global_Sales:\n")
cat("W statistic:", Global_SW_test$statistic, "\n")
cat("p-value:", Global_SW_test$p.value, "\n")
cat("Normality assumption for Global_Sales:", ifelse(Global_SW_test$p.value 
                                                     < 0.05, 
                                                     "Rejected", 
                                                     "Not Rejected"), "\n")


# 5. Create a plot that compares all the sales data to see if any correlations,
# exist between the three sales columns.
# Create a scatterplot of NA_Sales vs EU_Sales with Global_Sales as the hue.
NA_EU_Global_scatterplot <- ggplot(turtle_sales_subset2, 
                                   aes(x = NA_Sales, 
                                       y = EU_Sales, label = Product)) + 
  geom_point(aes(color = Global_Sales)) + 
  geom_smooth(method = "lm", se = FALSE, color = "purple", 
              show.legend = FALSE) +
  labs(title = "Scatterplot of North American vs European vs Global Sales",
       x = "NA Sales (in millions of £)",
       y = "EU Sales (in millions of £)",
       color = "") +
  scale_color_continuous(low = "yellow", high = "black",
                         name = "Global Sales 
(in millions of £)")

# Make the NA_EU_Global_scatterplot interactive.
NA_EU_Global_scatterplot <- ggplotly(NA_EU_Global_scatterplot,
                                     tooltip = c("NA_Sales", 
                                                 "EU_Sales", "label"))

# View the interactive NA_EU_Global_scatterplot.
NA_EU_Global_scatterplot


# 6. Create a visualisation comparing the sales columns by platform.  
# Group the data by the platform column by creating a new data frame.
turtle_sales_subset3 <- turtle_sales_subset %>%
  group_by(Platform) %>%
  summarise(NA_Sales = sum(NA_Sales),
            EU_Sales = sum(EU_Sales),
            Global_Sales = sum(Global_Sales))

# Reshape the turtle_sales_subset3 data frame to created a grouped barplot.
turtle_sales_subset3.1 <- tidyr::gather(turtle_sales_subset3, 
                                        key = "Sales_Type", value = "Sales", 
                                        -Platform)

# Create a grouped barplot for sales.
sales_barplot <- ggplot(turtle_sales_subset3.1, aes(x = Platform, y = Sales, 
                                                    fill = Sales_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sales by Platform", x = "Platform", 
       y = "Sales (in millions of £)", fill = "Sales Region") +
  scale_fill_manual(values = c("NA_Sales" = "red", "EU_Sales" = "blue", 
                               "Global_Sales" = "green")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Make the sales barplot interactive.
sales_barplot <- ggplotly(sales_barplot)

# View the interactive sales barplot.
sales_barplot


# 7. Find the percentage of NA_Sales and EU_Sales from the Global_Sales.
# Using turtle_sales_subset2, calculate the sum of each sales column.
NA_Sales_total <- sum(turtle_sales_subset2$NA_Sales)
EU_Sales_total <- sum(turtle_sales_subset2$EU_Sales)
Global_Sales_total <- sum(turtle_sales_subset2$Global_Sales)

# View the total sum per sales column.
NA_Sales_total
EU_Sales_total
Global_Sales_total 

# Calculate the percentage of NA_Sales from Global_Sales.
NA_Sales_percentage <- (NA_Sales_total / Global_Sales_total) * 100

# Calculate the percentage of EU_Sales from Global_Sales.
EU_Sales_percentage <- (EU_Sales_total / Global_Sales_total) * 100

# View the percentages.
NA_Sales_percentage
EU_Sales_percentage




# Assignment Activity Six.
# 1. Use turtle_sales_subset2 to create a new data frame that only contains,
# the sales columns.
turtle_sales_subset4  <- turtle_sales_subset2 %>% 
  select(-Product)


# 2. Simple linear regression models.
# Find a correlation between the sales columns.
sales_correlation <- cor(turtle_sales_subset4)

# View the correlation.
sales_correlation

# Plot the relationship between NA_Sales and EU_Sales.
plot(turtle_sales_subset4$NA_Sales, turtle_sales_subset4$EU_Sales)

# Create a simple linear regression model.
NA_EU_model <- lm(NA_Sales~EU_Sales, data=turtle_sales_subset4)

# View the model.
NA_EU_model

# View the summary of the model.
summary(NA_EU_model)

# Plot the relationship between NA_Sales and Global_Sales.
plot(turtle_sales_subset4$NA_Sales, turtle_sales_subset4$Global_Sales)

# Create a simple linear regression model.
NA_Global_model <- lm(NA_Sales~Global_Sales, data=turtle_sales_subset4)

# View the model.
NA_Global_model

# View the summary of the model.
summary(NA_Global_model)

# Plot the relationship between EU_Sales and Global_Sales.
plot(turtle_sales_subset4$EU_Sales, turtle_sales_subset4$Global_Sales)

# Create a simple linear regression model.
EU_Global_model <- lm(EU_Sales~Global_Sales, data=turtle_sales_subset4)

# View the model.
EU_Global_model

# View the summary of the model.
summary(EU_Global_model)


# 3. Multiple linear regression (MLR) model.
# View the correlation between the sales columns.
sales_correlation

# Create a MLR model.
mlr_model = lm(Global_Sales ~ NA_Sales + EU_Sales, data = turtle_sales_subset4)

# View the MLR model.
mlr_model

# View the summary of the MLR model.
summary(mlr_model)


# 4. Test the MLR model and create a scatterplot.
# Set a random seed for reproducibility.
set.seed(123)

# Split the data into 80% training and 20% testing.
index <- createDataPartition(turtle_sales_subset4$Global_Sales, p = 0.8, 
                             list = FALSE)
mlr_train_data <- turtle_sales_subset4[index, ]
mlr_test_data <- turtle_sales_subset4[-index, ]

# Create a MLR model using the training data.
mlr_train_model <- lm(Global_Sales ~ NA_Sales + EU_Sales, data = mlr_train_data)

# Use the trained MLR model to make predictions on the test data.
mlr_predictions <- predict(mlr_train_model, newdata = mlr_test_data)

# Calculate the evaluation metrics (RMSE, MAE, R-squared) using the test data.
rmse <- sqrt(mean((mlr_predictions - mlr_test_data$Global_Sales)^2))
mae <- mean(abs(mlr_predictions - mlr_test_data$Global_Sales))
ss_residual <- sum((mlr_test_data$Global_Sales - mlr_predictions)^2)
ss_total <- sum((mlr_test_data$Global_Sales - 
                   mean(mlr_test_data$Global_Sales))^2)
r_squared <- 1 - (ss_residual / ss_total)

# View the RMSE, MAE and R-squared.
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("R-squared:", r_squared, "\n")

# Create a data frame with the actual Global_Sales and predicted values.
mlr_data <- data.frame(Actual = mlr_test_data$Global_Sales,
                       Predicted = mlr_predictions)

# Create a scatterplot to visualise the predicted vs actual global sales.
mlr_plot <- ggplot(data = mlr_data, aes(x = Actual, 
                                        y = Predicted)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Predicted vs Actual Global Sales",
       x = "Actual Global Sales", y = "Predicted Global Sales")

# Make the mlr_plot interactive.
mlr_plot <- ggplotly(mlr_plot)

# Display the interactive mlr_plot.
mlr_plot


# 5. Predict global sales based on provided values.
# Create a data frame with the provided values.
new_df <- data.frame(NA_Sales = c(34.02, 3.93, 2.73, 2.26, 22.08),
                     EU_Sales = c(23.80, 1.56, 0.65, 0.97, 0.52))

# Predict global sales using the trained MLR model.
predicted_global_sales <- predict(mlr_train_model, newdata = new_df)

# Combine the predicted values with the provided values.
prediction_df <- data.frame(NA_Sales = new_df$NA_Sales,
                            EU_Sales = new_df$EU_Sales,
                            Predicted_Global_Sales = predicted_global_sales)

# View the prediction data frame.
prediction_df


# 6. Find the percentage of NA_Sales and EU_Sales from the,
# Predicted_Global_Sales to see whether it matches the percentage calculations,
# done previously for the actual data.
NA_Sales_total2 <- sum(prediction_df$NA_Sales)
EU_Sales_total2 <- sum(prediction_df$EU_Sales)
Predicted_Global_Sales_total <- sum(prediction_df$Predicted_Global_Sales)

# Calculate the percentage of NA_Sales from Predicted_Global_Sales.
NA_Sales_percentage2 <- (NA_Sales_total2 / Predicted_Global_Sales_total) * 100

# Calculate the percentage of EU_Sales from Predicted_Global_Sales.
EU_Sales_percentage2 <- (EU_Sales_total2 / Predicted_Global_Sales_total) * 100

# View the percentages.
NA_Sales_percentage2
EU_Sales_percentage2


# 7. Create a scatterplot of NA_Sales vs EU_Sales with Predicted_Global_Sales,
# as the hue.
prediction_plot <- ggplot(prediction_df, aes(x = NA_Sales, y = EU_Sales)) + 
  geom_point(aes(color = Predicted_Global_Sales)) +
  geom_smooth(method = "lm", se = FALSE, color = "purple", 
              show.legend = FALSE) +
  labs(title = 
         "Scatterplot of North American vs European vs Predicted Global Sales",
       x = "NA Sales (in millions of £)",
       y = "EU Sales (in millions of £)") + 
  scale_color_continuous(low = "yellow", high = "black", 
                         name = "Predicted Global Sales 
(in millions of £)")

# Make the prediction_plot interactive.
prediction_plot <- ggplotly(prediction_plot, 
                            tooltip = c("NA_Sales", "EU_Sales", 
                                        "Predicted_Global_Sales"))

# View the interactive prediction_plot.
prediction_plot