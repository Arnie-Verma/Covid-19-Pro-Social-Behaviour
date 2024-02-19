library(tidyverse)
library(dplyr)
library(ggplot2)
library(fastcluster)

setwd("~/arnie stuff/")
rm(list = ls())
set.seed(32505485) # XXXXXXXX = your student ID
cvbase = read.csv("PsyCoronaBaselineExtract.csv")
cvbase <- cvbase[sample(nrow(cvbase), 40000), ] # 40000 rows

#Replace employment NA with 0
cvbase$employstatus_1[is.na(cvbase$employstatus_1)] <- 0
cvbase$employstatus_2[is.na(cvbase$employstatus_2)] <- 0
cvbase$employstatus_3[is.na(cvbase$employstatus_3)] <- 0
cvbase$employstatus_4[is.na(cvbase$employstatus_4)] <- 0
cvbase$employstatus_5[is.na(cvbase$employstatus_5)] <- 0
cvbase$employstatus_6[is.na(cvbase$employstatus_6)] <- 0
cvbase$employstatus_7[is.na(cvbase$employstatus_7)] <- 0
cvbase$employstatus_8[is.na(cvbase$employstatus_8)] <- 0
cvbase$employstatus_9[is.na(cvbase$employstatus_9)] <- 0
cvbase$employstatus_10[is.na(cvbase$employstatus_10)] <- 0

# Calculate means of columns fro mean imputation
numeric_cols <- sapply(cvbase[ , 1:53], is.numeric)
col_means <- sapply(cvbase[ , 1:53][ , numeric_cols], mean, na.rm = TRUE)


col_means_rounded <- round(col_means)


#Replace NA with column means
for (i in seq_along(cvbase)) {
  if (i <= 53 && numeric_cols[i]) {
    cvbase[is.na(cvbase[[i]]), i] <- col_means_rounded[i]
  }
}

# Select relevant columns 
relevant_cols <- cvbase[ , 21:30] #change "xx:yy" to appropriate columns

# Create box plot of relevant columns, remember to change title name to appropriate columns
boxplot(relevant_cols, main = "Box plot of the Employment Status concept", col = "lightblue", 
        xlab = "Variable Name", ylab = "Response")



#Filter data for Russia and Other Countries
russian_data <- dplyr::filter(cvbase, coded_country == "Russia")
other_countries_data <- dplyr::filter(cvbase, coded_country != "Russia")


#Ensure columns are numeric, so avoid coded_country 
numeric_columns <- sapply(cvbase[, 1:53], is.numeric)

#Calculate stats such as mean, sd, median
russian_summary <- russian_data[, numeric_columns] %>% summarise_all(list(mean, sd, median))
other_countries_summary <- other_countries_data[, numeric_columns] %>% summarise_all(list(mean, sd, median))

#Reshape Data to format print
russian_summary_long <- russian_summary %>%
  pivot_longer(cols = everything(),
               names_to = "Predictor",
               values_to = "Value") %>%
  separate(Predictor, into = c("Predictor", "Statistic"), sep = "_")

other_countries_summary_long <- other_countries_summary %>%
  pivot_longer(cols = everything(),
               names_to = "Predictor",
               values_to = "Value") %>%
  separate(Predictor, into = c("Predictor", "Statistic"), sep = "_")

#Combine groups and rename columns to make it more understandable
russian_summary_long <- russian_summary_long %>%
  mutate(Statistic = recode(Statistic, fn1 = "mean", fn2 = "sd", fn3 = "median"))

other_countries_summary_long <- other_countries_summary_long %>%
  mutate(Statistic = recode(Statistic, fn1 = "mean", fn2 = "sd", fn3 = "median"))

russian_summary_long$Group <- "Russian"
other_countries_summary_long$Group <- "Other Countries"

combined_summary <- bind_rows(russian_summary_long, other_countries_summary_long) %>%
  filter(Statistic == "mean")

#Print output 
print(combined_summary)

#Display mean comparison in graph format
ggplot(combined_summary, aes(x = interaction(Predictor, Group, sep = " - "), y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.1)) + # Change width argument to your preference
  geom_text(aes(label = round(Value, 2)), position = position_dodge(width = 0.8), vjust = -0.5) + # Match width argument with the one in geom_bar
  labs(title = "Mean Comparison: Russia vs Other Countries",
       x = "Predictor Variables - Group",
       y = "Mean Value",
       fill = "Group") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))



# Get predictor columns
predictor_columns <- colnames(russian_data)[1:50]
# Identify columns with only one unique value (coded_country)
single_value_columns <- sapply(russian_data[predictor_columns], function(x) length(unique(x)) == 1)
# Filter out columns with only one unique value (everything but coded_country)
filtered_predictor_columns <- predictor_columns[!single_value_columns]
# Create the regression formula change this for other dependent variables
filtered_predictor_formula <- paste("c19ProSo01 ~", paste(filtered_predictor_columns, collapse = " + "))
filtered_regression_formula <- as.formula(filtered_predictor_formula)
# Fit the linear regression model
russian_regression <- lm(filtered_regression_formula, data = russian_data)
# Print the regression summary
russian_regression_summary <- summary(russian_regression)
print(russian_regression_summary)
# Create a dataframe with the coefficients, their standard errors, and predictor names
coef_df <- data.frame(
  Estimate = coef(russian_regression),
  Std.Error = sqrt(diag(vcov(russian_regression))),
  Predictor = names(coef(russian_regression)))
# Remove the intercept from the dataframe
coef_df <- coef_df[-1,]
# Create the coefficient plot and change this to change the title
ggplot(coef_df, aes(x = Predictor, y = Estimate, ymin = Estimate - 1.96 * Std.Error, ymax = Estimate + 1.96 * Std.Error)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Coefficient Plot for Russian Regression with c19ProSo01",
       x = "Predictor Variables",
       y = "Estimated Coefficients") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


#######################################################################################
# Get predictor columns
predictor_columns <- colnames(russian_data)[1:50]

# Identify columns with only one unique value (coded_country)
single_value_columns <- sapply(russian_data[predictor_columns], function(x) length(unique(x)) == 1)

# Filter out columns with only one unique value (everything but coded_country)
filtered_predictor_columns <- predictor_columns[!single_value_columns]

# Create the regression formula
filtered_predictor_formula <- paste("c19ProSo02 ~", paste(filtered_predictor_columns, collapse = " + "))
filtered_regression_formula <- as.formula(filtered_predictor_formula)

# Fit the linear regression model
russian_regression <- lm(filtered_regression_formula, data = russian_data)

# Print the regression summary
russian_regression_summary <- summary(russian_regression)
print(russian_regression_summary)



# Create a dataframe with the coefficients, their standard errors, and predictor names
coef_df <- data.frame(
  Estimate = coef(russian_regression),
  Std.Error = sqrt(diag(vcov(russian_regression))),
  Predictor = names(coef(russian_regression))
)

# Remove the intercept from the dataframe
coef_df <- coef_df[-1,]

# Create the coefficient plot
ggplot(coef_df, aes(x = Predictor, y = Estimate, ymin = Estimate - 1.96 * Std.Error, ymax = Estimate + 1.96 * Std.Error)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "Coefficient Plot for Russian Regression with c19ProSo02",
       x = "Predictor Variables",
       y = "Estimated Coefficients") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#######################################################################################
# Get predictor columns
predictor_columns <- colnames(russian_data)[1:50]

# Identify columns with only one unique value (coded_country)
single_value_columns <- sapply(russian_data[predictor_columns], function(x) length(unique(x)) == 1)

# Filter out columns with only one unique value (everything but coded_country)
filtered_predictor_columns <- predictor_columns[!single_value_columns]

# Create the regression formula
filtered_predictor_formula <- paste("c19ProSo03 ~", paste(filtered_predictor_columns, collapse = " + "))
filtered_regression_formula <- as.formula(filtered_predictor_formula)

# Fit the linear regression model
russian_regression <- lm(filtered_regression_formula, data = russian_data)

# Print the regression summary
russian_regression_summary <- summary(russian_regression)
print(russian_regression_summary)



# Create a dataframe with the coefficients, their standard errors, and predictor names
coef_df <- data.frame(
  Estimate = coef(russian_regression),
  Std.Error = sqrt(diag(vcov(russian_regression))),
  Predictor = names(coef(russian_regression))
)

# Remove the intercept from the dataframe
coef_df <- coef_df[-1,]

# Create the coefficient plot
ggplot(coef_df, aes(x = Predictor, y = Estimate, ymin = Estimate - 1.96 * Std.Error, ymax = Estimate + 1.96 * Std.Error)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "Coefficient Plot for Russian Regression with c19ProSo03",
       x = "Predictor Variables",
       y = "Estimated Coefficients") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
#######################################################################################
# Get predictor columns
predictor_columns <- colnames(russian_data)[1:50]

# Identify columns with only one unique value (coded_country)
single_value_columns <- sapply(russian_data[predictor_columns], function(x) length(unique(x)) == 1)

# Filter out columns with only one unique value (everything but coded_country)
filtered_predictor_columns <- predictor_columns[!single_value_columns]

# Create the regression formula
filtered_predictor_formula <- paste("c19ProSo04 ~", paste(filtered_predictor_columns, collapse = " + "))
filtered_regression_formula <- as.formula(filtered_predictor_formula)

# Fit the linear regression model
russian_regression <- lm(filtered_regression_formula, data = russian_data)

# Print the regression summary
russian_regression_summary <- summary(russian_regression)
print(russian_regression_summary)



# Create a dataframe with the coefficients, their standard errors, and predictor names
coef_df <- data.frame(
  Estimate = coef(russian_regression),
  Std.Error = sqrt(diag(vcov(russian_regression))),
  Predictor = names(coef(russian_regression))
)

# Remove the intercept from the dataframe
coef_df <- coef_df[-1,]

# Create the coefficient plot
ggplot(coef_df, aes(x = Predictor, y = Estimate, ymin = Estimate - 1.96 * Std.Error, ymax = Estimate + 1.96 * Std.Error)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "Coefficient Plot for Russian Regression with c19ProSo04",
       x = "Predictor Variables",
       y = "Estimated Coefficients") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#######################################################################################
#######################################################################################

predictor_columns <- colnames(other_countries_data)[1:50]
predictor_columns <- predictor_columns[predictor_columns != "coded_country"]
single_value_columns <- sapply(other_countries_data[predictor_columns], function(x) length(unique(x)) == 1)
filtered_predictor_columns <- predictor_columns[!single_value_columns]
filtered_predictor_formula <- paste("c19ProSo01 ~", paste(filtered_predictor_columns, collapse = " + "))
filtered_regression_formula <- as.formula(filtered_predictor_formula)
other_countries_regression <- lm(filtered_regression_formula, data = other_countries_data)
other_countries_regression_summary <- summary(other_countries_regression)
print(other_countries_regression_summary)
coef_df <- data.frame(
  Estimate = coef(other_countries_regression),
  Std.Error = sqrt(diag(vcov(other_countries_regression))),
  Predictor = names(coef(other_countries_regression)))
coef_df <- coef_df[-1,]
ggplot(coef_df, aes(x = Predictor, y = Estimate, ymin = Estimate - 1.96 * Std.Error, ymax = Estimate + 1.96 * Std.Error)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Coefficient Plot for Other Countries Regression with c19ProSo01",
       x = "Predictor Variables",
       y = "Estimated Coefficients") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))



#######################################################################################
# Get predictor columns
predictor_columns <- colnames(other_countries_data)[1:50]

# Remove the 'coded_country' variable if present in predictor_columns 
predictor_columns <- predictor_columns[predictor_columns != "coded_country"]

# Identify columns with only one unique value (coded_country)
single_value_columns <- sapply(other_countries_data[predictor_columns], function(x) length(unique(x)) == 1)

# Filter out columns with only one unique value (everything but coded_country)
filtered_predictor_columns <- predictor_columns[!single_value_columns]

# Create the regression formula
filtered_predictor_formula <- paste("c19ProSo02 ~", paste(filtered_predictor_columns, collapse = " + "))
filtered_regression_formula <- as.formula(filtered_predictor_formula)

# Fit the linear regression model
other_countries_regression <- lm(filtered_regression_formula, data = other_countries_data)

# Print the regression summary
other_countries_regression_summary <- summary(other_countries_regression)
print(other_countries_regression_summary)

# Create a dataframe with the coefficients, their standard errors, and predictor names
coef_df <- data.frame(
  Estimate = coef(other_countries_regression),
  Std.Error = sqrt(diag(vcov(other_countries_regression))),
  Predictor = names(coef(other_countries_regression))
)

# Remove the intercept from the dataframe
coef_df <- coef_df[-1,]

# Create the coefficient plot
ggplot(coef_df, aes(x = Predictor, y = Estimate, ymin = Estimate - 1.96 * Std.Error, ymax = Estimate + 1.96 * Std.Error)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "Coefficient Plot for Other Countries Regression with c19ProSo02",
       x = "Predictor Variables",
       y = "Estimated Coefficients") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


#######################################################################################
# Get predictor columns
predictor_columns <- colnames(other_countries_data)[1:50]

# Remove the 'coded_country' variable if present in predictor_columns 
predictor_columns <- predictor_columns[predictor_columns != "coded_country"]

# Identify columns with only one unique value (coded_country)
single_value_columns <- sapply(other_countries_data[predictor_columns], function(x) length(unique(x)) == 1)

# Filter out columns with only one unique value (everything but coded_country)
filtered_predictor_columns <- predictor_columns[!single_value_columns]

# Create the regression formula
filtered_predictor_formula <- paste("c19ProSo03 ~", paste(filtered_predictor_columns, collapse = " + "))
filtered_regression_formula <- as.formula(filtered_predictor_formula)

# Fit the linear regression model
other_countries_regression <- lm(filtered_regression_formula, data = other_countries_data)

# Print the regression summary
other_countries_regression_summary <- summary(other_countries_regression)
print(other_countries_regression_summary)

# Create a dataframe with the coefficients, their standard errors, and predictor names
coef_df <- data.frame(
  Estimate = coef(other_countries_regression),
  Std.Error = sqrt(diag(vcov(other_countries_regression))),
  Predictor = names(coef(other_countries_regression))
)

# Remove the intercept from the dataframe
coef_df <- coef_df[-1,]

# Create the coefficient plot
ggplot(coef_df, aes(x = Predictor, y = Estimate, ymin = Estimate - 1.96 * Std.Error, ymax = Estimate + 1.96 * Std.Error)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "Coefficient Plot for Other Countries Regression with c19ProSo03",
       x = "Predictor Variables",
       y = "Estimated Coefficients") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

#######################################################################################
# Get predictor columns
predictor_columns <- colnames(other_countries_data)[1:50]

# Remove the 'coded_country' variable if present in predictor_columns 
predictor_columns <- predictor_columns[predictor_columns != "coded_country"]

# Identify columns with only one unique value (coded_country)
single_value_columns <- sapply(other_countries_data[predictor_columns], function(x) length(unique(x)) == 1)

# Filter out columns with only one unique value (everything but coded_country)
filtered_predictor_columns <- predictor_columns[!single_value_columns]

# Create the regression formula
filtered_predictor_formula <- paste("c19ProSo04 ~", paste(filtered_predictor_columns, collapse = " + "))
filtered_regression_formula <- as.formula(filtered_predictor_formula)

# Fit the linear regression model
other_countries_regression <- lm(filtered_regression_formula, data = other_countries_data)

# Print the regression summary
other_countries_regression_summary <- summary(other_countries_regression)
print(other_countries_regression_summary)

# Create a dataframe with the coefficients, their standard errors, and predictor names
coef_df <- data.frame(
  Estimate = coef(other_countries_regression),
  Std.Error = sqrt(diag(vcov(other_countries_regression))),
  Predictor = names(coef(other_countries_regression))
)

# Remove the intercept from the dataframe
coef_df <- coef_df[-1,]

# Create the coefficient plot
ggplot(coef_df, aes(x = Predictor, y = Estimate, ymin = Estimate - 1.96 * Std.Error, ymax = Estimate + 1.96 * Std.Error)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "Coefficient Plot for Other Countries Regression with c19ProSo04",
       x = "Predictor Variables",
       y = "Estimated Coefficients") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))




#####################################################################################
#####################################################################################



set.seed(32505485)
ncvbase = cvbase
selected_columns <- c(1, 15, 19, 23, 31, 40, 46) #select specific columns
# Aggregate data by country
agg_data <- ncvbase %>%
  group_by(coded_country) %>%
  summarise(across(selected_columns, mean, na.rm = TRUE))
# Perform hierarchical clustering on aggregated data
ihfit <- hclust(dist(agg_data[,-1]), "ave") # Exclude the coded_country column in clustering
# Set country names as labels for the dendrogram
rownames(agg_data) <- agg_data$coded_country
ihfit$labels <- rownames(agg_data)
# Plot the dendrogram
plot(ihfit, hang = -1, main = "Cluster Dendrogram for Similar Countries to Russia", xlab = "Countries", ylab = "Distance")
# Cut the dendrogram into 30 clusters
cutihfit <- cutree(ihfit, k = 30)
# Add rectangles 
rect.hclust(ihfit, k = 30, border = "red")
# Create a table comparing the actual country codes with the cluster assignments
table(actual = agg_data$coded_country, fitted = cutihfit)


#################################################################################

# Filter the dataset to include only the specified countries
clustered_countries <- c("Japan", "Iceland", "Hong Kong S.A.R.", "Poland", "Pakistan", "Ukraine")
filtered_data <- cvbase[cvbase$coded_country %in% clustered_countries,]
# Get predictor columns
predictor_columns <- colnames(filtered_data)[1:50]
# Remove the 'coded_country' variable if present in predictor_columns
predictor_columns <- predictor_columns[predictor_columns != "coded_country"]
# Identify columns with only one unique value (coded_country)
single_value_columns <- sapply(filtered_data[predictor_columns], function(x) length(unique(x)) == 1)
# Filter out columns with only one unique value (everything but coded_country)
filtered_predictor_columns <- predictor_columns[!single_value_columns]
# Create the regression formula, change this for dependent variable 
filtered_predictor_formula <- paste("c19ProSo01 ~", paste(filtered_predictor_columns, collapse = " + "))
filtered_regression_formula <- as.formula(filtered_predictor_formula)
# Fit the linear regression model
filtered_countries_regression <- lm(filtered_regression_formula, data = filtered_data)
# Print the regression summary
filtered_countries_regression_summary <- summary(filtered_countries_regression)
print(filtered_countries_regression_summary)
# Create a dataframe with the coefficients, their standard errors, and predictor names
coef_df <- data.frame(
  Estimate = coef(filtered_countries_regression),
  Std.Error = sqrt(diag(vcov(filtered_countries_regression))),
  Predictor = names(coef(filtered_countries_regression))
)
# Remove the intercept from the dataframe
coef_df <- coef_df[-1,]
# Create the coefficient plot change this for title 
ggplot(coef_df, aes(x = Predictor, y = Estimate, ymin = Estimate - 1.96 * Std.Error, ymax = Estimate + 1.96 * Std.Error)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Coefficient Plot for Clustered Countries with c19ProSo01",
       x = "Predictor Variables",
       y = "Estimated Coefficients") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))



###################################################################


# Filter the dataset to include only the specified countries
clustered_countries <- c("Japan", "Iceland", "Hong Kong S.A.R.", "Poland", "Pakistan", "Ukraine")
filtered_data <- cvbase[cvbase$coded_country %in% clustered_countries,]

# Get predictor columns
predictor_columns <- colnames(filtered_data)[1:50]

# Remove the 'coded_country' variable if present in predictor_columns
predictor_columns <- predictor_columns[predictor_columns != "coded_country"]

# Identify columns with only one unique value (coded_country)
single_value_columns <- sapply(filtered_data[predictor_columns], function(x) length(unique(x)) == 1)

# Filter out columns with only one unique value (everything but coded_country)
filtered_predictor_columns <- predictor_columns[!single_value_columns]

# Create the regression formula
filtered_predictor_formula <- paste("c19ProSo02 ~", paste(filtered_predictor_columns, collapse = " + "))
filtered_regression_formula <- as.formula(filtered_predictor_formula)

# Fit the linear regression model
filtered_countries_regression <- lm(filtered_regression_formula, data = filtered_data)

# Print the regression summary
filtered_countries_regression_summary <- summary(filtered_countries_regression)
print(filtered_countries_regression_summary)

# Create a dataframe with the coefficients, their standard errors, and predictor names
coef_df <- data.frame(
  Estimate = coef(filtered_countries_regression),
  Std.Error = sqrt(diag(vcov(filtered_countries_regression))),
  Predictor = names(coef(filtered_countries_regression))
)

# Remove the intercept from the dataframe
coef_df <- coef_df[-1,]

# Create the coefficient plot
ggplot(coef_df, aes(x = Predictor, y = Estimate, ymin = Estimate - 1.96 * Std.Error, ymax = Estimate + 1.96 * Std.Error)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "Coefficient Plot for Clustered Countries Regression with c19ProSo02",
       x = "Predictor Variables",
       y = "Estimated Coefficients") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

###########################################################################

# Filter the dataset to include only the specified countries
clustered_countries <- c("Japan", "Iceland", "Hong Kong S.A.R.", "Poland", "Pakistan", "Ukraine")
filtered_data <- cvbase[cvbase$coded_country %in% clustered_countries,]

# Get predictor columns
predictor_columns <- colnames(filtered_data)[1:50]

# Remove the 'coded_country' variable if present in predictor_columns
predictor_columns <- predictor_columns[predictor_columns != "coded_country"]

# Identify columns with only one unique value (coded_country)
single_value_columns <- sapply(filtered_data[predictor_columns], function(x) length(unique(x)) == 1)

# Filter out columns with only one unique value (everything but coded_country)
filtered_predictor_columns <- predictor_columns[!single_value_columns]

# Create the regression formula
filtered_predictor_formula <- paste("c19ProSo03 ~", paste(filtered_predictor_columns, collapse = " + "))
filtered_regression_formula <- as.formula(filtered_predictor_formula)

# Fit the linear regression model
filtered_countries_regression <- lm(filtered_regression_formula, data = filtered_data)

# Print the regression summary
filtered_countries_regression_summary <- summary(filtered_countries_regression)
print(filtered_countries_regression_summary)

# Create a dataframe with the coefficients, their standard errors, and predictor names
coef_df <- data.frame(
  Estimate = coef(filtered_countries_regression),
  Std.Error = sqrt(diag(vcov(filtered_countries_regression))),
  Predictor = names(coef(filtered_countries_regression))
)

# Remove the intercept from the dataframe
coef_df <- coef_df[-1,]

# Create the coefficient plot
ggplot(coef_df, aes(x = Predictor, y = Estimate, ymin = Estimate - 1.96 * Std.Error, ymax = Estimate + 1.96 * Std.Error)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "Coefficient Plot for Clustered Countries Regression with c19ProSo03",
       x = "Predictor Variables",
       y = "Estimated Coefficients") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
###########################################################################

# Filter the dataset to include only the specified countries
clustered_countries <- c("Japan", "Iceland", "Hong Kong S.A.R.", "Poland", "Pakistan", "Ukraine")
filtered_data <- cvbase[cvbase$coded_country %in% clustered_countries,]

# Get predictor columns
predictor_columns <- colnames(filtered_data)[1:50]

# Remove the 'coded_country' variable if present in predictor_columns
predictor_columns <- predictor_columns[predictor_columns != "coded_country"]

# Identify columns with only one unique value (coded_country)
single_value_columns <- sapply(filtered_data[predictor_columns], function(x) length(unique(x)) == 1)

# Filter out columns with only one unique value (everything but coded_country)
filtered_predictor_columns <- predictor_columns[!single_value_columns]

# Create the regression formula
filtered_predictor_formula <- paste("c19ProSo04 ~", paste(filtered_predictor_columns, collapse = " + "))
filtered_regression_formula <- as.formula(filtered_predictor_formula)

# Fit the linear regression model
filtered_countries_regression <- lm(filtered_regression_formula, data = filtered_data)

# Print the regression summary
filtered_countries_regression_summary <- summary(filtered_countries_regression)
print(filtered_countries_regression_summary)

# Create a dataframe with the coefficients, their standard errors, and predictor names
coef_df <- data.frame(
  Estimate = coef(filtered_countries_regression),
  Std.Error = sqrt(diag(vcov(filtered_countries_regression))),
  Predictor = names(coef(filtered_countries_regression))
)

# Remove the intercept from the dataframe
coef_df <- coef_df[-1,]

# Create the coefficient plot
ggplot(coef_df, aes(x = Predictor, y = Estimate, ymin = Estimate - 1.96 * Std.Error, ymax = Estimate + 1.96 * Std.Error)) +
  geom_pointrange() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  labs(title = "Coefficient Plot for Clustered Countries Regression with c19ProSo04",
       x = "Predictor Variables",
       y = "Estimated Coefficients") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))





