library("rpart")
library("party")
library("partykit")
library("ranger")

# Task 1

# Reading in the CSV file and looking at the head of the dataset
housing <- read.csv("housing.csv", stringsAsFactors = TRUE) 
head(housing)
summary(housing)

# Handle missing values
print("Missing values per column:")
missing_values <- colSums(is.na(housing))
print(missing_values)

cleaned_housing <- na.omit(housing)

# Visualizing the distribution of features before and after cleaning missing values (histograms)
par(mfrow = c(2, 3))
for (col in colnames(housing)) 
{
  if (is.numeric(housing[[col]])) 
  {
    hist(housing[[col]], main = paste("Before -", col), col = "beige", xlab = col)
  } 
}

for (col in colnames(cleaned_housing)) 
{
  if (is.numeric(cleaned_housing[[col]])) 
  {
    hist(cleaned_housing[[col]], main = paste("After -", col), col = "orange", xlab = col)
  } 
}

# Visualizing the distribution of features before and after cleaning missing values (boxplots)
par(mfrow = c(2, 3))
for (col in colnames(housing)) 
{
  if (is.numeric(housing[[col]])) {
    boxplot(housing[[col]], main = paste("Before -", col), col = "beige", ylab = col)
  }
}

for (col in colnames(cleaned_housing)) 
{
  if (is.numeric(cleaned_housing[[col]])) {
    boxplot(cleaned_housing[[col]], main = paste("After -", col), col = "orange", ylab = col)
  }
}

# Task 2

# Fit a regression tree to the data
tree <- rpart(median_house_value ~ ., data = housing)

# Plot the tree
plot(as.party(tree))

# Task 3

# Fit a random forest model
random_forest <- ranger(median_house_value ~ ., data = cleaned_housing, probability = FALSE, importance = "permutation")

# Print the random forest model
print(random_forest)

# Plotting the permutation importance
plot(as.table(importance(random_forest)), ylab = "Importance")

# Partial dependency plot
grd <- seq(min(cleaned_housing$median_income), max(cleaned_housing$median_income), length.out = 20)
nd <- cleaned_housing[sample.int(nrow(cleaned_housing), 300), ]

prs <- lapply(grd, \(val) {
  nd$median_income <- val
  predict(random_forest, data = nd)$predictions
})

matplot(grd, t(do.call("cbind", prs)), type = "l", col = rgb(.1, .1, .1, .1),
        lty = 1, log = "x")

# Task 4

# Cross-validation setup
n <- nrow(cleaned_housing)
fold <- 10
folds <- sample(rep(1:fold, ceiling(n/fold)), n)
head(folds, n = 10)

# Cross-validation loop for MSE calculation
mse_scores <- list()

for (tfold in seq_len(fold)) {
  train_idx <- which(folds != tfold)
  test_idx <- which(folds == tfold)
  
  rf <- ranger(median_house_value ~ ., data = cleaned_housing[train_idx, ], probability = FALSE)
  tree <- rpart(median_house_value ~ ., data = cleaned_housing[train_idx, ])
  lrm <- glm(median_house_value ~ ., data = cleaned_housing[train_idx, ], family = "gaussian")
  
  p_rf <- predict(rf, data = cleaned_housing[test_idx, ])$predictions
  p_tr <- predict(tree, newdata = cleaned_housing[test_idx, ])
  p_lr <- predict(lrm, newdata = cleaned_housing[test_idx, ], type = "response")
  
  actual_values <- cleaned_housing[test_idx, "median_house_value"]
  mse_scores[[tfold]] <- unlist(lapply(
    list(rf = p_rf, tree = p_tr, lrm = p_lr), \(predicted) {
      mean((actual_values - predicted)^2)
    }
  ))
}

# Boxplot of MSE for model comparison
boxplot(do.call("rbind", mse_scores), ylab = "Cross-validated MSE")