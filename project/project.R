# Libraries
library(countrycode)
library(ggplot2)
library(rpart)
library(partykit)
library(ranger)
library(caret)
library(e1071)

# Data pre-processing
data_jobs <- read.csv("jobsInData.csv", stringsAsFactors = TRUE)
head(data_jobs)
str(data_jobs)
print(colSums(is.na(data_jobs)))

# Omitting unnecessary variables
data_jobs <- subset(data_jobs, select = -c(salary_currency, salary))
data_jobs <- subset(data_jobs, select = -c(job_title))

# Converting countries to continents
data_jobs$employee_continent <- countrycode(data_jobs$employee_residence, 
                                            origin = "country.name", 
                                            destination = "continent")
data_jobs$company_continent <- countrycode(data_jobs$company_location, 
                                           origin = "country.name", 
                                           destination = "continent")

# Remove country-level variables
data_jobs <- subset(data_jobs, select = -c(employee_residence, company_location))

str(data_jobs)

# Regression - Linear Regression
data_jobs$salary_in_usd <- log(data_jobs$salary_in_usd)
fitAll <- lm(salary_in_usd ~ ., data = data_jobs)
summary(fitAll)

fitStepwise = step(fitAll, direction = "backward")
summary(fitStepwise)

# Regression - Tree
full_tree <- rpart(salary_in_usd ~ ., data = data_jobs, control = list(cp = 0))
plot(as.party(full_tree))

printcp(full_tree)
rpart::plotcp(full_tree, xlim = c(0, 50))

pruned_tree <- prune(full_tree, cp = full_tree$cptable[7, "CP"])
plot(as.party(pruned_tree))

# Random Forest
rf <- ranger(salary_in_usd ~ ., data = data_jobs, probability = FALSE, importance = "permutation")
print(rf)

importance_df <- data.frame(Variable = names(importance(rf)), Importance = importance(rf))

ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Importance") +
  xlab("Variables") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

# Cross-validation and comparison
n <- nrow(data_jobs)
fold <- 10
set.seed(123)

folds <- sample(rep(1:fold, ceiling(n / fold)), n)
mse_results <- list()

for (tfold in seq_len(fold)) {
  train_idx <- which(folds != tfold)
  test_idx <- which(folds == tfold)
  
  rf_model <- ranger(salary_in_usd ~ ., data = data_jobs[train_idx, ])
  tree_model <- rpart(salary_in_usd ~ ., data = data_jobs[train_idx, ])
  prune(tree_model, cp = full_tree$cptable[7, "CP"])
  fitStepwise_model <- step(lm(salary_in_usd ~ ., data = data_jobs[train_idx, ]))
  
  p_rf <- predict(rf_model, data = data_jobs[test_idx, ])$predictions
  p_tr <- predict(tree_model, newdata = data_jobs[test_idx, ])
  p_lr <- predict(fitStepwise_model, newdata = data_jobs[test_idx, ])
  
  mse_results[[tfold]] <- unlist(lapply(
    list(rf = p_rf, tree = p_tr, lrm = p_lr), 
    function(predicted) {
      mean((data_jobs[test_idx, "salary_in_usd"] - predicted)^2)
    }
  ))
}

mse_matrix <- do.call("rbind", mse_results)

boxplot(mse_matrix, names = c("Random Forest", "Pruned Tree", "Linear Regression"),
        ylab = "Cross-validated MSE")

# Classification - Naive Bayes
data_jobs$salary_in_usd <- as.numeric(data_jobs$salary_in_usd)
median_salary <- median(data_jobs$salary_in_usd, na.rm = TRUE)
data_jobs$salary_in_usd <- as.factor(ifelse(data_jobs$salary_in_usd > median_salary, "High", "Low"))

set.seed(123)
trainIndex <- createDataPartition(data_jobs$salary_in_usd, p = 0.8, list = FALSE)
train_data <- data_jobs[trainIndex, ]
test_data <- data_jobs[-trainIndex, ]

model <- naiveBayes(salary_in_usd ~ experience_level + company_size + work_setting, data = train_data)

predictions <- predict(model, test_data)

conf_matrix <- confusionMatrix(predictions, test_data$salary_in_usd)
print(conf_matrix)

probabilities <- predict(model, test_data, type = "raw")
high_salary_probabilities <- probabilities[, "High"]

test_data$high_salary_probabilities <- high_salary_probabilities