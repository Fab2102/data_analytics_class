# Task 1

# Read in the CSV file:
data <- read.csv("airbnb.csv", sep=",", header = TRUE)

# Looking at the data:
head(data)
str(data)

# There is no missing data:
sum(is.na(data))

# Turning the categorical variable 'room_type' into a factor:
data$room_type <- factor(data$room_type)

# Eliminating the index X:
data <- subset(data, select = -c(1))

# Eliminating 'attr_index', 'rest_index', 'room_shared', and 'room_private':
data <- subset(data, select = -c(room_shared, room_private, attr_index, rest_index))

# Task 2

# Looking at the summary:
summary(data)

# Looking at the structure:
str(data)

# Looking at the correlation (only numeric values):
numeric_columns <- sapply(data, is.numeric)
numeric_data <- data[, numeric_columns]
cor(data[, 1], numeric_data)

# Plotting data: (Logarithmic scale for Y axis)
plot(realSum ~ person_capacity, data = data, log = "y")
plot(realSum ~ bedrooms, data = data, log = "y")
plot(realSum ~ dist, data = data, log = "y")
plot(realSum ~ metro_dist, data = data, log = "y")
plot(realSum ~ attr_index_norm, data = data, log = "y")
plot(realSum ~ rest_index_norm, data = data, log = "y")

# Task 3

# Fitting a linear regression model using all variables:
fitAll <- lm(realSum ~ ., data = data)
summary(fitAll)

# Task 4

# Performing a stepwise approach:
fitStepwise = step(fitAll, direction = "backward")
summary(fitStepwise)

# Task 5

# AIC comparison between models:
AIC(fitAll)
AIC(fitStepwise)

# Task 6

# Splitting the sample into 80% train vs 20% test sample:
total_rows <- nrow(data)
eighty_percent <- floor(0.8 * total_rows)
set.seed(123)

for_training <- sample(1:total_rows, eighty_percent, replace = FALSE)
train_data <- data[for_training,]
test_data <- data[-for_training,]

# Fitting the full model to the training data:
model_full <- lm(realSum ~ ., data = train_data)
summary(model_full)

# Creating a stepwise regression model on the training data:
model_step <- step(model_full, direction = "backward")
summary(model_step)

# Making predictions on the test data:
pred_test_model_full = predict(model_full, newdata = test_data)
pred_test_model_step = predict(model_step, newdata = test_data)

pred_train_model_full <- predict(model_full, newdata = train_data)
pred_train_model_step <- predict(model_step, newdata = train_data)

# Calculate the training and test MSE:
mse_train_model_full <- mean((train_data$realSum - pred_train_model_full)^2)
mse_train_model_step <- mean((train_data$realSum - pred_train_model_step)^2)

mse_test_model_full <- mean((test_data$realSum - pred_test_model_full)^2)
mse_test_model_step <- mean((test_data$realSum - pred_test_model_step)^2)

# Displaying the training MSE:
mse_train_model_full
mse_train_model_step

# Displaying the test MSE:
mse_test_model_full
mse_test_model_step
