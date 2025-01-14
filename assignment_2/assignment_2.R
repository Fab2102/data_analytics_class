library(caret)
library(e1071)

# Task 1

churn_data <- read.csv("BankChurners.csv", sep=",", header = TRUE)
head(churn_data)

churn_data <- subset(churn_data, select = -c(1,22, 23))
head(churn_data)

str(churn_data)

churn_data$Attrition_Flag <- factor(churn_data$Attrition_Flag, levels = c("Existing Customer", "Attrited Customer"))
churn_data$Income_Category <- factor(churn_data$Income_Category, levels = c("Less than $40K", "$40K - $60K", "$60K - $80K", "$80K - $120K", "$120K +", "Unknown"))
churn_data$Card_Category <- factor(churn_data$Card_Category, levels = c("Blue", "Silver", "Gold", "Platinum"))
churn_data$Education_Level <- factor(churn_data$Education_Level, levels = c("Uneducated", "High School", "College", "Graduate", "Post-Graduate", "Doctorate", "Unknown"))
churn_data$Marital_Status <- factor(churn_data$Marital_Status, levels = c("Single", "Married", "Divorced", "Unknown"))
churn_data$Gender <- factor(churn_data$Gender)

summary(churn_data)
churn_rate <- prop.table(table(churn_data$Attrition_Flag))["Attrited Customer"] * 100
print(paste("Churn Rate:", round(churn_rate, 2), "%"))

plot(Attrition_Flag ~ Customer_Age, data = churn_data)
plot(Attrition_Flag ~ Gender, data = churn_data)
plot(Attrition_Flag ~ Dependent_count, data = churn_data)
plot(Attrition_Flag ~ Education_Level, data = churn_data)
plot(Attrition_Flag ~ Marital_Status, data = churn_data)
plot(Attrition_Flag ~ Income_Category, data = churn_data)
plot(Attrition_Flag ~ Card_Category, data = churn_data)
plot(Attrition_Flag ~ Months_on_book, data = churn_data)
plot(Attrition_Flag ~ Total_Relationship_Count, data = churn_data)
plot(Attrition_Flag ~ Months_Inactive_12_mon, data = churn_data)
plot(Attrition_Flag ~ Contacts_Count_12_mon, data = churn_data)
plot(Credit_Limit ~ Attrition_Flag, data = churn_data)
plot(Total_Revolving_Bal ~ Attrition_Flag, data = churn_data)
plot(Avg_Open_To_Buy ~ Attrition_Flag, data = churn_data)
plot(Avg_Utilization_Ratio ~ Attrition_Flag, data = churn_data)
plot(Total_Amt_Chng_Q4_Q1 ~ Total_Ct_Chng_Q4_Q1, col = as.numeric(Attrition_Flag), data = churn_data)
plot(Total_Trans_Amt ~ Attrition_Flag, data = churn_data)
plot(Total_Trans_Ct ~ Attrition_Flag, data = churn_data)

# Task 2

set.seed(123)
n <- nrow(churn_data)
n1 <- floor(0.8 * n)
train_sample <- sample(1:n, n1)

train_data <- churn_data[train_sample, ]
test_data  <- churn_data[-train_sample, ]

# Task 3

logit_model <- glm(Attrition_Flag ~ ., data = train_data, family = binomial())
summary(logit_model)

# Task 4

stepwise_model <- step(logit_model, direction = "both")
summary(stepwise_model)

AIC(logit_model, stepwise_model)

# Task 5

knn_control <- trainControl(method = "cv", number = 5)

fit_knn <- train(Attrition_Flag ~ ., data=train_data, method="knn",
                 tuneGrid = data.frame(k=c(3, 5, 7, 9, 11, 13, 15)),
                 preProcess = "scale", trControl = knn_control)

print(fit_knn$bestTune[1, 1])

model_knn <- knn3(Attrition_Flag ~. , data=train_data,k=fit_knn$bestTune[1,1])  
print(model_knn)

# Task 6

model_naivbay <- naiveBayes(Attrition_Flag~., data=train_data)
print(model_naivbay)

# Task 7

pred_1 <- predict(logit_model, newdata=test_data, type="response")
outcome_pred_1 <- as.numeric(pred_1 > 0.5)

table_1 <- table(predicted=outcome_pred_1, observed=test_data$Attrition_Flag)
print(table_1)

accuracy_1 <- (table_1[1,1]+table_1[2,2])/(sum(table_1))
recall_1 <- table_1[2,2]/sum(table_1[,2])
precision_1 <- table_1[2,2]/sum(table_1[2,])
print(paste("Accuracy:", accuracy_1))
print(paste("Recall:", recall_1))
print(paste("Precision:", precision_1))

pred_2 <- predict(stepwise_model, newdata=test_data, type = "response")
outcome_pred_2 <- as.numeric(pred_2 > 0.5)

table_2 <-table(predicted=outcome_pred_2, observed=test_data$Attrition_Flag)
print(table_2)

accuracy_2 <- (table_2[1,1]+table_2[2,2])/(sum(table_2))
recall_2 <- table_2[2,2]/sum(table_2[,2])
precision_2 <- table_2[2,2]/sum(table_2[2,])

print(paste("Accuracy:", accuracy_2))
print(paste("Recall:", recall_2))
print(paste("Precision:", precision_2))

pred_3 <- predict(fit_knn, newdata = test_data)
conf_matr_knn <- confusionMatrix(pred_3, test_data$Attrition_Flag, positive = "Attrited Customer")
print(conf_matr_knn)

pred_4 <- predict(model_naivbay, newdata = test_data)
conf_matr_naiv_bay <- confusionMatrix(pred_4, test_data$Attrition_Flag, positive = "Attrited Customer")
print(conf_matr_naiv_bay)
