# Load necessary libraries
library(caret)
library(glmnet)
library(gbm)
library(xgboost)
library(dplyr)
library(lubridate)
library(pROC)

fight_data_reg <- fight_data %>% inner_join(df_records, by=c('date'='date', 'Winner'='fighter'))
fight_data_reg <- subset(fight_data_reg, select=-c(Loser,date))
fight_data_reg <- mutate(fight_data_reg, Winner=ifelse(fight_data_reg$Winner==fight_data_reg$R_fighter,1,0))
fight_data_reg <- subset(fight_data_reg, select=-c(R_fighter,B_fighter,last_round,last_round_time))
fight_data_reg <- fight_data_reg %>% select("Winner",everything())
names(fight_data_reg) <- gsub(" ","_",names(fight_data_reg))
names(fight_data_reg) <- gsub("-","",names(fight_data_reg))
names(fight_data_reg) <- gsub("/","",names(fight_data_reg))
names(fight_data_reg) <- gsub("'","",names(fight_data_reg))
names(fight_data_reg) <- gsub("\\(","",names(fight_data_reg))
names(fight_data_reg) <- gsub("\\)","",names(fight_data_reg))
names(fight_data_reg) <- gsub("\\+","",names(fight_data_reg))

# Ensure there are no missing values in the target variable 'Winner'
data <- na.omit(fight_data_reg)
fight_data_reg <- select(fight_data_reg, starts_with("R_") | starts_with("B_") | 'Winner')

# Separate features (X) and target variable (y)
X <- as.matrix(data[, -which(names(data) == "Winner")])
y <- as.matrix(data$Winner)

set.seed(42)

# all variables
model <- glm(Winner~., family=binomial(link="logit"), data=fight_data_reg)
model_summary <- summary(model)

# Extract p-values and select variables with p < 0.05
significant_vars <- rownames(coef(summary(model)))[coef(summary(model))[,4] < 0.05]
significant_vars <- significant_vars[significant_vars != "(Intercept)"]
data_filtered <- data[, c(significant_vars, "Winner")]

# all variables
model <- glm(Winner~., family=binomial(link="logit"), data=data_filtered)
model_summary <- summary(model)
significant_vars <- rownames(coef(summary(model)))[coef(summary(model))[,4] < 0.01]
significant_vars <- significant_vars[significant_vars != "(Intercept)"]
data_filtered <- data[, c(significant_vars, "Winner")]

# Split the dataset
trainIndex <- createDataPartition(data$Winner, p = .8, list = FALSE, times = 1)
X_train <- data[trainIndex, significant_vars]
y_train <- y[trainIndex]
X_test <- data[-trainIndex, significant_vars]
y_test <- y[-trainIndex]

# Data preprocessing (scaling and centering for the selected features)
preprocess_params <- preProcess(X_train, method = c("center", "scale"))
X_train <- predict(preprocess_params, X_train)
X_test <- predict(preprocess_params, X_test)

#---------------------------------------XGBoost--------------------------------

# Assuming your dataset is already pre-processed and ready for modeling
# Convert data to DMatrix object, which is optimized for XGBoost
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
dtest <- xgb.DMatrix(data = as.matrix(X_test))

# Define parameters for the XGBoost model
params <- list( booster = "gbtree", objective = "binary:logistic", eta = 0.3,
                max_depth = 4, min_child_weight = 1, subsample = 1, 
                colsample_bytree = 1, eval_metric = "auc"
)

# Train the model. take care of the number of rounds
xgb_model <- xgb.train(params = params, data = dtrain, nrounds = 31)

# Make predictions
predictions_xgb <- predict(xgb_model, dtest)
predictions_xgb <- ifelse(predictions_xgb > 0.5, 1, 0)

#---------------------Measurement of quality---------------------------------

# Confusion matrix
confusion_matrix <- confusionMatrix(factor(predictions_xgb), factor(y_test))

# Calculate probabilities instead of binary predictions
prob_predictions <- predict(xgb_model, dtest, type = "prob")
roc_curve <- roc(response = y_test, predictor = prob_predictions)
plot(roc_curve, main = "ROC Curve", print.auc = TRUE)

# Plot feature importance
importance_matrix <- xgb.importance(feature_names = colnames(X_train), model = xgb_model)
xgb.plot.importance(importance_matrix)

# Visualize the correlation matrix
# Calculate the correlation of 'Winner' with all other variables
correlations <- abs(cor(data_filtered))

# Extract just the correlations with 'Winner'
winner_correlations <- correlations["Winner", ]
if(!require(ggplot2)) install.packages("ggplot2")
library(ggplot2)

# Create a dataframe for plotting
cor_df <- data.frame(Variable = names(winner_correlations), Correlation = winner_correlations)

# Plot using ggplot
ggplot(cor_df, aes(x = reorder(Variable, Correlation), y = Correlation, fill = Correlation)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip coordinates for better visualization of variable names
  labs(x = "Variable", y = "Correlation with Winner") +
  ggtitle("Correlation of Variables with Winner") +
  theme_minimal() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limit = c(0,1), space = "Lab", name="Correlation")
