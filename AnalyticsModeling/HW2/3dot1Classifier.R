###############################################################################
# COMPREHENSIVE EXAMPLE: 10-FOLD CROSS-VALIDATION FOR KNN AND SVM USING CARET
###############################################################################

# 1) Load necessary packages
library(caret)
library(kknn)     # Provides kknn method (caret can interface with it)
library(kernlab)  # Provides SVM (ksvm), used internally by caret's svm* methods

# 2) Load or read in your data
#    Replace this with your actual data-loading step.
#    For example, if your file is 'credit_card_data-headers.txt':
# credit_card_data_headers <- read.table("credit_card_data-headers.txt", header=TRUE)
# If it's already in your environment, just ensure it's in a data frame called credit_card_data_headers.

data("credit_card_data_headers")

# Make sure the outcome (R1) is treated as a factor if you want classification
# (if it's already 0/1 numeric, that's okay too, but usually classification in caret uses factors).
# If R1 is numeric, uncomment:
# credit_card_data_headers$R1 <- as.factor(credit_card_data_headers$R1)

# 3) Set a seed for reproducibility
set.seed(123)

# 4) Define 10-fold cross-validation
train_control <- trainControl(
  method = "cv",      # cross-validation
  number = 10,        # 10 folds
  classProbs = TRUE   # if you'd like to track class probabilities
)

###############################################################################
# KNN MODEL WITH CARET + CROSS-VALIDATION
###############################################################################
# Define a grid of hyperparameters for the kknn method
knn_grid <- expand.grid(
  kmax    = c(1, 3, 5, 7, 9, 11, 15, 21, 42),  # different k values
  distance = 2,                               # Euclidean
  kernel   = "rectangular"                    # basic weighting kernel
)

# Train KNN using the caret "train" function
model_knn_cv <- train(
  R1 ~ ., 
  data      = credit_card_data_headers,
  method    = "kknn",
  trControl = train_control,
  tuneGrid  = knn_grid,
  metric    = "Accuracy",       # or "Kappa", etc.
  preProcess = c("center","scale")  # scale features (recommended for kNN)
)

# Show KNN results
print(model_knn_cv)
plot(model_knn_cv)

###############################################################################
# SVM MODEL WITH CARET + CROSS-VALIDATION (LINEAR SVM EXAMPLE)
###############################################################################
# Define a grid of C values for a linear SVM
svm_grid <- expand.grid(
  C = c(0.1, 1, 10, 100)
)

# Train SVM using caret and the svmLinear method
model_svm_cv <- train(
  R1 ~ .,
  data      = credit_card_data_headers,
  method    = "svmLinear", 
  trControl = train_control,
  tuneGrid  = svm_grid,
  metric    = "Accuracy",   
  preProcess = c("center","scale") # scale features (good practice for SVM)
)

# Show SVM results
print(model_svm_cv)
plot(model_svm_cv)

###############################################################################
# The printed/plot output will give you:
# - Cross-validated accuracy (and potentially other metrics)
# - The best hyperparameters (k for KNN, C for SVM)
# - Plots of accuracy vs. the tuning parameter
###############################################################################
