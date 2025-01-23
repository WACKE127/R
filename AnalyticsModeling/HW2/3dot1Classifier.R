library(caret)
library(kknn)
library(kernlab)
library(ggplot2)
library(shiny)

# Data loading
data("credit_card_data_headers")

# Convert outcome to factor with valid names
credit_card_data_headers$R1 <- factor(
  credit_card_data_headers$R1,
  levels = c(0, 1),
  labels = c("Class0", "Class1")
)

set.seed(123)

# 10-fold cross-validation setup
train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE)

# ----- KNN with caret -----
# Example: searching k=1..20 (adjust as you wish, e.g., 1:200)
knn_grid <- expand.grid(
  kmax     = 1:100,
  distance = 2,              # Euclidean
  kernel   = "rectangular"
)

model_knn_cv <- train(
  R1 ~ .,
  data       = credit_card_data_headers,
  method     = "kknn",
  trControl  = train_control,
  tuneGrid   = knn_grid,
  metric     = "Accuracy",
  preProcess = c("center", "scale")
)

cat("\n--- KNN CROSS-VALIDATION RESULTS ---\n")
print(model_knn_cv)

# Extract results for plotting
knn_plot_data <- model_knn_cv$results
p_knn <- ggplot(knn_plot_data, aes(x = kmax, y = Accuracy)) +
  geom_point() +
  labs(title = "KNN (10-fold CV)", x = "k", y = "Accuracy") +
  theme_minimal()

# ----- SVM with caret -----
# Example: searching C in {0.1, 1, 10, 100} or a sequence
svm_grid <- expand.grid(C = c(0.1, 1, 10, 100))
# or a sequence: seq(0.1, 100, by=10) for a quick demonstration

model_svm_cv <- train(
  R1 ~ .,
  data       = credit_card_data_headers,
  method     = "svmLinear",
  trControl  = train_control,
  tuneGrid   = svm_grid,
  metric     = "Accuracy",
  preProcess = c("center", "scale")
)

cat("\n--- SVM CROSS-VALIDATION RESULTS ---\n")
print(model_svm_cv)

# Extract results for plotting
svm_plot_data <- model_svm_cv$results
p_svm <- ggplot(svm_plot_data, aes(x = C, y = Accuracy)) +
  geom_point() +
  labs(title = "SVM (10-fold CV)", x = "C", y = "Accuracy") +
  theme_minimal()

# ----- Minimal Shiny App -----
ui <- fluidPage(
  h2("Cross-Validation Plots"),
  plotOutput("plot_knn"),
  plotOutput("plot_svm")
)

server <- function(input, output) {
  output$plot_knn <- renderPlot({ p_knn })
  output$plot_svm <- renderPlot({ p_svm })
}

shinyApp(ui, server)
