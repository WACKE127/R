# Directory structure:
# - your_project_folder
#   - app.R
#   - data
#     - uscrime.txt

# ---- app.R ----

library(shiny)
library(glmnet)
library(caret)
library(ggplot2)

# Load and preprocess data
crime_data <- read.table("data/uscrime.txt", header = TRUE)

# UI
ui <- navbarPage("Crime Prediction Models",
  tabPanel("Stepwise Regression",
    mainPanel(
      verbatimTextOutput("stepwise_summary"),
      plotOutput("stepwise_plot")
    )
  ),
  tabPanel("Lasso Regression",
    mainPanel(
      verbatimTextOutput("lasso_summary"),
      plotOutput("lasso_plot")
    )
  ),
  tabPanel("Elastic Net Regression",
    sidebarPanel(
      sliderInput("alpha", "Alpha (Elastic Net):",
                  min = 0, max = 1, value = 1, step = 0.1)
    ),
    mainPanel(
      verbatimTextOutput("elastic_summary"),
      plotOutput("elastic_plot")
    )
  ),
  tabPanel("Model Evaluation Guidelines",
    mainPanel(
      h3("Guidelines for Evaluating Metrics"),
      p("R-squared (R²): High values (close to 1) mean better explanatory power. Low values indicate weak explanatory power."),
      p("RMSE: Lower values indicate better prediction performance."),
      p("The best model balances high R² with low RMSE.")
    )
  )
)

server <- function(input, output) {
  
  # Scale and split data
  scaled_data <- scale(crime_data)
  set.seed(123)

  # Partition Data: 60% train, 20% validation, 20% test
  trainIndex <- createDataPartition(scaled_data[, "Crime"], p = 0.6, list = FALSE)
  remainingData <- scaled_data[-trainIndex, ]
  valIndex <- createDataPartition(remainingData[, "Crime"], p = 0.5, list = FALSE)

  trainData <- scaled_data[trainIndex, ]
  valData <- remainingData[valIndex, ]
  testData <- remainingData[-valIndex, ]

  # --- Stepwise Regression ---
  stepwise_model <- step(lm(Crime ~ ., data = as.data.frame(trainData)), direction = "both", trace = FALSE)
  
  stepwise_pred <- predict(stepwise_model, newdata = as.data.frame(valData))
  stepwise_actual <- valData[, "Crime"]

  stepwise_r2 <- cor(stepwise_pred, stepwise_actual)^2
  stepwise_rmse <- sqrt(mean((stepwise_pred - stepwise_actual)^2))

  output$stepwise_summary <- renderPrint({
    cat("R-squared (R²):", round(stepwise_r2, 3), "\n")
    cat("RMSE:", round(stepwise_rmse, 3), "\n\n")
    summary(stepwise_model)
  })

  output$stepwise_plot <- renderPlot({
    df <- data.frame(Actual = stepwise_actual, Predicted = stepwise_pred)
    ggplot(df, aes(x = Actual, y = Predicted)) +
      geom_point() +
      geom_abline(color = "blue") +
      labs(title = "Stepwise Regression Predictions",
           subtitle = paste("R² =", round(stepwise_r2, 3), "| RMSE =", round(stepwise_rmse, 3)))
  })

  # --- Lasso Regression ---
  lasso_model <- reactive({
    cv.glmnet(as.matrix(trainData[, -which(colnames(trainData) == "Crime")]),
              trainData[, "Crime"], alpha = 1)
  })

  output$lasso_summary <- renderPrint({
    pred <- predict(lasso_model(), s = "lambda.min", newx = as.matrix(valData[, -which(colnames(valData) == "Crime")]))
    actual <- valData[, "Crime"]

    r2 <- cor(as.vector(pred), actual)^2
    rmse <- sqrt(mean((as.vector(pred) - actual)^2))

    cat("R-squared (R²):", round(r2, 3), "\n")
    cat("RMSE:", round(rmse, 3), "\n\n")
    coef(lasso_model(), s = "lambda.min")
  })

  output$lasso_plot <- renderPlot({
    pred <- predict(lasso_model(), s = "lambda.min", newx = as.matrix(valData[, -which(colnames(valData) == "Crime")]))
    df <- data.frame(Actual = valData[, "Crime"], Predicted = as.vector(pred))

    r2 <- cor(df$Actual, df$Predicted)^2
    rmse <- sqrt(mean((df$Actual - df$Predicted)^2))

    ggplot(df, aes(x = Actual, y = Predicted)) +
      geom_point() +
      geom_abline(color = "red") +
      labs(title = "Lasso Regression Predictions",
           subtitle = paste("R² =", round(r2, 3), "| RMSE =", round(rmse, 3)))
  })

  # --- Elastic Net Regression ---
  elastic_model <- reactive({
    cv.glmnet(as.matrix(trainData[, -which(colnames(trainData) == "Crime")]),
              trainData[, "Crime"], alpha = input$alpha)
  })

  output$elastic_summary <- renderPrint({
    pred <- predict(elastic_model(), s = "lambda.min", newx = as.matrix(valData[, -which(colnames(valData) == "Crime")]))
    actual <- valData[, "Crime"]

    r2 <- cor(as.vector(pred), actual)^2
    rmse <- sqrt(mean((as.vector(pred) - actual)^2))

    cat("Alpha:", input$alpha, "\n")
    cat("R-squared (R²):", round(r2, 3), "\n")
    cat("RMSE:", round(rmse, 3), "\n\n")
    coef(elastic_model(), s = "lambda.min")
  })

  output$elastic_plot <- renderPlot({
    pred <- predict(elastic_model(), s = "lambda.min", newx = as.matrix(valData[, -which(colnames(valData) == "Crime")]))
    df <- data.frame(Actual = valData[, "Crime"], Predicted = as.vector(pred))

    r2 <- cor(df$Actual, df$Predicted)^2
    rmse <- sqrt(mean((df$Actual - df$Predicted)^2))

    ggplot(df, aes(x = Actual, y = Predicted)) +
      geom_point() +
      geom_abline(color = "green") +
      labs(title = "Elastic Net Predictions",
           subtitle = paste("R² =", round(r2, 3), "| RMSE =", round(rmse, 3)))
  })

}

# Run the application
shinyApp(ui = ui, server = server)
