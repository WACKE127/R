library(shiny)
library(ggplot2)
library(rpart)
library(randomForest)

raw_lines <- readLines("data/uscrime.txt", warn = FALSE)
raw_lines <- raw_lines[raw_lines != ""]
df_raw <- read.table(
  textConnection(raw_lines),
  header = FALSE,
  stringsAsFactors = FALSE,
  strip.white = TRUE,
  fill = TRUE,
  comment.char = ""
)

colnames(df_raw) <- c(
  "M", "So", "Ed", "Po1", "Po2", "LF", "M.F", "Pop",
  "NW", "U1", "U2", "Wealth", "Ineq", "Prob", "Time", "Crime"
)

for (col in names(df_raw)) {
  df_raw[[col]] <- suppressWarnings(as.numeric(df_raw[[col]]))
}
df_raw <- na.omit(df_raw)
crime_data <- df_raw

ui <- fluidPage(
  titlePanel("US Crime Data: Regression, PCA, Tree, & Random Forest"),
  sidebarLayout(
    sidebarPanel(
      p("Select which predictors to use in the linear model (check boxes are horizontal).
         Then explore regression outputs, diagnostic plots,
         a prediction for the new city, PCA results, and now tree/forest models."),
      checkboxGroupInput(
        inputId = "predictors",
        label   = "Choose Predictors for Crime Model:",
        choices = c("M", "So", "Ed", "Po1", "Po2", "LF", "M.F", "Pop",
                    "NW", "U1", "U2", "Wealth", "Ineq", "Prob", "Time"),
        selected = c("M", "So", "Ed", "Po1", "Po2", "LF", "M.F", "Pop",
                     "NW", "U1", "U2", "Wealth", "Ineq", "Prob", "Time"),
        inline   = TRUE
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Regression",
          h4("Selected Predictors:"),
          verbatimTextOutput("selectedPredictors"),
          h4("Model Coefficients"),
          tableOutput("modelCoefficients"),
          h4("Model Summary"),
          verbatimTextOutput("modelSummary"),
          h4("R-Squared Values"),
          tableOutput("modelRsquared"),
          h4("Regression Diagnostic Plots"),
          fluidRow(
            column(6, plotOutput("plotPredictedVsActual")),
            column(6, plotOutput("plotResidualsVsFitted"))
          ),
          h4("Prediction for New City"),
          tableOutput("predictionTable")
        ),
        tabPanel("PCA",
          h4("PCA Summary"),
          verbatimTextOutput("pcaSummary"),
          h4("PCA Biplot"),
          plotOutput("pcaBiplot"),
          h4("RMSE by Number of Principal Components"),
          plotOutput("pcaRmsePlot"),
          h5("Minor Writeup:"),
          uiOutput("pcaRmseWriteup")
        ),
        tabPanel("Tree & Forest",
          h4("Regression Tree Model (rpart)"),
          verbatimTextOutput("treeSummary"),
          fluidRow(
            column(6, plotOutput("treePlot", height = "600px"), uiOutput("treePlotCaption")),
            column(6, plotOutput("treePredVsActual"), uiOutput("treePredVsActualCaption"))
          ),
          h4("Random Forest Model"),
          verbatimTextOutput("rfSummary"),
          fluidRow(
            column(6, plotOutput("rfErrorPlot"), uiOutput("rfErrorPlotCaption")),
            column(6, plotOutput("rfPredVsActual"), uiOutput("rfPredVsActualCaption"))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # ------------------ DATA PREP ------------------
  model_data <- reactive({
    df <- crime_data
    sel <- input$predictors
    if (length(sel) == 0) {
      return(df[, "Crime", drop = FALSE])
    }
    df_subset <- df[, c(sel, "Crime"), drop = FALSE]
    df_subset
  })
  
  # ------------------ LINEAR REGRESSION ------------------
  fitModel <- reactive({
    df_sub <- model_data()
    if (ncol(df_sub) < 2) return(NULL)
    lm(Crime ~ ., data = df_sub)
  })
  
  output$selectedPredictors <- renderPrint({
    if (length(input$predictors) == 0) {
      "No predictors selected!"
    } else {
      input$predictors
    }
  })
  
  output$modelCoefficients <- renderTable({
    mod <- fitModel()
    if (is.null(mod)) return(NULL)
    coefs <- coef(mod)
    data.frame(Term = names(coefs), Coefficient = coefs)
  })
  
  output$modelSummary <- renderPrint({
    mod <- fitModel()
    if (is.null(mod)) {
      "No model can be fit because no predictors were selected."
    } else {
      summary(mod)
    }
  })
  
  output$modelRsquared <- renderTable({
    mod <- fitModel()
    if (is.null(mod)) return(NULL)
    s <- summary(mod)
    data.frame(R_Squared = s$r.squared, Adj_R_Squared = s$adj.r.squared)
  })
  
  output$plotPredictedVsActual <- renderPlot({
    mod <- fitModel()
    if (is.null(mod)) return(NULL)
    df_plot <- data.frame(
      Actual = mod$model$Crime,
      Predicted = mod$fitted.values
    )
    ggplot(df_plot, aes(x = Predicted, y = Actual)) +
      geom_point(color = "blue", size = 2) +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      theme_minimal() +
      labs(title = "Predicted vs Actual Crime (Linear Model)",
           x = "Predicted Crime",
           y = "Actual Crime")
  })
  
  output$plotResidualsVsFitted <- renderPlot({
    mod <- fitModel()
    if (is.null(mod)) return(NULL)
    df_plot <- data.frame(
      Fitted = mod$fitted.values,
      Residuals = mod$residuals
    )
    ggplot(df_plot, aes(x = Fitted, y = Residuals)) +
      geom_point(color = "darkgreen", size = 2) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      theme_minimal() +
      labs(title = "Residuals vs Fitted (Linear Model)",
           x = "Fitted Crime",
           y = "Residuals")
  })
  
  new_city_data <- reactive({
    data.frame(
      M = 14.0,
      So = 0,
      Ed = 10.0,
      Po1 = 12.0,
      Po2 = 15.5,
      LF = 0.640,
      M.F = 94.0,
      Pop = 150,
      NW = 1.1,
      U1 = 0.120,
      U2 = 3.6,
      Wealth = 3200,
      Ineq = 20.1,
      Prob = 0.04,
      Time = 39.0
    )
  })
  
  prediction <- reactive({
    mod <- fitModel()
    if (is.null(mod)) return(NA)
    predict(mod, newdata = new_city_data())
  })
  
  output$predictionTable <- renderTable({
    data.frame(Predicted_Crime = prediction())
  })
  
  pcaFit <- reactive({
    df <- model_data()
    if ("Crime" %in% colnames(df)) {
      df <- df[, setdiff(colnames(df), "Crime"), drop = FALSE]
    }
    num_df <- df[sapply(df, is.numeric)]
    if (ncol(num_df) < 2) return(NULL)
    prcomp(num_df, scale. = TRUE)
  })
  
  output$pcaSummary <- renderPrint({
    pca_model <- pcaFit()
    if (is.null(pca_model)) {
      "Not enough numeric predictors selected for PCA (need at least 2)."
    } else {
      summary(pca_model)
    }
  })
  
  output$pcaBiplot <- renderPlot({
    pca_model <- pcaFit()
    if (is.null(pca_model)) return(NULL)
    biplot(pca_model, main = "PCA Biplot (Scaled Data)")
  })
  
  output$pcaRmsePlot <- renderPlot({
    pca_model <- pcaFit()
    if (is.null(pca_model)) return(NULL)
    df <- model_data()
    df <- df[, setdiff(colnames(df), "Crime"), drop = FALSE]
    num_df <- df[sapply(df, is.numeric)]
    center_vec <- pca_model$center
    scale_vec <- pca_model$scale
    X <- scale(num_df, center = center_vec, scale = scale_vec)
    n_pcs <- ncol(pca_model$rotation)
    rmse_vals <- numeric(n_pcs)
    for (k in seq_len(n_pcs)) {
      X_hat_k <- pca_model$x[, 1:k] %*% t(pca_model$rotation[, 1:k])
      if (!is.null(center_vec) && !is.null(scale_vec)) {
        X_hat_k <- sweep(X_hat_k, 2, scale_vec, `*`)
        X_hat_k <- sweep(X_hat_k, 2, center_vec, `+`)
      }
      residuals_k <- as.matrix(num_df) - X_hat_k
      rmse_vals[k] <- sqrt(mean((residuals_k)^2))
    }
    df_rmse <- data.frame(NumComponents = seq_len(n_pcs), RMSE = rmse_vals)
    ggplot(df_rmse, aes(x = NumComponents, y = RMSE)) +
      geom_line(color = "steelblue") +
      geom_point(color = "steelblue", size = 2) +
      theme_minimal() +
      labs(title = "RMSE vs Number of Principal Components",
           x = "Number of PCs",
           y = "Reconstruction RMSE")
  })
  
  output$pcaRmseWriteup <- renderUI({
    pca_model <- pcaFit()
    if (is.null(pca_model)) {
      return("No PCA results. Please select at least two numeric predictors.")
    }
    HTML(paste(
      "<p><strong>Interpretation:</strong> As we increase the number of principal components,",
      "the RMSE for reconstructing the original data generally decreases. The first few components",
      "often capture most variance, so we see a rapid drop in RMSE initially. Additional components",
      "refine the fit but may yield diminishing returns.</p>",
      "<p>To choose the 'best' number of components, look for the 'elbow' in the chart or a minimal",
      "RMSE that balances simplicity and accuracy. Each principal component corresponds to a linear",
      "combination of the original predictors, emphasizing those with the greatest variance.</p>"
    ))
  })
  
  fitTree <- reactive({
    df_sub <- model_data()
    if (ncol(df_sub) < 2) return(NULL)
    rpart(Crime ~ ., data = df_sub, method = "anova")
  })
  
  output$treeSummary <- renderPrint({
    tree_mod <- fitTree()
    if (is.null(tree_mod)) {
      "Not enough predictors to build a tree model."
    } else {
      printcp(tree_mod)
    }
  })
  
  output$treePlot <- renderPlot({
    tree_mod <- fitTree()
    if (is.null(tree_mod)) return(NULL)
    plot(tree_mod, uniform = TRUE, main = "Regression Tree")
    text(tree_mod, use.n = TRUE, all = TRUE, cex = 0.8)
  })
  
  output$treePlotCaption <- renderUI({
    tree_mod <- fitTree()
    if (is.null(tree_mod)) {
      HTML("<p>No tree model available to summarize.</p>")
    } else {
      HTML("<p><strong>Figure Caption:</strong> This regression tree (built with rpart) splits the data to predict 'Crime' using the selected predictors. Each node shows a mean response value for observations falling into that branch.</p>")
    }
  })
  
  output$treePredVsActual <- renderPlot({
    tree_mod <- fitTree()
    if (is.null(tree_mod)) return(NULL)
    df_sub <- model_data()
    pred_tree <- predict(tree_mod, newdata = df_sub)
    df_plot <- data.frame(Actual = df_sub$Crime, Predicted = pred_tree)
    ggplot(df_plot, aes(x = Predicted, y = Actual)) +
      geom_point(color = "purple", size = 2) +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      theme_minimal() +
      labs(title = "Predicted vs Actual Crime (Regression Tree)",
           x = "Predicted Crime",
           y = "Actual Crime")
  })
  
  output$treePredVsActualCaption <- renderUI({
    tree_mod <- fitTree()
    if (is.null(tree_mod)) {
      HTML("<p>No tree model available to show predicted vs actual crime.</p>")
    } else {
      HTML("<p><strong>Figure Caption:</strong> Comparison of the tree's predicted Crime values vs. the actual Crime values in the dataset. The closer points lie to the diagonal line, the better the tree's predictions.</p>")
    }
  })
  
  fitRf <- reactive({
    df_sub <- model_data()
    if (ncol(df_sub) < 2) return(NULL)
    randomForest(Crime ~ ., data = df_sub, ntree = 500, mtry = 2, importance = TRUE)
  })
  
  output$rfSummary <- renderPrint({
    rf_mod <- fitRf()
    if (is.null(rf_mod)) {
      "Not enough predictors to build a random forest."
    } else {
      rf_mod
    }
  })
  
  output$rfErrorPlot <- renderPlot({
    rf_mod <- fitRf()
    if (is.null(rf_mod)) return(NULL)
    plot(rf_mod, main = "Random Forest Error vs. Number of Trees")
  })
  
  output$rfErrorPlotCaption <- renderUI({
    rf_mod <- fitRf()
    if (is.null(rf_mod)) {
      HTML("<p>No random forest model available to summarize OOB error.</p>")
    } else {
      HTML("<p><strong>Figure Caption:</strong> OOB (Out-of-Bag) error rate versus the number of trees in the random forest. This indicates how the model's error changes as more trees are added. Often, the error stabilizes after a certain number of trees.</p>")
    }
  })
  
  output$rfPredVsActual <- renderPlot({
    rf_mod <- fitRf()
    if (is.null(rf_mod)) return(NULL)
    df_sub <- model_data()
    pred_rf <- predict(rf_mod, newdata = df_sub)
    df_plot <- data.frame(Actual = df_sub$Crime, Predicted = pred_rf)
    ggplot(df_plot, aes(x = Predicted, y = Actual)) +
      geom_point(color = "orange", size = 2) +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      theme_minimal() +
      labs(title = "Predicted vs Actual Crime (Random Forest)",
           x = "Predicted Crime",
           y = "Actual Crime")
  })
  
  output$rfPredVsActualCaption <- renderUI({
    rf_mod <- fitRf()
    if (is.null(rf_mod)) {
      HTML("<p>No random forest model available to show predicted vs actual crime.</p>")
    } else {
      HTML("<p><strong>Figure Caption:</strong> Comparison of the random forest's predicted Crime values vs. the actual Crime values. Points closer to the 45-degree diagonal indicate more accurate predictions.</p>")
    }
  })
}

shinyApp(ui = ui, server = server)
