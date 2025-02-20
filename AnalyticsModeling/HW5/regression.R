library(shiny)
library(ggplot2)

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
  "M","So","Ed","Po1","Po2","LF","M.F","Pop",
  "NW","U1","U2","Wealth","Ineq","Prob","Time","Crime"
)

for (col in names(df_raw)) {
  df_raw[[col]] <- suppressWarnings(as.numeric(df_raw[[col]]))
}

df_raw <- na.omit(df_raw)

message("Data dimensions after cleaning: ", paste(dim(df_raw), collapse = " x "))

crime_data <- df_raw

ui <- fluidPage(
  titlePanel("US Crime Data: Regression & PCA"),
  
  sidebarLayout(
    sidebarPanel(
      p("Select which predictors to use in the linear model (check boxes are horizontal). 
         Then see regression outputs, diagnostic plots, 
         a prediction for the new city, and PCA results (including RMSE by number of PCs)."),
      
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
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  model_data <- reactive({
    df <- crime_data
    sel <- input$predictors
    if (length(sel) == 0) {
      return(df[, "Crime", drop = FALSE])
    }
    df_subset <- df[, c(sel, "Crime"), drop = FALSE]
    df_subset
  })
  
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
      Actual    = mod$model$Crime,
      Predicted = mod$fitted.values
    )
    
    ggplot(df_plot, aes(x = Predicted, y = Actual)) +
      geom_point(color = "blue", size = 2) +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
      theme_minimal() +
      labs(title = "Predicted vs Actual Crime",
           x = "Predicted Crime",
           y = "Actual Crime")
  })
  
  output$plotResidualsVsFitted <- renderPlot({
    mod <- fitModel()
    if (is.null(mod)) return(NULL)
    
    df_plot <- data.frame(
      Fitted    = mod$fitted.values,
      Residuals = mod$residuals
    )
    
    ggplot(df_plot, aes(x = Fitted, y = Residuals)) +
      geom_point(color = "darkgreen", size = 2) +
      geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
      theme_minimal() +
      labs(title = "Residuals vs Fitted",
           x = "Fitted Crime",
           y = "Residuals")
  })
  
  new_city_data <- reactive({
    data.frame(
      M      = 14.0,
      So     = 0,
      Ed     = 10.0,
      Po1    = 12.0,
      Po2    = 15.5,
      LF     = 0.640,
      M.F    = 94.0,
      Pop    = 150,
      NW     = 1.1,
      U1     = 0.120,
      U2     = 3.6,
      Wealth = 3200,
      Ineq   = 20.1,
      Prob   = 0.04,
      Time   = 39.0
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
    scale_vec  <- pca_model$scale
    
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
      rmse_k <- sqrt(mean((residuals_k)^2))
      rmse_vals[k] <- rmse_k
    }
    
    df_rmse <- data.frame(
      NumComponents = seq_len(n_pcs),
      RMSE = rmse_vals
    )
    
    ggplot(df_rmse, aes(x = NumComponents, y = RMSE)) +
      geom_line(color = "steelblue") +
      geom_point(color = "steelblue", size = 2) +
      theme_minimal() +
      labs(
        title = "RMSE vs Number of Principal Components",
        x = "Number of PCs",
        y = "Reconstruction RMSE"
      )
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
}

shinyApp(ui = ui, server = server)
