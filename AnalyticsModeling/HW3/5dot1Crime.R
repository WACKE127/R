library(shiny)
library(outliers)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Crime Data Outlier Detection with Grubbs’ Test"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId  = "outlierTestType",
        label    = "Select Grubbs’ Test Type:",
        choices  = c("Single Outlier" = "single", 
                     "Two Outliers"   = "double"),
        selected = "single"
      )
    ),
    
    mainPanel(
      plotOutput("crimePlot"),
      
      h4("Figure Caption:"),
      uiOutput("figureCaption"),
      
      h4("Grubbs' Test Output:"),
      verbatimTextOutput("grubbsResults")
    )
  )
)

server <- function(input, output, session) {
  
  uscrime_df <- read.table("data/uscrime.txt", header = TRUE)
  
  testOutputText <- reactiveVal(NULL)
  testObject <- reactiveVal(NULL)

  # 2) Reactive expression to run the selected Grubbs’ test
  #    and tag outliers so we can color them in the plot.
  flaggedData <- reactive({
    df <- uscrime_df
    
    df$OutlierFlag <- "No"
    
    if (input$outlierTestType == "single") {
      
      singleText <- capture.output({
        # We store the test object in a local variable so we can do the if-check below
        tObj <- grubbs.test(df$Crime)
        testObject(tObj)         # Make the test object globally available
      })
      # Now that testObject() is set, let's retrieve it to parse p-values, etc.
      g1 <- testObject()        
      testOutputText(singleText) # Store the console text lines

      # Single-outlier test (already stored in g1)
      if (g1$p.value < 0.05) {
        if (grepl("lowest value is an outlier", g1$alternative)) {
          out_idx <- which.min(df$Crime)
          df$OutlierFlag[out_idx] <- "Yes"
        } else if (grepl("highest value is an outlier", g1$alternative)) {
          out_idx <- which.max(df$Crime)
          df$OutlierFlag[out_idx] <- "Yes"
        }
      }
      
    } else {
      
      doubleText <- capture.output({
        tObj <- grubbs.test(df$Crime, type = 11)
        testObject(tObj)
      })
      g2 <- testObject()
      testOutputText(doubleText)

      # Two-outlier test, e.g. type=11 for two outliers on the same tail
      if (g2$p.value < 0.05) {
        if (grepl("lowest values are outliers", g2$alternative)) {
          out_idx <- order(df$Crime)[1:2]
          df$OutlierFlag[out_idx] <- "Yes"
        } else if (grepl("highest values are outliers", g2$alternative)) {
          out_idx <- order(df$Crime, decreasing = TRUE)[1:2]
          df$OutlierFlag[out_idx] <- "Yes"
        }
      }
    }
    df
  })
  
  # 3) Render the ggplot based on flagged data
  output$crimePlot <- renderPlot({
    df <- flaggedData()
    
    ggplot(df, aes(x = factor(1), y = Crime, color = OutlierFlag)) +
      geom_boxplot(outlier.shape = NA, alpha = 0.3) +
      geom_jitter(width = 0.1, size = 3) +
      scale_color_manual(values = c("No" = "black", "Yes" = "red")) +
      labs(
        title = paste("Crime Data - Grubbs’ Test:", input$outlierTestType),
        x = NULL
      ) +
      theme_minimal()
  })
  
  output$figureCaption <- renderUI({
    tObj <- testObject()
    if (is.null(tObj)) {
      return(HTML("No test performed yet."))
    }
    pval   <- signif(tObj$p.value, 5)
    gVal   <- signif(unname(tObj$statistic["G"]), 5)
    altHyp <- tObj$alternative
    
    HTML(paste0(
      "Points are flagged as 'Yes' (red) if p < 0.05 from Grubbs’ test. ",
      "Here, G = ", gVal, ", p-value = ", pval, 
      ", alt. hypothesis: '", altHyp, "'. All other points are black."
    ))
  })
  
  output$grubbsResults <- renderPrint({
    txt <- testOutputText()
    if (!is.null(txt)) {
      cat(txt, sep = "\n")
    }
  })
}

shinyApp(ui = ui, server = server)
