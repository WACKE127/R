# app.R
library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(scales)  # for scientific notation

ui <- fluidPage(
  titlePanel("Flagellar Regrowth Analysis with Density Tables & Statistical Tests"),
  tabsetPanel(
    # Tab 1: Plot & Regression
    tabPanel("Plot & Regression",
      sidebarLayout(
        sidebarPanel(
          selectInput("weekChoice", "Choose Week:", choices = c(1, 2), selected = 1),
          checkboxInput("showLines", "Show replicate lines (non-NDF)?", value = FALSE),
          checkboxInput("showErrorBars", "Show error bars (Mean ± SE)?", value = FALSE),
          checkboxInput("doLinear", "Show Linear Regression", value = TRUE),
          checkboxInput("doLog", "Show Logarithmic Regression", value = FALSE),
          helpText("Note: NDF is plotted as points only. For logarithmic regression, time=0 rows are excluded.")
        ),
        mainPanel(
          plotOutput("scatterPlot", height = "600px"),
          h4("Regression R² Table"),
          verbatimTextOutput("fitOutput"),
          h4("Statistical Tests: T-test and Chi-square/Fisher"),
          verbatimTextOutput("statTestsOutput")
        )
      )
    ),
    # Tab 2: Density Tables
    tabPanel("Density Tables",
      h3("Week 1 Density Table"),
      tableOutput("densityTableWeek1"),
      h3("Week 2 Density Table"),
      tableOutput("densityTableWeek2")
    )
  )
)

server <- function(input, output, session) {

  # 1) Connect to the database
  con <- dbConnect(SQLite(), "flagella_data.sqlite")
  onStop(function() dbDisconnect(con))

  # 2) Base density table for display
  df_base <- data.frame(
    week = c(1, 2),
    base_density = c(2.79e6, 4.50e6)
  )

  # 3) Load entire table from DB (reactive)
  df_raw <- reactive({
    dbGetQuery(con, "SELECT * FROM flagella_measurements")
  })

  # 4) Density tables (one for each week) with scientific notation
  output$densityTableWeek1 <- renderTable({
    df <- df_raw()
    if(nrow(df) == 0) return()
    df_w1 <- df %>%
      filter(week == 1, !is.na(density)) %>%
      select(week, condition, density) %>%
      distinct() %>%
      left_join(df_base, by = "week") %>%
      mutate(
        base_density_sci = scientific(base_density, digits = 3),
        density_sci      = scientific(density, digits = 3)
      ) %>%
      select(condition, base_density_sci, density_sci) %>%
      arrange(condition)
    df_w1
  }, striped = TRUE, spacing = "m")

  output$densityTableWeek2 <- renderTable({
    df <- df_raw()
    if(nrow(df) == 0) return()
    df_w2 <- df %>%
      filter(week == 2, !is.na(density)) %>%
      select(week, condition, density) %>%
      distinct() %>%
      left_join(df_base, by = "week") %>%
      mutate(
        base_density_sci = scientific(base_density, digits = 3),
        density_sci      = scientific(density, digits = 3)
      ) %>%
      select(condition, base_density_sci, density_sci) %>%
      arrange(condition)
    df_w2
  }, striped = TRUE, spacing = "m")

  # 5) Filtered data for main analysis (by selected week)
  df_filtered <- reactive({
    df <- df_raw()
    df <- df %>%
      filter(week == input$weekChoice, !is.na(length_um)) %>%
      mutate(time_min = as.numeric(time_min)) %>%
      filter(length_um >= 0)
    df
  })

  # 6) Regression fits (R² values) for non-NDF conditions
  output$fitOutput <- renderPrint({
    df <- df_filtered()
    if(nrow(df) == 0) {
      cat("No data found for this week.\n")
      return()
    }
    df_nonNDF <- df %>%
      filter(toupper(condition) != "NDF") %>%
      group_by(condition) %>%
      filter(n() >= 3) %>%
      ungroup()
    if(nrow(df_nonNDF) == 0) {
      cat("No non-NDF conditions have >= 3 data points.\n")
      return()
    }
    results <- data.frame(
      condition = character(),
      n_points  = numeric(),
      linear_r2 = numeric(),
      log_r2    = numeric(),
      stringsAsFactors = FALSE
    )
    for(cond in unique(df_nonNDF$condition)) {
      dfc <- df_nonNDF %>% filter(condition == cond)
      npts <- nrow(dfc)
      r2_lin <- NA
      r2_log <- NA
      if(input$doLinear) {
        lin_mod <- lm(length_um ~ time_min, data = dfc)
        r2_lin <- summary(lin_mod)$r.squared
      }
      if(input$doLog) {
        dfc_log <- dfc %>% filter(time_min > 0)
        if(nrow(dfc_log) >= 3) {
          log_mod <- lm(length_um ~ I(log(time_min)), data = dfc_log)
          r2_log <- summary(log_mod)$r.squared
        }
      }
      results <- rbind(results, data.frame(
        condition = cond,
        n_points  = npts,
        linear_r2 = r2_lin,
        log_r2    = r2_log,
        stringsAsFactors = FALSE
      ))
    }
    print(as.data.frame(results), row.names = FALSE)
  })

  # 7) Statistical tests: t-test and chi-square (or Fisher) comparing each non-NDF condition to NDF control
  output$statTestsOutput <- renderPrint({
    df <- df_filtered()
    if(nrow(df) == 0) {
      cat("No data found for this week.\n")
      return()
    }
    ndf_data <- df %>% filter(toupper(condition) == "NDF")
    if(nrow(ndf_data) < 1) {
      cat("No NDF control data available.\n")
      return()
    }
    ndf_mean <- mean(ndf_data$length_um, na.rm = TRUE)
    
    # For each non-NDF condition, perform t-test and chi-square test comparing to NDF
    df_nonNDF <- df %>% filter(toupper(condition) != "NDF")
    if(nrow(df_nonNDF) < 1) {
      cat("No non-NDF data available for tests.\n")
      return()
    }
    
    test_results <- data.frame(
      condition = character(),
      n_points  = numeric(),
      t_test_p  = numeric(),
      chisq_p   = numeric(),
      test_used = character(),
      stringsAsFactors = FALSE
    )
    
    for(cond in unique(df_nonNDF$condition)) {
      dfc <- df_nonNDF %>% filter(condition == cond)
      npts <- nrow(dfc)
      # Only perform tests if both groups have at least 2 replicates
      if(nrow(ndf_data) < 2 || npts < 2) next
      
      # t-test: compare means of condition vs. NDF
      t_res <- try(t.test(dfc$length_um, ndf_data$length_um), silent = TRUE)
      t_p <- if(inherits(t_res, "try-error")) NA else t_res$p.value
      
      # Chi-square or Fisher test:
      # Categorize each replicate as "Above" or "Below" the NDF mean.
      cat_cond <- ifelse(dfc$length_um >= ndf_mean, "Above", "Below")
      cat_ndf <- ifelse(ndf_data$length_um >= ndf_mean, "Above", "Below")
      
      tab <- table(
        Group = c(rep("Condition", length(cat_cond)), rep("NDF", length(cat_ndf))),
        Category = c(cat_cond, cat_ndf)
      )
      
      # If any expected frequency < 5, use Fisher's exact test.
      chisq_res <- try(chisq.test(tab), silent = TRUE)
      if(inherits(chisq_res, "try-error") || any(chisq_res$expected < 5)) {
        test_res <- try(fisher.test(tab), silent = TRUE)
        chi_p <- if(inherits(test_res, "try-error")) NA else test_res$p.value
        test_used <- "Fisher"
      } else {
        chi_p <- chisq_res$p.value
        test_used <- "Chi-square"
      }
      
      test_results <- rbind(test_results, data.frame(
        condition = cond,
        n_points  = npts,
        t_test_p  = t_p,
        chisq_p   = chi_p,
        test_used = test_used,
        stringsAsFactors = FALSE
      ))
    }
    
    if(nrow(test_results) == 0) {
      cat("No valid statistical tests could be performed.\n")
    } else {
      print(as.data.frame(test_results), row.names = FALSE)
    }
  })

  # 8) Main plot
  output$scatterPlot <- renderPlot({
    df <- df_filtered()
    if(nrow(df) == 0) {
      plot.new()
      title("No data for this selection.")
      return()
    }
    df_ndf <- df %>% filter(toupper(condition) == "NDF")
    df_others <- df %>% filter(toupper(condition) != "NDF")
    
    p <- ggplot() +
      geom_point(
        data = df_ndf,
        aes(x = time_min, y = length_um, color = condition),
        size = 3
      ) +
      geom_point(
        data = df_others,
        aes(x = time_min, y = length_um, color = condition),
        size = 2, alpha = 0.6
      )
    
    if(input$showLines) {
      p <- p + geom_line(
        data = df_others,
        aes(x = time_min, y = length_um, color = condition,
            group = interaction(condition, replicate)),
        alpha = 0.4
      )
    }
    
    if(input$showErrorBars) {
      df_eb <- df_others %>%
        group_by(condition, time_min) %>%
        summarise(
          mean_len = mean(length_um),
          se_len   = sd(length_um)/sqrt(n()),
          .groups  = "drop"
        )
      p <- p +
        geom_errorbar(
          data = df_eb,
          aes(x = time_min, ymin = mean_len - se_len, ymax = mean_len + se_len, color = condition),
          width = 1
        ) +
        geom_line(
          data = df_eb,
          aes(x = time_min, y = mean_len, color = condition),
          size = 0.8
        ) +
        geom_point(
          data = df_eb,
          aes(x = time_min, y = mean_len, color = condition),
          size = 2
        )
    }
    
    # Prepare prediction grid
    if(nrow(df_others) > 0) {
      tmin <- min(df_others$time_min, na.rm = TRUE)
      tmax <- max(df_others$time_min, na.rm = TRUE)
      if(tmax > tmin) {
        df_pred <- data.frame(time_min = seq(tmin, tmax, length.out = 100))
      }
    }
    
    if(exists("df_pred") && nrow(df_pred) > 1) {
      conds_nonNDF <- unique(df_others$condition)
      pred_lines <- data.frame()
      for(cond in conds_nonNDF) {
        dfc <- df_others %>% filter(condition == cond)
        if(nrow(dfc) < 3) next
        # Linear predictions
        if(input$doLinear) {
          lin_mod <- lm(length_um ~ time_min, data = dfc)
          df_linpred <- df_pred
          df_linpred$condition <- cond
          df_linpred$pred_lin <- predict(lin_mod, newdata = df_linpred)
          pred_lines <- rbind(pred_lines, df_linpred)
        }
        # Logarithmic predictions (exclude time_min <= 0)
        if(input$doLog) {
          dfc_log <- dfc %>% filter(time_min > 0)
          if(nrow(dfc_log) >= 3) {
            log_mod <- lm(length_um ~ I(log(time_min)), data = dfc_log)
            df_logpred <- df_pred %>% filter(time_min > 0)
            if(nrow(df_logpred) > 0) {
              df_logpred$condition <- cond
              df_logpred$pred_log <- predict(log_mod, newdata = df_logpred)
              pred_lines <- rbind(pred_lines, df_logpred)
            }
          }
        }
      }
      if(nrow(pred_lines) > 0) {
        if("pred_lin" %in% names(pred_lines)) {
          p <- p + geom_line(
            data = pred_lines,
            aes(x = time_min, y = pred_lin, color = condition),
            linetype = "dashed"
          )
        }
        if("pred_log" %in% names(pred_lines)) {
          p <- p + geom_line(
            data = pred_lines,
            aes(x = time_min, y = pred_log, color = condition),
            linetype = "dotdash"
          )
        }
      }
    }
    
    p <- p + scale_y_continuous(limits = c(0, NA)) +
      labs(
        x = "Time (min)",
        y = "Flagellar Length (µm)",
        title = paste("Week", input$weekChoice, "Flagellar Regrowth: Linear & Logarithmic Fits")
      ) +
      theme_minimal()
    
    print(p)
  }, height = 600)
}

shinyApp(ui = ui, server = server)
