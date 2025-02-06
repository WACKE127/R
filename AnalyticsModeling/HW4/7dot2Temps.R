library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
#Make sure to install cmake and nloptr to install this package
library(smooth)

# There is a lot of code here. Don't panic! It's a shiny webapp that utilizes CUSUM and Linear Regression methods
# to perform rudimentary data analysis predicting whether the end of summer is getting further into the year.

# The backbone for this program is my solution for last week's HW3.

# Reproducibility
set.seed(27)

ui <- fluidPage(
  titlePanel("Atlanta Temperature CUSUM Analysis (1996-2015)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selectedYear", "Select a Year:", choices = 1996:2015, selected = 2000),
      fluidRow(
        column(12, strong("Data Smoothing (Optional):")),
        column(12,
               selectInput("smoothingMethod", "Data Smoothing Method:",
                           choices = c("None", "es HoltWinters"),
                           selected = "None")
        )
      ),
      helpText("Reads daily high temperatures from data/temps.txt for July 1 - Oct 31, 1996-2015.
                A per-year CUSUM identifies the day each year cools off.
                A global CUSUM checks long-term warming.")
    ),
    mainPanel(
      plotOutput("yearlyTempPlot", height = "300px"),
      plotOutput("yearlyCUSUMPlot", height = "300px"),
      h4("Figure Caption (Selected Year):"),
      uiOutput("figureCaption"),
      hr(),
      plotOutput("yearlyAvgPlot", height = "300px"),
      h4("Overall Trend Commentary (Linear Regression):"),
      uiOutput("trendCaption"),
      hr(),
      plotOutput("overallCUSUMPlot", height = "400px"),
      h4("Full-Daily CUSUM Commentary:"),
      uiOutput("overallCUSUMCaption")
    )
  )
)

server <- function(input, output, session) {
  rawData <- reactive({
    read.table("data/temps.txt", header = TRUE, check.names = FALSE, sep = "\t")
  })
  
  tidyData <- reactive({
    df <- rawData()
    df_long <- df %>%
      pivot_longer(cols = -DAY, names_to = "Year", values_to = "Temp") %>%
      mutate(Year = as.numeric(Year))
    df_long
  })

  # Per-year smoothing with 'es' HoltWinters when selected
  smoothedData <- reactive({
    df_long <- tidyData()
    if (input$smoothingMethod == "None") {
      return(df_long)
    }
    # Split data by Year, apply smoothing to each subset
    year_list <- split(df_long, df_long$Year)
    smoothed_list <- lapply(year_list, function(subdf) {
      # Convert daily temps to a time series with frequency=1 (no seasonal component)
      tsData <- ts(subdf$Temp, frequency = 1)
      # Use es::es HoltWinters (trend only, no season) with model="ZZN"
      es_model <- es(tsData, model = "ZZN", silent = TRUE)
      subdf$Temp <- as.numeric(es_model$fitted)
      subdf
    })
    do.call(rbind, smoothed_list)
  })
  
  cusumData <- reactive({
    df_long <- smoothedData()
    df_cusum <- df_long %>%
      group_by(Year) %>%
      mutate(
        meanYear = mean(Temp, na.rm = TRUE),
        dev = Temp - meanYear,
        CUSUM = cumsum(dev)
      ) %>%
      ungroup() %>%
      group_by(Year) %>%
      mutate(
        maxCUSUM = max(CUSUM, na.rm = TRUE),
        dayOfMaxCUSUM = DAY[which.max(CUSUM)]
      ) %>%
      ungroup()
    df_cusum
  })
  
  singleYearData <- reactive({
    cusumData() %>% filter(Year == input$selectedYear)
  })
  
  output$yearlyTempPlot <- renderPlot({
    df <- singleYearData()
    coolOffDay <- unique(df$dayOfMaxCUSUM)
    df$DAY <- factor(df$DAY, levels = unique(df$DAY))
    ggplot(df, aes(x = DAY, y = Temp, group = 1)) +
      geom_line(color = "darkblue") +
      geom_point() +
      geom_vline(xintercept = which(levels(df$DAY) == coolOffDay), color = "red", linetype = "dashed") +
      labs(title = paste0("Daily High Temps for ", input$selectedYear),
           x = "Day (July 1 - Oct 31)",
           y = "Temperature (F)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$yearlyCUSUMPlot <- renderPlot({
    df <- singleYearData()
    coolOffDay <- unique(df$dayOfMaxCUSUM)
    df$DAY <- factor(df$DAY, levels = unique(df$DAY))
    ggplot(df, aes(x = DAY, y = CUSUM, group = 1)) +
      geom_line(color = "darkgreen") +
      geom_point() +
      geom_vline(xintercept = which(levels(df$DAY) == coolOffDay), color = "red", linetype = "dashed") +
      labs(title = paste0("CUSUM for ", input$selectedYear),
           x = "Day",
           y = "Cumulative Sum of (Temp - Mean)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$figureCaption <- renderUI({
    df <- singleYearData()
    coolOffDay <- unique(df$dayOfMaxCUSUM)
    meanTemp <- round(unique(df$meanYear), 1)
    HTML(paste0(
      "For ", input$selectedYear, ", the average July-Oct daily high was ~", meanTemp, 
      " F. A per-year CUSUM was cumsum(Temp - ", meanTemp, 
      "). The maximum CUSUM (red dashed line) indicates 'unofficial summer ends' after that day. 
       Detected end-of-summer: <b>", coolOffDay, "</b>."
    ))
  })
  
  output$yearlyAvgPlot <- renderPlot({
    df <- cusumData() %>%
      group_by(Year) %>%
      summarize(meanTemp = mean(Temp, na.rm = TRUE))
    ggplot(df, aes(x = Year, y = meanTemp)) +
      geom_point(size = 2) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
      labs(title = "Mean Daily High (July 1 - Oct 31), 1996-2015",
           x = "Year",
           y = "Avg. Daily High (F)") +
      theme_minimal()
  })
  
  output$trendCaption <- renderUI({
    df <- cusumData() %>%
      group_by(Year) %>%
      summarize(meanTemp = mean(Temp, na.rm = TRUE))
    mod <- lm(meanTemp ~ Year, data = df)
    slope <- round(coef(mod)["Year"], 3)
    pval <- summary(mod)$coefficients["Year", "Pr(>|t|)"]
    HTML(paste0(
      "Linear regression slope: <b>", slope, " deg/year</b>, p-value = ", round(pval, 5), ". 
       If positive and significant (p < 0.05), suggests warming over 1996–2015."
    ))
  })
  
  allDailyData <- reactive({
    df_long <- smoothedData()
    df_long <- df_long %>%
      mutate(
        DayYearStr = paste(DAY, Year),
        Date = as.Date(DayYearStr, format = "%d-%b %Y")
      ) %>%
      arrange(Date)
    df_long
  })
  
  allDailyCUSUMData <- reactive({
    df_all <- allDailyData()
    globalMean <- mean(df_all$Temp, na.rm = TRUE)
    df_all <- df_all %>%
      mutate(dev_global = Temp - globalMean,
             CUSUM_all = cumsum(dev_global))
    df_all
  })
  
  output$overallCUSUMPlot <- renderPlot({
    df <- allDailyCUSUMData()
    ggplot(df, aes(x = Date, y = CUSUM_all)) +
      geom_line(color = "purple") +
      labs(title = "Full-Daily CUSUM of (Temp - GlobalMean)",
           subtitle = "July 1 - Oct 31, 1996–2015",
           x = "Date",
           y = "CUSUM of (Temp - Overall Mean)") +
      theme_minimal()
  })
  
  output$overallCUSUMCaption <- renderUI({
    df <- allDailyCUSUMData()
    globalMean <- mean(df$Temp, na.rm = TRUE)
    idxMax <- which.max(df$CUSUM_all)
    idxMin <- which.min(df$CUSUM_all)
    dateMax <- df$Date[idxMax]
    dateMin <- df$Date[idxMin]
    HTML(paste0(
      "This chart shows cumsum(Temp - ", round(globalMean, 2), 
      ") across all days (1996–2015). Max CUSUM near ", dateMax, ", min near ", dateMin, 
      ". Significant upward drift indicates warming."
    ))
  })
}

shinyApp(ui = ui, server = server)
