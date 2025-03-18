# app.R
library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Researcher Variation Analysis"),

  sidebarLayout(
    sidebarPanel(
      h4("About this analysis"),
      p("Comparing Week 1 (2C, R1) vs Week 2 (C, R2), which have nearly identical densities."),
      p("We include time as a factor and check if there's a systematic difference in length by researcher.")
    ),
    mainPanel(
      h3("Boxplot: Length by Researcher (faceted by Time)"),
      plotOutput("boxPlot"),

      h3("ANOVA Summary"),
      verbatimTextOutput("anovaOutput"),

      h3("Optional T-test (ignoring Time)"),
      verbatimTextOutput("ttestOutput")
    )
  )
)

server <- function(input, output, session) {

  # 1) Connect to the SQLite database
  con <- dbConnect(RSQLite::SQLite(), "flagella_data.sqlite")
  onStop(function() { dbDisconnect(con) })  # close DB when app stops

  # 2) Read the entire table
  df_raw <- dbGetQuery(con, "SELECT * FROM flagella_measurements")

  # 3) Filter to the two groups of interest: 
  #    - Week 1, condition='2C', researcher=1
  #    - Week 2, condition='C',  researcher=2
  #    Then convert 'researcher' to factor, and create 'time_min_factor'.
  df_filtered <- df_raw %>%
    filter(
      (week == 1 & condition == "2C" & researcher == 1) |
      (week == 2 & condition == "C"  & researcher == 2)
    ) %>%
    filter(!is.na(length_um)) %>%
    mutate(
      researcher = factor(researcher),
      time_min_factor = factor(time_min)
    )

  # 4) ANOVA: length ~ researcher + time_min_factor
  output$anovaOutput <- renderPrint({
    if (nrow(df_filtered) == 0) {
      cat("No data available after filtering. Check your conditions.\n")
      return()
    }
    mod <- aov(length_um ~ researcher + time_min_factor, data = df_filtered)
    summary(mod)
  })

  # 5) T-test ignoring time
  output$ttestOutput <- renderPrint({
    if (nrow(df_filtered) == 0) {
      cat("No data available for T-test.\n")
      return()
    }
    t.test(length_um ~ researcher, data = df_filtered)
  })

  # 6) Plot: boxplot by researcher, facet by time
  output$boxPlot <- renderPlot({
    if (nrow(df_filtered) == 0) {
      plot.new()
      title("No data available to plot.")
      return()
    }

    ggplot(df_filtered, aes(x = researcher, y = length_um, fill = researcher)) +
      geom_boxplot(outlier.shape = 21) +
      facet_wrap(~ time_min_factor, nrow = 2) +
      labs(
        x = "Researcher",
        y = "Flagellar Length (µm)",
        title = "Distribution of Flagellar Lengths by Researcher and Time"
      ) +
      theme_minimal()
  })

}

shinyApp(ui = ui, server = server)
