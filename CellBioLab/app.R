# app.R

library(shiny)
library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)

# 1. Load & Summarise Data ----------------------------------------------------
con <- dbConnect(RSQLite::SQLite(), "flagella_data.sqlite")
measurements <- tbl(con, "flagella_measurements") %>% collect()
dbDisconnect(con)

summary_data <- measurements %>%
  group_by(condition, time_min) %>%
  summarize(
    mean_length = mean(length_um, na.rm = TRUE),
    sd_length   = sd(length_um,   na.rm = TRUE),
    n_obs       = n(),
    se_length   = sd_length / sqrt(n_obs),
    .groups = "drop"
  )

conditions <- summary_data %>% distinct(condition) %>% pull(condition)


# 2. UI ------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Flagella Regeneration Curves"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId  = "conds",
        label    = "Select conditions:",
        choices  = conditions,
        selected = conditions
      )
    ),
    mainPanel(
      plotOutput("regenPlot", height = "600px")
    )
  )
)


# 3. Server --------------------------------------------------------------------
server <- function(input, output, session) {
  # subset data to selected conditions
  df_sel <- reactive({
    req(input$conds)
    summary_data %>% filter(condition %in% input$conds)
  })

  output$regenPlot <- renderPlot({
    df <- df_sel()

    ggplot(df, aes(x = time_min, y = mean_length, color = condition)) +
      # mean line + points
      geom_line(size = 1) +
      geom_point(size = 2) +
      # error bars at measured times
      geom_errorbar(aes(ymin = mean_length - se_length,
                        ymax = mean_length + se_length),
                    width = 3,
                    size  = 0.8) +
      # only show actual measurement times on x-axis
      scale_x_continuous(
        breaks = sort(unique(summary_data$time_min)),
        labels = sort(unique(summary_data$time_min))
      ) +
      labs(
        x     = "Time after deflagellation (min)",
        y     = "Mean flagella length (µm)",
        title = "Combined Flagella Regeneration Across Conditions",
        color = "Condition"
      ) +
      theme_minimal(base_size = 16) +
      theme(
        plot.title      = element_text(face = "bold", size = 20),
        legend.position = "bottom"
      )
  })
}


# 4. Run -----------------------------------------------------------------------
shinyApp(ui, server)
