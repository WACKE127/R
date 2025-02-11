library(shiny)
library(tidyverse)

ui <- fluidPage(
  titlePanel("Regeneration Analysis"),
  sidebarLayout(
    sidebarPanel(
      h4("Experimental Overview"),
      p("In today's experiment, each participant measured cellular regeneration in two aliquots of cells:"),
      tags$ul(
        tags$li("Control cells: Measured only at 0 and 90 minutes."),
        tags$li("Experimental cells under various conditions:"),
        tags$ul(
          tags$li("Standard medium – the normal growth medium."),
          tags$li("Standard + 2 mg/ml colchicine – a microtubule polymerization inhibitor."),
          tags$li("Standard + 10 µg/ml cycloheximide – an inhibitor of translation."),
          tags$li("Standard + 50 µg/ml actinomycin D – an inhibitor of transcription.")
        )
      ),
      p("Cells were treated via a pH shock, resuspended in the appropriate medium, and sampled at 0, 10, 20, 30, 40, 50, 60, 75, and 90 minutes. For each time point, measurements from 10 individual cells were averaged."),
      p("The graph spacing accurately reflects the differences in sampling times.")
    ),
    mainPanel(
      plotOutput("analysisPlot", height = "500px"),
      br(),
      p(
        strong("Figure Caption:"), 
        "Regeneration Over Time. Mean measurement (µm) is plotted against time (min) for cells under various experimental conditions. Measurements were obtained by averaging values from 10 individual cells per sample. Control cells were measured at 0 and 90 minutes only, while experimental cells were sampled at 0, 10, 20, 30, 40, 50, 60, 75, and 90 minutes. The y-axis is fixed at a maximum of 10.0 µm for consistency, and the graph is scaled to reflect the true time intervals between sampling points."
      )
    )
  )
)

server <- function(input, output, session) {
  data_wide <- read_csv("flagella_data.csv") %>%
    rename(Time = `Time point (min)`)
  
  data_long <- data_wide %>%
    pivot_longer(
      cols = c("Non-deflagellated", 
               "TAP only", 
               "TAP + 2mg/ml colchicine", 
               "TAP + 10 ug/ml cyclohexamide", 
               "TAP + 50 ug/ml actinomycin D"),
      names_to = "Condition",
      values_to = "Measurement"
    ) %>%
    mutate(Condition = recode(
      Condition,
      "Non-deflagellated" = "Control",
      "TAP only" = "Standard",
      "TAP + 2mg/ml colchicine" = "Standard + Colchicine",
      "TAP + 10 ug/ml cyclohexamide" = "Standard + Cycloheximide",
      "TAP + 50 ug/ml actinomycin D" = "Standard + Actinomycin D"
    ))
  
  data_summary <- data_long %>%
    group_by(Time, Condition) %>%
    summarize(MeanMeasurement = mean(Measurement, na.rm = TRUE)) %>%
    ungroup()
  
  chart_plot <- ggplot(data_summary, aes(x = Time, y = MeanMeasurement, color = Condition)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 75, 90)) +
    scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, 1)) +
    labs(x = "Time (minutes)", y = "Measurement (µm)") +
    theme_minimal(base_size = 14) +
    theme(legend.title = element_blank())
  
  output$analysisPlot <- renderPlot({
    chart_plot
  })
}

shinyApp(ui = ui, server = server)
