library(shiny)
library(FrF2)

ui <- fluidPage(
  titlePanel("Fractional Factorial Design for House Features"),
  p("Each row represents a fictitious house. The columns (A to J) represent 10 yes/no features.
    '1' indicates the feature is included; '-1' indicates it is not included."),
  tableOutput("designTable")
)

server <- function(input, output) {
  output$designTable <- renderTable({
    # Generate a 16-run fractional factorial design for 10 factors
    # The generators below set:
    # E = A*B, F = A*C, G = A*D, H = B*C, I = B*D, J = C*D.
    design <- FrF2(16, 10, generators = c("AB", "AC", "AD", "BC", "BD", "CD"))
    
    # Convert design to a data frame and select only the first 10 columns (the factors)
    design_matrix <- as.data.frame(design)
    design_matrix[, 1:10]
  })
}

# Run the Shiny app.
shinyApp(ui, server)
