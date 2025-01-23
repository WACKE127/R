library(ggplot2)
library(shiny)
library(rlang)

predictor_sets <- list(
  c("Sepal.Length", "Sepal.Width"),
  c("Petal.Length", "Petal.Width"),
  c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
)

set.seed(123)
iris_data <- iris
all_plots <- list()

for (preds in predictor_sets) {
  subname <- paste(preds, collapse = ", ")
  df <- iris_data[, c(preds, "Species")]
  
  best_k <- NA
  best_acc <- -Inf
  
  for (k_val in 1:40) {
    # Increase iter.max to reduce "did not converge" warnings
    km <- kmeans(df[, preds], centers = k_val, nstart = 20, iter.max = 200)
    clust <- km$cluster
    mapping <- character(k_val)
    for (c_i in 1:k_val) {
      sp <- df$Species[clust == c_i]
      mapping[c_i] <- names(which.max(table(sp)))
    }
    pred_sp <- sapply(clust, function(x) mapping[x])
    acc <- mean(pred_sp == df$Species)
    if (acc > best_acc) {
      best_acc <- acc
      best_k <- k_val
    }
  }
  
  km_best <- kmeans(df[, preds], centers = best_k, nstart = 20, iter.max = 200)
  df$ClusterBest <- factor(km_best$cluster)
  
  km_3 <- kmeans(df[, preds], centers = 3, nstart = 20, iter.max = 200)
  clust_3 <- km_3$cluster
  map_3 <- character(3)
  for (c_i in 1:3) {
    sp <- df$Species[clust_3 == c_i]
    map_3[c_i] <- names(which.max(table(sp)))
  }
  pred_sp_3 <- sapply(clust_3, function(x) map_3[x])
  acc_3 <- mean(pred_sp_3 == df$Species)
  df$Cluster3 <- factor(km_3$cluster)
  
  cat("Subset:", subname, "| Best k =", best_k, "| Best-k Accuracy ~", best_acc,
      "| k=3 Accuracy ~", acc_3, "\n")
  
  # Tidy evaluation so we don't use aes_string
  x_var <- sym(preds[1])
  y_var <- sym(preds[length(preds)])
  
  p_species <- ggplot(df) +
    geom_point(aes(x = !!x_var, y = !!y_var, color = Species), size = 2) +
    ggtitle(paste0("Predictors: ", subname, " (True Species)")) +
    theme_minimal()
  
  p_best <- ggplot(df) +
    geom_point(aes(x = !!x_var, y = !!y_var, color = ClusterBest), size = 2) +
    ggtitle(paste0("k=", best_k, " (", subname, ")")) +
    theme_minimal()
  
  p_three <- ggplot(df) +
    geom_point(aes(x = !!x_var, y = !!y_var, color = Cluster3), size = 2) +
    ggtitle(paste0("k=3 (", subname, ")")) +
    theme_minimal()
  
  all_plots[[subname]] <- list(species = p_species, best = p_best, three = p_three)
}

tabs_list <- lapply(names(all_plots), function(sname) {
  tabPanel(sname,
    plotOutput(paste0("plot_species_", sname)),
    plotOutput(paste0("plot_best_", sname)),
    plotOutput(paste0("plot_three_", sname))
  )
})

ui <- fluidPage(do.call(tabsetPanel, tabs_list))

server <- function(input, output) {
  for (sname in names(all_plots)) {
    local({
      sn <- sname
      output[[paste0("plot_species_", sn)]] <- renderPlot({ all_plots[[sn]]$species })
      output[[paste0("plot_best_", sn)]]    <- renderPlot({ all_plots[[sn]]$best })
      output[[paste0("plot_three_", sn)]]   <- renderPlot({ all_plots[[sn]]$three })
    })
  }
}

shinyApp(ui, server)
