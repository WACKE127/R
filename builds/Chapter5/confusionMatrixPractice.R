library(caret)

setwd("../../mlba/mlba/data-raw")
owner.df <- read.csv("ownerExample.csv.gz")

confusionMatrix(factor(ifelse(owner.df$Probability>0.5,
                      "owner",
                      "nonowner")),
                      owner.df$Class)$table
                      
confusionMatrix(factor(ifelse(owner.df$Probability>0.25,
                      "owner",
                      "nonowner")),
                      owner.df$Class)$table

confusionMatrix(factor(ifelse(owner.df$Probability>0.75,
                      "owner",
                      "nonowner")),
                      owner.df$Class)$table