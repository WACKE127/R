###########################################################
#    ALL-IN-ONE R SCRIPT FOR FLAGELLAR REGENERATION DATA   #
###########################################################

# 1) LOAD REQUIRED PACKAGES
# If you don't already have tidyverse installed, uncomment:
# install.packages("tidyverse")
library(tidyverse)

# 2) READ IN YOUR COMBINED CSV
# Replace "flagella_data.csv" with the actual CSV file name/path.
flagella_wide <- read_csv("flagella_data.xlsx")

# 3) RESHAPE FROM WIDE TO LONG FORMAT
# Assumes your CSV columns are something like:
# Replicate,Time,NonDeflagellated,TAPonly,TAPcolchicine,TAPcyclohexamide,TAPactinomycin
flagella_long <- flagella_wide %>%
  pivot_longer(
    cols = c("NonDeflagellated", 
             "TAPonly", 
             "TAPcolchicine", 
             "TAPcyclohexamide", 
             "TAPactinomycin"),
    names_to = "Condition",
    values_to = "FlagellaLength"
  )

# 4) COMPUTE THE MEAN FOR EACH TIME AND CONDITION
flagella_means <- flagella_long %>%
  group_by(Time, Condition) %>%
  summarize(MeanLength = mean(FlagellaLength, na.rm = TRUE)) %>%
  ungroup()

# 5) PLOT WITH GGPLOT2
p <- ggplot(flagella_means, 
            aes(x = Time, 
                y = MeanLength, 
                color = Condition)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  # Make sure X is numeric with appropriate breaks
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 75, 90)) +
  # Label axes
  labs(
    title = "Flagellar Regeneration Over Time in Chlamydomonas",
    x = "Time (minutes)",
    y = "Flagellar length (µm)"
  ) +
  # Use a clean, readable theme
  theme_minimal(base_size = 14) +
  # Center the title, remove legend title
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_blank()
  )

# 6) DISPLAY THE PLOT
print(p)

# 7) (OPTIONAL) SAVE THE PLOT TO A FILE
ggsave("flagella_regeneration_plot.png", plot = p, width = 8, height = 6)

###########################################################
#           END OF ALL-IN-ONE ANALYSIS SCRIPT             #
###########################################################
