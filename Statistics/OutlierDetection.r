library(dplyr)
library(ggcorrplot)
library("FactoMineR")
library('corrr')
library(factoextra)
library(rstatix)

dataset <- read.csv("Statistics/test/TempDatasetPro10.csv")

filtered_data <- dataset %>%
  filter(!(Group == "MCI" & Age > 70))

table <- filtered_data[, 1046:1080]

out <- boxplot.stats(table$DEDE)$out
boxplot(table$DEDE,
  ylab = "DEDE",
  main = "Boxplot of DEDE"
)
mtext(paste("Outliers: ", paste(out, collapse = ", ")))

out <- boxplot.stats(table$DEDE)$out
out_ind <- which(table$DEDE %in% c(out))
print(out_ind)
