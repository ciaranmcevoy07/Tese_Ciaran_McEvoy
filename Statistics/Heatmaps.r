library(ggplot2)
library(reshape2)
library(heatmaply)

Table_LC_SR <- read.csv("Statistics/test/Table_LC_SR.csv")
Table_LC_SR_AD <- read.csv("Statistics/test/Table_LC_SR_AD.csv")
Table_LC_SR_CN <- read.csv("Statistics/test/Table_LC_SR_CN.csv")
Table_LC_SR_MCI <- read.csv("Statistics/test/Table_LC_SR_MCI.csv")

####### Lipid Classes x Brain Regions for all types of Patients ####################
# data_long <- melt(Table_LC_SR, id.vars = 'Lipids', variable.name = 'Region', value.name = 'Value')
# data_long$Color <- ifelse(data_long$Value > 0.05, "red", ifelse(data_long$Value < -0.05, "blue", "grey"))

# data_long$Label <- ifelse(data_long$Value > 0.05, "> 0.05", ifelse(data_long$Value < -0.05, "< -0.05", "Insignificant"))

# p1 <- ggplot(data_long, aes(x = Lipids, y = Region, fill = Color)) + 
#   geom_tile() +
#   scale_fill_manual(
#     values = c("red" = "red", "blue" = "blue", "grey" = "grey"),
#     name = "Value",
#     labels = c("red" = "> 0.05", "blue" = "< -0.05", "grey" = "Insignificant")
#   ) +
#   labs(x = 'Lipids', y = 'Brain Region') +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   ggtitle('Lipid Classes x Brain Regions for all types of Patients')

# ######## Lipid Classes x Brain Regions for AD Patients ####################
# data_long_AD <- melt(Table_LC_SR_AD, id.vars = 'Lipids', variable.name = 'Region', value.name = 'Value')
# data_long_AD$Color <- ifelse(data_long_AD$Value > 0.05, "red", ifelse(data_long_AD$Value < -0.05, "blue", "grey"))

# data_long_AD$Label <- ifelse(data_long_AD$Value > 0.05, "> 0.05", ifelse(data_long_AD$Value < -0.05, "< -0.05", "Insignificant"))

# p2 <- ggplot(data_long_AD, aes(x = Lipids, y = Region, fill = Color)) + 
#   geom_tile() +
#   scale_fill_manual(
#     values = c("red" = "red", "blue" = "blue", "grey" = "grey"),
#     name = "Value",
#     labels = c("red" = "> 0.05", "blue" = "< -0.05", "grey" = "Insignificant")
#   ) +
#   labs(x = 'Lipids', y = 'Brain Region') +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   ggtitle('Lipid Classes x Brain Regions AD Patients')

# ######## Lipid Classes x Brain Regions for CN Patients ####################
# data_long_CN <- melt(Table_LC_SR_CN, id.vars = 'Lipids', variable.name = 'Region', value.name = 'Value')
# data_long_CN$Color <- ifelse(data_long_CN$Value > 0.05, "red", ifelse(data_long_CN$Value < -0.05, "blue", "grey"))

# data_long_CN$Label <- ifelse(data_long_CN$Value > 0.05, "> 0.05", ifelse(data_long_CN$Value < -0.05, "< -0.05", "Insignificant"))

# p3 <- ggplot(data_long_CN, aes(x = Lipids, y = Region, fill = Color)) + 
#   geom_tile() +
#   scale_fill_manual(
#     values = c("red" = "red", "blue" = "blue", "grey" = "grey"),
#     name = "Value",
#     labels = c("red" = "> 0.05", "blue" = "< -0.05", "grey" = "Insignificant")
#   ) +
#   labs(x = 'Lipids', y = 'Brain Region') +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   ggtitle('Lipid Classes x Brain Regions for CN Patients')

# ######## Lipid Classes x Brain Regions for MCI Patients ####################
# data_long_MCI <- melt(Table_LC_SR_MCI, id.vars = 'Lipids', variable.name = 'Region', value.name = 'Value')
# data_long_MCI$Color <- ifelse(data_long_MCI$Value > 0.05, "red", ifelse(data_long_MCI$Value < -0.05, "blue", "grey"))

# data_long_MCI$Label <- ifelse(data_long_MCI$Value > 0.05, "> 0.05", ifelse(data_long_MCI$Value < -0.05, "< -0.05", "Insignificant"))

# p4 <- ggplot(data_long_MCI, aes(x = Lipids, y = Region, fill = Color)) + 
#   geom_tile() +
#   scale_fill_manual(
#     values = c("red" = "red", "blue" = "blue", "grey" = "grey"),
#     name = "Value",
#     labels = c("red" = "> 0.05", "blue" = "< -0.05", "grey" = "Insignificant")
#   ) +
#   labs(x = 'Lipids', y = 'Brain Region') +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   ggtitle('Lipid Classes x Brain Regions for MCI Patients')

#####################
data_long_MCI <- melt(Table_LC_SR_MCI, id.vars = 'Lipids', variable.name = 'Region', value.name = 'Value')
data_long_MCI$Color <- ifelse(data_long_MCI$Value > 0.05, "red", ifelse(data_long_MCI$Value < -0.05, "blue", "grey"))

# Create the plot with a color gradient
p4 <- ggplot(data_long_MCI, aes(x = Lipids, y = Region, fill = Color)) + 
  geom_tile() +
  scale_fill_manual(
    values = c("red" = "red", "blue" = "blue", "grey" = "grey"),
    name = "Value",
    labels = c("red" = "> 0.05", "blue" = "< -0.05", "grey" = "Insignificant")
  ) +
  labs(x = 'Lipids', y = 'Brain Region') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('Lipid Classes x Brain Regions for MCI Patients')

print(p7)