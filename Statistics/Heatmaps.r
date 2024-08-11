library(ggplot2)
library(reshape2)
library(heatmaply)

Table_LC_SR <- read.csv("Statistics/test/Table_LC_SR.csv")
Table_LC_SR_AD <- read.csv("Statistics/test/Table_LC_SR_AD.csv")
Table_LC_SR_CN <- read.csv("Statistics/test/Table_LC_SR_CN.csv")
Table_LC_SR_MCI <- read.csv("Statistics/test/Table_LC_SR_MCI.csv")

####### Lipid Classes x Brain Regions for all types of Patients ####################
# data_long <- melt(Table_LC_SR, id.vars = 'Lipids', variable.name = 'Region', value.name = 'Value')
# data_long$Value <- ifelse(data_long$Value > 0.05, 
#                               data_long$Value, 
#                               ifelse(data_long$Value < -0.05, 
#                                      data_long$Value, 
#                                      0))

# ######## Lipid Classes x Brain Regions for AD Patients ####################
# data_long <- melt(Table_LC_SR_AD, id.vars = 'Lipids', variable.name = 'Region', value.name = 'Value')
# data_long$Value <- ifelse(data_long$Value > 0.05, 
#                               data_long$Value, 
#                               ifelse(data_long$Value < -0.05, 
#                                      data_long$Value, 
#                                      0))

# ######## Lipid Classes x Brain Regions for CN Patients ####################
# data_long <- melt(Table_LC_SR_CN, id.vars = 'Lipids', variable.name = 'Region', value.name = 'Value')
# data_long$Value <- ifelse(data_long$Value > 0.05, 
#                               data_long$Value, 
#                               ifelse(data_long$Value < -0.05, 
#                                      data_long$Value, 
#                                      0))

# ######## Lipid Classes x Brain Regions for MCI Patients ####################
data_long <- melt(Table_LC_SR_MCI, id.vars = 'Lipids', variable.name = 'Region', value.name = 'Value')
data_long$Value <- ifelse(data_long$Value > 0.05, 
                              data_long$Value, 
                              ifelse(data_long$Value < -0.05, 
                                     data_long$Value, 
                                     0))

#################################################################################################

# Create the plot with a color gradient
p <- ggplot(data_long, aes(x = Lipids, y = Region, fill = Value)) + 
  geom_tile() +
  scale_fill_gradient2(
    low = "blue", 
    mid = "grey",
    high = "red", 
    midpoint = 0, # Define the midpoint of the gradient
    limits = c(-0.25, 0.25), # Setting the limits for the gradient scale
    breaks = c(-0.25, -0.05, 0.05, 0.25), # Defining the breaks on the scale
    name = "P-Value"
  ) +
  labs(x = 'Lipids', y = 'Brain Region') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle('Lipid Classes x Brain Regions for MCI Patients')

# Save the plot
ggsave("HeatMap.pdf", plot = p, width = 10, height = 15, limitsize = FALSE)