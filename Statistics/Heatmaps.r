library(ggplot2)
library(reshape2)
library(heatmaply)

Table_LC_SR <- read.csv("Statistics/dataframes/Table_LC_SR.csv")
Table_LC_SR_AD <- read.csv("Statistics/dataframes/Table_LC_SR_AD.csv")
Table_LC_SR_CN <- read.csv("Statistics/dataframes/Table_LC_SR_CN.csv")
Table_LC_SR_MCI <- read.csv("Statistics/dataframes/Table_LC_SR_MCI.csv")
Table_LongFormat_LxV_AD <- read.csv("Statistics/dataframes/LongFormat_LxV_AD.csv")
Table_LongFormat_LxV_MCI <- read.csv("Statistics/dataframes/LongFormat_LxV_MCI.csv")
Table_LongFormat_LxV_CN <- read.csv("Statistics/dataframes/LongFormat_LxV_CN.csv")
Table_LongFormat_VxV_AD <- read.csv("Statistics/dataframes/LongFormat_VxV_AD.csv")
Table_LongFormat_VxV_MCI <- read.csv("Statistics/dataframes/LongFormat_VxV_MCI.csv")
Table_LongFormat_VxV_CN <- read.csv("Statistics/dataframes/LongFormat_VxV_CN.csv")

# print(max(Table_LongFormat_LxV_AD$r[Table_LongFormat_LxV_AD$p_value > 0.05], na.rm = TRUE))
# print(max(Table_LongFormat_LxV_MCI$r[Table_LongFormat_LxV_MCI$p_value > 0.05], na.rm = TRUE))
# print(max(Table_LongFormat_LxV_CN$r[Table_LongFormat_LxV_CN$p_value > 0.05], na.rm = TRUE))

# print(max(Table_LongFormat_VxV_AD$r[Table_LongFormat_VxV_AD$p_value > 0.05], na.rm = TRUE))
# print(max(Table_LongFormat_VxV_MCI$r[Table_LongFormat_VxV_MCI$p_value > 0.05], na.rm = TRUE))
# print(max(Table_LongFormat_VxV_CN$r[Table_LongFormat_VxV_CN$p_value > 0.05], na.rm = TRUE))

top_5_max_corr <- Table_LongFormat_LxV_AD[Table_LongFormat_LxV_AD$p_value > 0.05, ]
top_5_max_corr <- top_5_max_corr[order(abs(top_5_max_corr$r), decreasing = TRUE), ][1:5, ]

for (i in 1:nrow(top_5_max_corr)) {
  print(paste0(i, ". Correlation: ", format(top_5_max_corr$r[i], digits = 3), ", Lipids: ", top_5_max_corr$Lipids[i], ", Regions: ", top_5_max_corr$Regions[i]))
}


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

# data_long <- melt(Table_LC_SR_MCI, id.vars = 'Lipids', variable.name = 'Region', value.name = 'Value')
# Table_FC_AD$Value <- ifelse(data_long$Value > 0.05, 
#                               data_long$Value, 
#                               ifelse(data_long$Value < -0.05, 
#                                      data_long$Value, 
#                                      0))
# print(data_long)

#################################################################################################

# df_filtered <- subset(Table_LongFormat_LxV_CN, p_value >= 0.05)
# # print(df_filtered)
# ####################################################################################################

# # Create the plot with a color gradient
# # p <- ggplot(data_long, aes(x = Lipids, y = Region, fill = FoldChange)) + 
# #   geom_tile() +
# #   scale_fill_gradient2(
# #     low = "blue", 
# #     mid = "grey",
# #     high = "red", 
# #     midpoint = 0, # Define the midpoint of the gradient
# #     limits = c(-0.25, 0.25), # Setting the limits for the gradient scale
# #     breaks = c(-0.25, -0.05, 0.05, 0.25), # Defining the breaks on the scale
# #     name = "Fold Change"
# #   ) +
# #   labs(x = 'Lipids', y = 'Brain Region') +
# #   theme_minimal() +
# #   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
# #   ggtitle('Lipid Classes x Brain Regions for MCI Patients')


# p <- ggplot(df_filtered, aes(x = Regions1, y = Regions2, fill = r)) +
#   geom_tile() +                           
#   scale_fill_gradient2(low = "blue", mid = "grey", high = "yellow", 
#                        midpoint = 0, limits = c(-0.1, 0.1), 
#                        oob = scales::squish) + 
#   theme_bw() + 
#   labs(fill = "R") +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
#   ) +
#   ggtitle('Brain Regions x Brain Regions for CN Patients')

# # print(p)
# ggsave("Pics/LxV_CN_HeatMap.pdf", plot = p, width = 25, height = 15, limitsize = FALSE)