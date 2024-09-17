library(dplyr)
library(ggcorrplot)
library("FactoMineR")
library('corrr')
library(factoextra)
library(ggplot2)
library(ggfortify)

dataset <- read.csv("Statistics/test/TempDatasetPro10.csv")

############ PCA Analysis for all Subjects #####################

# filtered_data <- dataset %>%
#   filter(!(Group == "MCI" & Age > 70))
# print(nrow(filtered_data))

# table <- filtered_data[, 1046:1080]
table <- dataset[, 1046:1080]

############ PCA Analysis for AD Subjects #####################

# filtered_dataset <- dataset[dataset$Group == 'AD', ]
# table <- filtered_dataset[, 1046:1080]

############ PCA Analysis for CN Subjects #####################

# filtered_dataset <- dataset[dataset$Group == 'CN', ]
# table <- filtered_dataset[, 1046:1080]

############ PCA Analysis for MCI Subjects #####################

# values_to_filter <- c('MCI', 'LMCI', 'EMCI', 'SMC')
# filtered_dataset <- dataset[dataset$Group %in% values_to_filter, ]
# table <- filtered_dataset[, 1046:1080]

####################################################################
data.pca <- princomp(table)
data.pca$loadings[, 1:2]


# print(fviz_eig(data.pca, addlabels = TRUE))
# fviz_pca_var(data.pca, col.var="contrib")+
#  scale_color_gradient2(low="white", mid="blue",
#            high="red", midpoint=96) +
#  theme_minimal()
# print(fviz_cos2(data.pca, choice = "var", axes = 1:2))
# print(fviz_pca_var(data.pca, col.var = "cos2",
#             gradient.cols = c("black", "orange", "green"),
#             repel = TRUE))
p <- fviz_pca_ind(data.pca, label="none", habillage=dataset$Group,
        addEllipses=TRUE, ellipse.level=0.15, select.ind = list(cos2 =200)) + labs(title ="PCA", x = "PC1", y = "PC2")

# Create the PCA biplot and save to a file
# p <- fviz_pca_biplot(data.pca, 
#                 # Individuals
#                 geom.ind = "point",
#                 fill.ind = dataset$Group, col.ind = "black",
#                 pointshape = 21, pointsize = 2,
#                 palette = "jco",
#                 addEllipses = TRUE,
#                 ellipse.level=0.30,
#                 select.ind = list(cos2 = 100),
#                 select.var = list(contrib = 5),
#                 # Variables
#                 alpha.var ="contrib", col.var = "contrib",
#                 gradient.cols = "RdYlBu",
                
#                 legend.title = list(fill = "Groups", color = "Contrib",
#                                     alpha = "Contrib")
#                 )

###############################################################################################################
# Save the plot
# ggsave("PCA.pdf", plot = p, width = 10, height = 15, limitsize = FALSE)

# pca_result <- prcomp(table, center = TRUE, scale. = TRUE)

# pca_scores <- as.data.frame(pca_result$x)

# z_scores_pc1 <- abs(scale(pca_scores$PC1))
# z_scores_pc2 <- abs(scale(pca_scores$PC2))

# # Define a threshold for identifying outliers (e.g., Z-score > 3)
# outliers <- which(z_scores_pc1 > 3 | z_scores_pc2 > 3)

# Print the rows that are outliers
# print(nrow(table[outliers, ]))
# print(outliers)

#---------------------------------------------------------------------------------
#Com Labels
# responsible_column <- apply(table[outliers, ], 1, function(row) {
#   colnames(table)[which.max(abs(row))]
# })


# pca_scores$outlier <- "No"
# pca_scores$outlier[outliers] <- responsible_column


# p <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = outlier != "No")) +
#   geom_point(alpha = 0.7) +
#   geom_text(aes(label = ifelse(outlier != "No", outlier, "")), 
#             hjust = 0, vjust = 1, size = 3) +
#   scale_color_manual(values = c("FALSE" = "blue", "TRUE" = "red")) +
#   labs(title = "PCA - Outlier Detection with Responsible Column",
#        x = "Principal Component 1",
#        y = "Principal Component 2") +
#   theme_minimal() +
#   theme(legend.position = "none")

# print(p)
#---------------------------------------------------------------------------
#Sem labels
# pca_scores$outlier <- "No"
# pca_scores$outlier[outliers] <- "Yes"


# p <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = outlier)) +
#   geom_point(alpha = 0.7) +
#   scale_color_manual(values = c("No" = "blue", "Yes" = "red")) +
#   labs(title = "PCA - Outlier Detection",
#        x = "PC1",
#        y = "PC2") +
#   theme_minimal()

# print(p)

ggsave("Pics/test.pdf", plot = p, width = 10, height = 15, limitsize = FALSE)