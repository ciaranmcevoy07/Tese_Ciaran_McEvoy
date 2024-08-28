library(dplyr)
library(ggcorrplot)
library("FactoMineR")
library('corrr')
library(factoextra)

dataset <- read.csv("Statistics/test/TempDatasetPro10.csv")

############ PCA Analysis for all Subjects #####################

filtered_data <- dataset %>%
  filter(!(Group == "MCI" & Age > 70))

table <- filtered_data[, 1046:1080]
# table <- dataset[, 1046:1080]

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
summary(data.pca)
data.pca$loadings[, 1:2]
# print(fviz_eig(data.pca, addlabels = TRUE))
fviz_pca_var(data.pca, col.var="contrib")+
 scale_color_gradient2(low="white", mid="blue",
           high="red", midpoint=96) +
 theme_minimal()
# print(fviz_cos2(data.pca, choice = "var", axes = 1:2))
# print(fviz_pca_var(data.pca, col.var = "cos2",
#             gradient.cols = c("black", "orange", "green"),
#             repel = TRUE))
print(fviz_pca_ind(data.pca, label="none", habillage=filtered_data$Group,
        addEllipses=TRUE, ellipse.level=0.30, select.ind = list(cos2 =100)) + labs(title ="PCA", x = "PC1", y = "PC2"))

# Create the PCA biplot and save to a file
# p <- fviz_pca_biplot(data.pca, 
#                 # Individuals
#                 geom.ind = "point",
#                 fill.ind = filtered_data$Group, col.ind = "black",
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

# Save the plot
# ggsave("PCA.pdf", plot = p, width = 10, height = 15, limitsize = FALSE)